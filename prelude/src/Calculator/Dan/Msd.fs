namespace Prelude.Calculator.Dan

open System
open System.Diagnostics
open System.Globalization
open System.IO
open System.Reflection
open System.Text
open System.Text.Json
open Prelude
open Prelude.Charts

type MsdScores =
    {
        Overall: float
        Stream: float
        Jumpstream: float
        Handstream: float
        Stamina: float
        Jackspeed: float
        Chordjack: float
        Technical: float
    }

module Msd =

    let mutable private exe_path: string option = None
    let mutable private last_error: string = "not configured"

    let last_failure () : string = last_error

    let private set_error (msg: string) : unit = last_error <- msg

    let configure (path: string) : unit =
        if File.Exists path then
            exe_path <- Some path
            last_error <- ""
        else
            exe_path <- None
            set_error (sprintf "configure missing: %s" path)

    let private candidate_paths () : string array =
        let base_dir = AppContext.BaseDirectory
        let asm_dir =
            try
                let loc = Assembly.GetExecutingAssembly().Location

                if String.IsNullOrWhiteSpace loc then
                    base_dir
                else
                    Path.GetDirectoryName loc
            with _ ->
                base_dir

        [|
            match exe_path with
            | Some p -> yield p
            | None -> ()
            yield Path.Combine(base_dir, "DanCalc", "msd.exe")
            yield Path.Combine(base_dir, "msd.exe")
            yield Path.Combine(asm_dir, "DanCalc", "msd.exe")
            yield Path.Combine(asm_dir, "msd.exe")
        |]
        |> Array.distinct

    let private find_exe () : string option =
        match candidate_paths () |> Array.tryFind File.Exists with
        | Some p ->
            exe_path <- Some p
            last_error <- ""
            Some p
        | None ->
            set_error (sprintf "msd.exe not found; tried: %s" (String.Join(" | ", candidate_paths ())))
            None

    let is_available () : bool = find_exe().IsSome

    let private build_rows (rate: Rate, note_data: NoteData) : string option =
        if note_data.Keys <> 4 && note_data.Keys <> 7 then
            set_error (sprintf "unsupported keys=%i" note_data.Keys)
            None
        else
            let rate_f = float (rate / 1.0f<rate>)
            let rows = System.Collections.Generic.SortedDictionary<float, int>()

            for { Time = time; Data = row } in note_data.Notes do
                let t = Adapter.scale_time_s_msd (float (time / 1.0f<ms>), rate_f)
                let mutable mask = 0

                for k = 0 to note_data.Keys - 1 do
                    if row.[k] = NoteType.NORMAL || row.[k] = NoteType.HOLDHEAD then
                        mask <- mask ||| (1 <<< k)

                if mask <> 0 then
                    let existing = if rows.ContainsKey t then rows.[t] else 0
                    rows.[t] <- existing ||| mask

            if rows.Count = 0 then
                set_error "no note rows for msd"
                None
            else
                let sb = StringBuilder()
                sb.Append('[') |> ignore
                let mutable first = true

                for KeyValue(t, notes) in rows do
                    if not first then
                        sb.Append(',') |> ignore

                    first <- false
                    sb.AppendFormat("{{\"notes\":{0},\"time\":{1}}}", notes, t.ToString(CultureInfo.InvariantCulture))
                    |> ignore

                sb.Append(']') |> ignore
                Some(sb.ToString())

    let private parse_scores (json: string) : MsdScores option =
        try
            use doc = JsonDocument.Parse(json)
            let root = doc.RootElement

            match root.TryGetProperty("error") with
            | true, _ ->
                set_error (sprintf "msd error json: %s" (json.Substring(0, min 200 json.Length)))
                None
            | _ ->
                let get (name: string) =
                    match root.TryGetProperty(name) with
                    | true, p when p.ValueKind = JsonValueKind.Number -> p.GetDouble()
                    | _ ->
                        match root.TryGetProperty(name.ToLowerInvariant()) with
                        | true, p when p.ValueKind = JsonValueKind.Number -> p.GetDouble()
                        | _ -> 0.0

                let scores =
                    {
                        Overall = get "overall"
                        Stream = get "stream"
                        Jumpstream = get "jumpstream"
                        Handstream = get "handstream"
                        Stamina = get "stamina"
                        Jackspeed = get "jackspeed"
                        Chordjack = get "chordjack"
                        Technical = get "technical"
                    }

                if
                    scores.Overall = 0.0
                    && scores.Stream = 0.0
                    && scores.Jumpstream = 0.0
                    && scores.Technical = 0.0
                then
                    set_error (sprintf "msd empty scores: %s" (json.Substring(0, min 200 json.Length)))
                    None
                else
                    last_error <- ""
                    Some scores
        with ex ->
            set_error (sprintf "msd parse: %s" ex.Message)
            None

    let private calc_native (rate: Rate, note_data: NoteData) : MsdScores option =
        match find_exe (), build_rows (rate, note_data) with
        | Some exe, Some input ->
            try
                let utf8 = UTF8Encoding(false)

                use proc =
                    new Process(
                        StartInfo =
                            ProcessStartInfo(
                                FileName = exe,
                                WorkingDirectory = Path.GetDirectoryName exe,
                                RedirectStandardInput = true,
                                RedirectStandardOutput = true,
                                RedirectStandardError = true,
                                UseShellExecute = false,
                                CreateNoWindow = true,
                                StandardOutputEncoding = utf8,
                                StandardErrorEncoding = utf8
                            )
                    )

                if not (proc.Start()) then
                    set_error "msd process failed to start"
                    None
                else
                    let bytes = utf8.GetBytes(input)
                    proc.StandardInput.BaseStream.Write(bytes, 0, bytes.Length)
                    proc.StandardInput.BaseStream.Flush()
                    proc.StandardInput.Close()

                    let stdout = proc.StandardOutput.ReadToEnd()
                    let stderr = proc.StandardError.ReadToEnd()

                    if not (proc.WaitForExit(30000)) then
                        try
                            proc.Kill(true)
                        with _ ->
                            ()

                        set_error "msd timed out"
                        None
                    elif proc.ExitCode <> 0 then
                        set_error (sprintf "msd exit=%i stderr=%s" proc.ExitCode (stderr.Trim().Substring(0, min 160 (stderr.Trim().Length))))
                        None
                    elif String.IsNullOrWhiteSpace stdout then
                        set_error (sprintf "msd empty stdout stderr=%s" (stderr.Trim()))
                        None
                    else
                        parse_scores stdout
            with ex ->
                set_error (sprintf "msd exception: %s" ex.Message)
                None
        | None, _ -> None
        | _, None -> None

    let calculate (rate: Rate, note_data: NoteData) : MsdScores option =
        let r = Math.Round(float (rate / 1.0f<rate>), 4)
        let native = [| 0.75; 1.0; 1.5 |]

        if Array.contains r native then
            calc_native (rate, note_data)
        else
            let lo, hi, t =
                if r < native.[0] then
                    native.[0], native.[1], (r - native.[0]) / (native.[1] - native.[0])
                elif r > native.[2] then
                    native.[1], native.[2], 1.0 + (r - native.[2]) / (native.[2] - native.[1])
                elif r <= native.[1] then
                    native.[0], native.[1], (r - native.[0]) / (native.[1] - native.[0])
                else
                    native.[1], native.[2], (r - native.[1]) / (native.[2] - native.[1])

            match calc_native (float32 lo * 1.0f<rate>, note_data), calc_native (float32 hi * 1.0f<rate>, note_data) with
            | Some a, Some b ->
                let lerp (x: float, y: float) = max 0.0 (x + t * (y - x))

                Some
                    {
                        Overall = lerp (a.Overall, b.Overall)
                        Stream = lerp (a.Stream, b.Stream)
                        Jumpstream = lerp (a.Jumpstream, b.Jumpstream)
                        Handstream = lerp (a.Handstream, b.Handstream)
                        Stamina = lerp (a.Stamina, b.Stamina)
                        Jackspeed = lerp (a.Jackspeed, b.Jackspeed)
                        Chordjack = lerp (a.Chordjack, b.Chordjack)
                        Technical = lerp (a.Technical, b.Technical)
                    }
            | _ -> None

    let overall_msd (scores: MsdScores) : float =
        max scores.Overall (
            max scores.Stream (
                max scores.Jumpstream (
                    max scores.Handstream (
                        max scores.Stamina (max scores.Jackspeed (max scores.Chordjack scores.Technical))
                    )
                )
            )
        )
