namespace Prelude.Calculator.Dan

open System
open System.Collections.Generic
open Prelude
open Prelude.Charts

module RhythmProfile =

    let private STABILITY_THRESHOLD = 5.0
    let private BPM_CLUSTER_TOLERANCE = 5.0
    let private IMPORTANT_CLUSTER_RATIO = 0.5
    let private JACK_MIN_BPM = 90.0
    let private SHIELD_MAX_BEAT_RATIO = 0.25

    let private primary_weight (rhythm: string) : float =
        match rhythm with
        | "linear_stream" -> 1.0 / 3.0
        | "harmonic_flow" -> 0.65
        | "anchor_burst" -> 0.90
        | "coordination" -> 0.75
        | "density" -> 0.90
        | "wildcard" -> 1.0
        | _ -> 1.0

    let private texture_family (rhythm: string) : string option =
        match rhythm with
        | "linear_stream" -> Some "stream"
        | "harmonic_flow" -> Some "speed"
        | "anchor_burst" -> Some "jack"
        | "coordination" -> Some "tech"
        | "density" -> Some "stamina"
        | "wildcard" -> Some "tech"
        | _ -> None

    let private sub_texture_family (sub: string) : string option =
        match sub with
        | "rolls" -> Some "stream"
        | "trills"
        | "mini_trills" -> Some "tech"
        | "chord_stream" -> Some "speed"
        | "split_trill"
        | "jump_trill" -> Some "tech"
        | "jumpstream" -> Some "speed"
        | "handstream" -> Some "stamina"
        | "double_jump"
        | "triple_jump" -> Some "speed"
        | "quad_stream" -> Some "stamina"
        | "light_chords" -> Some "speed"
        | "dense_chords" -> Some "stamina"
        | "chord_roll"
        | "brackets" -> Some "tech"
        | "long_jacks"
        | "chord_jacks"
        | "mini_jacks"
        | "gluts" -> Some "jack"
        | "column_lock"
        | "shield"
        | "release" -> Some "tech"
        | "inverse" -> Some "tech"
        | "js_density" -> Some "speed"
        | "hs_density"
        | "ds_density"
        | "dcs_density" -> Some "stamina"
        | "lcs_density" -> Some "speed"
        | "jack_wild" -> Some "jack"
        | "speed_wild" -> Some "speed"
        | _ -> None

    let private hands_per_side (key_count: int) : int =
        match key_count with
        | 3 -> 2
        | 4 -> 2
        | 5 -> 3
        | 6 -> 3
        | 7 -> 4
        | 8 -> 4
        | 9 -> 5
        | 10 -> 5
        | _ -> max 1 (key_count / 2)

    let private is_same_hand (col_a: int) (col_b: int) (split: int) : bool =
        if abs (col_a - col_b) <> 1 then
            false
        else
            (col_a < split) = (col_b < split)

    type private Frame =
        {
            OffsetMs: float
            MsPerBeat: float
            BeatLength: float
            ActiveCount: int
            JackCount: int
            Direction: string
            IsRoll: bool
            KeyCount: int
            LeftKeys: int
            HeadCols: int list
            BodyCols: int list
            TailCols: int list
            TapCols: int list
            RawCols: int list
        }

    type private Texture =
        {
            Rhythm: string
            SubTexture: string option
            IsVolatile: bool
            StartMs: float
            EndMs: float
            MsPerBeat: float
        }

    type private Cluster =
        {
            AnchorMpb: float
            mutable SumMpb: float
            mutable Count: int
            mutable Bpm: int
        }

    type private Profile =
        {
            Rhythm: string
            DominantSub: string option
            Bpm: int
            IsVolatile: bool
            CoverageMs: float
            Importance: float
        }

    let private detect_flow (prev_cols: int list) (cur_cols: int list) : string * bool =
        if List.isEmpty prev_cols || List.isEmpty cur_cols then
            "none", false
        else
            let pl = List.head prev_cols
            let pr = List.last prev_cols
            let cl = List.head cur_cols
            let cr = List.last cur_cols
            let dl = cl - pl
            let dr = cr - pr

            let direction =
                if dl > 0 then
                    if dr > 0 then "right" else "inward"
                elif dl < 0 then
                    if dr < 0 then "left" else "outward"
                elif dr < 0 then
                    "inward"
                elif dr > 0 then
                    "outward"
                else
                    "none"

            let is_roll = pl > cr || pr < cl
            direction, is_roll

    let private build_beat_frames (rows: (float * NoteType[])[]) (key_count: int) (ms_per_beat: float) : Frame[] =
        if rows.Length = 0 then
            [||]
        else
            let first_time, first_data = rows.[0]
            let left_keys = hands_per_side key_count

            let mutable previous_cols =
                [
                    for k in 0 .. key_count - 1 do
                        if k < first_data.Length && (first_data.[k] = NoteType.NORMAL || first_data.[k] = NoteType.HOLDHEAD) then
                            yield k
                ]

            if List.isEmpty previous_cols then
                [||]
            else
                let mutable previous_time = first_time
                let frames = ResizeArray<Frame>()

                for i = 1 to rows.Length - 1 do
                    let t, row_data = rows.[i]

                    let cols_of (kind: NoteType) =
                        [
                            for k in 0 .. key_count - 1 do
                                if k < row_data.Length && row_data.[k] = kind then
                                    yield k
                        ]

                    let tap_cols = cols_of NoteType.NORMAL
                    let head_cols = cols_of NoteType.HOLDHEAD
                    let body_cols = cols_of NoteType.HOLDBODY
                    let tail_cols = cols_of NoteType.HOLDTAIL
                    let active_cols = tap_cols @ head_cols

                    if
                        not (
                            List.isEmpty active_cols
                            && List.isEmpty head_cols
                            && List.isEmpty body_cols
                            && List.isEmpty tail_cols
                        )
                    then
                        let mutable direction = "none"
                        let mutable is_roll = false
                        let mutable jacks = 0

                        if not (List.isEmpty active_cols) then
                            let d, r = detect_flow previous_cols active_cols
                            direction <- d
                            is_roll <- r
                            let prev_set = Set.ofList previous_cols
                            jacks <- active_cols |> List.filter (fun c -> prev_set.Contains c) |> List.length

                        frames.Add
                            {
                                OffsetMs = t - first_time
                                MsPerBeat = (t - previous_time) * 4.0
                                BeatLength = ms_per_beat
                                ActiveCount = List.length active_cols
                                JackCount = jacks
                                Direction = direction
                                IsRoll = is_roll
                                KeyCount = key_count
                                LeftKeys = left_keys
                                HeadCols = head_cols
                                BodyCols = body_cols
                                TailCols = tail_cols
                                TapCols = tap_cols
                                RawCols = active_cols
                            }

                        if not (List.isEmpty active_cols) then
                            previous_cols <- active_cols

                        previous_time <- t

                frames.ToArray()

    let private detect_stream (fr: Frame[]) (s: int) : int =
        if fr.Length - s < 5 then
            0
        else
            let a, b, c, d, e = fr.[s], fr.[s + 1], fr.[s + 2], fr.[s + 3], fr.[s + 4]

            if
                a.ActiveCount = 1
                && a.JackCount = 0
                && b.ActiveCount = 1
                && b.JackCount = 0
                && c.ActiveCount = 1
                && c.JackCount = 0
                && d.ActiveCount = 1
                && d.JackCount = 0
                && e.ActiveCount = 1
                && e.JackCount = 0
            then
                if List.head a.RawCols <> List.head e.RawCols then 5 else 0
            else
                0

    let private detect_anchor_burst (fr: Frame[]) (s: int) : int =
        if fr.Length - s < 1 then
            0
        else
            let f0 = fr.[s]
            if f0.JackCount > 1 && f0.MsPerBeat < 2000.0 then 1 else 0

    let private detect_harmonic_flow (fr: Frame[]) (s: int) : int =
        if fr.Length - s < 4 then
            0
        else
            let a, b, c, d = fr.[s], fr.[s + 1], fr.[s + 2], fr.[s + 3]

            if
                a.ActiveCount > 1
                && a.JackCount = 0
                && b.JackCount = 0
                && c.JackCount = 0
                && d.JackCount = 0
            then
                if b.ActiveCount > 1 || c.ActiveCount > 1 || d.ActiveCount > 1 then
                    4
                else
                    0
            else
                0

    let private detect_coordination (fr: Frame[]) (s: int) : int =
        if fr.Length - s < 1 then
            0
        else
            let a = fr.[s]

            if not (List.isEmpty a.HeadCols) || not (List.isEmpty a.BodyCols) || not (List.isEmpty a.TailCols) then
                1
            else
                0

    let private detect_density (fr: Frame[]) (s: int) : int =
        if fr.Length - s < 1 then
            0
        elif not (List.isEmpty fr.[s].HeadCols) then
            1
        else
            0

    let private detect_wildcard (fr: Frame[]) (s: int) : int = detect_density fr s

    let private sub_long_jacks (fr: Frame[]) (s: int) : int =
        if fr.Length - s < 5 then
            0
        else
            let a, b, c, d, e = fr.[s], fr.[s + 1], fr.[s + 2], fr.[s + 3], fr.[s + 4]

            if a.JackCount > 0 && b.JackCount > 0 && c.JackCount > 0 && d.JackCount > 0 && e.JackCount > 0 then
                let found =
                    a.RawCols
                    |> List.exists (fun col ->
                        List.contains col b.RawCols
                        && List.contains col c.RawCols
                        && List.contains col d.RawCols
                        && List.contains col e.RawCols
                    )

                if found then 5 else 0
            else
                0

    let private sub_chord_jacks (fr: Frame[]) (s: int) : int =
        if fr.Length - s < 2 then
            0
        else
            let a, b = fr.[s], fr.[s + 1]

            if a.ActiveCount > 2 && b.ActiveCount > 1 && b.JackCount >= 1 then
                if b.ActiveCount < a.ActiveCount || b.JackCount < b.ActiveCount then
                    2
                else
                    0
            else
                0

    let private sub_mini_jacks (fr: Frame[]) (s: int) : int =
        if fr.Length - s < 2 then
            0
        else
            let a, b = fr.[s], fr.[s + 1]
            if a.JackCount > 0 && b.JackCount = 0 then 2 else 0

    let private sub_handstream (fr: Frame[]) (s: int) : int =
        if fr.Length - s < 4 then
            0
        else
            let a, b, c, d = fr.[s], fr.[s + 1], fr.[s + 2], fr.[s + 3]

            if a.ActiveCount = 3 && a.JackCount = 0 && b.JackCount = 0 && c.JackCount = 0 && d.JackCount = 0 then
                4
            else
                0

    let private sub_jumpstream (fr: Frame[]) (s: int) : int =
        if fr.Length - s < 4 then
            0
        else
            let a, b, c, d = fr.[s], fr.[s + 1], fr.[s + 2], fr.[s + 3]

            if
                a.ActiveCount = 2
                && a.JackCount = 0
                && b.ActiveCount = 1
                && b.JackCount = 0
                && c.JackCount = 0
                && d.JackCount = 0
            then
                if c.ActiveCount < 3 && d.ActiveCount < 3 then 4 else 0
            else
                0

    let private sub_jump_trill (fr: Frame[]) (s: int) : int =
        if fr.Length - s < 4 then
            0
        else
            let a, b, c, d = fr.[s], fr.[s + 1], fr.[s + 2], fr.[s + 3]

            if
                a.ActiveCount = 2
                && b.ActiveCount = 2
                && c.ActiveCount = 2
                && d.ActiveCount = 2
                && b.IsRoll
                && c.IsRoll
                && d.IsRoll
            then
                4
            else
                0

    let private sub_split_trill (fr: Frame[]) (s: int) : int =
        if fr.Length - s < 3 then
            0
        else
            let a, b, c = fr.[s], fr.[s + 1], fr.[s + 2]

            if
                a.ActiveCount = 2
                && b.ActiveCount = 2
                && c.ActiveCount = 2
                && b.JackCount = 0
                && c.JackCount = 0
                && not b.IsRoll
                && not c.IsRoll
            then
                3
            else
                0

    let private sub_gluts (fr: Frame[]) (s: int) : int =
        if fr.Length - s < 3 then
            0
        else
            let a, b, c = fr.[s], fr.[s + 1], fr.[s + 2]

            if b.JackCount = 1 && c.JackCount = 1 then
                let shared = a.RawCols |> List.exists (fun col -> List.contains col b.RawCols && List.contains col c.RawCols)
                if shared then 0 else 3
            else
                0

    let private sub_quad_stream (fr: Frame[]) (s: int) : int =
        if fr.Length - s < 4 then
            0
        else
            let a, c, d = fr.[s], fr.[s + 2], fr.[s + 3]
            if a.ActiveCount = 4 && c.JackCount = 0 && d.JackCount = 0 then 4 else 0

    let private sub_roll (fr: Frame[]) (s: int) : int =
        if fr.Length - s < 3 then
            0
        else
            let a, b, c = fr.[s], fr.[s + 1], fr.[s + 2]

            if a.ActiveCount = 1 && b.ActiveCount = 1 && c.ActiveCount = 1 then
                let left = a.Direction = "left" && b.Direction = "left" && c.Direction = "left"
                let right = a.Direction = "right" && b.Direction = "right" && c.Direction = "right"
                if left || right then 3 else 0
            else
                0

    let private sub_trill (fr: Frame[]) (s: int) : int =
        if fr.Length - s < 4 then
            0
        else
            let a, b, c, d = fr.[s], fr.[s + 1], fr.[s + 2], fr.[s + 3]

            if b.JackCount = 0 && c.JackCount = 0 && d.JackCount = 0 then
                if a.RawCols = c.RawCols && b.RawCols = d.RawCols then 4 else 0
            else
                0

    let private sub_mini_trill (fr: Frame[]) (s: int) : int =
        if fr.Length - s < 4 then
            0
        else
            let a, b, c, d = fr.[s], fr.[s + 1], fr.[s + 2], fr.[s + 3]

            if b.JackCount = 0 && c.JackCount = 0 then
                if a.RawCols = c.RawCols && b.RawCols <> d.RawCols then 4 else 0
            else
                0

    let private sub_column_lock (fr: Frame[]) (s: int) : int =
        if fr.Length - s < 3 then
            0
        else
            let f0 = fr.[s]
            let split = f0.LeftKeys

            match f0.HeadCols with
            | [] -> 0
            | ln_col :: _ ->
                let adj_cols =
                    [ ln_col - 1; ln_col + 1 ]
                    |> List.filter (fun c -> 0 <= c && c < f0.KeyCount && is_same_hand ln_col c split)

                let check (adj: int) : bool =
                    let hits =
                        [
                            for i in 0 .. (min 8 (fr.Length - s)) - 1 do
                                let row = fr.[s + i]

                                if List.contains ln_col row.BodyCols && List.contains adj row.TapCols then
                                    yield row.OffsetMs
                        ]

                    if List.length hits < 3 then
                        false
                    else
                        let arr = List.toArray hits

                        let bpms =
                            [
                                for i in 0 .. arr.Length - 2 do
                                    yield (if arr.[i + 1] > arr.[i] then 15000.0 / (arr.[i + 1] - arr.[i]) else 90.0)
                            ]

                        (not (List.isEmpty bpms)) && List.max bpms >= JACK_MIN_BPM

                if List.isEmpty adj_cols then 0
                elif List.exists check adj_cols then 3
                else 0

    let private sub_shield (fr: Frame[]) (s: int) : int =
        if fr.Length - s < 2 then
            0
        else
            let a, b = fr.[s], fr.[s + 1]
            let dt = b.OffsetMs - a.OffsetMs
            let beat_limit = b.BeatLength * SHIELD_MAX_BEAT_RATIO

            if dt < 0.0 || dt > beat_limit then
                0
            elif a.TapCols |> List.exists (fun col -> List.contains col b.HeadCols) then
                2
            elif a.TailCols |> List.exists (fun col -> List.contains col b.TapCols) then
                2
            else
                0

    let private primary_detectors: (string * (Frame[] -> int -> int)) list =
        [
            "linear_stream", detect_stream
            "harmonic_flow", detect_harmonic_flow
            "anchor_burst", detect_anchor_burst
            "coordination", detect_coordination
            "density", detect_density
            "wildcard", detect_wildcard
        ]

    let private specific_4k (rhythm: string) : (string * (Frame[] -> int -> int)) list =
        match rhythm with
        | "linear_stream" -> [ "rolls", sub_roll; "trills", sub_trill; "mini_trills", sub_mini_trill ]
        | "harmonic_flow" ->
            [
                "handstream", sub_handstream
                "split_trill", sub_split_trill
                "jump_trill", sub_jump_trill
                "jumpstream", sub_jumpstream
            ]
        | "anchor_burst" ->
            [
                "long_jacks", sub_long_jacks
                "quad_stream", sub_quad_stream
                "gluts", sub_gluts
                "chord_jacks", sub_chord_jacks
                "mini_jacks", sub_mini_jacks
            ]
        | "coordination" -> [ "column_lock", sub_column_lock; "shield", sub_shield ]
        | _ -> []

    let private extract_textures (frames: Frame[]) (key_count: int) (total_ms: float) : Texture list =
        let results = ResizeArray<Texture>()

        for s = 0 to frames.Length - 1 do
            let remaining_len = frames.Length - s

            for rhythm_name, detector in primary_detectors do
                let n = detector frames s

                if n <> 0 then
                    let sub_list = if key_count = 4 then specific_4k rhythm_name else []
                    let mutable best_len = n
                    let mutable best_sub: string option = None

                    for sub_name, sub_fn in sub_list do
                        let m = sub_fn frames s

                        if m > best_len then
                            best_len <- m
                            best_sub <- Some sub_name

                    let matched_len = if best_len <= 0 then n else best_len
                    let seg_len = min matched_len remaining_len
                    let mutable sum_mpb = 0.0

                    for i = 0 to seg_len - 1 do
                        sum_mpb <- sum_mpb + frames.[s + i].MsPerBeat

                    let mean_mpb = sum_mpb / float seg_len
                    let mutable stable = true

                    for i = 0 to seg_len - 1 do
                        if not (abs (frames.[s + i].MsPerBeat - mean_mpb) < STABILITY_THRESHOLD) then
                            stable <- false

                    let end_ms =
                        if matched_len < remaining_len then
                            frames.[s + matched_len].OffsetMs
                        else
                            total_ms

                    results.Add
                        {
                            Rhythm = rhythm_name
                            SubTexture = best_sub
                            IsVolatile = not stable
                            StartMs = frames.[s].OffsetMs
                            EndMs = end_ms
                            MsPerBeat =
                                if rhythm_name = "density" && best_sub = Some "inverse" then
                                    0.0
                                else
                                    mean_mpb
                        }

        List.ofSeq results

    let private bundle_textures (textures: Texture list) : (Texture * Cluster) list =
        let stable_groups = ResizeArray<Cluster>()
        let volatile_groups = Dictionary<string, Cluster>()

        let add_stable (mpb: float) : Cluster =
            match stable_groups |> Seq.tryFind (fun g -> abs (g.AnchorMpb - mpb) < BPM_CLUSTER_TOLERANCE) with
            | Some g ->
                g.SumMpb <- g.SumMpb + mpb
                g.Count <- g.Count + 1
                g
            | None ->
                let g =
                    {
                        AnchorMpb = mpb
                        SumMpb = mpb
                        Count = 1
                        Bpm = 0
                    }

                stable_groups.Add g
                g

        let add_volatile (rhythm: string) (mpb: float) : Cluster =
            match volatile_groups.TryGetValue rhythm with
            | true, g ->
                g.SumMpb <- g.SumMpb + mpb
                g.Count <- g.Count + 1
                g
            | _ ->
                let g =
                    {
                        AnchorMpb = mpb
                        SumMpb = mpb
                        Count = 1
                        Bpm = 0
                    }

                volatile_groups.[rhythm] <- g
                g

        let pairs =
            textures
            |> List.map (fun tx ->
                let c =
                    if tx.IsVolatile then
                        add_volatile tx.Rhythm tx.MsPerBeat
                    else
                        add_stable tx.MsPerBeat

                tx, c
            )

        let finalize (g: Cluster) =
            let avg = g.SumMpb / float (max g.Count 1)
            g.Bpm <- if avg > 0.0 then int (Math.Round(60000.0 / avg)) else 0

        for g in stable_groups do
            finalize g

        for v in volatile_groups.Values do
            finalize v

        pairs

    let private profile_bundles (pairs: (Texture * Cluster) list) : Profile list =
        let order = ResizeArray<string * bool * int>()
        let groups = Dictionary<string * bool * int, ResizeArray<Texture>>()

        for tx, cluster in pairs do
            let key = tx.Rhythm, tx.IsVolatile, cluster.Bpm

            match groups.TryGetValue key with
            | true, items -> items.Add tx
            | _ ->
                let items = ResizeArray<Texture>()
                items.Add tx
                groups.[key] <- items
                order.Add key

        let profiles =
            [
                for key in order do
                    let rhythm, is_volatile, bpm = key
                    let items = groups.[key]

                    let starts_ends =
                        items |> Seq.map (fun it -> it.StartMs, it.EndMs) |> Seq.sort |> Seq.toArray

                    let mutable total_duration = 0.0

                    if starts_ends.Length > 0 then
                        let mutable cur_start, cur_end = starts_ends.[0]

                        for i = 1 to starts_ends.Length - 1 do
                            let st, e = starts_ends.[i]

                            if cur_end < e then
                                total_duration <- total_duration + (cur_end - cur_start)
                                cur_start <- st
                                cur_end <- e
                            else
                                cur_end <- max cur_end e

                        total_duration <- total_duration + (cur_end - cur_start)

                    let names = ResizeArray<string>()
                    let counts = Dictionary<string, int>()

                    for it in items do
                        match it.SubTexture with
                        | Some sub ->
                            match counts.TryGetValue sub with
                            | true, n -> counts.[sub] <- n + 1
                            | _ ->
                                counts.[sub] <- 1
                                names.Add sub
                        | None -> ()

                    let mutable dominant: string option = None
                    let mutable best = 0

                    for name in names do
                        if counts.[name] > best then
                            best <- counts.[name]
                            dominant <- Some name

                    let weight = primary_weight rhythm

                    yield
                        {
                            Rhythm = rhythm
                            DominantSub = dominant
                            Bpm = bpm
                            IsVolatile = is_volatile
                            CoverageMs = total_duration
                            Importance = total_duration * weight * float (max 1 bpm)
                        }
            ]

        profiles |> List.sortWith (fun a b -> compare b.Importance a.Importance)

    let private profile_rhythms
        (rows: (float * NoteType[])[])
        (key_count: int)
        (ms_per_beat: float)
        (total_ms: float)
        : Profile list =
        let frames = build_beat_frames rows key_count ms_per_beat

        if frames.Length = 0 then
            []
        else
            let true_total = if total_ms <> 0.0 then total_ms else 60000.0
            let textures = extract_textures frames key_count true_total
            let paired = bundle_textures textures
            let profiles = profile_bundles paired

            let filtered =
                profiles
                |> List.filter (fun p ->
                    not (
                        profiles
                        |> List.exists (fun other ->
                            other.Rhythm = p.Rhythm
                            && other.CoverageMs * 0.5 > p.CoverageMs
                            && other.Bpm > p.Bpm
                        )
                    )
                )

            let rhythm_counts = Dictionary<string, int>()

            let capped =
                filtered
                |> List.filter (fun p ->
                    let n =
                        match rhythm_counts.TryGetValue p.Rhythm with
                        | true, v -> v
                        | _ -> 0

                    if n < 3 then
                        rhythm_counts.[p.Rhythm] <- n + 1
                        true
                    else
                        false
                )

            capped |> List.sortWith (fun a b -> compare b.Importance a.Importance)

    let private profile_family (p: Profile) : string =
        match p.DominantSub |> Option.bind sub_texture_family with
        | Some f -> f
        | None -> texture_family p.Rhythm |> Option.defaultValue "hybrid"

    let private classify_texture_profile (profiles: Profile list) (bpm: float) (drain_s: float) : string * float =
        match profiles with
        | [] -> "hybrid", 0.0
        | first :: _ ->
            let top_importance = first.Importance

            let important =
                profiles
                |> List.filter (fun p -> p.Importance / max top_importance 1.0 > IMPORTANT_CLUSTER_RATIO)

            let primary =
                match important with
                | p :: _ -> p
                | [] -> first

            let secondary =
                match important with
                | _ :: s :: _ -> Some s
                | _ -> None

            let mutable primary_family = profile_family primary

            let is_hybrid =
                match secondary with
                | Some sec when sec.DominantSub.IsSome ->
                    let sec_family =
                        match sub_texture_family sec.DominantSub.Value with
                        | Some f -> f
                        | None -> texture_family sec.Rhythm |> Option.defaultValue "hybrid"

                    sec_family <> primary_family
                | _ -> false

            let mutable is_tech = primary.IsVolatile || (is_hybrid && primary.Bpm < 150)

            match primary.DominantSub with
            | Some("trills" | "mini_trills" | "split_trill" | "jump_trill" | "chord_roll" | "brackets" | "column_lock" | "shield" | "inverse") ->
                is_tech <- true
            | _ -> ()

            let mutable family =
                if is_hybrid && is_tech then "tech"
                elif is_hybrid then "hybrid"
                elif is_tech then "tech"
                else primary_family

            if (family = "speed" || family = "stream") && bpm > 200.0 && drain_s > 100.0 then
                family <- "stamina"

                if primary_family = "speed" || primary_family = "stream" then
                    primary_family <- "stamina"

            let total_importance =
                (profiles |> List.truncate 5 |> List.sumBy (fun p -> p.Importance)) + 1.0

            let confidence = min 1.0 (Math.Round(primary.Importance / total_importance, 2))
            family, confidence

    let classify (rate: Rate, note_data: NoteData, bpm: float) : string * float =
        let key_count = note_data.Keys
        let rate_f = float (rate / 1.0f<rate>)
        let time_map = Dictionary<float, NoteType[]>()

        for { Time = time; Data = row } in note_data.Notes do
            let t = Adapter.scale_time_ms (float (time / 1.0f<ms>), rate_f)

            for k = 0 to key_count - 1 do
                let nt = row.[k]

                if nt = NoteType.NORMAL || nt = NoteType.HOLDHEAD || nt = NoteType.HOLDBODY || nt = NoteType.HOLDTAIL then
                    let data =
                        match time_map.TryGetValue t with
                        | true, d -> d
                        | _ ->
                            let d = Array.create key_count NoteType.NOTHING
                            time_map.[t] <- d
                            d

                    data.[k] <- nt

        if time_map.Count = 0 then
            "hybrid", 0.0
        else
            let rows =
                time_map
                |> Seq.map (fun kv -> kv.Key, kv.Value)
                |> Seq.sortBy fst
                |> Seq.toArray

            let first_time = fst rows.[0]
            let last_time = fst rows.[rows.Length - 1]
            let span = last_time - first_time
            let total_ms = if span <> 0.0 then span else 60000.0
            let drain_s = if span <> 0.0 then span / 1000.0 else 0.0
            let ms_per_beat = if bpm > 0.0 then 60000.0 / bpm else 500.0
            let profiles = profile_rhythms rows key_count ms_per_beat total_ms
            classify_texture_profile profiles bpm drain_s
