namespace Prelude.Calculator.Dan

open System
open System.Text.Json
open Prelude
open Prelude.Charts

module LnCourse =

    let private STAGE_KEYS =
        [|
            "1st"
            "2nd"
            "3rd"
            "4th"
            "5th"
            "6th"
            "7th"
            "8th"
            "9th"
            "10th"
            "Yoake"
            "Yuugure"
            "Yoru"
            "Yami"
            "Yume"
            "Yokaze"
        |]

    let private profiles = lazy (Config.load_json "ln_course_profiles.json")

    let private stage_sr_means (family_key: string) : (float * int) list =
        match profiles.Value with
        | None -> []
        | Some doc ->
            let root = doc.RootElement

            let family_data =
                match root.TryGetProperty(family_key) with
                | true, f -> Some f
                | _ ->
                    match root.TryGetProperty("global") with
                    | true, g -> Some g
                    | _ ->
                        match root.TryGetProperty("allround") with
                        | true, a -> Some a
                        | _ -> None

            match family_data with
            | None -> []
            | Some family_data ->
                let mutable items = []

                for i = 0 to STAGE_KEYS.Length - 1 do
                    let key = STAGE_KEYS.[i]

                    match family_data.TryGetProperty(key) with
                    | true, slot ->
                        match Config.try_get_float (slot, "sr_mean") with
                        | Some sr when sr > 0.0 -> items <- (sr, i + 1) :: items
                        | _ -> ()
                    | _ -> ()

                let ordered = items |> List.rev |> List.sortBy snd
                let mutable prev = -infinity

                [
                    for sr, dp in ordered do
                        let sr' = if sr <= prev then prev + 0.001 else sr
                        prev <- sr'
                        yield sr', dp
                ]

    let private boundaries (ruler: (float * int) list) : (float * float * int) list =
        let means = ruler |> List.map fst |> Array.ofList
        let dps = ruler |> List.map snd |> Array.ofList
        let n = means.Length

        if n = 0 then
            []
        else
            [
                for i = 0 to n - 1 do
                    let lower =
                        if i > 0 then
                            (means.[i - 1] + means.[i]) / 2.0
                        else
                            let gap = if n > 1 then means.[1] - means.[0] else 1.0
                            means.[0] - gap / 2.0

                    let upper =
                        if i < n - 1 then
                            (means.[i] + means.[i + 1]) / 2.0
                        else
                            let gap = if n > 1 then means.[n - 1] - means.[n - 2] else 1.0
                            means.[n - 1] + gap / 2.0

                    yield lower, upper, dps.[i]
            ]

    let private sr_to_dp_ln (sr: float, family_key: string) : float option =
        let ruler = stage_sr_means family_key
        let bounds = boundaries ruler

        if bounds.IsEmpty then
            None
        else
            let first_lower, _, first_dp = bounds.[0]
            let _, last_upper, last_dp = bounds.[bounds.Length - 1]

            if sr < first_lower then
                Some(max 0.5 (float first_dp))
            elif sr >= last_upper then
                Some(min 16.99 (float last_dp + 0.99))
            else
                let mutable result = float last_dp

                for lower, upper, dp_int in bounds do
                    if lower <= sr && sr < upper then
                        let t = (sr - lower) / max (upper - lower) 1e-6
                        result <- float dp_int + t

                Some result

    let private regression_dp (dp_sr: float, feats: Features.LnFeatures) : float option =
        match profiles.Value with
        | None -> None
        | Some doc ->
            match doc.RootElement.TryGetProperty("regression") with
            | true, reg ->
                match reg.TryGetProperty("coefs"), reg.TryGetProperty("feature_order") with
                | (true, coefs_el), (true, order_el) when
                    coefs_el.ValueKind = JsonValueKind.Array
                    && order_el.ValueKind = JsonValueKind.Array
                    ->
                    let coefs = [| for c in coefs_el.EnumerateArray() -> c.GetDouble() |]
                    let order = [| for o in order_el.EnumerateArray() -> o.GetString() |]

                    if coefs.Length <> order.Length then
                        None
                    else
                        let feat_vals =
                            Map.ofList
                                [
                                    "dp_sr", dp_sr
                                    "hold_occupancy", feats.HoldOccupancy
                                    "simultaneous_hold", feats.SimultaneousHold
                                    "release_density", feats.ReleaseDensity
                                    "ln_duration_cv", feats.LnDurationCv
                                    "dp_sr_x_hold_occ", dp_sr * feats.HoldOccupancy
                                    "bias", 1.0
                                ]

                        let mutable sum = 0.0

                        for i = 0 to coefs.Length - 1 do
                            let v = feat_vals |> Map.tryFind order.[i] |> Option.defaultValue 0.0
                            sum <- sum + coefs.[i] * v

                        Some(max 1.0 (min 16.99 sum))
                | _ -> None
            | _ -> None

    let analyze (rate: Rate, note_data: NoteData, sunny: Sunny.Result, msd: MsdScores option) : float * DanTier * DanSublevel =
        let feats = Features.extract_ln (rate, note_data)

        let dp_sr_opt =
            match sr_to_dp_ln (sunny.Sr, "global") with
            | Some v -> Some v
            | None -> sr_to_dp_ln (sunny.Sr, "allround")

        let dp =
            match dp_sr_opt with
            | Some dp_sr ->
                match regression_dp (dp_sr, feats) with
                | Some v -> v
                | None -> dp_sr
            | None ->
                match msd with
                | Some scores -> max 1.0 (min 16.99 (Msd.overall_msd scores / 3.2))
                | None -> max 1.0 (min 16.99 (sunny.Sr * 1.1))

        let dp = Math.Round(dp, 3)
        let tier = Format.ln_tier_from_dp dp
        let sub = Format.sublevel_from_dp dp
        dp, tier, sub
