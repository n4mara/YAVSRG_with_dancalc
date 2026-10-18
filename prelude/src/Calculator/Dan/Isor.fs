namespace Prelude.Calculator.Dan

open System
open System.Collections.Generic
open Prelude
open Prelude.Charts

module Isor =

    let private DAN_ORDER = DanOrder.RICE_KEYS

    let private SKILLSET_SR_MEANS =
        Map.ofList
            [
                "jack",
                Map.ofList
                    [
                        "1st", 2.34; "2nd", 2.35; "3rd", 3.17; "4th", 3.48; "5th", 3.98
                        "6th", 4.75; "7th", 4.97; "8th", 5.84; "9th", 5.85; "10th", 6.30
                        "Alpha", 6.45; "Beta", 6.78; "Gamma", 6.95; "Delta", 7.85; "Epsilon", 8.76
                        "Zeta", 9.35; "Eta", 10.38; "Theta", 10.96; "Iota", 12.27; "Kappa", 13.13
                    ]
                "speed",
                Map.ofList
                    [
                        "1st", 2.94; "2nd", 3.50; "3rd", 3.78; "4th", 4.16; "5th", 4.86
                        "6th", 5.10; "7th", 5.25; "8th", 5.45; "9th", 5.70; "10th", 6.00
                        "Alpha", 6.55; "Beta", 6.80; "Gamma", 7.45; "Delta", 7.76; "Epsilon", 9.16
                        "Zeta", 9.55; "Eta", 10.05; "Theta", 10.67; "Iota", 11.16; "Kappa", 12.05
                    ]
                "stamina",
                Map.ofList
                    [
                        "1st", 3.38; "2nd", 3.48; "3rd", 3.79; "4th", 4.69; "5th", 5.23
                        "6th", 5.65; "7th", 5.75; "8th", 6.15; "9th", 6.26; "10th", 6.45
                        "Alpha", 6.58; "Beta", 6.88; "Gamma", 7.35; "Delta", 7.85; "Epsilon", 8.92
                        "Zeta", 9.60; "Eta", 9.96; "Theta", 10.81; "Iota", 11.66; "Kappa", 12.41
                    ]
                "tech",
                Map.ofList
                    [
                        "1st", 2.84; "2nd", 3.08; "3rd", 3.09; "4th", 3.90; "5th", 4.18
                        "6th", 4.50; "7th", 5.43; "8th", 5.69; "9th", 6.31; "10th", 6.55
                        "Alpha", 6.62; "Beta", 6.85; "Gamma", 7.30; "Delta", 8.17; "Epsilon", 9.22
                        "Zeta", 9.60; "Eta", 10.25; "Theta", 10.64; "Iota", 11.69; "Kappa", 12.10
                    ]
                "general",
                Map.ofList
                    [
                        "1st", 2.94; "2nd", 3.23; "3rd", 3.51; "4th", 4.16; "5th", 4.71
                        "6th", 5.12; "7th", 5.36; "8th", 5.80; "9th", 6.00; "10th", 6.25
                        "Alpha", 6.50; "Beta", 6.80; "Gamma", 7.35; "Delta", 7.85; "Epsilon", 9.00
                        "Zeta", 9.40; "Eta", 10.13; "Theta", 10.74; "Iota", 11.68; "Kappa", 12.25
                    ]
            ]

    let private isotonic_monotone (values: float array) : float array =
        let blocks = ResizeArray<ResizeArray<float>>()

        for v in values do
            blocks.Add(ResizeArray([ v ]))

        let mutable changed = true

        while changed do
            changed <- false
            let mutable i = 0

            while i < blocks.Count - 1 do
                let mean_i = Seq.average blocks.[i]
                let mean_j = Seq.average blocks.[i + 1]

                if mean_i > mean_j then
                    blocks.[i].AddRange(blocks.[i + 1])
                    blocks.RemoveAt(i + 1)
                    changed <- true
                else
                    i <- i + 1

        let flat = ResizeArray<float>()

        for b in blocks do
            let m = Seq.average b

            for _ in b do
                flat.Add(m)

        let out = ResizeArray<float>()

        for v in flat do
            if out.Count > 0 && v <= out.[out.Count - 1] then
                out.Add(out.[out.Count - 1] + 0.005)
            else
                out.Add(v)

        out.ToArray()

    let private sanitize_ruler (means: Map<string, float>) : Map<string, float> =
        let values =
            [|
                for d in DAN_ORDER do
                    yield means |> Map.tryFind d |> Option.defaultValue 0.0
            |]

        let mono = isotonic_monotone values

        [
            for i = 0 to DAN_ORDER.Length - 1 do
                yield DAN_ORDER.[i], Math.Round(mono.[i], 3)
        ]
        |> Map.ofList

    let private SKILLSET_CHOKE_MEANS =
        [
            "speed",
            [
                "1st", 11.5; "2nd", 13.0; "3rd", 14.5; "4th", 16.0; "5th", 18.0
                "6th", 18.5; "7th", 19.2; "8th", 19.8; "9th", 20.4; "10th", 21.0
                "Alpha", 25.2; "Beta", 26.5; "Gamma", 28.0; "Delta", 30.0; "Epsilon", 31.8
                "Zeta", 35.0; "Eta", 36.5; "Theta", 41.5; "Iota", 45.0; "Kappa", 49.0
            ]
            "stamina",
            [
                "1st", 12.9; "2nd", 15.4; "3rd", 15.5; "4th", 17.8; "5th", 23.0
                "6th", 20.6; "7th", 22.3; "8th", 23.5; "9th", 24.5; "10th", 24.0
                "Alpha", 24.4; "Beta", 25.5; "Gamma", 28.5; "Delta", 31.4; "Epsilon", 34.0
                "Zeta", 38.6; "Eta", 42.7; "Theta", 40.3; "Iota", 45.9; "Kappa", 50.0
            ]
            "jack",
            [
                "1st", 10.5; "2nd", 12.0; "3rd", 10.5; "4th", 13.1; "5th", 14.3
                "6th", 14.4; "7th", 17.9; "8th", 19.5; "9th", 20.5; "10th", 24.8
                "Alpha", 24.8; "Beta", 26.5; "Gamma", 27.2; "Delta", 31.0; "Epsilon", 34.0
                "Zeta", 35.2; "Eta", 39.1; "Theta", 45.0; "Iota", 49.3; "Kappa", 53.7
            ]
            "tech",
            [
                "1st", 10.5; "2nd", 12.0; "3rd", 10.5; "4th", 13.1; "5th", 14.3
                "6th", 14.4; "7th", 17.9; "8th", 19.5; "9th", 20.5; "10th", 21.8
                "Alpha", 22.5; "Beta", 23.8; "Gamma", 26.0; "Delta", 26.8; "Epsilon", 29.8
                "Zeta", 36.0; "Eta", 39.2; "Theta", 39.3; "Iota", 42.4; "Kappa", 46.6
            ]
            "general",
            [
                "1st", 11.5; "2nd", 13.5; "3rd", 14.5; "4th", 16.0; "5th", 18.5
                "6th", 19.5; "7th", 20.5; "8th", 21.5; "9th", 22.5; "10th", 22.8
                "Alpha", 24.2; "Beta", 25.0; "Gamma", 27.0; "Delta", 30.0; "Epsilon", 32.5
                "Zeta", 35.5; "Eta", 38.5; "Theta", 42.0; "Iota", 46.0; "Kappa", 50.5
            ]
        ]
        |> List.map (fun (sk, pairs) -> sk, sanitize_ruler (Map.ofList pairs))
        |> Map.ofList

    let private MSD_MEANS =
        Map.ofList
            [
                "1st", 13.5; "2nd", 15.5; "3rd", 16.8; "4th", 18.5; "5th", 20.5
                "6th", 22.0; "7th", 23.5; "8th", 24.8; "9th", 25.8; "10th", 26.8
                "Alpha", 27.5; "Beta", 28.5; "Gamma", 30.5; "Delta", 33.0; "Epsilon", 36.0
                "Zeta", 37.8; "Eta", 40.5; "Theta", 43.5; "Iota", 46.5; "Kappa", 50.0
            ]

    let private BPM_GAIN = 0.06
    let private BPM_PURITY_K = 20.0
    let private BPM_PURITY_X0 = 0.85
    let private BPM_K = 0.08
    let private BPM_X0 = 200.0
    let private CHORD_GAIN = 0.35
    let private CHORD_K = 25.0
    let private CHORD_X0 = 0.12
    let private BURST_GAIN = 0.08
    let private JACK2_GAIN = 0.05
    let private DAMP_GAIN = 0.20
    let private STREAM_STAM_THR = 0.35
    let private HYBRID_STAM_THR = 0.35
    let private STAM_FAM_THR = 0.25
    let private JD_JACK_THR = 0.15
    let private MARA_CAP = 0.08
    let private MARA_DUR_K = 0.02
    let private MARA_DUR_X0 = 200.0
    let private MARA_CV_REF = 0.4
    let private FATIGUE_TOTAL_CAP = 0.12
    type ChokeInfo =
        {
            Choke10sNps: float
            MeanNps: float
            BurstRatio: float
            MinColDeltaMs: float
            JackBurstDensity: float
        }

    let private sigmoid (x: float, k: float, x0: float) : float =
        let z = max -50.0 (min 50.0 (k * (x - x0)))
        1.0 / (1.0 + exp -z)

    let private interpolate_ruler (value: float, means: Map<string, float>) : float =
        let vals =
            [|
                for d in DAN_ORDER do
                    yield means |> Map.tryFind d |> Option.defaultValue 0.0
            |]

        let n = vals.Length

        let boundaries =
            [|
                for i = 0 to n - 1 do
                    let lo =
                        if i > 0 then
                            (vals.[i - 1] + vals.[i]) / 2.0
                        else
                            vals.[0] - (vals.[1] - vals.[0]) / 2.0

                    let hi =
                        if i < n - 1 then
                            (vals.[i] + vals.[i + 1]) / 2.0
                        else
                            vals.[n - 1] + (vals.[n - 1] - vals.[n - 2]) / 2.0

                    yield lo, hi, float (i + 1)
            |]

        let lo0, _, _ = boundaries.[0]
        let _, hi_last, _ = boundaries.[boundaries.Length - 1]

        if value < lo0 then
            let t = max 0.0 ((value - (lo0 - 1.0)) / 1.0)
            1.0 + t * 0.20
        elif value >= hi_last then
            20.99
        else
            let mutable result = 20.0

            for lo, hi, dp_base in boundaries do
                if lo <= value && value < hi then
                    let t = (value - lo) / max (hi - lo) 1e-6
                    result <- dp_base + t

            result

    let extract_choke (note_data: NoteData) : ChokeInfo =
        let notes =
            [|
                for { Time = time; Data = row } in note_data.Notes do
                    let t = float (time / 1.0f<ms>)

                    for k = 0 to note_data.Keys - 1 do
                        if row.[k] = NoteType.NORMAL || row.[k] = NoteType.HOLDHEAD then
                            yield t, k
            |]
            |> Array.sortBy fst

        if notes.Length = 0 then
            {
                Choke10sNps = 0.0
                MeanNps = 0.0
                BurstRatio = 1.0
                MinColDeltaMs = 9999.0
                JackBurstDensity = 0.0
            }
        else
            let times = notes |> Array.map fst
            let t_min = times.[0]
            let t_max = times.[times.Length - 1]
            let drain_s = max 1.0 ((t_max - t_min) / 1000.0)
            let mean_nps = float notes.Length / drain_s
            let window_ms = 10000.0
            let stride_ms = 250.0
            let mutable max_notes = 0
            let mutable left_idx = 0
            let mutable right_idx = 0
            let mutable t = t_min

            while t <= t_max do
                let lo = t
                let hi = t + window_ms

                while left_idx < times.Length && times.[left_idx] < lo do
                    left_idx <- left_idx + 1

                while right_idx < times.Length && times.[right_idx] <= hi do
                    right_idx <- right_idx + 1

                max_notes <- max max_notes (right_idx - left_idx)
                t <- t + stride_ms

            let choke = float max_notes / (window_ms / 1000.0)
            let burst = choke / max mean_nps 1.0

            let by_col = Dictionary<int, ResizeArray<float>>()

            for tm, c in notes do
                let mutable list = Unchecked.defaultof<_>

                if not (by_col.TryGetValue(c, &list)) then
                    list <- ResizeArray()
                    by_col.[c] <- list

                list.Add(tm)

            let mutable min_delta = 9999.0
            let mutable jack_burst = 0
            let mutable pairs = 0

            for KeyValue(_, col_times) in by_col do
                let sorted = col_times.ToArray() |> Array.sort

                for i = 1 to sorted.Length - 1 do
                    let dt = sorted.[i] - sorted.[i - 1]
                    pairs <- pairs + 1
                    min_delta <- min min_delta dt

                    if dt <= 120.0 then
                        jack_burst <- jack_burst + 1

            {
                Choke10sNps = Math.Round(choke, 2)
                MeanNps = Math.Round(mean_nps, 2)
                BurstRatio = Math.Round(burst, 3)
                MinColDeltaMs = Math.Round(min_delta, 1)
                JackBurstDensity = Math.Round(float jack_burst / float (max notes.Length 1), 3)
            }

    let private skillset_key
        (family: string, jbar_max: float, jump_ratio: float, jack_density: float)
        : string =
        let fam = family.ToLowerInvariant()

        if fam.Contains("jack") || jbar_max > 55.0 then
            "jack"
        elif fam.Contains("tech") || fam.Contains("ln") then
            "tech"
        elif jack_density >= JD_JACK_THR then
            "jack"
        elif fam = "stream" || fam = "speed" then
            if jump_ratio >= STREAM_STAM_THR then "stamina" else "speed"
        elif fam = "chordstream" || fam = "stamina" then
            if jump_ratio >= STAM_FAM_THR then "stamina" else "speed"
        else if jump_ratio >= HYBRID_STAM_THR then
            "stamina"
        else
            "general"

    let private top_scale_gate (base_dp: float, dp_sr: float) : float =
        let gate_lo, gate_hi, top_target = 18.8, 19.2, 20.2

        if base_dp <= gate_lo then
            base_dp
        else
            let mutable gt = min 1.0 ((base_dp - gate_lo) / (gate_hi - gate_lo))
            gt <- 0.5 * (1.0 - Math.Cos(Math.PI * gt))
            let lift = min 0.6 (max 0.0 (top_target - dp_sr))
            gt * (dp_sr + lift) + (1.0 - gt) * base_dp

    let rec analyze
        (
            rate: Rate,
            note_data: NoteData,
            sunny: Sunny.Result,
            features: Features.ChartFeatures,
            family: string,
            msd: MsdScores option,
            bio: BioResult option,
            od: float,
            apply_rate_floor: bool
        ) : float * DanTier * DanSublevel * DanDebug =
        let choke = extract_choke note_data
        let raw_sr = sunny.Sr
        let jbar_max = sunny.JbarMax
        let burst_ratio = choke.BurstRatio
        let choke_nps = choke.Choke10sNps

        let burst_activation = sigmoid (burst_ratio, 5.0, 1.35)
        let jbar_intensity = sigmoid (jbar_max, 0.08, 60.0)
        let mutable delta_burst = BURST_GAIN * burst_activation * jbar_intensity

        let mutable delta_jack =
            if (family = "jack" || family = "tech" || family = "hybrid") && jbar_max > 55.0 then
                let jack_factor = sigmoid (jbar_max, 0.10, 70.0)
                JACK2_GAIN * jack_factor * min 1.0 (choke.JackBurstDensity * 2.0)
            else
                0.0

        let fatigue_spike =
            if burst_ratio < 1.15 && raw_sr > 9.5 && (family = "tech" || family = "jack") then
                min 0.06 (sigmoid (raw_sr, 2.0, 9.7) * (1.15 - burst_ratio) * 0.40)
            else
                0.0

        let rate_f = float (rate / 1.0f<rate>)
        let duration_s = features.DurationS
        let density_cv = features.DensityCv
        let mara_dur_sig = sigmoid (duration_s, MARA_DUR_K, MARA_DUR_X0)
        let cv_pen = 1.0 - min 1.0 (density_cv / MARA_CV_REF)
        let delta_marathon = MARA_CAP * mara_dur_sig * cv_pen
        let delta_fatigue = min FATIGUE_TOTAL_CAP (fatigue_spike + delta_marathon)

        let bpm_base = features.Bpm

        let mutable delta_bpm =
            BPM_GAIN
            * sigmoid (features.StreamPurity, BPM_PURITY_K, BPM_PURITY_X0)
            * sigmoid (bpm_base, BPM_K, BPM_X0)

        let taper = max 0.25 (1.0 - max 0.0 (raw_sr - 9.0) * 0.12)
        delta_burst <- delta_burst * taper
        delta_jack <- delta_jack * taper
        delta_bpm <- delta_bpm * taper

        let mod_sr = raw_sr * (1.0 + delta_burst + delta_jack + delta_bpm - delta_fatigue)

        let sk_key =
            skillset_key (family, jbar_max, features.JumpRatio, features.JackDensity)

        let sr_means =
            SKILLSET_SR_MEANS
            |> Map.tryFind sk_key
            |> Option.defaultValue SKILLSET_SR_MEANS.["general"]

        let choke_means =
            SKILLSET_CHOKE_MEANS
            |> Map.tryFind sk_key
            |> Option.defaultValue SKILLSET_CHOKE_MEANS.["general"]

        let dp_sr = interpolate_ruler (mod_sr, sr_means)
        let dp_choke = interpolate_ruler (choke_nps, choke_means)

        let dp_msd, has_msd =
            match msd with
            | Some scores -> interpolate_ruler (scores.Overall, MSD_MEANS), true
            | None -> dp_sr, false

        let mutable w_sr = 0.0
        let mutable w_choke = 0.0
        let mutable w_msd = 0.0

        if has_msd then
            let choke_flat = sigmoid (13.0 - choke_nps, 1.0, 0.0)
            let sr_signal = sigmoid (raw_sr - 3.0, 2.0, 0.0)
            w_sr <- min 0.80 (0.50 + 0.25 * choke_flat * sr_signal)
            let disagree = sigmoid (abs (dp_sr - dp_choke) - 1.2, 2.0, 0.0)
            w_msd <- min 0.35 (0.20 + 0.15 * disagree)
            w_choke <- max 0.05 (1.0 - w_sr - w_msd)
            let total = w_sr + w_choke + w_msd
            w_sr <- w_sr / total
            w_choke <- w_choke / total
            w_msd <- w_msd / total
        else
            w_sr <- 0.60
            w_choke <- 0.40
            w_msd <- 0.0

        let mutable combined = w_sr * dp_sr + w_choke * dp_choke + w_msd * dp_msd

        if combined <= 4.5 && choke_nps < 18.0 then
            let low_density = sigmoid (16.0 - choke_nps, 0.5, 2.0)
            let damped = combined - DAMP_GAIN * low_density * max 0.0 (combined - 1.5)
            combined <- max 1.0 damped

        let hand_ratio = features.HandRatio

        if combined >= 9.8 && combined <= 12.8 then
            let alpha_signal = sigmoid (choke_nps, 3.0, 25.2) * sigmoid (hand_ratio, 20.0, 0.15)
            combined <- combined + 0.20 * alpha_signal
            let beta_signal = sigmoid (choke_nps, 3.0, 27.2)
            combined <- combined + 0.25 * beta_signal
            let chord_signal = sigmoid (hand_ratio, CHORD_K, CHORD_X0) - 0.5
            combined <- combined + CHORD_GAIN * chord_signal

        let triang_dp = Math.Round(combined, 2)

        let mutable base_dp =
            Ridge.lin_base_blend (triang_dp, dp_sr, dp_choke, dp_msd, sk_key)

        base_dp <- top_scale_gate (base_dp, dp_sr)

        let ln_ratio = Detect.hold_ratio note_data |> float
        let dual_low = Ridge.is_available ()

        let mutable ridge_corr = 0.0

        let mutable dp =
            if ln_ratio <= float Detect.LN_RIDGE_RATIO && (base_dp >= 10.5 || (dual_low && base_dp >= 1.2)) then
                let subrank =
                    {
                        SkKey = sk_key
                        DpSr = dp_sr
                        DpChoke = dp_choke
                        DpMsd = dp_msd
                        ModSr = mod_sr
                        RawSr = raw_sr
                        Ti = features.TimingIrregularity
                        BurstRatio = burst_ratio
                        Dur = duration_s
                        Quad = features.QuadRatio
                        Hand = hand_ratio
                        Choke = choke_nps
                        Bpm = bpm_base
                    }

                let corrected, corr =
                    Ridge.apply_correction base_dp bio msd sunny features.StreamPurity subrank

                ridge_corr <- corr
                corrected
            else
                base_dp

        dp <- min dp 20.5

        if apply_rate_floor then
            let r = Math.Round(rate_f, 4)

            if r <> 0.75 && r <> 1.0 && r <> 1.5 then
                let anchor =
                    if r > 0.75 then
                        if r >= 1.5 then 1.5
                        elif r >= 1.0 then 1.0
                        else 0.75
                    else
                        0.75

                let anchor_rate = float32 anchor * 1.0f<rate>
                let od_v = if od > 0.0 then od else Adapter.DEFAULT_OD

                match Sunny.calculate (anchor_rate, note_data, od_v) with
                | Some sunny_a ->
                    let feats_a = features
                    let bio_a = bio
                    let msd_a = Msd.calculate (anchor_rate, note_data)
                    let fam_a, _ = Classifier.classify_family (sunny_a, feats_a, feats_a.Bpm, feats_a.DurationS)

                    let fam_a =
                        if sunny_a.Sr < 7.0 then
                            let rp_f, rp_c = RhythmProfile.classify (1.0f<rate>, note_data, feats_a.Bpm)

                            if rp_c >= 0.10 then rp_f else fam_a
                        else
                            fam_a

                    let dp_a, _, _, _ =
                        analyze (anchor_rate, note_data, sunny_a, feats_a, fam_a, msd_a, bio_a, od_v, false)

                    if r > 0.75 then
                        dp <- max dp dp_a
                    else
                        dp <- min dp dp_a
                | None -> ()

        dp <- Math.Round(max 0.5 (min 20.5 dp), 2)
        let tier = Format.rice_tier_from_dp dp
        let sub = Format.sublevel_from_dp dp

        let msd_overall =
            match msd with
            | Some s -> s.Overall
            | None -> 0.0

        let debug =
            {
                Family = family
                SkKey = sk_key
                Od = if od > 0.0 then od else Adapter.DEFAULT_OD
                Rate = rate_f
                RawSr = Math.Round(raw_sr, 4)
                ModSr = Math.Round(mod_sr, 4)
                ChokeNps = choke_nps
                MsdOverall = Math.Round(msd_overall, 4)
                HasMsd = has_msd
                MsdError = if has_msd then "" else Msd.last_failure ()
                HasBio = bio.IsSome
                LnRatio = Math.Round(ln_ratio, 4)
                DpSr = Math.Round(dp_sr, 2)
                DpChoke = Math.Round(dp_choke, 2)
                DpMsd = Math.Round(dp_msd, 2)
                WSr = Math.Round(w_sr, 3)
                WChoke = Math.Round(w_choke, 3)
                WMsd = Math.Round(w_msd, 3)
                TriangDp = triang_dp
                BaseDp = Math.Round(base_dp, 2)
                RidgeCorr = Math.Round(ridge_corr, 3)
                FinalDp = dp
            }

        dp, tier, sub, debug
