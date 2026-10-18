namespace Prelude.Calculator.Dan

open System
open System.Collections.Generic
open System.Numerics
open Prelude
open Prelude.Charts

type StrainRate =
    {
        Base: float
        Offset: float
        Power: float
        Cap: float
    }

type StreamTau =
    {
        BurstTau: float
        SustainTau: float
        BurstMix: float
    }

type LinearMap =
    {
        P02: float
        P98: float
        Y0: float
        Y1: float
    }

type AggWeights =
    {
        AggQ97: float
        AggQ90: float
        AggTail: float
        AggQ75: float
        AggPower: float
        AggQ50: float
    }

type BioConfig =
    {
        RowToleranceMs: float
        EntropyWindowMs: float
        NpsWindowsMs: float array
        SectionMs: float
        SectionDecay: float
        SectionEmaAlpha: float
        CorrectionClamp: float
        RawMap: LinearMap
        BioSrMap: LinearMap
        StreamNames: string array
        Streams: Map<string, StreamTau>
        StreamWeights: Map<string, float>
        R: StrainRate
        RSameCol: StrainRate
        RFastHand: StrainRate
        SpeedWRow: float
        SpeedWHandMax: float
        SpeedWHandMean: float
        JackWChord: float
        JackWAnchor: float
        JackAnchorMs: float
        HandWBase: float
        HandWRotation: float
        RHand: StrainRate
        ChordWSpeed: float
        ChordWSameHand: float
        BodyR: StrainRate
        ChordjackWJack: float
        ChordjackWOverlap: float
        ChordjackWHand: float
        TechWChaos: float
        TechWEntropy: float
        TechWTransition: float
        TechWChange: float
        StaminaWNps1k: float
        StaminaWNps4k: float
        StaminaWHand: float
        StaminaHandTau: float
        StaminaLogBase: float
        CourseDurGate: float * float
        CourseBreakDamp: float * float
        CourseWBreak: float
        Agg: AggWeights
        PowerMeanP: float
        TailRatio: float
    }

type BioRow =
    {
        T: float
        Mask: int
        RowSize: int
        LeftCount: int
        RightCount: int
        HandMask: int array
    }

type BioResult =
    {
        BioNumeric: float
        RawAgg: float
        Summaries: Map<string, {| Aggregate: float; Q97: float |}>
        Stats: Map<string, float>
        Streams: Map<string, float>
        StreamsQ97: Map<string, float>
    }

module StrainBio =

    let BIO_CONFIG: BioConfig =
        {
            RowToleranceMs = 2.0
            EntropyWindowMs = 750.0
            NpsWindowsMs = [| 250.0; 500.0; 1000.0; 4000.0 |]
            SectionMs = 400.0
            SectionDecay = 0.9
            SectionEmaAlpha = 0.15
            CorrectionClamp = 1.25
            RawMap = { P02 = 4.0; P98 = 7.5; Y0 = -2.0; Y1 = 20.0 }
            BioSrMap = { P02 = 2.0; P98 = 9.0; Y0 = 1.0; Y1 = 14.0 }
            StreamNames = [| "speed"; "hand"; "jack"; "chordjack"; "tech"; "stamina"; "course" |]
            Streams =
                Map.ofList
                    [
                        "speed", { BurstTau = 200.0; SustainTau = 2500.0; BurstMix = 0.75 }
                        "hand", { BurstTau = 240.0; SustainTau = 3000.0; BurstMix = 0.78 }
                        "jack", { BurstTau = 280.0; SustainTau = 2500.0; BurstMix = 0.85 }
                        "chordjack", { BurstTau = 260.0; SustainTau = 3000.0; BurstMix = 0.80 }
                        "tech", { BurstTau = 450.0; SustainTau = 3500.0; BurstMix = 0.70 }
                        "stamina", { BurstTau = 1200.0; SustainTau = 10000.0; BurstMix = 0.58 }
                        "course", { BurstTau = 30000.0; SustainTau = 120000.0; BurstMix = 0.35 }
                    ]
            StreamWeights =
                Map.ofList
                    [
                        "speed", 0.22
                        "hand", 0.18
                        "jack", 0.16
                        "chordjack", 0.16
                        "tech", 0.12
                        "stamina", 0.11
                        "course", 0.05
                    ]
            R = { Base = 180.0; Offset = 40.0; Power = 1.08; Cap = 8.0 }
            RSameCol = { Base = 185.0; Offset = 35.0; Power = 1.18; Cap = 8.0 }
            RFastHand = { Base = 205.0; Offset = 45.0; Power = 1.05; Cap = 8.0 }
            SpeedWRow = 0.55
            SpeedWHandMax = 0.30
            SpeedWHandMean = 0.15
            JackWChord = 0.20
            JackWAnchor = 0.15
            JackAnchorMs = 220.0
            HandWBase = 0.70
            HandWRotation = 0.30
            RHand = { Base = 180.0; Offset = 38.0; Power = 1.10; Cap = 8.0 }
            ChordWSpeed = 0.18
            ChordWSameHand = 0.22
            BodyR = { Base = 150.0; Offset = 80.0; Power = 0.85; Cap = 8.0 }
            ChordjackWJack = 0.55
            ChordjackWOverlap = 0.30
            ChordjackWHand = 0.15
            TechWChaos = 0.32
            TechWEntropy = 0.24
            TechWTransition = 0.24
            TechWChange = 0.20
            StaminaWNps1k = 0.40
            StaminaWNps4k = 0.35
            StaminaWHand = 0.25
            StaminaHandTau = 8000.0
            StaminaLogBase = 24.0
            CourseDurGate = (90.0, 300.0)
            CourseBreakDamp = (0.006, 0.018)
            CourseWBreak = 0.25
            Agg =
                {
                    AggQ97 = 0.30
                    AggQ90 = 0.22
                    AggTail = 0.18
                    AggQ75 = 0.15
                    AggPower = 0.10
                    AggQ50 = 0.05
                }
            PowerMeanP = 2.4
            TailRatio = 0.04
        }

    let private clamp (x: float) (lo: float) (hi: float) : float = max lo (min hi x)

    let private gate (x: float) (a: float) (b: float) : float =
        if b <= a then
            (if x >= b then 1.0 else 0.0)
        else
            clamp ((x - a) / (b - a)) 0.0 1.0

    let private inverse_gate (x: float) (a: float) (b: float) : float = 1.0 - gate x a b

    let private strain_rate (dt: float) (r: StrainRate) : float =
        let effective = max 16.0 (dt + r.Offset)
        let value = Math.Pow(r.Base / effective, r.Power)

        if Double.IsFinite value then min r.Cap value else 0.0

    let private decay (state: float) (input_val: float) (dt: float) (tau: float) : float =
        let delta = if dt > 0.0 then dt else 0.0
        state * Math.Exp(-delta / tau) + input_val

    let private bitcount (mask: int) : int =
        BitOperations.PopCount(uint32 (mask &&& 15))

    let private quantile (sorted_vals: float array) (q: float) : float =
        if sorted_vals.Length = 0 then
            0.0
        else
            let t = q * float (sorted_vals.Length - 1)
            let lo = int t
            let hi = min (sorted_vals.Length - 1) (lo + 1)
            let w = t - float lo
            sorted_vals.[lo] * (1.0 - w) + sorted_vals.[hi] * w

    let build_bio_rows (parsed_rows: (float * int array) list, tolerance_ms: float) : BioRow array =
        let merged = ResizeArray<float * Set<int>>()

        for (t, cols) in parsed_rows do
            let col_set = cols |> Array.sort |> Set.ofArray

            if merged.Count > 0 && abs (t - fst merged.[merged.Count - 1]) <= tolerance_ms then
                let (pt, pcols) = merged.[merged.Count - 1]
                merged.[merged.Count - 1] <- (pt, Set.union pcols col_set)
            else
                merged.Add((t, col_set))

        merged
        |> Seq.choose (fun (t, cols) ->
            let mask =
                cols
                |> Seq.filter (fun c -> c >= 0 && c <= 3)
                |> Seq.fold (fun m c -> m ||| (1 <<< c)) 0

            if mask = 0 then
                None
            else
                let left = mask &&& 0b0011
                let right = mask &&& 0b1100

                Some
                    {
                        T = t
                        Mask = mask
                        RowSize = bitcount mask
                        LeftCount = bitcount left
                        RightCount = bitcount right
                        HandMask = [| left; right |]
                    }
        )
        |> Seq.toArray

    type private Activity =
        {
            InactiveMs: float
            BreakCount: int
            ActiveDurationS: float
            BreakDensity: float
            AvgNps: float
        }

    let private compute_activity_stats (rows: BioRow array) : Activity =
        if rows.Length < 2 then
            {
                InactiveMs = 0.0
                BreakCount = 0
                ActiveDurationS = 1.0
                BreakDensity = 0.0
                AvgNps = float rows.Length
            }
        else
            let mutable inactive_ms = 0.0
            let mutable break_count = 0

            for i = 1 to rows.Length - 1 do
                let gap = rows.[i].T - rows.[i - 1].T

                if gap > 1000.0 then
                    inactive_ms <- inactive_ms + (gap - 1000.0)
                    break_count <- break_count + 1

            let duration_ms = max 1.0 (rows.[rows.Length - 1].T - rows.[0].T - inactive_ms)
            let active_s = duration_ms / 1000.0

            {
                InactiveMs = inactive_ms
                BreakCount = break_count
                ActiveDurationS = active_s
                BreakDensity = float break_count / max (active_s / 60.0) 1.0
                AvgNps = float rows.Length / max active_s 1.0
            }

    let private compute_nps_rows (rows: BioRow array) (windows: float array) : float array array =
        let starts = Array.zeroCreate<int> windows.Length
        let n = rows.Length
        let result = Array.zeroCreate<float array> n
        let mutable end_ = 0

        for i = 0 to n - 1 do
            let row = rows.[i]

            while end_ < n && rows.[end_].T <= row.T + 1e-9 do
                end_ <- end_ + 1

            let values = Array.zeroCreate<float> windows.Length

            for w = 0 to windows.Length - 1 do
                let min_time = row.T - windows.[w]

                while starts.[w] < n && rows.[starts.[w]].T <= min_time do
                    starts.[w] <- starts.[w] + 1

                values.[w] <- float (end_ - starts.[w]) / (windows.[w] / 1000.0)

            result.[i] <- values

        result

    type private Curve =
        {
            Series: float array array
            LocalRaw: float array
            CurveStats: Map<string, float>
        }

    let private compute_bio_curve (rows: BioRow array) (cfg: BioConfig) : Curve =
        let names = cfg.StreamNames
        let n_streams = names.Length
        let burst_state = Array.zeroCreate<float> n_streams
        let sustain_state = Array.zeroCreate<float> n_streams
        let series = Array.init n_streams (fun _ -> ResizeArray<float>())
        let stream_cfg = names |> Array.map (fun nm -> cfg.Streams.[nm])
        let stream_w = names |> Array.map (fun nm -> cfg.StreamWeights.[nm])

        let last_col_time = Array.create 4 Double.NaN
        let last_hand_time = Array.create 2 Double.NaN
        let prev_hand_mask = [| 0; 0 |]
        let hand_stamina = [| 0.0; 0.0 |]

        let col_counts = Array.zeroCreate<int> 4
        let dt_same_vals = ResizeArray<float>()
        let dt_hand_vals = ResizeArray<float>()
        let local_raw = ResizeArray<float>()

        let mask_counts = Array.zeroCreate<int> 16
        let transition_counts = Array.zeroCreate<int> 256
        let active_transitions = HashSet<int>()
        let queue = ResizeArray<struct (float * int * int)>()
        let mutable queue_head = 0
        let mutable mask_total = 0
        let mutable transition_total = 0

        let mutable prev_row_time = if rows.Length > 0 then rows.[0].T - 1000.0 else 0.0
        let mutable prev_dt_row = 1000.0
        let mutable prev_mask = 0

        let dt_row_vals = ResizeArray<float>()

        let mutable left_load = 0.0
        let mutable right_load = 0.0
        let mutable chord_rows = 0
        let mutable three_rows = 0
        let mutable overlap_events_total = 0.0
        let mutable rotation_total = 0
        let mutable eligible_hand = 0
        let mutable anchor_strength_total = 0.0
        let mutable fast_jack_strength_total = 0.0

        let activity = compute_activity_stats rows
        let nps_rows = compute_nps_rows rows cfg.NpsWindowsMs
        let (d1, d2) = cfg.CourseDurGate
        let (b1, b2) = cfg.CourseBreakDamp
        let log_base_ln = Math.Log cfg.StaminaLogBase

        for ri = 0 to rows.Length - 1 do
            let row = rows.[ri]
            let t = row.T
            let dt_row = max 1.0 (t - prev_row_time)
            dt_row_vals.Add dt_row
            let mask = row.Mask
            let hand_masks = row.HandMask

            let dt_hand = [| Double.NaN; Double.NaN |]
            let rotation = [| 0.0; 0.0 |]
            let mutable overlap_row = 0

            for h = 0 to 1 do
                if hand_masks.[h] <> 0 then
                    if Double.IsFinite last_hand_time.[h] then
                        dt_hand.[h] <- max 1.0 (t - last_hand_time.[h])
                        dt_hand_vals.Add dt_hand.[h]
                        eligible_hand <- eligible_hand + 1

                        if prev_hand_mask.[h] <> 0 && (hand_masks.[h] &&& prev_hand_mask.[h]) = 0 then
                            rotation.[h] <- 1.0
                            rotation_total <- rotation_total + 1

                        if (hand_masks.[h] &&& prev_hand_mask.[h]) <> 0 then
                            overlap_row <- overlap_row + 1

            let same_hand_overlap = float overlap_row / 2.0
            overlap_events_total <- overlap_events_total + same_hand_overlap

            let mutable jack_max = 0.0
            let mutable anchor_row = 0.0

            for c = 0 to 3 do
                if (mask &&& (1 <<< c)) <> 0 then
                    col_counts.[c] <- col_counts.[c] + 1

                    if Double.IsFinite last_col_time.[c] then
                        let dt_same = max 1.0 (t - last_col_time.[c])
                        dt_same_vals.Add dt_same
                        anchor_row <- max anchor_row (inverse_gate dt_same cfg.JackAnchorMs (cfg.JackAnchorMs + 40.0))
                        fast_jack_strength_total <- fast_jack_strength_total + inverse_gate dt_same 120.0 150.0
                        jack_max <- max jack_max (strain_rate dt_same cfg.RSameCol)

            anchor_strength_total <- anchor_strength_total + anchor_row

            left_load <- left_load + float row.LeftCount
            right_load <- right_load + float row.RightCount

            if row.RowSize >= 2 then
                chord_rows <- chord_rows + 1

            if row.RowSize >= 3 then
                three_rows <- three_rows + 1

            mask_counts.[mask] <- mask_counts.[mask] + 1
            mask_total <- mask_total + 1
            let mutable transition_code = -1

            if prev_mask <> 0 then
                transition_code <- (prev_mask <<< 4) ||| mask
                transition_counts.[transition_code] <- transition_counts.[transition_code] + 1
                transition_total <- transition_total + 1
                active_transitions.Add transition_code |> ignore

            queue.Add(struct (t, mask, transition_code))

            let mutable go = true

            while go && queue_head < queue.Count do
                let struct (old_t, old_mask, old_code) = queue.[queue_head]

                if old_t < t - cfg.EntropyWindowMs then
                    mask_counts.[old_mask] <- mask_counts.[old_mask] - 1
                    mask_total <- mask_total - 1

                    if old_code >= 0 then
                        transition_counts.[old_code] <- transition_counts.[old_code] - 1

                        if transition_counts.[old_code] = 0 then
                            active_transitions.Remove old_code |> ignore

                        transition_total <- transition_total - 1

                    queue_head <- queue_head + 1
                else
                    go <- false

            let entropy_mask =
                if mask_total > 0 then
                    let mutable ent_m = 0.0

                    for m = 0 to 15 do
                        let c = mask_counts.[m]

                        if c > 0 then
                            let p = float c / float mask_total
                            ent_m <- ent_m - p * Math.Log2 p

                    clamp (ent_m / 4.0) 0.0 1.0
                else
                    0.0

            let entropy_transition =
                if transition_total > 0 && active_transitions.Count > 0 then
                    let mutable ent_t = 0.0

                    for code in active_transitions do
                        let c = transition_counts.[code]

                        if c > 0 then
                            let p = float c / float transition_total
                            ent_t <- ent_t - p * Math.Log2 p

                    clamp (ent_t / 8.0) 0.0 1.0
                else
                    0.0

            let row_chord = float (row.RowSize - 1) / 3.0

            let same_hand_chord =
                float (max 0 (row.LeftCount - 1) + max 0 (row.RightCount - 1)) / 2.0

            let hand_rates = ResizeArray<float>()

            for h = 0 to 1 do
                if hand_masks.[h] <> 0 then
                    let hdt = if Double.IsFinite dt_hand.[h] then dt_hand.[h] else 1000.0
                    let rate = strain_rate hdt cfg.RHand
                    hand_rates.Add rate
                    hand_stamina.[h] <- decay hand_stamina.[h] rate hdt cfg.StaminaHandTau

            for h = 0 to 1 do
                if hand_masks.[h] = 0 then
                    hand_stamina.[h] <- decay hand_stamina.[h] 0.0 dt_row cfg.StaminaHandTau

            let hand_max = if hand_rates.Count > 0 then Seq.max hand_rates else 0.0

            let hand_mean =
                if hand_rates.Count > 0 then
                    Seq.sum hand_rates / float hand_rates.Count
                else
                    0.0

            let speed_in =
                cfg.SpeedWRow * strain_rate dt_row cfg.R
                + cfg.SpeedWHandMax * hand_max
                + cfg.SpeedWHandMean * hand_mean

            let jack_in = jack_max * (1.0 + cfg.JackWChord * row_chord + cfg.JackWAnchor * anchor_row)
            let mutable hand_in = 0.0

            for h = 0 to 1 do
                if hand_masks.[h] <> 0 then
                    let hdt = if Double.IsFinite dt_hand.[h] then dt_hand.[h] else 1000.0
                    let base_rate = strain_rate hdt cfg.RHand
                    let fast_rate = strain_rate hdt cfg.RFastHand
                    hand_in <- max hand_in (cfg.HandWBase * base_rate + cfg.HandWRotation * rotation.[h] * fast_rate)

            let body = float (max 0 (row.RowSize - 2)) * strain_rate dt_row cfg.BodyR

            let chord_in =
                row_chord * (1.0 + cfg.ChordWSpeed * speed_in) + cfg.ChordWSameHand * same_hand_chord + body

            let chordjack_in =
                row_chord
                * (cfg.ChordjackWJack * jack_in
                   + cfg.ChordjackWOverlap * same_hand_overlap
                   + cfg.ChordjackWHand * hand_in)

            let rhythm_chaos =
                if prev_mask <> 0 then
                    min 2.0 (abs (Math.Log2((dt_row + 24.0) / (prev_dt_row + 24.0)))) / 2.0
                else
                    0.0

            let tech_in =
                cfg.TechWChaos * rhythm_chaos
                + cfg.TechWEntropy * entropy_mask
                + cfg.TechWTransition * entropy_transition
                + cfg.TechWChange * (if mask <> prev_mask then 1.0 else 0.0)

            let nps1k = nps_rows.[ri].[2]
            let nps4k = nps_rows.[ri].[3]

            let stamina_in =
                cfg.StaminaWNps1k * Math.Log(1.0 + nps1k) / log_base_ln
                + cfg.StaminaWNps4k * Math.Log(1.0 + nps4k) / log_base_ln
                + cfg.StaminaWHand * max hand_stamina.[0] hand_stamina.[1]

            let course_in =
                stamina_in
                * gate activity.ActiveDurationS d1 d2
                * (1.0 - cfg.CourseWBreak * gate activity.BreakDensity b1 b2)

            let inputs =
                [| speed_in; hand_in; jack_in; chordjack_in; tech_in; stamina_in; course_in |]

            let mutable raw = 0.0

            for si = 0 to n_streams - 1 do
                let sc = stream_cfg.[si]
                burst_state.[si] <- decay burst_state.[si] inputs.[si] dt_row sc.BurstTau
                sustain_state.[si] <- decay sustain_state.[si] inputs.[si] dt_row sc.SustainTau

                let v = sc.BurstMix * burst_state.[si] + (1.0 - sc.BurstMix) * sustain_state.[si]
                series.[si].Add v
                raw <- raw + stream_w.[si] * v

            local_raw.Add raw

            for c = 0 to 3 do
                if (mask &&& (1 <<< c)) <> 0 then
                    last_col_time.[c] <- t

            for h = 0 to 1 do
                if hand_masks.[h] <> 0 then
                    last_hand_time.[h] <- t
                    prev_hand_mask.[h] <- hand_masks.[h]

            prev_row_time <- t
            prev_dt_row <- dt_row
            prev_mask <- mask

        let n_rows = rows.Length

        let peak_to_sustain_gap =
            if local_raw.Count > 0 then
                let sorted_raw = local_raw.ToArray() |> Array.sort
                let q97_local = quantile sorted_raw 0.97
                let q75_local = quantile sorted_raw 0.75
                clamp ((q97_local - q75_local) / max q97_local 1e-6) 0.0 1.0
            else
                0.0

        let nps1k_sorted = nps_rows |> Array.map (fun a -> a.[2]) |> Array.sort
        let nps4k_sorted = nps_rows |> Array.map (fun a -> a.[3]) |> Array.sort
        let q50_1k = quantile nps1k_sorted 0.50
        let q75_1k = quantile nps1k_sorted 0.75
        let q90_1k = quantile nps1k_sorted 0.90
        let q97_1k = quantile nps1k_sorted 0.97

        let log_nps_vals =
            nps1k_sorted |> Array.map (fun v -> Math.Log(1.0 + max 0.0 v) / log_base_ln)

        let dt_row_sorted = dt_row_vals.ToArray() |> Array.sort
        let dt_hand_sorted = dt_hand_vals.ToArray() |> Array.sort
        let dt_same_sorted = dt_same_vals.ToArray() |> Array.sort

        let stats =
            Map.ofList
                [
                    "inactive_ms", activity.InactiveMs
                    "break_count", float activity.BreakCount
                    "active_duration_s", activity.ActiveDurationS
                    "break_density", activity.BreakDensity
                    "avg_nps", activity.AvgNps
                    "chord_rate", float chord_rows / float (max n_rows 1)
                    "three_rate", float three_rows / float (max n_rows 1)
                    "overlap_rate", overlap_events_total / float (max n_rows 1)
                    "rotation_rate", float rotation_total / float (max eligible_hand 1)
                    "same_hand_q10", (if dt_hand_vals.Count > 0 then quantile dt_hand_sorted 0.10 else 0.0)
                    "fast_jack_rate", fast_jack_strength_total / float (max dt_same_vals.Count 1)
                    "anchor_rate", anchor_strength_total / float (max n_rows 1)
                    "anchor_imbalance",
                    (if n_rows > 0 then
                         float (Array.max col_counts - Array.min col_counts) / float (max n_rows 1)
                     else
                         0.0)
                    "hand_bias", abs (left_load - right_load) / max (max left_load right_load) 1e-6
                    "peak_to_sustain_gap", peak_to_sustain_gap
                    "nps1k_q50", q50_1k
                    "nps1k_q75", q75_1k
                    "nps1k_q90", q90_1k
                    "nps4k_q50", quantile nps4k_sorted 0.50
                    "nps4k_q75", quantile nps4k_sorted 0.75
                    "nps4k_q90", quantile nps4k_sorted 0.90
                    "nps1k_flatness", clamp (q50_1k / max q97_1k 1e-6) 0.0 1.0
                    "log_nps1k_mean", Array.sum log_nps_vals / float (max log_nps_vals.Length 1)
                    "dt_row_q25", quantile dt_row_sorted 0.25
                    "dt_row_q50", quantile dt_row_sorted 0.50
                    "dt_row_q75", quantile dt_row_sorted 0.75
                    "dt_hand_q25", quantile dt_hand_sorted 0.25
                    "dt_hand_median", quantile dt_hand_sorted 0.50
                    "dt_same_q25", quantile dt_same_sorted 0.25
                    "dt_same_median", quantile dt_same_sorted 0.50
                ]

        {
            Series = series |> Array.map (fun s -> s.ToArray())
            LocalRaw = local_raw.ToArray()
            CurveStats = stats
        }

    let private summarize (values: float array) (cfg: BioConfig) : {| Aggregate: float; Q97: float |} =
        if values.Length = 0 then
            {| Aggregate = 0.0; Q97 = 0.0 |}
        else
            let sorted_vals = Array.sort values
            let n = sorted_vals.Length
            let q50 = quantile sorted_vals 0.50
            let q75 = quantile sorted_vals 0.75
            let q90 = quantile sorted_vals 0.90
            let q97 = quantile sorted_vals 0.97
            let tail_count = max 1 (int (Math.Ceiling(float n * cfg.TailRatio)))
            let tail_mean = Array.sum sorted_vals.[n - tail_count ..] / float tail_count
            let p = cfg.PowerMeanP

            let power_mean =
                Math.Pow((sorted_vals |> Array.sumBy (fun v -> Math.Pow(v, p))) / float n, 1.0 / p)

            let a = cfg.Agg

            let agg =
                a.AggQ97 * q97
                + a.AggQ90 * q90
                + a.AggTail * tail_mean
                + a.AggQ75 * q75
                + a.AggPower * power_mean
                + a.AggQ50 * q50

            {| Aggregate = agg; Q97 = q97 |}

    let private section_aggregate (rows: BioRow array) (local_raw: float array) (cfg: BioConfig) : float =
        if rows.Length = 0 || local_raw.Length = 0 then
            0.0
        else
            let first_t = rows.[0].T
            let section_max = System.Collections.Generic.Dictionary<int, float>()
            let mutable smoothed = local_raw.[0]

            for i = 0 to rows.Length - 1 do
                let section = max 0 (int ((rows.[i].T - first_t) / cfg.SectionMs))
                let raw = local_raw.[i]
                smoothed <- smoothed + cfg.SectionEmaAlpha * (raw - smoothed)

                let prev =
                    match section_max.TryGetValue section with
                    | true, v -> v
                    | _ -> 0.0

                section_max.[section] <- max prev smoothed

            let values =
                section_max.Values
                |> Seq.filter (fun v -> v > 0.0)
                |> Seq.sortDescending
                |> Seq.toArray

            if values.Length = 0 then
                0.0
            else
                let mutable weight = 1.0
                let mutable total = 0.0
                let mutable weight_total = 0.0

                for v in values do
                    total <- total + v * weight
                    weight_total <- weight_total + weight
                    weight <- weight * cfg.SectionDecay

                total / weight_total

    let private round4 (x: float) : float = Math.Round(x, 4)

    let private stat (stats: Map<string, float>) (key: string) : float =
        match Map.tryFind key stats with
        | Some v -> v
        | None -> 0.0

    let private compute_corrections (stats: Map<string, float>) (cfg: BioConfig) : float =
        let st = stat stats
        let mutable total = 0.0

        let apply (gain: float) (build: unit -> float) =
            if gain = 0.0 then
                ()
            else
                total <- total + gain * build ()

        apply 0.0 (fun () ->
            let mutable v = 1.0
            v <- v * gate (st "chord_rate") 0.48 0.68
            v <- v * gate (st "overlap_rate") 0.75 1.25
            v <- v * (1.0 - gate (st "avg_nps") 19.0 23.0)
            v <- v * (1.0 - gate (st "anchor_imbalance") 0.06 0.12)
            v)

        apply 0.0 (fun () ->
            let mutable v = 1.0
            v <- v * gate (st "rotation_rate") 0.68 0.86
            v <- v * inverse_gate (st "same_hand_q10") 100.0 130.0
            v <- v * gate (st "chord_rate") 0.25 0.42
            v <- v * gate (st "overlap_rate") 0.65 0.95
            v)

        apply 0.0 (fun () ->
            let mutable v = 1.0
            v <- v * gate (st "active_duration_s") 240.0 480.0
            v <- v * gate (st "break_density") 0.006 0.018
            v <- v * gate (st "peak_to_sustain_gap") 0.35 0.75
            v <- v * (1.0 - gate (st "avg_nps") 12.0 18.0)
            v)

        apply 0.0 (fun () ->
            let mutable v = 1.0
            v <- v * gate (st "active_duration_s") 240.0 600.0
            v <- v * gate (st "break_density") 0.004 0.012
            v <- v * gate (st "peak_to_sustain_gap") 0.15 0.45
            v <- v * (1.0 - gate (st "avg_nps") 15.0 21.0)
            v)

        apply 0.0 (fun () ->
            let mutable v = 1.0
            v <- v * gate (st "anchor_rate") 0.18 0.38
            v <- v * gate (st "fast_jack_rate") 0.25 0.55
            v <- v * gate (st "chord_rate") 0.65 0.85
            v)

        apply 0.0 (fun () ->
            let mutable v = 1.0
            v <- v * gate (st "hand_bias") 0.25 0.55
            v <- v * (1.0 - gate (st "avg_nps") 12.0 20.0)
            v)

        apply 0.0 (fun () ->
            let mutable v = 1.0
            v <- v * gate (st "rotation_rate") 0.60 0.80
            v <- v * inverse_gate (st "overlap_rate") 0.60 0.90
            v <- v * gate (st "chord_rate") 0.30 0.50
            v)

        apply 0.0 (fun () ->
            let mutable v = 1.0
            v <- v * gate (st "chord_rate") 0.55 0.75
            v <- v * inverse_gate (st "fast_jack_rate") 0.50 0.70
            v <- v * gate (st "overlap_rate") 0.40 0.70
            v)

        clamp total (-cfg.CorrectionClamp) cfg.CorrectionClamp

    let biomech_numeric (rows: BioRow array) : BioResult =
        let cfg = BIO_CONFIG

        if rows.Length = 0 then
            {
                BioNumeric = 1.0
                RawAgg = 0.0
                Summaries = Map.empty
                Stats = Map.empty
                Streams = Map.empty
                StreamsQ97 = Map.empty
            }
        else
            let curve = compute_bio_curve rows cfg
            let summaries =
                cfg.StreamNames
                |> Array.mapi (fun i name -> name, summarize curve.Series.[i] cfg)
                |> Map.ofArray

            let weighted_agg =
                cfg.StreamNames
                |> Array.sumBy (fun name -> cfg.StreamWeights.[name] * summaries.[name].Aggregate)

            let section_agg = section_aggregate rows curve.LocalRaw cfg
            let raw_agg = 0.80 * weighted_agg + 0.20 * section_agg
            let log_raw = Math.Log(1.0 + max 0.0 raw_agg)
            let rm = cfg.RawMap

            let pre_numeric =
                clamp (rm.Y0 + (log_raw - rm.P02) / max 1e-9 (rm.P98 - rm.P02) * (rm.Y1 - rm.Y0)) (-2.5) 21.0

            let correction_total = compute_corrections curve.CurveStats cfg
            let numeric = clamp (pre_numeric + correction_total) (-2.0) 20.0

            {
                BioNumeric = round4 numeric
                RawAgg = round4 raw_agg
                Summaries = summaries
                Stats = curve.CurveStats
                Streams = summaries |> Map.map (fun _ s -> s.Aggregate)
                StreamsQ97 = summaries |> Map.map (fun _ s -> s.Q97)
            }

    let from_note_data (rate: Rate, note_data: NoteData) : BioResult option =
        if note_data.Keys <> 4 then
            None
        else
            let rate_f = float (rate / 1.0f<rate>)

            let parsed =
                note_data.Notes
                |> Array.toList
                |> List.choose (fun { Time = time; Data = row } ->
                    let cols =
                        [|
                            for k = 0 to note_data.Keys - 1 do
                                if row.[k] = NoteType.NORMAL || row.[k] = NoteType.HOLDHEAD then
                                    yield k
                        |]

                    if cols.Length = 0 then
                        None
                    else
                        Some(Adapter.scale_time_ms (float (time / 1.0f<ms>), rate_f), cols)
                )

            let rows = build_bio_rows (parsed, BIO_CONFIG.RowToleranceMs)

            if rows.Length = 0 then
                None
            else
                Some(biomech_numeric rows)
