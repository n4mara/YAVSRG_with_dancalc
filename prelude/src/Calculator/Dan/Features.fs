namespace Prelude.Calculator.Dan

open System
open Prelude
open Prelude.Charts

module Features =

    type LnFeatures =
        {
            HoldOccupancy: float
            SimultaneousHold: float
            ReleaseDensity: float
            LnDurationCv: float
            LnRatio: float
        }

    type ChartFeatures =
        {
            StreamPurity: float
            JumpRatio: float
            HandRatio: float
            QuadRatio: float
            JackRatio: float
            JackDensity: float
            VibroDensity: float
            AnchorRatio: float
            MinijackRatio: float
            DensityCv: float
            TransitionVar: float
            NpsP90: float
            NpsSustainedTop30: float
            NpsActiveRatio: float
            NpsActiveCv: float
            Bpm: float
            DurationS: float
            PatternIrregularity: float
            TimingIrregularity: float
            ChordComplexity: float
        }

    let private cv (values: float array) : float =
        if values.Length < 2 then
            0.0
        else
            let mu = Array.average values

            if mu < 1e-6 then
                0.0
            else
                let var = values |> Array.averageBy (fun v -> (v - mu) ** 2.0)
                sqrt var / mu

    let private nps_windows (times: float array, window_ms: float, stride_ms: float) : (float * float) array =
        if times.Length = 0 then
            [||]
        else
            let t_min = times.[0]
            let t_max = times.[times.Length - 1]
            let half = window_ms / 2.0
            let duration_s = window_ms / 1000.0
            let results = ResizeArray<float * float>()
            let mutable left_idx = 0
            let mutable right_idx = 0
            let mutable t = t_min

            while t <= t_max do
                let lo = t - half
                let hi = t + half

                while left_idx < times.Length && times.[left_idx] < lo do
                    left_idx <- left_idx + 1

                while right_idx < times.Length && times.[right_idx] <= hi do
                    right_idx <- right_idx + 1

                let count = right_idx - left_idx
                let nps = if duration_s > 0.0 then float count / duration_s else 0.0
                results.Add(t, nps)
                t <- t + stride_ms

            results.ToArray()

    let extract_ln (rate: Rate, note_data: NoteData) : LnFeatures =
        let rate_f = float (rate / 1.0f<rate>)
        let keys = note_data.Keys
        let notes, lnotes = Chart.notecount note_data
        let ln_ratio = if notes <= 0 then 0.0 else float lnotes / float notes

        let mutable open_holds = Array.zeroCreate keys
        let mutable hold_time = 0.0
        let mutable chart_span = 0.0
        let mutable sim_samples = 0
        let mutable sim_active = 0
        let mutable releases = 0
        let durations = ResizeArray<float>()

        if note_data.Notes.Length > 0 then
            let t0 = Adapter.scale_time_ms (float (note_data.Notes.[0].Time / 1.0f<ms>), rate_f)
            let t1 = Adapter.scale_time_ms (float (note_data.Notes.[note_data.Notes.Length - 1].Time / 1.0f<ms>), rate_f)
            chart_span <- max 1.0 (t1 - t0)

        let mutable prev_t = 0.0

        for { Time = time; Data = row } in note_data.Notes do
            let t = Adapter.scale_time_ms (float (time / 1.0f<ms>), rate_f)
            let dt = max 0.0 (t - prev_t)
            let active = open_holds |> Array.filter (fun x -> x > 0) |> Array.length
            hold_time <- hold_time + float active * dt
            sim_samples <- sim_samples + 1
            sim_active <- sim_active + active
            prev_t <- t

            for k = 0 to keys - 1 do
                if row.[k] = NoteType.HOLDHEAD then
                    open_holds.[k] <- open_holds.[k] + 1
                elif row.[k] = NoteType.HOLDTAIL && open_holds.[k] > 0 then
                    open_holds.[k] <- open_holds.[k] - 1
                    releases <- releases + 1
                    durations.Add(dt)

        let hold_occupancy = if chart_span <= 0.0 then 0.0 else min 1.0 (hold_time / (chart_span * float keys))
        let simultaneous = if sim_samples = 0 then 0.0 else float sim_active / float sim_samples / float keys
        let release_density = if chart_span <= 0.0 then 0.0 else float releases / (chart_span / 1000.0)

        let ln_duration_cv =
            if durations.Count < 2 then
                0.0
            else
                let arr = durations.ToArray()
                let mean = Array.average arr

                if mean <= 1e-9 then
                    0.0
                else
                    let var = arr |> Array.averageBy (fun v -> (v - mean) ** 2.0)
                    sqrt var / mean

        {
            HoldOccupancy = hold_occupancy
            SimultaneousHold = simultaneous
            ReleaseDensity = release_density
            LnDurationCv = ln_duration_cv
            LnRatio = ln_ratio
        }

    let peak_nps_window (rate: Rate, note_data: NoteData, window_ms: float) : float =
        let rate_f = float (rate / 1.0f<rate>)

        let times =
            [|
                for { Time = time; Data = row } in note_data.Notes do
                    let t = Adapter.scale_time_ms (float (time / 1.0f<ms>), rate_f)

                    for k = 0 to note_data.Keys - 1 do
                        if row.[k] = NoteType.NORMAL || row.[k] = NoteType.HOLDHEAD then
                            yield t
            |]
            |> Array.sort

        if times.Length = 0 then
            0.0
        else
            let mutable best = 0
            let mutable lo = 0

            for hi = 0 to times.Length - 1 do
                while times.[hi] - times.[lo] > window_ms do
                    lo <- lo + 1

                best <- max best (hi - lo + 1)

            float best / (window_ms / 1000.0)

    let extract (rate: Rate, note_data: NoteData, bpm: float) : ChartFeatures =
        let rate_f = float (rate / 1.0f<rate>)
        let keys = note_data.Keys

        let rows =
            [|
                for { Time = time; Data = row } in note_data.Notes do
                    let t = Adapter.scale_time_ms (float (time / 1.0f<ms>), rate_f)
                    let cols =
                        [|
                            for k = 0 to keys - 1 do
                                if row.[k] = NoteType.NORMAL || row.[k] = NoteType.HOLDHEAD then
                                    yield k
                        |]

                    if cols.Length > 0 then
                        yield t, cols
            |]

        let notes =
            [|
                for t, cols in rows do
                    for c in cols do
                        yield t, c
            |]
            |> Array.sortBy fst

        let note_count = notes.Length
        let row_count = rows.Length

        let stream_purity, jump_ratio, hand_ratio, quad_ratio =
            if row_count = 0 then
                0.0, 0.0, 0.0, 0.0
            else
                let sizes = rows |> Array.map (fun (_, cols) -> cols.Length)
                float (sizes |> Array.filter ((=) 1) |> Array.length) / float row_count,
                float (sizes |> Array.filter ((=) 2) |> Array.length) / float row_count,
                float (sizes |> Array.filter ((=) 3) |> Array.length) / float row_count,
                float (sizes |> Array.filter (fun s -> s >= 4) |> Array.length) / float row_count

        let by_col = Array.init keys (fun _ -> ResizeArray<float>())

        for t, c in notes do
            if c >= 0 && c < keys then
                by_col.[c].Add(t)

        let mutable jack_hits = 0
        let mutable jack_strict_hits = 0
        let mutable vibro_hits = 0
        let mutable minijack_hits = 0
        let mutable total_pairs = 0

        for col in by_col do
            let times = col.ToArray() |> Array.sort

            for i = 1 to times.Length - 1 do
                let gap = times.[i] - times.[i - 1]
                total_pairs <- total_pairs + 1

                if gap <= 80.0 then
                    vibro_hits <- vibro_hits + 1
                    jack_strict_hits <- jack_strict_hits + 1
                    jack_hits <- jack_hits + 1
                elif gap <= 120.0 then
                    jack_strict_hits <- jack_strict_hits + 1
                    jack_hits <- jack_hits + 1
                elif gap <= 180.0 then
                    jack_hits <- jack_hits + 1

                if gap >= 100.0 && gap <= 200.0 then
                    minijack_hits <- minijack_hits + 1

        let jack_ratio, jack_density, vibro_density =
            if note_count > 0 && total_pairs > 0 then
                float jack_hits / float total_pairs,
                float jack_strict_hits / float total_pairs,
                float vibro_hits / float total_pairs
            else
                0.0, 0.0, 0.0

        let minijack_ratio = float minijack_hits / float (max total_pairs 1)

        let anchor_ratio =
            if note_count = 0 then
                0.0
            else
                let col_counts = by_col |> Array.map (fun c -> c.Count)
                let max_col = if col_counts.Length = 0 then 0 else Array.max col_counts
                let num_cols = max 1 (col_counts |> Array.filter (fun c -> c > 0) |> Array.length)
                let expected = float note_count / float num_cols
                max 0.0 ((float max_col - expected) / float (max note_count 1))

        let transition_var =
            if rows.Length < 2 then
                0.0
            else
                let diffs = ResizeArray<float>()

                for i = 1 to rows.Length - 1 do
                    let prev = Set.ofArray (snd rows.[i - 1])
                    let curr = Set.ofArray (snd rows.[i])
                    let union = Set.union prev curr |> Set.count
                    let inter = Set.intersect prev curr |> Set.count

                    if union > 0 then
                        diffs.Add(1.0 - float inter / float union)

                if diffs.Count = 0 then 0.0 else Seq.average diffs

        let times_only = notes |> Array.map fst
        let nps_data = nps_windows (times_only, 500.0, 250.0)
        let nps_values = if nps_data.Length = 0 then [| 0.0 |] else nps_data |> Array.map snd
        let density_cv = min 2.0 (cv nps_values)
        let nps_sorted = Array.sort nps_values

        let nps_p90 =
            if nps_sorted.Length = 0 then
                0.0
            else
                nps_sorted.[min (int (0.90 * float nps_sorted.Length)) (nps_sorted.Length - 1)]

        let nps_sustained_top30 =
            let cutoff = int (0.70 * float nps_sorted.Length)
            let top = if nps_sorted.Length = 0 then [||] else nps_sorted.[cutoff..]
            if top.Length = 0 then 0.0 else Array.average top

        let active_vals = nps_values |> Array.filter (fun v -> v > 2.0)
        let nps_active_ratio = float active_vals.Length / float (max nps_values.Length 1)
        let nps_active_cv = if active_vals.Length >= 2 then cv active_vals else 0.0

        let drain_s =
            if times_only.Length = 0 then
                0.0
            else
                (times_only.[times_only.Length - 1] - times_only.[0]) / 1000.0

        let pattern_irregularity =
            if times_only.Length = 0 || drain_s <= 0.0 then
                0.0
            else
                let t_min = times_only.[0]
                let t_max = times_only.[times_only.Length - 1]
                let entropies = ResizeArray<float>()
                let mutable t = t_min
                let mutable ni = 0

                while t <= t_max do
                    let lo = t
                    let hi = t + 2000.0

                    while ni < times_only.Length && times_only.[ni] < lo do
                        ni <- ni + 1

                    let col_counts_w = Array.zeroCreate 4
                    let mutable total_w = 0

                    let mutable j = ni

                    while j < notes.Length && fst notes.[j] <= hi do
                        let c = snd notes.[j]

                        if c >= 0 && c <= 3 then
                            col_counts_w.[c] <- col_counts_w.[c] + 1
                            total_w <- total_w + 1

                        j <- j + 1

                    if total_w >= 4 then
                        let mutable h = 0.0

                        for cnt in col_counts_w do
                            if cnt > 0 then
                                let p = float cnt / float total_w
                                h <- h - p * Math.Log(p, 2.0)

                        entropies.Add(h)

                    t <- t + 1000.0

                if entropies.Count >= 2 then cv (entropies.ToArray()) else 0.0

        let timing_irregularity =
            if row_count < 3 then
                0.0
            else
                let gaps =
                    [|
                        for i = 1 to rows.Length - 1 do
                            yield fst rows.[i] - fst rows.[i - 1]
                    |]

                cv gaps

        let chord_complexity =
            let frac = hand_ratio + quad_ratio

            if frac <= 0.0 || row_count = 0 || nps_values.Length = 0 then
                0.0
            else
                let chord_times =
                    rows
                    |> Array.choose (fun (t, cols) -> if cols.Length >= 3 then Some t else None)
                    |> Set.ofArray

                if chord_times.IsEmpty then
                    0.0
                else
                    let overall_mean = Array.average nps_values

                    let during =
                        nps_data
                        |> Array.choose (fun (center, nps) ->
                            let lo = center - 250.0
                            let hi = center + 250.0

                            if chord_times |> Set.exists (fun ct -> lo <= ct && ct <= hi) then
                                Some nps
                            else
                                None)

                    if during.Length > 0 && overall_mean > 0.0 then
                        let density_amp = Array.average during / overall_mean
                        frac * (1.0 + density_amp)
                    else
                        frac

        {
            StreamPurity = stream_purity
            JumpRatio = jump_ratio
            HandRatio = hand_ratio
            QuadRatio = quad_ratio
            JackRatio = jack_ratio
            JackDensity = jack_density
            VibroDensity = vibro_density
            AnchorRatio = anchor_ratio
            MinijackRatio = minijack_ratio
            DensityCv = density_cv
            TransitionVar = transition_var
            NpsP90 = nps_p90
            NpsSustainedTop30 = nps_sustained_top30
            NpsActiveRatio = nps_active_ratio
            NpsActiveCv = nps_active_cv
            Bpm = bpm
            DurationS = drain_s
            PatternIrregularity = pattern_irregularity
            TimingIrregularity = timing_irregularity
            ChordComplexity = chord_complexity
        }
