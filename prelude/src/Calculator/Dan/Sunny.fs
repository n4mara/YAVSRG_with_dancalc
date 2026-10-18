namespace Prelude.Calculator.Dan

open System
open Prelude
open Prelude.Charts

module Sunny =

    type Result =
        {
            Sr: float
            JbarMax: float
            PbarMax: float
            XbarMax: float
            AbarMean: float
            TotalNotes: int
            TotalNotesEff: int
            DurationMs: float
            JbarShare: float
            JackRatio: float
            D93: float
            D83: float
            DWeightedMean: float
        }

    let private clip (lo: int) (hi: int) (v: int) : int = max lo (min hi v)

    let private searchsorted_left (arr: float array) (v: float) : int =
        let mutable lo = 0
        let mutable hi = arr.Length

        while lo < hi do
            let mid = (lo + hi) / 2

            if arr.[mid] < v then
                lo <- mid + 1
            else
                hi <- mid

        lo

    let private searchsorted_right (arr: float array) (v: float) : int =
        let mutable lo = 0
        let mutable hi = arr.Length

        while lo < hi do
            let mid = (lo + hi) / 2

            if arr.[mid] <= v then
                lo <- mid + 1
            else
                hi <- mid

        lo

    let gaussian_filter1d (data: float array, sigma: float, mode: string, cval: float) : float array =
        let kernel_radius = int (4.0 * sigma + 0.5)
        let kernel_len = 2 * kernel_radius + 1
        let kernel = Array.zeroCreate<float> kernel_len
        let mutable kernel_sum = 0.0

        for i = 0 to kernel_len - 1 do
            let x = float (i - kernel_radius)
            let v = exp (-0.5 * (x / sigma) ** 2.0)
            kernel.[i] <- v
            kernel_sum <- kernel_sum + v

        for i = 0 to kernel_len - 1 do
            kernel.[i] <- kernel.[i] / kernel_sum

        let n = data.Length
        let padded = Array.zeroCreate<float> (n + 2 * kernel_radius)

        if mode = "constant" then
            for i = 0 to kernel_radius - 1 do
                padded.[i] <- cval
                padded.[n + kernel_radius + i] <- cval

            for i = 0 to n - 1 do
                padded.[kernel_radius + i] <- data.[i]
        else
            for i = 0 to n - 1 do
                padded.[kernel_radius + i] <- data.[i]

            for i = 0 to kernel_radius - 1 do
                padded.[i] <- data.[0]
                padded.[n + kernel_radius + i] <- data.[n - 1]

        let result = Array.zeroCreate<float> n

        for i = 0 to n - 1 do
            let mutable acc = 0.0

            for j = 0 to kernel_len - 1 do
                acc <- acc + padded.[i + j] * kernel.[j]

            result.[i] <- acc

        result

    let cumulative_sum (x: float array, f: float array) : float array =
        let n = x.Length
        let F = Array.zeroCreate<float> n
        let mutable running = 0.0

        for i = 1 to n - 1 do
            running <- running + f.[i - 1] * (x.[i] - x.[i - 1])
            F.[i] <- running

        F

    let smooth_on_corners (x: float array, f: float array, window: float, scale: float, mode: string) : float array =
        let n = x.Length
        let F = cumulative_sum (x, f)
        let result = Array.zeroCreate<float> n
        let max_idx = n - 2

        let query (q: float) : float =
            let idx = clip 0 max_idx (searchsorted_left x q - 1)
            F.[idx] + f.[idx] * (q - x.[idx])

        for i = 0 to n - 1 do
            let a = max x.[0] (min x.[n - 1] (x.[i] - window))
            let b = max x.[0] (min x.[n - 1] (x.[i] + window))
            let value = query b - query a

            if mode = "avg" then
                let span = b - a
                result.[i] <- if span > 0.0 then value / span else 0.0
            else
                result.[i] <- scale * value

        result

    let interp_values (new_x: float array, old_x: float array, old_vals: float array) : float array =
        let n_old = old_x.Length
        let result = Array.zeroCreate<float> new_x.Length

        for i = 0 to new_x.Length - 1 do
            let q = new_x.[i]

            if q <= old_x.[0] then
                result.[i] <- old_vals.[0]
            elif q >= old_x.[n_old - 1] then
                result.[i] <- old_vals.[n_old - 1]
            else
                let hi = searchsorted_left old_x q
                let lo = hi - 1
                let x0 = old_x.[lo]
                let x1 = old_x.[hi]
                let t = if x1 > x0 then (q - x0) / (x1 - x0) else 0.0
                result.[i] <- old_vals.[lo] + t * (old_vals.[hi] - old_vals.[lo])

        result

    let step_interp (new_x: float array, old_x: float array, old_vals: float array) : float array =
        let result = Array.zeroCreate<float> new_x.Length
        let max_idx = old_vals.Length - 1

        for i = 0 to new_x.Length - 1 do
            let idx = clip 0 max_idx (searchsorted_right old_x new_x.[i] - 1)
            result.[i] <- old_vals.[idx]

        result

    let rescale_high (sr: float) : float =
        if sr <= 9.0 then sr else 9.0 + (sr - 9.0) / 1.2

    let get_corners (T: float, note_seq: NoteHit list) : float array * float array * float array =
        let corners_base =
            seq {
                yield 0.0
                yield T

                for n in note_seq do
                    let h = n.TimeMs
                    yield h
                    yield h + 501.0
                    yield h - 499.0
                    yield h + 1.0
            }
            |> Seq.filter (fun s -> s >= 0.0 && s <= T)
            |> Seq.distinct
            |> Seq.sort
            |> Seq.toArray

        let corners_A =
            seq {
                yield 0.0
                yield T

                for n in note_seq do
                    let h = n.TimeMs
                    yield h
                    yield h + 1000.0
                    yield h - 1000.0
            }
            |> Seq.filter (fun s -> s >= 0.0 && s <= T)
            |> Seq.distinct
            |> Seq.sort
            |> Seq.toArray

        let all_corners =
            Seq.append corners_base corners_A
            |> Seq.distinct
            |> Seq.sort
            |> Seq.toArray

        all_corners, corners_base, corners_A

    let get_key_usage (K: int, T: float, note_seq: NoteHit list, base_corners: float array) : bool array array =
        let key_usage = Array.init K (fun _ -> Array.zeroCreate<bool> base_corners.Length)
        let by_col = Array.init K (fun _ -> ResizeArray<float>())

        for n in note_seq do
            by_col.[n.Column].Add n.TimeMs

        for k = 0 to K - 1 do
            let col_notes = by_col.[k]

            if col_notes.Count > 0 then
                let arr_k = key_usage.[k]

                for i = 0 to col_notes.Count - 1 do
                    let h = col_notes.[i]
                    let start = max (h - 150.0) 0.0
                    let end_ = min (h + 150.0) (T - 1.0)
                    let li = searchsorted_left base_corners start
                    let ri = searchsorted_left base_corners end_

                    for j = li to ri - 1 do
                        arr_k.[j] <- true

        key_usage

    let get_key_usage_400 (K: int, T: float, note_seq: NoteHit list, base_corners: float array) : float array array =
        let _ = T
        let key_usage_400 = Array.init K (fun _ -> Array.zeroCreate<float> base_corners.Length)
        let inv_400_sq = 3.75 / 160000.0
        let by_col = Array.init K (fun _ -> ResizeArray<float>())

        for n in note_seq do
            by_col.[n.Column].Add(max n.TimeMs 0.0)

        for k = 0 to K - 1 do
            let col_notes = by_col.[k]

            if col_notes.Count > 0 then
                let arr_k = key_usage_400.[k]

                for i = 0 to col_notes.Count - 1 do
                    let start = col_notes.[i]
                    let li = searchsorted_left base_corners (start - 400.0)
                    let ri = searchsorted_left base_corners (start + 400.0)
                    let mid = searchsorted_left base_corners start
                    arr_k.[mid] <- arr_k.[mid] + 3.75

                    if mid > li then
                        for j = li to mid - 1 do
                            let diff = base_corners.[j] - start
                            arr_k.[j] <- arr_k.[j] + 3.75 - inv_400_sq * (diff * diff)

                    if ri > mid + 1 then
                        for j = mid + 1 to ri - 1 do
                            let diff = base_corners.[j] - start
                            arr_k.[j] <- arr_k.[j] + 3.75 - inv_400_sq * (diff * diff)

        key_usage_400

    let compute_anchor (K: int, key_usage_400: float array array, base_corners: float array) : float array =
        let n = base_corners.Length
        let result = Array.zeroCreate<float> n
        let row = Array.zeroCreate<float> K

        for i = 0 to n - 1 do
            for k = 0 to K - 1 do
                row.[k] <- key_usage_400.[k].[i]

            Array.Sort(row, fun a b -> compare b a)

            let mutable n_nz = 0

            for k = 0 to K - 1 do
                if row.[k] > 0.0 then
                    n_nz <- n_nz + 1

            let mutable walk = 0.0
            let mutable max_walk = 0.0

            for k = 0 to K - 2 do
                let c0 = row.[k]
                let c1 = row.[k + 1]

                if c0 > 0.0 && c1 > 0.0 then
                    let ratio = c1 / c0
                    let weight = 1.0 - 4.0 * (0.5 - ratio) ** 2.0
                    walk <- walk + c0 * weight
                    max_walk <- max_walk + c0

            let raw_anchor =
                if n_nz > 1 then
                    walk / max max_walk 1e-9
                else
                    0.0

            result.[i] <- 1.0 + min (raw_anchor - 0.18) (5.0 * (raw_anchor - 0.22) ** 3.0)

        result

    let private jack_nerfer (delta: float) : float =
        1.0 - 7e-5 * (0.15 + abs (delta - 0.08)) ** -4.0

    let compute_Jbar
        (K: int, T: float, x: float, note_seq_by_column: NoteHit list array, base_corners: float array)
        : float array array * float array =
        let _ = T
        let n = base_corners.Length
        let J_ks = Array.init K (fun _ -> Array.zeroCreate<float> n)
        let delta_ks = Array.init K (fun _ -> Array.create n 1e9)

        for k = 0 to K - 1 do
            let notes = note_seq_by_column.[k]

            if notes.Length >= 2 then
                let arr_j = J_ks.[k]
                let arr_d = delta_ks.[k]

                for i = 0 to notes.Length - 2 do
                    let start = notes.[i].TimeMs
                    let end_ = notes.[i + 1].TimeMs
                    let delta = 0.001 * (end_ - start)
                    let value =
                        delta ** -1.0
                        * (delta + 0.11 * x ** 0.25) ** -1.0
                        * jack_nerfer delta

                    let li = searchsorted_left base_corners start
                    let ri = searchsorted_left base_corners end_

                    if ri > li then
                        for j = li to ri - 1 do
                            arr_j.[j] <- value
                            arr_d.[j] <- delta

        let Jbar_ks =
            Array.init K (fun k ->
                smooth_on_corners (base_corners, J_ks.[k], 500.0, 0.001, "sum"))

        let Jbar = Array.zeroCreate<float> n

        for i = 0 to n - 1 do
            let mutable num = 0.0
            let mutable den = 0.0

            for k = 0 to K - 1 do
                let weight = 1.0 / delta_ks.[k].[i]
                let jv = max Jbar_ks.[k].[i] 0.0
                num <- num + jv ** 5.0 * weight
                den <- den + weight

            Jbar.[i] <- (num / max den 1e-9) ** 0.2

        delta_ks, Jbar

    let private cross_matrix: float array array =
        [|
            [| -1.0 |]
            [| 0.075; 0.075 |]
            [| 0.125; 0.05; 0.125 |]
            [| 0.125; 0.125; 0.125; 0.125 |]
            [| 0.175; 0.25; 0.05; 0.25; 0.175 |]
            [| 0.175; 0.25; 0.175; 0.175; 0.25; 0.175 |]
            [| 0.225; 0.35; 0.25; 0.05; 0.25; 0.35; 0.225 |]
            [| 0.225; 0.35; 0.25; 0.225; 0.225; 0.25; 0.35; 0.225 |]
            [| 0.275; 0.45; 0.35; 0.25; 0.05; 0.25; 0.35; 0.45; 0.275 |]
            [| 0.275; 0.45; 0.35; 0.25; 0.275; 0.275; 0.25; 0.35; 0.45; 0.275 |]
            [| 0.325; 0.55; 0.45; 0.35; 0.25; 0.05; 0.25; 0.35; 0.45; 0.55; 0.325 |]
        |]

    let private active_contains (active: int list array) (corner: int) (key: int) : bool =
        List.contains key active.[corner]

    let compute_Xbar
        (
            K: int,
            T: float,
            x: float,
            note_seq_by_column: NoteHit list array,
            active_columns: int list array,
            base_corners: float array
        )
        : float array =
        let _ = T
        let cross_coeff = cross_matrix.[K]
        let n = base_corners.Length
        let X_ks = Array.init (K + 1) (fun _ -> Array.zeroCreate<float> n)
        let fast_cross = Array.init (K + 1) (fun _ -> Array.zeroCreate<float> n)

        for k = 0 to K do
            let notes_in_pair =
                if k = 0 then
                    note_seq_by_column.[0]
                elif k = K then
                    note_seq_by_column.[K - 1]
                else
                    List.append note_seq_by_column.[k - 1] note_seq_by_column.[k]
                    |> List.sortBy (fun t -> t.TimeMs)

            if notes_in_pair.Length >= 2 then
                let arr_x = X_ks.[k]
                let arr_fc = fast_cross.[k]
                let x_floor = max 0.06 (0.75 * x)

                for i = 0 to notes_in_pair.Length - 2 do
                    let start = notes_in_pair.[i].TimeMs
                    let end_ = notes_in_pair.[i + 1].TimeMs
                    let delta = 0.001 * (end_ - start)
                    let mutable value = 0.16 * (max x delta) ** -2.0
                    let fc_val = max 0.0 (0.4 * (max delta x_floor) ** -2.0 - 80.0)
                    let li = searchsorted_left base_corners start
                    let ri = searchsorted_left base_corners end_

                    if ri > li then
                        let left_inactive =
                            not (active_contains active_columns li (k - 1))
                            && not (active_contains active_columns ri (k - 1))

                        let right_inactive =
                            not (active_contains active_columns li k)
                            && not (active_contains active_columns ri k)

                        if left_inactive || right_inactive then
                            value <- value * (1.0 - cross_coeff.[k])

                        for j = li to ri - 1 do
                            arr_x.[j] <- value
                            arr_fc.[j] <- fc_val

        let X_base = Array.zeroCreate<float> n

        for i = 0 to n - 1 do
            let mutable acc = 0.0

            for k = 0 to K do
                acc <- acc + X_ks.[k].[i] * cross_coeff.[k]

            for k = 0 to K - 1 do
                acc <-
                    acc
                    + sqrt (
                        fast_cross.[k].[i]
                        * cross_coeff.[k]
                        * fast_cross.[k + 1].[i]
                        * cross_coeff.[k + 1]
                    )

            X_base.[i] <- acc

        smooth_on_corners (base_corners, X_base, 500.0, 0.001, "sum")

    let private stream_booster (delta: float) : float =
        let bpm = max 0.0 (min 420.0 (7.5 / delta))
        let primary = 0.10 / (1.0 + exp (-0.06 * (bpm - 175.0)))

        let secondary =
            if bpm >= 200.0 && bpm <= 350.0 then
                0.30 * (1.0 - exp (-0.02 * (bpm - 200.0)))
            else
                0.0

        1.0 + primary + secondary

    let compute_Pbar
        (K: int, T: float, x: float, note_seq: NoteHit list, anchor: float array, base_corners: float array)
        : float array =
        let _ = K, T
        let n = base_corners.Length
        let P_step = Array.zeroCreate<float> n
        let notes = List.toArray note_seq

        if notes.Length > 1 then
            let base_inc = (0.08 * x ** -1.0 * (1.0 - 24.0 * x ** -1.0 * (x / 6.0) ** 2.0)) ** 0.25
            let spike_val = 1000.0 * (0.02 * (4.0 / x - 24.0)) ** 0.25

            for i = 0 to notes.Length - 2 do
                let h_l = notes.[i].TimeMs
                let h_r = notes.[i + 1].TimeMs
                let dt = h_r - h_l
                let li = searchsorted_left base_corners h_l

                if dt < 1e-9 then
                    let ri_spike = searchsorted_right base_corners h_l

                    if ri_spike > li then
                        for j = li to ri_spike - 1 do
                            P_step.[j] <- P_step.[j] + spike_val
                else
                    let ri = searchsorted_left base_corners h_r

                    if ri > li then
                        let delta = 0.001 * dt
                        let b_val = stream_booster delta

                        let inc =
                            if delta < 2.0 * x / 3.0 then
                                delta ** -1.0
                                * (0.08 * x ** -1.0 * (1.0 - 24.0 * x ** -1.0 * (delta - x / 2.0) ** 2.0))
                                  ** 0.25
                                * max b_val 1.0
                            else
                                delta ** -1.0 * base_inc * max b_val 1.0

                        for j = li to ri - 1 do
                            let seg_anchor = anchor.[j]
                            let capped = min (inc * seg_anchor) (max inc (inc * 2.0 - 10.0))
                            P_step.[j] <- P_step.[j] + capped

        smooth_on_corners (base_corners, P_step, 500.0, 0.001, "sum")

    let compute_Abar
        (
            K: int,
            T: float,
            x: float,
            note_seq_by_column: NoteHit list array,
            active_columns: int list array,
            delta_ks: float array array,
            A_corners: float array,
            base_corners: float array
        )
        : float array =
        let _ = T, x, note_seq_by_column
        let n_base = base_corners.Length
        let dks = Array.init (max 0 (K - 1)) (fun _ -> Array.zeroCreate<float> n_base)

        for i = 0 to n_base - 1 do
            let cols = active_columns.[i]

            for j = 0 to cols.Length - 2 do
                let k0 = cols.[j]
                let k1 = cols.[j + 1]
                let dk0 = delta_ks.[k0].[i]
                let dk1 = delta_ks.[k1].[i]

                dks.[k0].[i] <-
                    abs (dk0 - dk1) + 0.4 * max 0.0 (max dk0 dk1 - 0.11)

        let A_step = Array.create A_corners.Length 1.0
        let max_bc = n_base - 1

        for i = 0 to A_corners.Length - 1 do
            let idx = clip 0 max_bc (searchsorted_left base_corners A_corners.[i])
            let cols = active_columns.[idx]

            for j = 0 to cols.Length - 2 do
                let k0 = cols.[j]
                let k1 = cols.[j + 1]
                let d_val = dks.[k0].[idx]
                let dk0 = delta_ks.[k0].[idx]
                let dk1 = delta_ks.[k1].[idx]

                if d_val < 0.02 then
                    A_step.[i] <- A_step.[i] * min (0.75 + 0.5 * max dk0 dk1) 1.0
                elif d_val < 0.07 then
                    A_step.[i] <- A_step.[i] * min (0.65 + 5.0 * d_val + 0.5 * max dk0 dk1) 1.0

        smooth_on_corners (A_corners, A_step, 250.0, 1.0, "avg")

    let compute_C_and_Ks
        (K: int, T: float, note_seq: NoteHit list, key_usage: bool array array, base_corners: float array)
        : float array * float array =
        let _ = T
        let note_hit_times =
            note_seq
            |> List.map (fun n -> n.TimeMs)
            |> List.sort
            |> List.toArray

        let n = base_corners.Length
        let C_step = Array.zeroCreate<float> n
        let Ks_step = Array.zeroCreate<float> n

        for i = 0 to n - 1 do
            let lo = searchsorted_left note_hit_times (base_corners.[i] - 500.0)
            let hi = searchsorted_left note_hit_times (base_corners.[i] + 500.0)
            C_step.[i] <- float (hi - lo)

            let mutable active = 0

            for k = 0 to K - 1 do
                if key_usage.[k].[i] then
                    active <- active + 1

            Ks_step.[i] <- float (max active 1)

        C_step, Ks_step

    let private array_max (arr: float array) : float =
        let mutable m = arr.[0]

        for i = 1 to arr.Length - 1 do
            if arr.[i] > m then
                m <- arr.[i]

        m

    let private array_mean (arr: float array) : float =
        let mutable s = 0.0

        for i = 0 to arr.Length - 1 do
            s <- s + arr.[i]

        s / float arr.Length

    let calculate (rate: Rate, note_data: NoteData, od: float) : Result option =
        let K, x, note_seq = Adapter.to_note_seq (rate, note_data, od)

        if List.isEmpty note_seq then
            None
        else
            let T = (note_seq |> List.map (fun n -> n.TimeMs) |> List.max) + 1.0
            let note_seq_by_column =
                Array.init K (fun k -> note_seq |> List.filter (fun n -> n.Column = k))

            let all_corners, base_corners, A_corners = get_corners (T, note_seq)
            let key_usage = get_key_usage (K, T, note_seq, base_corners)

            let active_columns =
                Array.init base_corners.Length (fun i ->
                    [
                        for k = 0 to K - 1 do
                            if key_usage.[k].[i] then
                                yield k
                    ])

            let key_usage_400 = get_key_usage_400 (K, T, note_seq, base_corners)
            let anchor = compute_anchor (K, key_usage_400, base_corners)
            let delta_ks, Jbar0 = compute_Jbar (K, T, x, note_seq_by_column, base_corners)
            let Jbar = interp_values (all_corners, base_corners, Jbar0)
            let Xbar0 = compute_Xbar (K, T, x, note_seq_by_column, active_columns, base_corners)
            let Xbar = interp_values (all_corners, base_corners, Xbar0)
            let Pbar0 = compute_Pbar (K, T, x, note_seq, anchor, base_corners)
            let Pbar = interp_values (all_corners, base_corners, Pbar0)
            let Abar0 = compute_Abar (K, T, x, note_seq_by_column, active_columns, delta_ks, A_corners, base_corners)
            let Abar = interp_values (all_corners, A_corners, Abar0)
            let C_step, Ks_step = compute_C_and_Ks (K, T, note_seq, key_usage, base_corners)
            let C_arr = step_interp (all_corners, base_corners, C_step)
            let Ks_arr = step_interp (all_corners, base_corners, Ks_step)

            let n = all_corners.Length
            let D_all = Array.zeroCreate<float> n
            let gaps = Array.zeroCreate<float> n

            for i = 0 to n - 1 do
                let S =
                    ((0.4
                      * (Abar.[i] ** (3.0 / Ks_arr.[i]) * min Jbar.[i] (8.0 + 0.85 * Jbar.[i]))
                        ** 1.5)
                     + (0.6 * (Abar.[i] ** (2.0 / 3.0) * (0.8 * Pbar.[i])) ** 1.5))
                    ** (2.0 / 3.0)

                let t_term = (Abar.[i] ** (3.0 / Ks_arr.[i]) * Xbar.[i]) / (Xbar.[i] + S + 1.0)
                D_all.[i] <- 2.7 * (S ** 0.5) * (t_term ** 1.5) + S * 0.27

            gaps.[0] <- (all_corners.[1] - all_corners.[0]) / 2.0
            gaps.[n - 1] <- (all_corners.[n - 1] - all_corners.[n - 2]) / 2.0

            for i = 1 to n - 2 do
                gaps.[i] <- (all_corners.[i + 1] - all_corners.[i - 1]) / 2.0

            let effective_weights = Array.init n (fun i -> C_arr.[i] * gaps.[i])
            let indices = Array.init n id
            Array.Sort(D_all, indices)
            let D_sorted = Array.copy D_all
            let w_sorted = Array.init n (fun i -> effective_weights.[indices.[i]])

            let cum_weights = Array.zeroCreate<float> n
            let mutable running = 0.0

            for i = 0 to n - 1 do
                running <- running + w_sorted.[i]
                cum_weights.[i] <- running

            let total_w = cum_weights.[n - 1]
            let norm_cum = Array.init n (fun i -> cum_weights.[i] / total_w)

            let target_percentiles =
                [| 0.945; 0.935; 0.925; 0.915; 0.845; 0.835; 0.825; 0.815 |]

            let pct_indices =
                Array.map (fun p -> searchsorted_left norm_cum p) target_percentiles

            let mutable sum93 = 0.0
            let mutable sum83 = 0.0

            for i = 0 to 3 do
                sum93 <- sum93 + D_sorted.[pct_indices.[i]]
                sum83 <- sum83 + D_sorted.[pct_indices.[i + 4]]

            let percentile_93 = sum93 / 4.0
            let percentile_83 = sum83 / 4.0

            let mutable num5 = 0.0
            let mutable den5 = 0.0

            for i = 0 to n - 1 do
                num5 <- num5 + D_sorted.[i] ** 5.0 * w_sorted.[i]
                den5 <- den5 + w_sorted.[i]

            let weighted_mean = (num5 / den5) ** 0.2
            let total_notes = note_seq.Length
            let jbar_max = array_max Jbar
            let pbar_max = array_max Pbar
            let xbar_max = array_max Xbar
            let comp_total = jbar_max + pbar_max + xbar_max + 1.0
            let j_mean = jbar_max
            let nj_mean = pbar_max + xbar_max

            let mutable sr =
                0.88 * percentile_93 * 0.25 + 0.94 * percentile_83 * 0.2 + weighted_mean * 0.55

            sr <- sr * float total_notes / (float total_notes + 60.0)
            sr <- rescale_high sr * 0.975

            Some
                {
                    Sr = sr
                    JbarMax = jbar_max
                    PbarMax = pbar_max
                    XbarMax = xbar_max
                    AbarMean = array_mean Abar
                    TotalNotes = total_notes
                    TotalNotesEff = n
                    DurationMs = T - 1.0
                    JbarShare = jbar_max / comp_total
                    JackRatio = j_mean / max (j_mean + nj_mean) 1e-9
                    D93 = percentile_93
                    D83 = percentile_83
                    DWeightedMean = weighted_mean
                }
