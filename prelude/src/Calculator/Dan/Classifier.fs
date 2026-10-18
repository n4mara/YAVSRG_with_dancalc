namespace Prelude.Calculator.Dan

module Classifier =

    let private clamp (lo: float) (hi: float) (v: float) : float = max lo (min hi v)

    let classify_family
        (sunny: Sunny.Result, features: Features.ChartFeatures, bpm: float, drain_s: float)
        : string * float =
        let jbar_max = sunny.JbarMax
        let pbar_max = sunny.PbarMax
        let xbar_max = sunny.XbarMax
        let d93 = sunny.D93
        let d83 = sunny.D83

        let bar_total = jbar_max + pbar_max + 1.0
        let jack_dom = jbar_max / bar_total
        let stream_dom = pbar_max / bar_total
        let tech_dom =
            if (xbar_max + bar_total) > 0.0 then
                xbar_max / (xbar_max + bar_total)
            else
                0.0

        let peak_ratio = if d83 > 0.1 then d93 / max d83 0.01 else 1.0

        let stream_purity = features.StreamPurity
        let jack_ratio_broad = features.JackRatio
        let jack_density = features.JackDensity
        let vibro_density = features.VibroDensity
        let density_cv = min 2.0 features.DensityCv
        let transition_var = features.TransitionVar
        let jump_ratio = features.JumpRatio
        let hand_ratio = features.HandRatio
        let quad_ratio = features.QuadRatio
        let nps_sustained = features.NpsSustainedTop30
        let nps_active_ratio = features.NpsActiveRatio
        let nps_active_cv = features.NpsActiveCv
        let anchor_ratio = features.AnchorRatio
        let pattern_irregularity = features.PatternIrregularity
        let timing_irregularity = features.TimingIrregularity

        let chord_fraction = jump_ratio + hand_ratio + quad_ratio
        let rep_margin = max 0.0 (0.83 - transition_var)
        let is_repetitive = min 1.0 (rep_margin * 12.0)

        let jack_baseline =
            if bpm > 0.0 then
                let quarter_ms = 60000.0 / bpm
                min 0.9 (max 0.0 (1.0 - quarter_ms / 250.0))
            else
                0.3

        let jack_excess = max 0.0 (jack_ratio_broad - jack_baseline)
        let bpm_signal = min 1.5 (max 0.0 (bpm - 140.0) / 80.0)

        let jack_score =
            is_repetitive * 50.0
            + jack_density * 45.0
            + chord_fraction * 20.0
            + jack_excess * 15.0
            + vibro_density * 20.0
            + anchor_ratio * 15.0
            + jack_dom * 10.0

        let stream_score =
            stream_purity * 45.0
            + max 0.0 (1.0 - chord_fraction * 3.0) * 15.0
            - chord_fraction * 12.0
            + stream_dom * 20.0
            + max 0.0 (1.0 - density_cv) * 15.0
            + max 0.0 (0.5 - jack_density) * 10.0

        let tech_score =
            density_cv * 55.0
            + pattern_irregularity * 25.0
            + nps_active_cv * 20.0
            + tech_dom * 20.0
            + transition_var * 12.0
            + chord_fraction * 8.0

        let speed_raw =
            bpm_signal * 42.0
            + stream_purity * 22.0
            + max 0.0 (1.0 - density_cv) * 10.0
            + max 0.0 (peak_ratio - 1.02) * 12.0

        let speed_chord_gate = max 0.25 (1.0 - chord_fraction * 1.8)
        let speed_regularity = max 0.5 (1.0 - max 0.0 (density_cv - 0.30) * 2.5)
        let speed_score = speed_raw * speed_chord_gate * speed_regularity

        let drain_gate = max 0.0 (drain_s - 60.0) / 120.0
        let short_penalty = max 0.0 (1.0 - drain_s / 90.0)
        let chord_req = min 1.0 (max 0.0 (chord_fraction - 0.15) / 0.20)

        let stamina_raw =
            drain_gate * 28.0
            + chord_fraction * 25.0
            - stream_purity * 20.0
            + nps_active_ratio * 12.0
            + max 0.0 (1.0 - density_cv) * 8.0
            + nps_sustained * 0.3
            + min 1.0 (drain_s / 150.0) * 12.0

        let mutable stamina_score = stamina_raw * chord_req * max 0.4 (1.0 - short_penalty * 0.5)

        if is_repetitive > 0.15 then
            stamina_score <- stamina_score * max 0.3 (1.0 - is_repetitive * 0.7)

        let scores =
            [
                "stream", stream_score
                "jack", jack_score
                "tech", tech_score
                "speed", speed_score
                "stamina", stamina_score
            ]

        let ranked = scores |> List.sortByDescending snd
        let top_family, top_score = ranked.[0]
        let second_score = if ranked.Length > 1 then snd ranked.[1] else 0.0
        let gap = top_score - second_score
        let total = scores |> List.sumBy snd

        let mutable confidence =
            if total > 0.0 then
                min 1.0 (gap / max (total * 0.10) 1.0)
            else
                0.0

        let mutable family =
            if confidence < 0.15 then
                confidence <- max 0.1 confidence
                "hybrid"
            else
                top_family

        let score_of name =
            scores |> List.tryFind (fun (n, _) -> n = name) |> Option.map snd |> Option.defaultValue 0.0

        if (family = "hybrid" || family = "stream") && density_cv > 0.35 then
            let tech_s = score_of "tech"
            let stream_s = score_of "stream"

            if tech_s > 0.0 && stream_s > 0.0 && tech_s / stream_s > 0.75 then
                family <- "tech"
                confidence <- max confidence 0.3

        if family = "stream" && bpm >= 155.0 && stream_purity > 0.70 && chord_fraction < 0.30 then
            let speed_s = score_of "speed"
            let stream_s = score_of "stream"

            if speed_s > 0.0 && stream_s > 0.0 && speed_s / stream_s > 0.35 then
                family <- "speed"
                confidence <- max confidence 0.25

        if family = "jack" && float sunny.TotalNotesEff > 15000.0 then
            family <- if score_of "stamina" > score_of "tech" then "stamina" else "hybrid"
            confidence <- min confidence 0.5

        if (family = "stream" || family = "tech") && drain_s > 120.0 && chord_fraction > 0.25 then
            let stamina_s = score_of "stamina"
            let stream_s = score_of "stream"
            let tech_s = score_of "tech"

            if stamina_s > 0.0 && stamina_s > max stream_s tech_s * 0.85 then
                family <- "stamina"
                confidence <- max confidence 0.35

        if family <> "tech" && family <> "jack" && timing_irregularity > 0.4 then
            if
                jack_density > 0.08
                && chord_fraction > 0.15
                && chord_fraction < 0.55
                && stream_purity > 0.40
                && stream_purity < 0.85
            then
                let tech_s = score_of "tech"
                let winner_s = score_of family

                if tech_s > 0.0 && winner_s > 0.0 && tech_s > winner_s * 0.75 then
                    family <- "tech"
                    confidence <- max confidence 0.35

        family, clamp 0.0 1.0 confidence
