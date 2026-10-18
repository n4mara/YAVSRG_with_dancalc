namespace Prelude.Calculator.Dan

open System
open System.Text.Json

type RidgeSubrank =
    {
        SkKey: string
        DpSr: float
        DpChoke: float
        DpMsd: float
        ModSr: float
        RawSr: float
        Ti: float
        BurstRatio: float
        Dur: float
        Quad: float
        Hand: float
        Choke: float
        Bpm: float
    }

type RidgeBand =
    {
        Mean: float array
        Scale: float array
        Beta: float array
    }

type RidgeModel =
    {
        FeatureSet: string
        Dual: bool
        High: RidgeBand option
        Low: RidgeBand option
        Single: RidgeBand option
        LinBase: Map<string, float array>
        LinBlendK: float
        LinBlendX0: float
    }

module Ridge =

    let RIDGE_CAP = 1.0

    let private stream_names =
        [| "speed"; "hand"; "jack"; "chordjack"; "tech"; "stamina"; "course" |]

    let private stat_keys =
        [|
            "chord_rate"
            "three_rate"
            "overlap_rate"
            "rotation_rate"
            "fast_jack_rate"
            "anchor_rate"
            "hand_bias"
            "avg_nps"
            "active_duration_s"
        |]

    let private sustain_keys =
        [|
            "nps1k_q50"
            "nps1k_q75"
            "nps1k_q90"
            "nps4k_q50"
            "nps4k_q75"
            "nps4k_q90"
            "nps1k_flatness"
            "log_nps1k_mean"
            "dt_row_q25"
            "dt_row_q50"
            "dt_row_q75"
            "dt_hand_q25"
            "dt_hand_median"
            "dt_same_q25"
            "dt_same_median"
        |]

    let private sustain_center_nps_q75 = 19.5
    let private sustain_center_dt_row = 90.0

    let private sigmoid (x: float) (k: float) (x0: float) : float =
        let z = max (-50.0) (min 50.0 (k * (x - x0)))
        1.0 / (1.0 + Math.Exp(-z))

    let private float_array (el: JsonElement) : float array =
        [| for v in el.EnumerateArray() -> v.GetDouble() |]

    let private parse_band (el: JsonElement) : RidgeBand option =
        match el.TryGetProperty("mean"), el.TryGetProperty("scale"), el.TryGetProperty("beta") with
        | (true, m), (true, s), (true, b) when m.ValueKind = JsonValueKind.Array ->
            Some
                {
                    Mean = float_array m
                    Scale = float_array s
                    Beta = float_array b
                }
        | _ -> None

    let private parse_model (root: JsonElement) : RidgeModel =
        let feature_set =
            match root.TryGetProperty("feature_set") with
            | true, p when p.ValueKind = JsonValueKind.String -> p.GetString()
            | _ -> ""

        let dual =
            match root.TryGetProperty("dual") with
            | true, p -> p.ValueKind = JsonValueKind.True
            | _ -> false

        let sub (name: string) =
            match root.TryGetProperty(name) with
            | true, p when p.ValueKind = JsonValueKind.Object -> parse_band p
            | _ -> None

        let lin_base =
            match root.TryGetProperty("lin_base") with
            | true, p when p.ValueKind = JsonValueKind.Object ->
                seq {
                    for prop in p.EnumerateObject() do
                        if prop.Value.ValueKind = JsonValueKind.Array then
                            yield prop.Name, float_array prop.Value
                }
                |> Map.ofSeq
            | _ -> Map.empty

        let blend_k, blend_x0 =
            match root.TryGetProperty("lin_blend") with
            | true, p when p.ValueKind = JsonValueKind.Object ->
                defaultArg (Config.try_get_float (p, "k")) 8.0, defaultArg (Config.try_get_float (p, "x0")) 11.0
            | _ -> 8.0, 11.0

        {
            FeatureSet = feature_set
            Dual = dual
            High = sub "high"
            Low = sub "low"
            Single = if dual then None else parse_band root
            LinBase = lin_base
            LinBlendK = blend_k
            LinBlendX0 = blend_x0
        }

    let private model: Lazy<RidgeModel option> =
        lazy
            (try
                match Config.load_json "vsrg_ridge_model.json" with
                | Some doc ->
                    let m = parse_model doc.RootElement
                    doc.Dispose()
                    Some m
                | None -> None
             with _ ->
                 None)

    let is_available () : bool = model.Value.IsSome

    let private stat (stats: Map<string, float>) (key: string) : float =
        match Map.tryFind key stats with
        | Some v when not (Double.IsNaN v) -> v
        | _ -> 0.0

    let private feature_vector
        (feature_set: string)
        (bio: BioResult)
        (msd: MsdScores option)
        (sunny: Sunny.Result)
        (purity: float)
        (sub: RidgeSubrank)
        : float array =
        let stats = bio.Stats
        let st = stat stats
        let v = ResizeArray<float>()

        let summary_agg (k: string) =
            match Map.tryFind k bio.Summaries with
            | Some s -> s.Aggregate
            | None -> defaultArg (Map.tryFind k bio.Streams) 0.0

        let summary_q97 (k: string) =
            match Map.tryFind k bio.Summaries with
            | Some s -> s.Q97
            | None -> defaultArg (Map.tryFind k bio.StreamsQ97) 0.0

        for k in stream_names do
            v.Add(summary_agg k)

        let jack_agg = summary_agg "jack"
        let chordjack_agg = summary_agg "chordjack"
        let speed_agg = summary_agg "speed"
        let stamina_agg = summary_agg "stamina"

        let log_raw = Math.Log(1.0 + max 0.0 bio.RawAgg)
        v.Add log_raw

        for k in stat_keys do
            v.Add(st k)

        for k in stream_names do
            v.Add(summary_q97 k)

        let m =
            match msd with
            | Some m -> m
            | None ->
                {
                    Overall = 0.0
                    Stream = 0.0
                    Jumpstream = 0.0
                    Handstream = 0.0
                    Stamina = 0.0
                    Jackspeed = 0.0
                    Chordjack = 0.0
                    Technical = 0.0
                }

        let role_jack = max m.Jackspeed m.Chordjack
        let role_speed = max m.Stream m.Jumpstream
        let role_stamina = 0.7 * m.Stamina + 0.3 * m.Handstream
        let role_tech = m.Technical
        let total_roles = role_jack + role_speed + role_stamina + role_tech
        let total = if total_roles = 0.0 then 1.0 else total_roles

        v.Add(role_jack / total)
        v.Add(role_speed / total)
        v.Add(role_stamina / total)
        v.Add(role_tech / total)

        v.Add sunny.JbarMax
        v.Add sunny.PbarMax
        v.Add sunny.XbarMax
        v.Add sunny.AbarMean

        v.Add m.Overall
        v.Add m.Stream
        v.Add m.Jumpstream
        v.Add m.Handstream
        v.Add m.Stamina
        v.Add m.Jackspeed
        v.Add m.Chordjack
        v.Add m.Technical

        v.Add(st "chord_rate" * st "fast_jack_rate")
        v.Add(st "overlap_rate" * st "rotation_rate")
        v.Add(jack_agg / (chordjack_agg + 1e-6))
        v.Add(speed_agg * purity)
        v.Add(st "anchor_imbalance")
        v.Add(st "peak_to_sustain_gap")
        v.Add(st "break_density")

        let burst_ratio = if sub.BurstRatio = 0.0 then 1.0 else sub.BurstRatio

        v.Add(st "fast_jack_rate" * (100.0 / (st "same_hand_q10" + 10.0)))
        v.Add(st "rotation_rate" * sub.Ti)
        v.Add(burst_ratio * st "peak_to_sustain_gap")
        v.Add(st "avg_nps" * sub.Dur / 60.0)
        v.Add(sub.Quad * sub.Hand)
        v.Add(st "rotation_rate" * (1.0 - st "overlap_rate"))
        v.Add(sub.Choke / (st "avg_nps" + 1.0))
        v.Add(abs (sub.DpSr - sub.DpMsd))

        let dp_sr = sub.DpSr
        let dp_choke = sub.DpChoke
        let dp_msd = sub.DpMsd
        let mod_sr = sub.ModSr
        let raw_sr = sub.RawSr
        let jbar = sunny.JbarMax
        let pbar = sunny.PbarMax
        let msd_overall = m.Overall

        v.Add(Math.Sqrt(max 0.0 log_raw))
        v.Add(log_raw * log_raw)
        v.Add(raw_sr / (dp_sr + 1e-6))
        v.Add(dp_choke / (dp_msd + 1e-6))
        v.Add(mod_sr - raw_sr)
        v.Add((dp_sr + dp_choke + dp_msd) / 3.0)
        v.Add(max 0.0 (dp_msd - dp_sr))
        v.Add(max 0.0 (dp_sr - dp_msd))
        v.Add(max 0.0 (dp_choke - dp_sr))
        v.Add(abs (jbar - pbar))
        v.Add(jbar / (pbar + 1e-6))
        v.Add(msd_overall / (raw_sr * 3.5 + 1e-6))

        let bpm_val = if sub.Bpm = 0.0 then 180.0 else sub.Bpm
        let bpm_excess = max 0.0 (bpm_val - 240.0) / 100.0

        v.Add bpm_excess
        v.Add(bpm_excess * stamina_agg)
        v.Add(bpm_excess * speed_agg)
        v.Add(stamina_agg / (dp_choke + 1e-6))
        v.Add(jack_agg / (dp_choke + 1e-6))
        v.Add(speed_agg / (dp_sr + 1e-6))

        let dur_val = if sub.Dur = 0.0 then 120.0 else sub.Dur
        let avg_nps = st "avg_nps"

        v.Add((dp_choke - dp_sr) * purity)
        v.Add((dp_msd - dp_sr) * purity)
        v.Add(Math.Log(1.0 + max 0.0 dur_val) / Math.Log(1.0 + 180.0))
        v.Add(bpm_val / (avg_nps * 12.0 + 1e-6))

        v.Add dp_sr
        v.Add dp_choke
        v.Add dp_msd
        v.Add mod_sr
        v.Add raw_sr

        let q10 = st "same_hand_q10"
        v.Add(max 0.0 (min 1.0 ((q10 - 60.0) / 40.0)))

        if feature_set = "sustain_v1" || feature_set = "sustain_v2" then
            for k in sustain_keys do
                v.Add(st k)

        if feature_set = "sustain_v2" then
            let nps_q75 = st "nps1k_q75"
            let dt_row = st "dt_row_q50"

            for tag in [| "jack"; "speed"; "stamina"; "tech" |] do
                let oh = if sub.SkKey = tag then 1.0 else 0.0
                v.Add(oh * (nps_q75 - sustain_center_nps_q75) / 10.0)
                v.Add(oh * (dt_row - sustain_center_dt_row) / 100.0)

        v.ToArray()

    let private band_dot (band: RidgeBand) (v: float array) : float =
        let mutable acc = 0.0

        for i = 0 to v.Length - 1 do
            acc <- acc + (v.[i] - band.Mean.[i]) / band.Scale.[i] * band.Beta.[i]

        acc

    let private band_ok (band: RidgeBand) (v: float array) : bool =
        band.Mean.Length = v.Length && band.Scale.Length = v.Length && band.Beta.Length = v.Length

    let private clip_cap (x: float) : float = max (-RIDGE_CAP) (min RIDGE_CAP x)

    let apply_correction
        (dp_base: float)
        (bio: BioResult option)
        (msd: MsdScores option)
        (sunny: Sunny.Result)
        (purity: float)
        (subrank: RidgeSubrank)
        : float * float =
        match bio, model.Value with
        | Some b, Some m ->
            try
                let v = feature_vector m.FeatureSet b msd sunny purity subrank

                match m.Dual, m.High, m.Low, m.Single with
                | true, Some high, Some low, _ ->
                    if not (band_ok high v) || not (band_ok low v) then
                        dp_base, 0.0
                    else
                        let corr_h = clip_cap (band_dot high v)
                        let corr_l = clip_cap (band_dot low v)
                        let g = sigmoid dp_base 4.0 10.0
                        let correction = g * corr_h + (1.0 - g) * corr_l
                        Math.Round(dp_base + correction, 2), Math.Round(correction, 3)
                | false, _, _, Some single ->
                    if not (band_ok single v) then
                        dp_base, 0.0
                    else
                        let correction = clip_cap (band_dot single v)
                        Math.Round(dp_base + correction, 2), Math.Round(correction, 3)
                | _ -> dp_base, 0.0
            with _ ->
                dp_base, 0.0
        | _ -> dp_base, 0.0

    let lin_base_blend (triang_dp: float, dp_sr: float, dp_choke: float, dp_msd: float, sk_key: string) : float =
        match model.Value with
        | Some m when not m.LinBase.IsEmpty ->
            let coef =
                match Map.tryFind sk_key m.LinBase with
                | Some c when c.Length >= 4 -> Some c
                | _ ->
                    match Map.tryFind "general" m.LinBase with
                    | Some c when c.Length >= 4 -> Some c
                    | _ -> None

            match coef with
            | Some c ->
                let base_lin = c.[0] + c.[1] * dp_sr + c.[2] * dp_choke + c.[3] * dp_msd
                let w = sigmoid triang_dp m.LinBlendK m.LinBlendX0
                w * base_lin + (1.0 - w) * triang_dp
            | None -> triang_dp
        | _ -> triang_dp
