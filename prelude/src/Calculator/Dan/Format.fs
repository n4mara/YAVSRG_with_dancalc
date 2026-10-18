namespace Prelude.Calculator.Dan

open System

module Format =

    let private sublevel_suffix (sublevel: DanSublevel) : string =
        match sublevel with
        | DanSublevel.Low -> "--"
        | DanSublevel.MidLow -> "-"
        | DanSublevel.Mid -> ""
        | DanSublevel.MidHigh -> "+"
        | DanSublevel.High -> "++"

    let private tier_body (tier: DanTier) : string =
        match tier with
        | DanTier.Numeric n -> string n
        | DanTier.Letter c -> c
        | DanTier.Ln n -> sprintf "LN %i" n

    let format (result: DanResult) : string =
        tier_body result.Tier + sublevel_suffix result.Sublevel

    let format_debug (d: DanDebug) : string =
        let msd =
            if d.HasMsd then
                sprintf "%.2f" d.MsdOverall
            elif String.IsNullOrWhiteSpace d.MsdError then
                "n/a"
            else
                sprintf "n/a (%s)" d.MsdError

        sprintf
            "SR %.2f→%.2f | choke %.1f | MSD %s | dp %.2f/%.2f/%.2f | w %.2f/%.2f/%.2f | tri %.2f base %.2f ridge %+.2f → %.2f | %s/%s od %.1f ln %.2f bio %b"
            d.RawSr
            d.ModSr
            d.ChokeNps
            msd
            d.DpSr
            d.DpChoke
            d.DpMsd
            d.WSr
            d.WChoke
            d.WMsd
            d.TriangDp
            d.BaseDp
            d.RidgeCorr
            d.FinalDp
            d.Family
            d.SkKey
            d.Od
            d.LnRatio
            d.HasBio

    let sublevel_from_frac (frac: float) : DanSublevel =
        let f = System.Math.Round(frac, 2)

        if f <= 0.20 then DanSublevel.Low
        elif f <= 0.40 then DanSublevel.MidLow
        elif f <= 0.60 then DanSublevel.Mid
        elif f <= 0.80 then DanSublevel.MidHigh
        else DanSublevel.High

    let sublevel_from_dp (dp: float) : DanSublevel =
        let frac = if dp >= 1.0 then dp - floor dp else 0.0
        sublevel_from_frac frac

    let rice_tier_from_dp (dp: float) : DanTier =
        let idx = max 0 (min (DanOrder.RICE_SHORT.Length - 1) (int (floor dp) - 1))
        let short = DanOrder.RICE_SHORT.[idx]

        if idx < 10 then
            DanTier.Numeric (idx + 1)
        else
            DanTier.Letter short

    let rice_key_from_dp (dp: float) : string =
        let idx = max 0 (min (DanOrder.RICE_KEYS.Length - 1) (int (floor dp) - 1))
        DanOrder.RICE_KEYS.[idx]

    let ln_tier_from_dp (dp: float) : DanTier =
        let stage = max 1 (min DanOrder.LN_STAGE_COUNT (int dp))
        DanTier.Ln stage
