namespace Prelude.Calculator.Dan

open System
open System.Collections.Concurrent
open Prelude
open Prelude.Charts

module DanCalc =

    type private CacheKey =
        {
            ChartHash: string
            Rate: float32
            ModFingerprint: string
            Bpm: int
            Od: float
        }

    let private cache = ConcurrentDictionary<CacheKey, DanResult option>()

    let clear_cache () : unit = cache.Clear()

    let private make_result
        (is_ln: bool, dp: float, tier: DanTier, sub: DanSublevel, sr: float, debug: DanDebug option)
        : DanResult =
        let accent =
            match tier with
            | DanTier.Letter short when not is_ln ->
                DanColors.accent_for_key (short, Format.rice_key_from_dp dp)
            | _ -> DanColors.accent tier

        {
            Tier = tier
            Sublevel = sub
            IsLn = is_ln
            Dp = dp
            Sr = sr
            Accent = accent
            Debug = debug
        }

    let private resolve_family
        (note_data: NoteData, sunny: Sunny.Result, features: Features.ChartFeatures)
        : string =
        let drain_s = if features.DurationS > 0.0 then features.DurationS else sunny.DurationMs / 1000.0
        let bar_family, bar_conf = Classifier.classify_family (sunny, features, features.Bpm, drain_s)

        if sunny.Sr < 7.0 then
            let rp_family, rp_conf = RhythmProfile.classify (1.0f<rate>, note_data, features.Bpm)

            if rp_conf >= 0.10 then
                rp_family
            else
                bar_family
        else
            let _ = bar_conf
            bar_family

    let analyze (rate: Rate, note_data: NoteData, bpm: float, od: float) : DanResult option =
        if note_data.Notes.Length = 0 then
            None
        else
            let od_v = if od > 0.0 then od else Adapter.DEFAULT_OD
            let is_ln = Detect.is_ln_chart note_data

            if not (Detect.isor_domain_ok (note_data, is_ln)) then
                None
            else
                let bpm_base = if bpm > 0.0 then bpm else 0.0

                match Sunny.calculate (rate, note_data, od_v) with
                | None -> None
                | Some sunny ->
                    let msd = Msd.calculate (rate, note_data)
                    let features = Features.extract (1.0f<rate>, note_data, bpm_base)
                    let bio = StrainBio.from_note_data (1.0f<rate>, note_data)
                    let family = resolve_family (note_data, sunny, features)

                    let result =
                        if is_ln then
                            let dp, tier, sub = LnCourse.analyze (rate, note_data, sunny, msd)

                            let debug =
                                {
                                    Family = "ln"
                                    SkKey = "ln"
                                    Od = od_v
                                    Rate = float (rate / 1.0f<rate>)
                                    RawSr = Math.Round(sunny.Sr, 4)
                                    ModSr = Math.Round(sunny.Sr, 4)
                                    ChokeNps = 0.0
                                    MsdOverall =
                                        match msd with
                                        | Some s -> Math.Round(s.Overall, 4)
                                        | None -> 0.0
                                    HasMsd = msd.IsSome
                                    MsdError = if msd.IsSome then "" else Msd.last_failure ()
                                    HasBio = false
                                    LnRatio = Math.Round(float (Detect.hold_ratio note_data), 4)
                                    DpSr = dp
                                    DpChoke = 0.0
                                    DpMsd = 0.0
                                    WSr = 0.0
                                    WChoke = 0.0
                                    WMsd = 0.0
                                    TriangDp = dp
                                    BaseDp = dp
                                    RidgeCorr = 0.0
                                    FinalDp = dp
                                }

                            make_result (true, dp, tier, sub, sunny.Sr, Some debug)
                        else
                            let dp, tier, sub, debug =
                                Isor.analyze (rate, note_data, sunny, features, family, msd, bio, od_v, true)

                            make_result (false, dp, tier, sub, sunny.Sr, Some debug)

                    Some result

    let analyze_cached
        (
            chart_hash: string,
            mod_fingerprint: string,
            rate: Rate,
            note_data: NoteData,
            bpm: int,
            od: float
        ) : DanResult option =
        let od_v = if od > 0.0 then od else Adapter.DEFAULT_OD

        let key =
            {
                ChartHash = chart_hash
                Rate = float32 (rate / 1.0f<rate>)
                ModFingerprint = mod_fingerprint
                Bpm = bpm
                Od = od_v
            }

        match cache.TryGetValue key with
        | true, value -> value
        | false, _ ->
            let value = analyze (rate, note_data, float bpm, od_v)
            cache.[key] <- value
            value

    let format_label (rate: Rate, note_data: NoteData, bpm: float, od: float) : string option =
        analyze (rate, note_data, bpm, od) |> Option.map Format.format
