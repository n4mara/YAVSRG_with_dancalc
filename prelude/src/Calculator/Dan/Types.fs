namespace Prelude.Calculator.Dan

open Prelude

[<RequireQualifiedAccess>]
type DanSublevel =
    | Low
    | MidLow
    | Mid
    | MidHigh
    | High

[<RequireQualifiedAccess>]
type DanTier =
    | Numeric of int
    | Letter of string
    | Ln of int

type DanDebug =
    {
        Family: string
        SkKey: string
        Od: float
        Rate: float
        RawSr: float
        ModSr: float
        ChokeNps: float
        MsdOverall: float
        HasMsd: bool
        MsdError: string
        HasBio: bool
        LnRatio: float
        DpSr: float
        DpChoke: float
        DpMsd: float
        WSr: float
        WChoke: float
        WMsd: float
        TriangDp: float
        BaseDp: float
        RidgeCorr: float
        FinalDp: float
    }

type DanResult =
    {
        Tier: DanTier
        Sublevel: DanSublevel
        IsLn: bool
        Dp: float
        Sr: float
        Accent: Color
        Debug: DanDebug option
    }

module DanOrder =

    let RICE_SHORT: string array =
        [|
            "1"
            "2"
            "3"
            "4"
            "5"
            "6"
            "7"
            "8"
            "9"
            "10"
            "α"
            "β"
            "γ"
            "δ"
            "ε"
            "ζ"
            "η"
            "θ"
            "ι"
            "κ"
        |]

    let RICE_KEYS: string array =
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
            "Alpha"
            "Beta"
            "Gamma"
            "Delta"
            "Epsilon"
            "Zeta"
            "Eta"
            "Theta"
            "Iota"
            "Kappa"
        |]

    let LN_STAGE_COUNT = 16
