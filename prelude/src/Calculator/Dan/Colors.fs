namespace Prelude.Calculator.Dan

open Prelude

module DanColors =

    let private rgb (r: int, g: int, b: int) : Color = Color.FromArgb(255, r, g, b)

    let private NUMERIC: Color array =
        [|
            rgb (118, 199, 255)
            rgb (113, 180, 255)
            rgb (115, 166, 255)
            rgb (120, 155, 255)
            rgb (139, 152, 255)
            rgb (166, 146, 255)
            rgb (196, 143, 255)
            rgb (235, 160, 114)
            rgb (255, 175, 99)
            rgb (255, 199, 108)
        |]

    let private LETTER_PRIMARY: Map<string, Color> =
        Map.ofList
            [
                "a", rgb (70, 60, 180)
                "b", rgb (255, 245, 180)
                "y", rgb (255, 0, 0)
                "d", rgb (95, 60, 125)
                "e", rgb (215, 45, 95)
                "z", rgb (240, 130, 105)
                "h", rgb (145, 0, 0)
                "i", rgb (75, 10, 10)
                "κ", rgb (125, 125, 125)
            ]

    let private LETTER_ACCENT: Map<string, Color> =
        Map.ofList
            [
                "a", rgb (220, 65, 80)
                "b", rgb (255, 165, 0)
                "y", rgb (0, 60, 255)
                "d", rgb (215, 130, 50)
                "e", rgb (30, 80, 150)
                "z", rgb (130, 190, 225)
                "h", rgb (200, 80, 70)
                "i", rgb (145, 50, 50)
                "κ", rgb (125, 125, 125)
            ]

    let private ETA_ACCENT = rgb (200, 80, 70)
    let private LN_FALLBACK = rgb (247, 64, 128)

    let accent (tier: DanTier) : Color =
        match tier with
        | DanTier.Numeric n when n >= 1 && n <= 10 -> NUMERIC.[n - 1]
        | DanTier.Letter "κ" -> LETTER_PRIMARY.["κ"]
        | DanTier.Letter c ->
            match LETTER_ACCENT.TryFind c with
            | Some color -> color
            | None -> LN_FALLBACK
        | DanTier.Ln n when n >= 1 && n <= 10 -> NUMERIC.[n - 1]
        | DanTier.Ln _ -> LN_FALLBACK
        | _ -> LN_FALLBACK

    let accent_for_key (short: string, rice_key: string) : Color =
        if rice_key = "Eta" then
            ETA_ACCENT
        else
            accent (DanTier.Letter short)
