namespace Interlude.Features.Score

open System.Threading
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Windowing
open Prelude
open Prelude.Charts
open Prelude.Mods
open Prelude.Gameplay.Scoring
open Prelude.Data.UserDatabase
open Prelude.Calculator
open Prelude.Calculator.Dan
open Interlude.Options
open Interlude.UI

#nowarn "3370"

type Sidebar(stats: ScoreScreenStats ref, score_info: ScoreInfo) =
    inherit Container(NodeType.None)

    static let JUDGEMENT_TEXT_MAX_HEIGHT = 70.0f

    let show_more_info = Setting.simple false
    let show_dan = options.DanCalcEnabled.Value

    let mutable dan_label = "--"
    let mutable dan_accent = Color.White
    let mutable dan_generation = 0

    let mod_string = ModState.format(score_info.Rate, score_info.Mods)

    let category, main_clusters =
        let c = score_info.ChartMeta.Patterns

        c.Category,
        c.ImportantClusters |> Seq.truncate 3 |> Seq.map(fun c -> c.Format(score_info.Rate)) |> String.concat ", "

    let keymode_label =
        if score_info.WithMods.Keys <> score_info.Chart.Keys then
            sprintf "%iK->%iK" score_info.Chart.Keys score_info.WithMods.Keys
        else
            sprintf "%iK" score_info.WithMods.Keys

    do
        if show_dan then
            let generation = Interlocked.Increment(&dan_generation)
            let rate = score_info.Rate
            let mods_for_dan = Map.remove "mirror" score_info.WithMods.ModsSelected
            let note_data = (ModState.apply mods_for_dan score_info.Chart).ToNoteData()
            let chart_hash = score_info.ChartMeta.Hash

            let mod_fingerprint =
                mods_for_dan
                |> ModState.in_priority_order
                |> Seq.map (fun (id, _, state) -> sprintf "%s:%i" id state)
                |> String.concat ","

            let bpm = score_info.ChartMeta.BPM

            let od =
                score_info.ChartMeta.Origins
                |> Seq.tryPick (function
                    | ChartOrigin.Osu o -> Some(float o.SourceOD)
                    | _ -> None)
                |> Option.defaultValue Adapter.DEFAULT_OD

            async {
                let result =
                    DanCalc.analyze_cached(chart_hash, mod_fingerprint, rate, note_data, bpm, od)

                GameThread.defer (fun () ->
                    if generation = dan_generation then
                        match result with
                        | Some dan ->
                            dan_label <- Format.format dan
                            dan_accent <- dan.Accent
                        | None ->
                            dan_label <- "--"
                            dan_accent <- Color.White
                )
            }
            |> Async.Start

    override this.Init(parent: Widget) : unit =
        let metrics = Position.ShrinkT(530.0f).SliceT(70.0f).ShrinkX(25.0f)

        this.Add(
            Text(sprintf "%s  %s" Icons.ZAP mod_string)
                .Align(Alignment.CENTER)
                .Position(Position.SliceT(90.0f).ShrinkX(25.0f)),
            Text(fun () -> sprintf "%s  %s  •  %s" Icons.BAR_CHART keymode_label score_info.Ruleset.Name)
                .Color(Colors.text_subheading)
                .Align(Alignment.CENTER)
                .Position(Position.ShrinkT(90.0f).SliceT(70.0f).ShrinkX(25.0f))
        )

        if show_dan then
            this.Add(
                Text(sprintf "%s %.2f" Icons.STAR score_info.Rating.Overall)
                    .Color(Colors.white, Difficulty.color score_info.Rating.Overall)
                    .Align(Alignment.LEFT)
                    .Position(metrics.SlicePercentL(0.25f)),
                Text(fun () -> dan_label)
                    .Color(fun () -> (dan_accent, Colors.shadow_2))
                    .Align(Alignment.CENTER)
                    .Position(metrics.SlicePercentL(0.25f, 0.25f)),
                Text(fun () -> sprintf "%ix" score_info.Scoring.BestCombo)
                    .Align(Alignment.CENTER)
                    .Position(metrics.SlicePercentL(0.5f, 0.25f)),
                Text(sprintf "%.2f" score_info.Performance)
                    .Color(Colors.white, Difficulty.color score_info.Performance)
                    .Align(Alignment.RIGHT)
                    .Position(metrics.SlicePercentR(0.25f))
            )
        else
            this.Add(
                Text(sprintf "%s %.2f" Icons.STAR score_info.Rating.Overall)
                    .Color(Colors.white, Difficulty.color score_info.Rating.Overall)
                    .Align(Alignment.LEFT)
                    .Position(metrics),
                Text(fun () -> sprintf "%ix" score_info.Scoring.BestCombo)
                    .Align(Alignment.CENTER)
                    .Position(metrics),
                Text(sprintf "%.2f" score_info.Performance)
                    .Color(Colors.white, Difficulty.color score_info.Performance)
                    .Align(Alignment.RIGHT)
                    .Position(metrics)
            )

        this.Add(
            Button(
                (fun () ->
                    sprintf
                        "MA: %s  •  PA: %s  •  M: %.2fms  •  SD: %.2fms"
                        (!stats).MA
                        (!stats).PA
                        (!stats).TapMean
                        (!stats).TapStandardDeviation
                ),
                (fun () -> show_more_info.Set(true))
            )
                .TextColor(fun () -> if (!stats).ColumnFilterApplied then Colors.text_green else Colors.text)
                .Position(Position.ShrinkT(600.0f).SliceT(40.0f).ShrinkX(25.0f))
                .Conditional(show_more_info.Get >> not),
            Text(category)
                .Align(Alignment.LEFT)
                .Position(Position.ShrinkB(85.0f).SliceB(60.0f).ShrinkX(25.0f))
                .Conditional(show_more_info.Get >> not),
            Text(main_clusters)
                .Color(Colors.text_subheading)
                .Align(Alignment.LEFT)
                .Position(Position.ShrinkB(50.0f).SliceB(40.0f).ShrinkX(25.0f))
                .Conditional(show_more_info.Get >> not)

            ,
            Button(
                (fun () -> sprintf "MA: %s  •  PA: %s" (!stats).MA (!stats).PA),
                (fun () -> show_more_info.Set(false))
            )
                .TextColor(fun () -> if (!stats).ColumnFilterApplied then Colors.text_green else Colors.text)
                .Position(Position.ShrinkT(600.0f).SliceT(40.0f).ShrinkX(25.0f))
                .Conditional(show_more_info.Get),
            Text(fun () ->
                sprintf
                    "%s ~ M: %.2fms  •  SD: %.2fms  •  G: %i"
                    (%"score.stats.taps")
                    (!stats).TapMean
                    (!stats).TapStandardDeviation
                    (!stats).GhostTaps
            )
                .Color(fun () -> if (!stats).ColumnFilterApplied then Colors.text_green else Colors.text)
                .Align(Alignment.CENTER)
                .Position(Position.ShrinkT(640.0f).SliceT(40.0f).ShrinkX(25.0f))
                .Conditional(show_more_info.Get),
            Text(fun () ->
                sprintf
                    "%.1fms %s  •  +%.1fms %s  •  %.1f%% %s"
                    (fst (!stats).TapRange)
                    (%"score.stats.earliest")
                    (snd (!stats).TapRange)
                    (%"score.stats.latest")
                    (100.0 * (!stats).TapEarlyPercent)
                    (%"score.stats.early_percent")
            )
                .Color(fun () -> if (!stats).ColumnFilterApplied then Colors.text_green else Colors.text_subheading)
                .Align(Alignment.CENTER)
                .Position(Position.ShrinkT(675.0f).SliceT(40.0f).ShrinkX(25.0f))
                .Conditional(show_more_info.Get),
            Text(fun () ->
                sprintf
                    "%s ~ M: %.2fms  •  SD: %.2fms"
                    (%"score.stats.releases")
                    (!stats).ReleaseMean
                    (!stats).ReleaseStandardDeviation
            )
                .Color(fun () -> if (!stats).ColumnFilterApplied then Colors.text_green else Colors.text)
                .Align(Alignment.CENTER)
                .Position(Position.ShrinkT(715.0f).SliceT(40.0f).ShrinkX(25.0f))
                .Conditional(show_more_info.Get),
            Text(fun () ->
                sprintf
                    "%.1fms %s  •  +%.1fms %s  •  %.1f%% %s"
                    (fst (!stats).ReleaseRange)
                    (%"score.stats.earliest")
                    (snd (!stats).ReleaseRange)
                    (%"score.stats.latest")
                    (100.0 * (!stats).ReleaseEarlyPercent)
                    (%"score.stats.early_percent")
            )
                .Color(fun () -> if (!stats).ColumnFilterApplied then Colors.text_green else Colors.text_subheading)
                .Align(Alignment.CENTER)
                .Position(Position.ShrinkT(750.0f).SliceT(40.0f).ShrinkX(25.0f))
                .Conditional(show_more_info.Get)
        )

        base.Init(parent)

    override this.Draw() : unit =
        Render.rect (this.Bounds.Translate(Style.PADDING * 2.0f, Style.PADDING * 2.0f)) Colors.black
        Background.draw(this.Bounds, Colors.white, 2.0f)
        Render.rect_c this.Bounds (Quad.gradient_top_to_bottom (!*Palette.DARKER.O3) Colors.shadow_2.O3)
        base.Draw()

        // accuracy info
        let counters = this.Bounds.ShrinkX(25.0f).SliceT(170.0f, 350.0f)

        let judgement_counts = (!stats).Judgements
        let judgements = score_info.Ruleset.Judgements |> Array.indexed
        let judgement_box_height = counters.Height / float32 judgements.Length
        let mutable y = 0.0f

        for i, j in judgements do
            let percentage_of_total =
                if (!stats).JudgementCount = 0 then
                    0.0f
                else
                    float32 judgement_counts.[i] / float32 (!stats).JudgementCount

            let judgement_box = counters.SliceT(y, judgement_box_height)
            Render.rect judgement_box j.Color.O1
            Render.rect (judgement_box.SlicePercentL(percentage_of_total)) j.Color.O2

            let padding = Style.PADDING + 6.0f - float32 judgement_counts.Length |> max 0.0f

            Text.fill_b(
                Style.font,
                sprintf "%s: %i" j.Name judgement_counts.[i],
                judgement_box
                    .SliceY(min judgement_box_height JUDGEMENT_TEXT_MAX_HEIGHT)
                    .Shrink(Style.PADDING * 2.0f, padding)
                    .ShrinkR(200.0f),
                (if (!stats).ColumnFilterApplied then Colors.text_green else Colors.text),
                Alignment.LEFT
            )

            Text.fill_b(
                Style.font,
                sprintf "%.2f%%" (percentage_of_total * 100.0f),
                judgement_box
                    .SliceY(min judgement_box_height JUDGEMENT_TEXT_MAX_HEIGHT)
                    .SliceR(200.0f)
                    .Shrink(Style.PADDING * 2.0f, if show_more_info.Value then padding * 2.0f else padding),
                (if (!stats).ColumnFilterApplied then Colors.text_green else Colors.text),
                Alignment.RIGHT
            )

            if show_more_info.Value && i > 0 then
                Text.fill_b(
                    Style.font,
                    JudgementRatio.FormatWithSpaces(judgement_counts, i - 1),
                    judgement_box
                        .SliceY(min judgement_box_height JUDGEMENT_TEXT_MAX_HEIGHT)
                        .TranslateY(-judgement_box_height * 0.5f)
                        .Shrink(Style.PADDING * 2.0f, Style.PADDING + padding * 2.0f),
                    (if (!stats).ColumnFilterApplied then
                         (Colors.green_accent.O3, Colors.green_shadow)
                     else
                         (Colors.white.O3, Colors.shadow_2)),
                    Alignment.RIGHT
                )

            y <- y + judgement_box_height
