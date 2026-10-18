namespace Interlude.Features.LevelSelect

open System
open System.Threading
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Windowing
open Prelude
open Prelude.Charts
open Prelude.Calculator
open Prelude.Calculator.Dan
open Prelude.Mods
open Interlude.Options
open Interlude.UI
open Interlude.Features.Gameplay

type GameplayInfo() =
    inherit Container(NodeType.None)

    let never_played = %"levelselect.last_played.never"

    let mutable rating = 0.0f
    let mutable notecounts = ""
    let mutable last_played = K never_played
    let mutable mod_string = "--"
    let mutable mod_status = ModStatus.Ranked
    let mutable dan_label = "--"
    let mutable dan_debug = ""
    let mutable dan_accent = Color.White
    let mutable dan_debug_on = options.DanCalcDebug.Value
    let mutable dan_generation = 0

    let refresh_dan (info: LoadedChartInfo) =
        dan_debug_on <- options.DanCalcDebug.Value
        dan_label <- "--"
        dan_debug <- ""
        dan_accent <- Color.White

        if not options.DanCalcEnabled.Value then
            ()
        else
            let generation = Interlocked.Increment(&dan_generation)
            let rate = SelectedChart.rate.Value
            let note_data = info.Chart.ToNoteData()
            let chart_hash = info.ChartMeta.Hash
            let mod_fingerprint = "base"
            let bpm = info.ChartMeta.BPM
            let show_debug = dan_debug_on

            let od =
                info.ChartMeta.Origins
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

                            match show_debug, dan.Debug with
                            | true, Some d ->
                                let line = Format.format_debug d
                                dan_debug <- line
                                Logging.Info "DanCalc: %s | %s" dan_label line
                            | true, None -> dan_debug <- ""
                            | false, _ -> dan_debug <- ""
                        | None ->
                            dan_label <- "--"
                            dan_accent <- Color.White
                            dan_debug <- if show_debug then "domain reject / no result" else ""
                )
            }
            |> Async.Start

    let refresh (info: LoadedChartInfo) =
        rating <- info.Difficulty.Overall
        notecounts <- info.NotecountsString

        last_played <-
            let mutable ts = info.SaveData.LastPlayed

            let text (ts: int64) =
                if ts <= 0L then
                    never_played
                else
                    [ Timestamp.since info.SaveData.LastPlayed |> format_timespan ] %> "levelselect.last_played"

            let mutable t = text ts

            fun () ->
                if info.SaveData.LastPlayed <> ts then
                    ts <- info.SaveData.LastPlayed
                    t <- text ts

                t

        mod_string <- ModState.format(SelectedChart.rate.Value, SelectedChart.selected_mods.Value)
        mod_status <- info.WithMods.Status
        refresh_dan info

    static member HEIGHT = 230.0f

    override this.Init(parent: Widget) : unit =
        SelectedChart.on_chart_change_finished.Add(refresh)
        SelectedChart.on_chart_update_finished.Add(refresh)
        SelectedChart.if_loaded refresh

        this.Add(GameplayInfoBoxes().Position(Position.SliceT(100.0f).Shrink(10.0f)))

        base.Init(parent)

    override this.Update(elapsed_ms: float, moved: bool) : unit =
        base.Update(elapsed_ms, moved)

        if options.DanCalcEnabled.Value && options.DanCalcDebug.Value <> dan_debug_on then
            SelectedChart.if_loaded refresh_dan

    override this.Draw() : unit =

        let play_info = this.Bounds.SliceT(90.0f, 40.0f).ShrinkX(15.0f)
        let mod_string_ap = mod_string + (if SelectedChart.autoplay then ", AP" else "")
        Text.fill_b(Style.font, mod_string_ap, play_info, Colors.text, Alignment.LEFT)
        Text.fill_b(Style.font, last_played(), play_info, Colors.text, Alignment.RIGHT)

        if mod_status <> ModStatus.Ranked then
            let text, color =
                match mod_status with
                | ModStatus.Unstored -> Icons.X_CIRCLE + " " + %"mods.mod_status.unstored", Colors.text_red
                | ModStatus.Offline -> Icons.CLOUD_OFF + " " + %"mods.mod_status.offline", Colors.text_yellow_2
                | ModStatus.Unranked -> Icons.CLOUD_OFF + " " + %"mods.mod_status.unranked", Colors.text_yellow_2
                | ModStatus.Ranked -> failwith "impossible"

            let x = Text.measure(Style.font, mod_string_ap) * play_info.Height * 0.6f + 10.0f
            Text.fill_b(Style.font, text, play_info.ShrinkL(x).ShrinkY(2.0f), color, Alignment.LEFT)

        let chart_info = this.Bounds.SliceT(130.0f, 30.0f).ShrinkX(15.0f)

        Text.fill_b(
            Style.font,
            (match SelectedChart.CACHE_DATA with
             | Some chart_meta -> chart_meta.DifficultyName
             | None -> ""),
            chart_info.SlicePercentL(0.5f),
            Colors.text_subheading,
            Alignment.LEFT
        )

        Text.fill_b(Style.font, notecounts, chart_info, Colors.text_subheading, Alignment.RIGHT)

        let icon_infos = this.Bounds.SliceT(155.0f, 70.0f).ShrinkX(15.0f)
        let stars = sprintf "%s %.2f" Icons.STAR rating

        if options.DanCalcEnabled.Value then
            let col_w = icon_infos.Width * 0.25f

            Text.fill_b(
                Style.font,
                stars,
                icon_infos.SliceL(col_w),
                (Colors.white, Difficulty.color rating),
                Alignment.LEFT
            )

            Text.fill_b(
                Style.font,
                dan_label,
                icon_infos.ShrinkL(col_w).SliceL(col_w),
                (dan_accent, Colors.shadow_2),
                Alignment.CENTER
            )

            Text.fill_b(
                Style.font,
                SelectedChart.FMT_BPM,
                icon_infos.ShrinkL(col_w * 2.0f).SliceL(col_w),
                Colors.text,
                Alignment.CENTER
            )

            Text.fill_b(Style.font, SelectedChart.FMT_DURATION, icon_infos.SliceR(col_w), Colors.text, Alignment.RIGHT)

            if dan_debug <> "" then
                let debug_bounds = this.Bounds.SliceB(22.0f).ShrinkX(10.0f)
                Text.fill_b(Style.font, dan_debug, debug_bounds, Colors.text_greyout, Alignment.LEFT)
        else
            Text.fill_b(
                Style.font,
                stars,
                icon_infos,
                (Colors.white, Difficulty.color rating),
                Alignment.LEFT
            )

            Text.fill_b(Style.font, SelectedChart.FMT_BPM, icon_infos, Colors.text, Alignment.CENTER)
            Text.fill_b(Style.font, SelectedChart.FMT_DURATION, icon_infos, Colors.text, Alignment.RIGHT)

        base.Draw()
