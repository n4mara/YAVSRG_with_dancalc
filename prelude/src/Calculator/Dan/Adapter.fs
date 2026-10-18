namespace Prelude.Calculator.Dan

open System
open Prelude
open Prelude.Charts

type NoteHit = { Column: int; TimeMs: float }

module Adapter =

    let DEFAULT_OD = 8.0

    let compute_x (od: float) : float =
        let x = 0.3 * ((64.5 - ceil (od * 3.0)) / 500.0) ** 0.5
        min x (0.6 * (x - 0.09) + 0.09)

    let scale_time_ms (time_ms: float, rate_f: float) : float =
        let r = Math.Round(rate_f, 4)

        if abs (r - 1.5) < 1e-6 then
            floor (time_ms * 2.0 / 3.0)
        elif abs (r - 0.75) < 1e-6 then
            floor (time_ms * 4.0 / 3.0)
        elif abs (r - 1.0) < 1e-6 then
            time_ms
        else
            Math.Round(time_ms / rate_f)

    let scale_time_s_msd (time_ms: float, rate_f: float) : float =
        Math.Round((time_ms / max rate_f 1e-9) / 1000.0, 4)

    let to_note_seq (rate: Rate, note_data: NoteData, od: float) : int * float * NoteHit list =
        let keys = note_data.Keys
        let rate_f = float (rate / 1.0f<rate>)
        let od_v = if od > 0.0 then od else DEFAULT_OD

        let hits =
            note_data.Notes
            |> Array.collect (fun { Time = time; Data = row } ->
                let time_ms = scale_time_ms (float (time / 1.0f<ms>), rate_f)

                [|
                    for k = 0 to keys - 1 do
                        if row.[k] = NoteType.NORMAL || row.[k] = NoteType.HOLDHEAD then
                            yield { Column = k; TimeMs = time_ms }
                |])
            |> Array.toList
            |> List.sortBy (fun n -> n.TimeMs, n.Column)

        keys, compute_x od_v, hits
