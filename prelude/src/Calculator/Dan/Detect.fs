namespace Prelude.Calculator.Dan

open Prelude
open Prelude.Charts

module Detect =

    let [<Literal>] LN_HOLD_RATIO_THRESHOLD = 0.45f
    let [<Literal>] LN_RIDGE_RATIO = 0.18f
    let [<Literal>] MIN_NOTES = 20
    let [<Literal>] MIN_DRAIN_S = 5.0

    let hold_ratio (note_data: NoteData) : float32 =
        let notes, lnotes = Chart.notecount note_data

        if notes <= 0 then
            0.0f
        else
            float32 lnotes / float32 notes

    let is_ln_chart (note_data: NoteData) : bool =
        hold_ratio note_data > LN_HOLD_RATIO_THRESHOLD

    let note_hit_count (note_data: NoteData) : int =
        let mutable n = 0

        for { Data = row } in note_data.Notes do
            for k = 0 to note_data.Keys - 1 do
                if row.[k] = NoteType.NORMAL || row.[k] = NoteType.HOLDHEAD then
                    n <- n + 1

        n

    let drain_s (note_data: NoteData) : float =
        if note_data.Notes.Length = 0 then
            0.0
        else
            let t0 = float (note_data.Notes.[0].Time / 1.0f<ms>)
            let t1 = float (note_data.Notes.[note_data.Notes.Length - 1].Time / 1.0f<ms>)
            max 0.0 ((t1 - t0) / 1000.0)

    let isor_domain_ok (note_data: NoteData, is_ln: bool) : bool =
        if note_data.Keys <> 4 then
            false
        elif note_hit_count note_data < MIN_NOTES then
            false
        elif drain_s note_data < MIN_DRAIN_S then
            false
        else
            let _ = is_ln
            true
