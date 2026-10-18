namespace Prelude.Calculator.Dan

open System
open System.IO
open System.Reflection
open System.Text.Json

module Config =

    let private assembly = Assembly.GetExecutingAssembly()

    let private read_embedded (suffix: string) : string option =
        assembly.GetManifestResourceNames()
        |> Array.tryFind (fun n -> n.EndsWith(suffix, StringComparison.OrdinalIgnoreCase))
        |> Option.map (fun name ->
            use stream = assembly.GetManifestResourceStream(name)
            use reader = new StreamReader(stream)
            reader.ReadToEnd())

    let private read_file (file_name: string) : string option =
        let candidates =
            [|
                Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "DanCalc", "config", file_name)
                Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "config", file_name)
            |]

        candidates
        |> Array.tryFind File.Exists
        |> Option.map File.ReadAllText

    let load_json (file_name: string) : JsonDocument option =
        match read_embedded file_name with
        | Some text -> Some(JsonDocument.Parse(text))
        | None ->
            match read_file file_name with
            | Some text -> Some(JsonDocument.Parse(text))
            | None -> None

    let try_get_float (el: JsonElement, name: string) : float option =
        match el.TryGetProperty(name) with
        | true, p when p.ValueKind = JsonValueKind.Number -> Some(p.GetDouble())
        | _ -> None

    let dan_float_map (obj: JsonElement) : Map<string, float> =
        seq {
            for prop in obj.EnumerateObject() do
                if prop.Value.ValueKind = JsonValueKind.Number then
                    yield prop.Name, prop.Value.GetDouble()
        }
        |> Map.ofSeq
