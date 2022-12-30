[<AutoOpen>]
module Utils

open System
open System.Collections.Concurrent


module Seq =

    let safeSkip count xs =
        if xs |> Seq.length < count then
            xs
        else
            xs |> Seq.skip count


module Guid =

    let tryParse (str: string) =
        match Guid.TryParse str with
        | false, _ -> None
        | true, guid -> Some guid


module DateTimeOffset =

    let tryParse (str: string) =
        match DateTimeOffset.TryParse str with
        | false, _ -> None
        | true, dto -> Some dto



[<AutoOpen>]
module Extensions =

    type ConcurrentDictionary<'a, 'b> with

        member this.TryFind key =
            match this.TryGetValue key with
            | true, x -> Some x
            | false, _ -> None
