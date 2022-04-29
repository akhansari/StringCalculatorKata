[<AutoOpen>]
module Helpers

open FsCheck

let private alwaysTrue _ = true

type NonNegativeIntArray = NonNegativeIntArray of int32 array
type Int0to2000 = Int0to2000 of int32
type Int0to2000Array = Int0to2000Array of int32 array

type ArbitraryModifiers =

    static member NonNegativeIntArray() =
        Arb.Default.Array<int32>()
        |> Arb.mapFilter (Array.map abs) alwaysTrue
        |> Arb.convert NonNegativeIntArray (fun (NonNegativeIntArray arr) -> arr)

    static member Int0to2000() =
        Gen.choose(0, 2000)
        |> Arb.fromGen
        |> Arb.convert Int0to2000 (fun (Int0to2000 i) -> i)

    static member Int0to2000Array() =
        Arb.generate<Int0to2000 array>
        |> Gen.map (Array.map (fun (Int0to2000 i) -> i))
        |> Arb.fromGen
        |> Arb.convert Int0to2000Array (fun (Int0to2000Array arr) -> arr)
        
type ExtPropertiesAttribute () =
    inherit Xunit.PropertiesAttribute (
        Arbitrary = [| typeof<ArbitraryModifiers> |])
