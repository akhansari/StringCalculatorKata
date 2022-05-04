[<ExtProperties>]
module ``StringCalculator should``

open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open StringCalculator

let joinWith sep integers = integers |> Seq.map string |> String.concat sep
let joinWithComma = joinWith ","
let joinWithNewLine = joinWith "\n"

let addShouldBeSumOf numbers input =
    let actual = add input
    let expected = Array.sum numbers
    test <@ actual = expected @>

[<Fact>]
let ``return 0 if empty string`` () =
    let expected = 0
    add null =! expected
    add ""   =! expected
    add "  " =! expected

[<Property>]
let ``parse a single number`` (NonNegativeInt number) =
    string number
    |> addShouldBeSumOf [| number |]

[<Property>]
let ``sum two numbers`` (NonNegativeInt number1) (NonNegativeInt number2) =
    $"{number1},{number2}"
    |> addShouldBeSumOf [| number1; number2 |]

[<Property>]
let ``sum multiple numbers`` (NonNegativeIntArray numbers) =
    joinWithComma numbers
    |> addShouldBeSumOf numbers

[<Property>]
let ``sum multiple numbers with newline separator`` (NonNegativeIntArray numbers) =
    joinWithNewLine numbers
    |> addShouldBeSumOf numbers

[<Property>]
let ``sum multiple numbers with a mix of new line and comma separators`` (NonNegativeIntArray numbers) =
    if Array.length numbers > 1 then
        [ joinWithComma numbers[..numbers.Length/2-1]
          joinWithNewLine numbers[numbers.Length/2..] ]
        |> String.concat ","
    else
        joinWith "" numbers
    |> addShouldBeSumOf numbers

[<Property>]
let ``sum multiple numbers with a defined separator`` (UnicodeChar separator) (NonNegativeIntArray numbers) =
    $"//{separator}\n{joinWith (string separator) numbers}"
    |> addShouldBeSumOf numbers

[<Property>]
let ``fail if negative number`` (NegativeInt number) =
    raises<NegativesNotAllowedException> <@ add (string number) @>

[<Property>]
let ``fail with negative numbers`` numbers =
    Array.exists isNegative numbers ==> lazy
    let input = joinWithComma numbers
    let expected = Array.filter isNegative numbers
    raisesWith<NegativesNotAllowedException>
        <@ add input @>
        (fun e -> <@ e.Data0 = expected @>)

[<Property>]
let ``sum multiple numbers ignoring those greater than 1000`` (Int0to2000Array numbers) =
    joinWithComma numbers
    |> addShouldBeSumOf (numbers |> Array.filter (fun i -> i <= 1000))

[<Property>]
let ``sum multiple numbers with a defined long separator`` (UnicodeString separator) (NonNegativeIntArray numbers) =
    separator <> "" ==> lazy
    $"//[{separator}]\n{joinWith separator numbers}"
    |> addShouldBeSumOf numbers

[<Property>]
let ``sum multiple numbers with two defined long separators``
    (UnicodeString separator1) (UnicodeString separator2) (NonNegativeIntArray numbers)
    =
    (separator1 <> "" && separator2 <> "") ==> lazy
    if Array.length numbers > 1 then
        [ joinWith separator1 numbers[..numbers.Length/2-1]
          joinWith separator2 numbers[numbers.Length/2..] ]
        |> String.concat separator1
    else
        joinWith separator1 numbers
    |> fun joinedNumbers -> $"//[{separator1}][{separator2}]\n{joinedNumbers}"
    |> addShouldBeSumOf numbers
