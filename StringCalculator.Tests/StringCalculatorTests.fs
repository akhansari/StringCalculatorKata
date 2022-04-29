[<ExtProperties>]
module ``StringCalculator should``

open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open StringCalculator

let join sep integers = integers |> Seq.map string |> String.concat sep
let joinWithComma = join ","
let joinWithNewLine = join "\n"

[<Fact>]
let ``return 0 if empty string`` () =
    let expected = 0
    add null =! expected
    add ""   =! expected
    add "  " =! expected

[<Property>]
let ``parse a single number`` (NonNegativeInt number) =
    add (string number) =! number

[<Property>]
let ``sum two numbers`` (NonNegativeInt number1) (NonNegativeInt number2) =
    let input = $"{number1},{number2}"
    let expected = number1 + number2
    add input =! expected

[<Property>]
let ``sum multiple numbers`` (NonNegativeIntArray numbers) =
    let input = joinWithComma numbers
    let expected = Array.sum numbers
    add input =! expected

[<Property>]
let ``sum multiple numbers with newline separator`` (NonNegativeIntArray numbers) =
    let input = joinWithNewLine numbers
    let expected = Array.sum numbers
    add input =! expected

[<Property>]
let ``sum multiple numbers with a mix of new line and comma separators`` (NonNegativeIntArray numbers) =
    let input =
        if Array.length numbers > 1 then
            [ joinWithComma numbers[..numbers.Length/2-1]
              joinWithNewLine numbers[numbers.Length/2..] ]
            |> String.concat ","
        else
            join "" numbers

    let expected = Array.sum numbers
    add input =! expected

[<Property>]
let ``sum multiple numbers with a defined separator`` (UnicodeChar separator) (NonNegativeIntArray numbers) =
    let input = $"//{separator}\n{join (string separator) numbers}"
    let expected = Array.sum numbers
    add input =! expected

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
    let input = joinWithComma numbers
    let expected = numbers |> Array.filter (fun i -> i <= 1000) |> Array.sum
    stdout.WriteLine (System.String.Join(',', numbers))
    add input =! expected

[<Property>]
let ``sum multiple numbers with a defined long separator`` (UnicodeString separator) (NonNegativeIntArray numbers) =
    separator <> "" ==> lazy
    let input = $"//[{separator}]\n{join separator numbers}"
    let expected = Array.sum numbers
    add input =! expected

[<Property>]
let ``sum multiple numbers with two defined long separators``
    (UnicodeString separator1) (UnicodeString separator2) (NonNegativeIntArray numbers)
    =
    (separator1 <> "" && separator2 <> "") ==> lazy
    let input =
        if Array.length numbers > 1 then
            [ join separator1 numbers[..numbers.Length/2-1]
              join separator2 numbers[numbers.Length/2..] ]
            |> String.concat separator1
        else
            join separator1 numbers
    let input = $"//[{separator1}][{separator2}]\n{input}"
    let expected = Array.sum numbers
    add input =! expected
