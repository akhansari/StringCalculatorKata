module StringCalculator

open System
open System.Text.RegularExpressions

// step 7

exception NegativesNotAllowedException of int32 array
let defaultSeparators = [| ","; "\n" |]
let private separatorPattern = Regex(@"\[(.+?(?=\]))\]", RegexOptions.Compiled)

let isNegative = (>) 0

let parseNumbers (separators: string array) value =
    if String.IsNullOrEmpty value then
        Array.empty
    else
        value.Split(separators, StringSplitOptions.None)
        |> Array.map Int32.Parse

let parseSeparators (value: string) =
    if value[..2] = "//[" then
        let newline = value.IndexOf "\n"
        let separators =
            value[..newline]
            |> separatorPattern.Matches
            |> Seq.map (fun x -> x.Groups[1].Value)
            |> Seq.toArray
        (separators, value[newline+1..])
    elif value[..1] = "//" then
        ([| string value[2] |], value[4..])
    else
        (defaultSeparators, value)

let parse value =
    if String.IsNullOrWhiteSpace value then
        Array.empty
    else
        parseSeparators value
        ||> parseNumbers

let add input =
    let numbers = parse input
    if Array.exists isNegative numbers then
        numbers
        |> Array.filter isNegative
        |> NegativesNotAllowedException
        |> raise
    else
        numbers
        |> Array.fold (fun acc i ->
            if i > 1000
            then acc
            else acc + i) 0

// step 1
(*
let add numbers =
    if String.IsNullOrWhiteSpace numbers then 0 else
    0

let add numbers =
    if String.IsNullOrWhiteSpace numbers then 0 else
    Int32.Parse numbers

let add numbers =
    if String.IsNullOrWhiteSpace numbers then 0 else
    match numbers.IndexOf ',' with
    | -1 ->
        Int32.Parse numbers
    | index ->
        Int32.Parse(numbers.Substring(0, index))
        + Int32.Parse(numbers.Substring(index + 1))
*)

// step 2
(*
let add numbers =
    if String.IsNullOrWhiteSpace numbers then 0 else
    numbers.Split ','
    |> Array.map Int32.Parse
    |> Array.sum
*)

// step 3
(*
let add numbers =
    if String.IsNullOrWhiteSpace numbers then 0 else
    numbers.Split(',', '\n')
    |> Array.map Int32.Parse
    |> Array.sum
*)

// step 4
(*
let defaultSeparators = [| ','; '\n' |]

let add input =
    if String.IsNullOrWhiteSpace input then 0 else

    let separators, numbers =
        if input[..1] = "//"
        then [| input[2] |], input[4..]
        else defaultSeparators, input

    if numbers.Length = 0 then
        0
    else
        numbers.Split separators
        |> Array.map Int32.Parse
        |> Array.sum
*)

// step 5
(*
exception NegativesNotAllowedException of int32 array
let defaultSeparators = [| ','; '\n' |]

let isNegative = (>) 0

let parseNumbers separators value =
    if String.IsNullOrEmpty value then
        Array.empty
    else
        value.Split separators
        |> Array.map Int32.Parse

let parse value =
    if String.IsNullOrWhiteSpace value then
        Array.empty
    elif value[..1] = "//" then
        parseNumbers [| value[2] |] value[4..]
    else
        parseNumbers defaultSeparators value

let add input =
    let numbers = parse input
    if Array.exists isNegative numbers then
        numbers
        |> Array.filter isNegative
        |> NegativesNotAllowedException
        |> raise
    else
        numbers
        |> Array.sum
*)

// step 6
(*
exception NegativesNotAllowedException of int32 array
let defaultSeparators = [| ','; '\n' |]

let isNegative = (>) 0

let parseNumbers separators value =
    if String.IsNullOrEmpty value then
        Array.empty
    else
        value.Split separators
        |> Array.map Int32.Parse

let parse value =
    if String.IsNullOrWhiteSpace value then
        Array.empty
    elif value[..1] = "//" then
        parseNumbers [| value[2] |] value[4..]
    else
        parseNumbers defaultSeparators value

let add input =
    let numbers = parse input
    if Array.exists isNegative numbers then
        numbers
        |> Array.filter isNegative
        |> NegativesNotAllowedException
        |> raise
    else
        numbers
        |> Array.fold (fun acc i ->
            if i > 1000
            then acc
            else acc + i) 0
*)
