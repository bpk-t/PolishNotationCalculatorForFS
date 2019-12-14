open System
open FParsec

type Ast =
    | Number of int
    | Add of lhs:Ast * rhs:Ast
    | Sub of lhs:Ast * rhs:Ast
    | Mul of lhs:Ast * rhs:Ast
    | Div of lhs:Ast * rhs:Ast

let parseBy p str = 
    match run (spaces >>. p .>> eof) str with 
    | Success(res, _, _) -> res
    | Failure(msg, _, _) -> failwithf "parse error: %s" msg

let ast, astRef = createParserForwardedToRef()
let numberParser = pint32 .>> spaces |>> Number
let binaryOpParser (op:char) = (pchar op >>. spaces) >>. ast .>>. ast
let addParser = binaryOpParser '+' |>> (fun (lhs, rhs) -> Add(lhs, rhs))
let subParser = binaryOpParser '-' |>> (fun (lhs, rhs) -> Sub(lhs, rhs))
let mulParser = binaryOpParser '*' |>> (fun (lhs, rhs) -> Mul(lhs, rhs))
let divParser = binaryOpParser '/' |>> (fun (lhs, rhs) -> Div(lhs, rhs))

astRef:= choice [numberParser; addParser; subParser; mulParser; divParser]

let rec calc (x:Ast):int = 
    match x with 
    | Number(y) -> y
    | Add(lhs, rhs) -> calc(lhs) + calc(rhs)
    | Sub(lhs, rhs) -> calc(lhs) - calc(rhs)
    | Mul(lhs, rhs) -> calc(lhs) * calc(rhs)
    | Div(lhs, rhs) -> calc(lhs) / calc(rhs)

[<EntryPoint>]
let main argv =
    let a = "+ 100 200" |> parseBy ast
    printfn "%A" a

    let a = "* 100 200" |> parseBy ast
    printfn "%A" a

    let a = "* 100 + 5 7" |> parseBy ast
    printfn "%A" a

    let a = "* 13 + 5 7" |> parseBy ast
    printfn "%A = %d" a (calc a)

    0 // return an integer exit code
