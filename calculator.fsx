#r @"D:\Projects\PolishCalculator\packages\FParsec\lib\portable-net45+win8+wp8+wpa81\FParsecCS.dll"
open System
#r @"D:\Projects\PolishCalculator\packages\FParsec\lib\portable-net45+win8+wp8+wpa81\FParsec.dll"

module Domain =
    type UnaryOperator = Factorial
    type BinaryOperator = Add | Substract | Multiply | Divide
    type Operator = 
    | Unary of UnaryOperator
    | Binary of BinaryOperator
    
    type Number = 
    | Integer of int
    | FloatingPoint of float
    type CalculatorElement =
    | Operator of Operator
    | Number of Number
    type CalculatorExpression = CalculatorElement list

    type CalculatorError =
    | ParsingError of string
    | UnexpectedEndOfExpression
    | UnexpectedOperator of Operator

    type CalculatorResult = Result<Number,CalculatorError>
    type Calculate = CalculatorExpression -> CalculatorResult

module CalculatorExpression =
    open FParsec
    open Domain

    let parseSingleOp opChar opElement =
        pchar opChar >>% Operator opElement
    let parseOp = 
        pchar '+' >>% Operator (Binary Add)
        <|> (pchar '-' >>% Operator (Binary Substract))
        <|> (pchar '*' >>% Operator (Binary Multiply))
        <|> (pchar '/' >>% Operator (Binary Divide))
        <|> (pchar '!' >>% Operator (Unary Factorial))

    let pfloatStrict =
        pfloat
        >>= (fun f -> if f % 1.0 > Double.Epsilon then preturn f else fail "Not strictely a float")

    let pf = pfloatStrict |>> FloatingPoint
    let pi = pint32 |>> Integer
    let pnum = attempt pf <|> pi |>> Number

    let pelem = pnum <|> parseOp //Order is important here!
    let pexpr = sepBy pelem (pchar ' ') .>> eof

    let mapResult = function
    | Success (r,_,_) -> FSharp.Core.Ok r
    | Failure (msg,_,_) -> FSharp.Core.Error (ParsingError msg)

    let parseString = run pexpr >> mapResult

module Operator =
    open Domain

    let toFloatingPoint = function
    | Integer n -> float n
    | FloatingPoint n -> n

    let evaluateUnary (n:Number) = function
    | Factorial ->
        match n with
        | FloatingPoint _ -> failwith "Factorial only supports positive integers"
        | Integer i -> [1..i] |> List.reduce (*)

    let evaluateIntegers n1 n2 = function
    | Add -> Integer (n1 + n2) | Substract -> Integer (n1 - n2) | Multiply -> Integer (n1 * n2)
    | Divide ->
        if n1 % n2 = 0 then Integer (n1 / n2) else FloatingPoint (float n1 / float n2)

    let evaluateFloatingPoints (n1:float) (n2:float) = function
    | Add -> FloatingPoint (n1 + n2) 
    | Substract -> FloatingPoint (n1 - n2) 
    | Multiply -> FloatingPoint (n1 * n2)
    | Divide -> FloatingPoint (n1 / n2)

    let evaluateBinary op n1 n2 = 
        match (n1,n2) with
        | (Integer x,Integer y) -> evaluateIntegers x y op
        | (x,y) -> evaluateFloatingPoints (toFloatingPoint x) (toFloatingPoint y) op    

module Calculator =
    open Domain
    open CalculatorExpression

    let (>>=) m f = Result.bind f m

    let private numberToFloat = function
    | FloatingPoint n -> n
    | Integer i -> float i

    let private evaluateExpression expr : CalculatorResult =
        let rec evaluate' = function
        | ([],[]) -> Ok (Integer 0)
        | ([],[n]) -> Ok n
        | ((Number n)::xs,ys) -> evaluate' (xs,n::ys)
        | ((Operator (Binary op))::xs,n2::n1::ys) -> evaluate' (xs,(Operator.evaluateBinary op n1 n2)::ys)
        | ([],_) -> Error UnexpectedEndOfExpression
        | ((Operator op)::_,_) -> Error (UnexpectedOperator op)

        evaluate' (expr,[])

    let evaluateString expr = parseString expr >>= evaluateExpression