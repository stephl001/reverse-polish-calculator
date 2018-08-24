#r @"D:\Projects\PolishCalculator\packages\FParsec\lib\portable-net45+win8+wp8+wpa81\FParsecCS.dll"
#r @"D:\Projects\PolishCalculator\packages\FParsec\lib\portable-net45+win8+wp8+wpa81\FParsec.dll"

module Domain =
    type BinaryOperator = Add | Substract | Multiply | Divide
    type Operator = 
    | Binary of BinaryOperator
    
    type CalculatorElement =
    | Operator of Operator
    | Number of float
    type CalculatorExpression = CalculatorElement list

    type CalculatorError =
    | ParsingError of string
    | UnexpectedEndOfExpression
    | UnexpectedOperator of Operator

    type CalculatorResult = Result<float,CalculatorError>
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
    let pnum = pfloat |>> Number
    let pelem = pnum <|> parseOp //Order is important here!
    let pexpr = sepBy pelem (pchar ' ') .>> eof

    let mapResult = function
    | Success (r,_,_) -> FSharp.Core.Ok r
    | Failure (msg,_,_) -> FSharp.Core.Error (ParsingError msg)

    let parseString = run pexpr >> mapResult

module Operator =
    open Domain

    let evaluate (n1:float) n2 = function
    | Binary Add -> n1 + n2 
    | Binary Substract -> n1 - n2
    | Binary Multiply -> n1 * n2
    | Binary Divide -> n1 / n2

module Calculator =
    open Domain
    open CalculatorExpression

    let (>>=) m f = Result.bind f m

    let private evaluateExpression expr =
        let rec evaluate' = function
        | ([],[]) -> Ok 0.0
        | ([],[n]) -> Ok n
        | ((Number n)::xs,ys) -> evaluate' (xs,n::ys)
        | ((Operator op)::xs,n2::n1::ys) -> evaluate' (xs,(Operator.evaluate n1 n2 op)::ys)
        | ([],_) -> Error UnexpectedEndOfExpression
        | ((Operator op)::_,_) -> Error (UnexpectedOperator op)

        evaluate' (expr,[])

    let evaluateString expr = parseString expr >>= evaluateExpression