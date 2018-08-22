#r @"D:\Projects\PolishCalculator\packages\FParsec\lib\portable-net45+win8+wp8+wpa81\FParsecCS.dll"
#r @"D:\Projects\PolishCalculator\packages\FParsec\lib\portable-net45+win8+wp8+wpa81\FParsec.dll"

module Domain =
    type Operator = Add | Substract | Multiply | Divide
    
    type CalculatorElement =
    | Operator of Operator
    | Number of float
    type CalculatorExpression = CalculatorElement list

    type CalculatorError =
    | ParsingError of string
    | SequenceError of CalculatorElement

    type CalculatorResult = Result<float,CalculatorError>
    type Calculate = CalculatorExpression -> CalculatorResult

module CalculatorExpression =
    open FParsec
    open Domain

    let parseSingleOp opChar opElement =
        pchar opChar >>% Operator opElement
    let parseOp = 
        pchar '+' >>% Operator Add
        <|> (pchar '-' >>% Operator Substract)
        <|> (pchar '*' >>% Operator Multiply)
        <|> (pchar '/' >>% Operator Divide)
        <?> "operator"
    let pnum = pfloat |>> Number <?> "number"
    let pelem = parseOp <|> pnum
    let pexpr = sepBy pelem (pchar ' ') .>> eof

    let mapResult = function
    | Success (r,_,_) -> FSharp.Core.Ok r
    | Failure (msg,_,_) -> FSharp.Core.Error (ParsingError msg)

    let parseString = run pexpr >> mapResult    

module Calculator =
    open Domain
    open CalculatorExpression

    let private evaluate (expr:CalculatorExpression) : CalculatorResult =
        Ok 56.4

    let evaluateString str = 
        str |> parseString |> Result.bind evaluate