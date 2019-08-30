module FSharp.Data.GraphQL.VariableConverter

open FSharp.Data.GraphQL.Ast
open FParsec

let stringLiteral : Parser<string, unit> =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')

    let toHex d =
        if d >= '0' && d <= '9' then int32 d - int32 '0'
        elif d >= 'a' && d <= 'f' then int32 d - int32 'a' + 10
        elif d >= 'A' && d <= 'F' then int32 d - int32 'A' + 10
        else failwith "Invalid Hexdigit"
    let hexdigit = anyOf "0123456789abcdefABCDEF" |>> toHex

    let toHexNumeral digits = Seq.fold (fun number digit -> (16 * number) + digit ) 0 digits
    let unicodeChar =
        pstring "\\u" >>.
        choiceL [
          parray 4 hexdigit |>> toHexNumeral                                 // Unicode Escape: \u007a
          (between (pchar '{') (pchar '}') (many hexdigit |>> toHexNumeral)) // Unicode Code Point Escape: \u{7A}
        ] "Invalid Unicode Escape"
        |>> char

    let unescape = function
        | 'b' -> '\b'
        | 'f' -> '\f'
        | 'n' -> '\n'
        | 'r' -> '\r'
        | 't' -> '\t'
        | c   -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\/bfnrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"") (manyChars (normalChar <|> unicodeChar <|> escapedChar))


let value, valueRef = createParserForwardedToRef<Value, unit> ()

// Strip spaces at the end of a parser
let token (p : Parser<'a, unit>) : Parser<'a, unit> = p .>> spaces

// Tokenized char
let tchar (c : char) : Parser<char, unit> = token (pchar c)

let intValue : Parser<Value, unit> = token (pint64 |>> IntValue)
let floatValue : Parser<Value, unit> = token (pfloat |>> FloatValue)
let boolValue : Parser<Value, unit> = token ((stringReturn "true" (BooleanValue true) <|> stringReturn "false" (BooleanValue false)))
let nullValue : Parser<Value, unit> = token (stringReturn "null" NullValue)
let stringValue : Parser<Value, unit> = token (stringLiteral |>> StringValue)
let keyValuePairs : Parser<Map<string, Value>, unit> =
    between (tchar '{') (tchar '}') (sepBy (token stringLiteral .>> tchar ':' .>>. value) (tchar ',')) |>> Map.ofList
let listValue : Parser<Value, unit> =
    between (tchar '[') (tchar ']') (sepBy value (tchar ',')) |>> ListValue

do valueRef := choiceL [
        intValue
        floatValue
        boolValue
        stringValue
        nullValue
        listValue
        keyValuePairs |>> ObjectValue
    ] "Invalid JSON"

let parseVariables (str : string) : Map<string, Value> =
    match run keyValuePairs str with
    | Success (vars, _, _) -> vars
    | Failure (msg, err, _) -> failwithf "Failed to parse variables: %s %A" msg err
