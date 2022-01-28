open Belt

let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Year2020Day2.input.txt")

let handleOption = (input, none) => switch input {
    | None => none
    | Some(value) => value
}

let splitCase = input => Js.String.split("\n",input);
let splitInfo = input => Js.String.splitByRe(%re("/\s|-|:\s/"), input) 
                        -> Belt.Array.map((x) => x->handleOption(""));


type password = {
    min: int,
    max: int,
    letter: string,
    pw: string
}

let stringToInt = str => switch str -> Belt.Int.fromString {
    | None => 0
    | Some(num) => num
}

let arrayToPassword = info => {
    min: switch info[0] {
        | None => 0
        | Some(str) => str -> stringToInt
    },
    max: switch info[1] {
        | None => 0
        | Some(str) => str -> stringToInt
    },
    letter: switch info[2] {
        | None => ""
        | Some(str) => str
    },
    pw: switch info[3] {
        | None => ""
        | Some(str) => str
    }
}

let isInRange = (num:int, min:int, max:int) => switch (num >= min, num <= max) {
    | (true, true) => 1
    | _ => 0
}

let countOccurrence = pw => 
{
    pw.letter -> Js.Re.fromStringWithFlags(~flags="g")
            -> Js.String.match_(pw.pw)
            -> handleOption([])
            -> Belt.Array.length
}

let checkValidity = pw => {
    pw -> countOccurrence
        -> isInRange(pw.min, pw.max)
}

let sum = array => array ->Belt.Array.reduce(0,(acc, x) => acc+x)

// let checkValidity = data => 

input -> splitCase
    -> Belt.Array.map((x) => x->splitInfo)
    -> Belt.Array.map((x) => x->arrayToPassword)
    // -> Belt.Array.map((x) => x->countOccurrence)
    -> Belt.Array.map((x) => x->checkValidity)
    -> sum
    -> Js.log