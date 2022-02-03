open Belt

let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Year2020Day2.input.txt")

type password = {
  min: int,
  max: int,
  letter: string,
  pw: string,
}

// string => option<password>
let parsePassword = input => {
  let splitCase = input => Js.String.split("\n", input)
  let splitInfo = input => Js.String.splitByRe(%re("/\s|-|:\s/"), input)
  let stringToInt = str => str->Belt.Int.fromString->Belt.Option.getWithDefault(0)

  let liftOptionUp = input =>
    switch input->Belt.Array.some(Belt.Option.isNone) {
    | true => None
    | false => Some(input->Belt.Array.keepMap(x => x))
    }

  let stringArrayToPassword = data => {
    let min = data[0]
    let max = data[1]
    let letter = data[2]
    let pw = data[3]

    switch (min, max, letter, pw) {
    | (Some(num1), Some(num2), Some(str1), Some(str2)) =>
      Some({
        min: num1->stringToInt,
        max: num2->stringToInt,
        letter: str1,
        pw: str2,
      })
    | _ => None
    }
  }

  input
  ->splitCase
  ->Belt.Array.map(splitInfo)
  ->Belt.Array.map(liftOptionUp) // option 1
  ->Belt.Array.map(x => x->Belt.Option.flatMap(stringArrayToPassword))
  // ->Belt.Array.map(xs => xs->Belt.Array.keepMap(x => x)) // option 2
  // ->Belt.Array.map(stringArrayToPassword)
}

// password => bool
let checkValidity = data => {
  let isInRange = (num: int, min: int, max: int): bool =>
    switch (num >= min, num <= max) {
    | (true, true) => true
    | _ => false
    }

  let countOccurrence = data =>
    data.letter
    ->Js.Re.fromStringWithFlags(~flags="g")
    ->Js.String.match_(data.pw)
    ->Belt.Option.mapWithDefault(0, Belt.Array.length)

  data->countOccurrence->isInRange(data.min, data.max)
}

let filterNone = data => data->Belt.Array.keepMap(x => x)

// Option.map(checkValidity) => option<password> => option<boolean>
// array<boolean> => int
let count = boolArray => {
  let boolToInt = x =>
    switch x {
    | true => 1
    | false => 0
    }
  let sum = array => array->Belt.Array.reduce(0, (acc, x) => acc + x)
  boolArray->Belt.Array.map(boolToInt)->sum
}

input
->parsePassword // parse
->Belt.Array.map(x => x->Belt.Option.map(checkValidity)) // process
->filterNone // aggregate
->count
->Js.log // print

// part2
// password => bool
let checkValidity2 = data => {
  let checkPosition = (pos, letter, pw) => Js.String.charAt(pos - 1, pw) == letter

  let checkPolicy = data =>
    switch (
      checkPosition(data.min, data.letter, data.pw),
      checkPosition(data.max, data.letter, data.pw),
    ) {
    | (false, false) | (true, true) => false
    | _ => true
    }
  data->checkPolicy
}

input
->parsePassword // parse
->Belt.Array.map(x => x->Belt.Option.map(checkValidity2)) // process
->filterNone // aggregate
->count
->Js.log // print
