open Belt

let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Year2020Day2.input.txt")

let splitCase = input => Js.String.split("\n", input)
let splitInfo = input =>
  Js.String.splitByRe(%re("/\s|-|:\s/"), input)->Belt.Array.map(x =>
    x->Belt.Option.getWithDefault("")
  )

type password = {
  min: int,
  max: int,
  letter: string,
  pw: string,
}

let stringToInt = str => str->Belt.Int.fromString->Belt.Option.getWithDefault(0)

let arrayToPassword = info => {
  min: info[0]->Belt.Option.mapWithDefault(0, stringToInt),
  max: info[1]->Belt.Option.mapWithDefault(0, stringToInt),
  letter: info[2]->Belt.Option.getWithDefault(""),
  pw: info[3]->Belt.Option.getWithDefault(""),
}

// let parsePassword = info => {

// }

let isInRange = (num: int, min: int, max: int) =>
  switch (num >= min, num <= max) {
  | (true, true) => 1
  | _ => 0
  }

let countOccurrence = data =>
  data.letter
  ->Js.Re.fromStringWithFlags(~flags="g")
  ->Js.String.match_(data.pw)
  ->Belt.Option.mapWithDefault(0, Belt.Array.length)

let checkValidity = data => data->countOccurrence->isInRange(data.min, data.max)

let sum = array => array->Belt.Array.reduce(0, (acc, x) => acc + x)

input
->splitCase
->Belt.Array.map(splitInfo)
->Belt.Array.map(arrayToPassword)
->Belt.Array.map(checkValidity)
->sum
->Js.log

// part2

let checkPosition = (pos, letter, password) => Js.String.charAt(pos - 1, password) == letter

let checkValidity2 = data =>
  switch (
    checkPosition(data.min, data.letter, data.pw),
    checkPosition(data.max, data.letter, data.pw),
  ) {
  | (false, false) | (true, true) => 0
  | _ => 1
  }

input
->splitCase
->Belt.Array.map(splitInfo)
->Belt.Array.map(arrayToPassword)
->Belt.Array.map(checkValidity2)
->sum
->Js.log
