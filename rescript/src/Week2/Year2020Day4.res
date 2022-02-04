// --- Day 4: Passport Processing ---
// part1

/*
1. passport 타입을 생각해봅시다. *문제와 동일하게* record로 선언하세요.
*/

type passport = {
  byr: string, // range 표현이 어려움
  iyr: string,
  eyr: string,
  hgt: string,
  hcl: string,
  ecl: string,
  pid: string,
  cid: option<string>,
}

let input = Node.Fs.readFileAsUtf8Sync("input/Week2/Year2020Day4.input.txt")

/*
2. string 타입의 입력을 passport 타입으로 파싱하는 parsePassport 함수를 작성해보세요.
   (우선 parsePassport 타입의 타입 시그니처를 생각해보세요)
*/

// string => option<passport>
let parsePassport = input => {
  let splitCase = data => Js.String.split("\n\n", data)
  let splitField = data => Js.String.splitByRe(%re("/\s/"), data)

  let splitKeyValue = data => Js.String.split(":", data)

  let arrayToTuple = data =>
    data
    ->Belt.Array.map(x => {
      switch (x->Belt.Array.get(0), x->Belt.Array.get(1)) {
      | (Some(k), Some(v)) => Some((k, v))
      | _ => None
      }
    })
    ->Belt.Array.keepMap(x => x)

  let arrayToMap = data => data->Belt.Map.String.fromArray

  let mapToPassport = data => {
    let byr = data->Belt.Map.String.get("byr")
    let iyr = data->Belt.Map.String.get("iyr")
    let eyr = data->Belt.Map.String.get("eyr")
    let hgt = data->Belt.Map.String.get("hgt")
    let hcl = data->Belt.Map.String.get("hcl")
    let ecl = data->Belt.Map.String.get("ecl")
    let pid = data->Belt.Map.String.get("pid")
    let cid = data->Belt.Map.String.get("cid")

    switch (byr, iyr, eyr, hgt, hcl, ecl, pid, cid) {
    | (Some(by), Some(iy), Some(ey), Some(hg), Some(hc), Some(ec), Some(pi), _) =>
      Some({
        byr: by,
        iyr: iy,
        eyr: ey,
        hgt: hg,
        hcl: hc,
        ecl: ec,
        pid: pi,
        cid: cid,
      })
    | _ => None
    }
  }

  input
  ->splitCase
  ->Belt.Array.map(splitField)
  ->Belt.Array.map(xs => xs->Belt.Array.keepMap(x => x))
  ->Belt.Array.map(x => x->Belt.Array.map(splitKeyValue))
  ->Belt.Array.map(arrayToTuple)
  ->Belt.Array.map(arrayToMap)
  ->Belt.Array.map(mapToPassport)
}

let filterNone = data => data->Belt.Array.keepMap(x => x)

/*
3. 올바른 Passport를 세는 countPassport 함수를 만들어서 문제를 해결해봅시다.
*/
let countPassport: array<'a> => int = data => data->Belt.Array.size

input->parsePassport->filterNone->countPassport->Js.log

// part2
/*
4. part1과 동일하게, *문제를 그대로* 코드로 옮겨보세요.
*/

type height = Cm(int) | In(int)
type eyeColor = Amb | Blu | Brn | Gry | Grn | Hzl | Oth

type passport2 = {
  byr: int, // range 표현이 어려움
  iyr: int,
  eyr: int,
  hgt: height,
  hcl: string,
  ecl: eyeColor,
  pid: string,
  cid: option<string>,
}

let parsePassport2 = input => {
  let splitter: string => array<array<array<string>>> = data => {
    let splitCase = data => Js.String.split("\n\n", data)
    let splitField = data => Js.String.splitByRe(%re("/\s/"), data)
    let splitKeyValue = data => Js.String.split(":", data)

    data
    ->splitCase
    ->Belt.Array.map(splitField)
    ->Belt.Array.map(xs => xs->Belt.Array.keepMap(x => x))
    ->Belt.Array.map(x => x->Belt.Array.map(splitKeyValue))
  }

  let arrayToTuple: array<'a> => option<('a, 'a)> = data =>
    switch (data->Belt.Array.get(0), data->Belt.Array.get(1)) {
    | (Some(k), Some(v)) => Some((k, v))
    | _ => None
    }

  let sequence: array<option<('a, 'a)>> => option<array<('a, 'a)>> = input => {
    switch input->Belt.Array.some(Belt.Option.isNone) {
    | true => None
    | false => Some(input->Belt.Array.keepMap(x => x))
    }
  }

  let tupleArrayToMap: array<(string, string)> => Belt.Map.String.t<string> = data =>
    data->Belt.Map.String.fromArray

  let parseRange: (int, int, int) => option<int> = (num, min, max) => {
    switch (num >= min, num <= max) {
    | (true, true) => Some(num)
    | _ => None
    }
  }

  let parseHeight: string => option<height> = str => {
    let stringToHeight: string => option<height> = str => {
      let unit = Js.String.sliceToEnd(~from=-2, str)
      let value = Js.String.slice(~from=0, ~to_=-2, str)->Belt.Int.fromString
      switch (unit, value) {
      | ("cm", Some(num)) => Some(Cm(num))
      | ("in", Some(num)) => Some(In(num))
      | _ => None
      }
    }

    let parseValueByUnit: height => option<height> = hgt => {
      switch hgt {
      | Cm(value) => value->parseRange(150, 193)->Belt.Option.map(x => Cm(x))
      | In(value) => value->parseRange(59, 76)->Belt.Option.map(x => In(x))
      }
    }
    str->stringToHeight->Belt.Option.flatMap(parseValueByUnit)
  }

  let parseHairColor: string => option<string> = str => {
    let rule = %re("/^#[a-z0-9+]{6}$/")
    switch Js.Re.test_(rule, str) {
    | true => Some(str)
    | false => None
    }
  }

  let parseEyeColor: string => option<eyeColor> = str => {
    switch str {
    | "amb" => Some(Amb)
    | "blu" => Some(Blu)
    | "brn" => Some(Brn)
    | "gry" => Some(Gry)
    | "grn" => Some(Grn)
    | "hzl" => Some(Hzl)
    | "oth" => Some(Oth)
    | _ => None
    }
  }

  let parsePassportId: string => option<string> = str => {
    let rule = %re("/^0*[0-9+]{9}$/")
    switch Js.Re.test_(rule, str) {
    | true => Some(str)
    | false => None
    }
  }

  let mapToPassport2: Belt.Map.String.t<string> => option<passport2> = data => {
    let byr =
      data
      ->Belt.Map.String.get("byr")
      ->Belt.Option.flatMap(Belt.Int.fromString)
      ->Belt.Option.flatMap(x => x->parseRange(1920, 2002))
    let iyr =
      data
      ->Belt.Map.String.get("iyr")
      ->Belt.Option.flatMap(Belt.Int.fromString)
      ->Belt.Option.flatMap(x => x->parseRange(2010, 2020))
    let eyr =
      data
      ->Belt.Map.String.get("eyr")
      ->Belt.Option.flatMap(Belt.Int.fromString)
      ->Belt.Option.flatMap(x => x->parseRange(2020, 2030))
    let hgt = data->Belt.Map.String.get("hgt")->Belt.Option.flatMap(parseHeight)
    let hcl = data->Belt.Map.String.get("hcl")->Belt.Option.flatMap(parseHairColor)
    let ecl = data->Belt.Map.String.get("ecl")->Belt.Option.flatMap(parseEyeColor)
    let pid = data->Belt.Map.String.get("pid")->Belt.Option.flatMap(parsePassportId)
    let cid = data->Belt.Map.String.get("cid")

    switch (byr, iyr, eyr, hgt, hcl, ecl, pid, cid) {
    | (Some(by), Some(iy), Some(ey), Some(hg), Some(hc), Some(ec), Some(pi), _) =>
      Some({
        byr: by,
        iyr: iy,
        eyr: ey,
        hgt: hg,
        hcl: hc,
        ecl: ec,
        pid: pi,
        cid: cid,
      })
    | _ => None
    }
  }

  input
  ->splitter
  ->Belt.Array.map(x => x->Belt.Array.map(arrayToTuple))
  ->Belt.Array.map(sequence)
  ->Belt.Array.map(x => x->Belt.Option.map(tupleArrayToMap))
  ->Belt.Array.map(x => x->Belt.Option.flatMap(mapToPassport2))
}

input->parsePassport2->filterNone->countPassport->Js.log

/*
참고 링크
- https://rescript-lang.org/docs/manual/latest/record
- https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
*/
