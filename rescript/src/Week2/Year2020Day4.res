// --- Day 4: Passport Processing ---
// part1

/*
1. passport 타입을 생각해봅시다. *문제와 동일하게* record로 선언하세요.
*/
type passport = {
  byr: string,
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

let stringToArrayMap = input => {
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
  input
  ->splitCase
  ->Belt.Array.map(splitField)
  ->Belt.Array.map(xs => xs->Belt.Array.keepMap(x => x))
  ->Belt.Array.map(x => x->Belt.Array.map(splitKeyValue))
  ->Belt.Array.map(arrayToTuple)
  ->Belt.Array.map(arrayToMap)
}

// string => option<passport>
let parsePassport = input => {
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

  input->stringToArrayMap->Belt.Array.map(mapToPassport)
}

let filterNone = data => data->Belt.Array.keepMap(x => x)

/*
3. 올바른 Passport를 세는 countPassport 함수를 만들어서 문제를 해결해봅시다.
*/
let countPassport: array<passport> => int = data => data->Belt.Array.size

input->parsePassport->filterNone->countPassport->Js.log

// part2
/*
4. part1과 동일하게, *문제를 그대로* 코드로 옮겨보세요.
*/

let parsePassport2 = input => {
  let returnIfValidRange = (data, min, max) => {
    data
    ->Belt.Option.flatMap(Belt.Int.fromString)
    ->Belt.Option.flatMap(x => {
      switch (x >= min, x <= max) {
      | (true, true) => data
      | _ => None
      }
    })
  }

  let eclSet = Belt.Set.String.fromArray(["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

  let checkValidityByr = byr => byr->returnIfValidRange(1920, 2002)
  let checkValidityIyr = iyr => iyr->returnIfValidRange(2010, 2020)
  let checkValidityEyr = eyr => eyr->returnIfValidRange(2020, 2030)
  let checkValidityHgt = hgt => {
    let unit = hgt->Belt.Option.map(x => Js.String.sliceToEnd(~from=-2, x))
    let num = hgt->Belt.Option.map(x => Js.String.slice(~from=0, ~to_=-2, x))
    switch unit {
    | Some("cm") => num->returnIfValidRange(150, 193)
    | Some("in") => num->returnIfValidRange(59, 76)
    | _ => None
    }
  }
  let checkValidityHcl = hcl => {
    let rule = %re("/^#[a-z0-9+]{6}$/")
    switch hcl->Belt.Option.map(x => Js.Re.test_(rule, x)) {
    | Some(true) => hcl
    | Some(false) | None => None
    }
  }
  let checkValidityEcl = ecl => {
    switch ecl->Belt.Option.map(x => Belt.Set.String.has(eclSet, x)) {
    | Some(true) => ecl
    | Some(false) | None => None
    }
  }
  let checkValidityPid = pid => {
    let rule = %re("/^0*[0-9+]{9}$/")
    switch pid->Belt.Option.map(x => Js.Re.test_(rule, x)) {
    | Some(true) => pid
    | Some(false) | None => None
    }
  }

  let mapToPassport = data => {
    let byr = data->Belt.Map.String.get("byr")->checkValidityByr
    let iyr = data->Belt.Map.String.get("iyr")->checkValidityIyr
    let eyr = data->Belt.Map.String.get("eyr")->checkValidityEyr
    let hgt = data->Belt.Map.String.get("hgt")->checkValidityHgt
    let hcl = data->Belt.Map.String.get("hcl")->checkValidityHcl
    let ecl = data->Belt.Map.String.get("ecl")->checkValidityEcl
    let pid = data->Belt.Map.String.get("pid")->checkValidityPid
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

  input->stringToArrayMap->Belt.Array.map(mapToPassport)
}
//passport -> option<passport>
let checkPassportValidity = data => {
  let {byr, iyr, eyr, hgt, hcl, ecl, pid} = data

  let isInValidRange = (data, min, max) => {
    let num = data->Belt.Int.fromString

    switch num->Belt.Option.map(x => (x >= min, x <= max)) {
    | Some((true, true)) => true
    | Some(_) | None => false
    }
  }

  let eclSet = Belt.Set.String.fromArray(["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

  let checkByrValidity = byr => byr->isInValidRange(1920, 2002)
  let checkIyrValidity = iyr => iyr->isInValidRange(2010, 2020)
  let checkEyrValidity = eyr => eyr->isInValidRange(2020, 2030)
  let checkHgtValidity = hgt => {
    let unit = Js.String.sliceToEnd(~from=-2, hgt)
    let num = Js.String.slice(~from=0, ~to_=-2, hgt)
    switch unit {
    | "cm" => num->isInValidRange(150, 193)
    | "in" => num->isInValidRange(59, 76)
    | _ => false
    }
  }
  let checkHclValidity = hcl => {
    let rule = %re("/^#[a-z0-9+]{6}$/")
    Js.Re.test_(rule, hcl)
  }
  let checkEclValidity = ecl => {
    eclSet->Belt.Set.String.has(ecl)
  }
  let checkPidValidity = pid => {
    let rule = %re("/^0*[0-9+]{9}$/")
    Js.Re.test_(rule, pid)
  }

  switch (
    byr->checkByrValidity,
    iyr->checkIyrValidity,
    eyr->checkEyrValidity,
    hgt->checkHgtValidity,
    hcl->checkHclValidity,
    ecl->checkEclValidity,
    pid->checkPidValidity,
  ) {
  | (true, true, true, true, true, true, true) => Some(data)
  | _ => None
  }
}
input->parsePassport2->filterNone->countPassport->Js.log

input
->parsePassport // parse
->Belt.Array.map(x => x->Belt.Option.flatMap(checkPassportValidity)) // process
->filterNone //aggregate
->countPassport
->Js.log // print

/*
참고 링크
- https://rescript-lang.org/docs/manual/latest/record
- https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
*/
