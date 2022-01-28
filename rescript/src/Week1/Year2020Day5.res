open Belt

let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day5.input.txt")

// BFFBBBFLLR
// BFFBBBF   LLR
// 0110001   001
// 10->      10->
// (* 8) +

// BFFBBBFLLR
// 0110001001

let splitNewLine = pattern => Js.String.split("\n", pattern)
let stringToArray = str => Js.String.split("", str)

let seatSignToBinary = info =>
  switch info {
  | "B" | "R" => 1
  | _ => 0
  }

let getSeatId = num =>
  num->Belt.Array.reduceWithIndex(0, (acc, v, i) => {
    let exp = Belt.Array.length(num) - (i + 1)
    let increment = Js.Math.pow_int(~base=2, ~exp)
    acc + v * increment
  })

let getMaxId = ids => Js.Math.maxMany_int(ids)

let getMaxId2 = ids =>
  ids
  ->Belt.SortArray.Int.stableSort
  ->Belt.Array.reverse
  ->Belt.Array.get(0)
  ->Belt.Option.getWithDefault(0)

let seatIds =
  input
  ->splitNewLine
  ->Belt.Array.map(stringToArray)
  ->Belt.Array.map(xs => xs->Belt.Array.map(seatSignToBinary))
  ->Belt.Array.map(getSeatId)

seatIds->getMaxId2->Js.log

// part2
// [3,7,4,2,....]
// [2,3,4,5,7....]
// [(2,3),(3,4),(4,5),(5,7)....]
// => find, keep => 차가 1이 아닌 원소를

let getMinId = ids => Js.Math.minMany_int(ids)

let maxId = seatIds->getMaxId2
let minId = seatIds->getMinId

let setSlidingWindow = ids =>
  ids->Belt.Array.mapWithIndex((i, x) => {
    switch ids[i + 1] {
    | None => (x, x + 1)
    | Some(num) => (x, num)
    }
  })

let getEmptySeatIds = idPairs =>
  idPairs->Belt.Array.map(xs => {
    let (x, y) = xs
    Belt.Array.range(x + 1, y - 1)
  })

let getDiff = idPair => {
  let (x, y) = idPair
  y - x
}

let concatSeatId = idPairs => idPairs->Belt.Array.concatMany

let sortListAscending = data => data->Belt.List.sort((a, b) => a - b)

let rec setSlidingWindowList = (ids, pairList) => {
  switch ids {
  | list{} => pairList
  | list{_} => pairList
  | list{seatId1, ...otherSeatIds} => {
      let seatId2 = otherSeatIds->Belt.List.headExn
      let new = Belt.List.concat(pairList, list{(seatId1, seatId2)})
      setSlidingWindowList(otherSeatIds, new)
    }
  }
}

let keepEmptyPair = idPairs => idPairs->Belt.List.keep(p => p->getDiff != 1)

let getEmptyId = idPair => {
  let (min, max) = idPair
  Belt.Array.range(min + 1, max - 1)
}

let getEmptySeatIdsList = idPairs => idPairs->Belt.List.map(getEmptyId)

// seatIds -> Belt.SortArray.Int.stableSort
//     -> setSlidingWindow
//     -> getEmptySeatIds
//     -> concatSeatId
//     -> Js.log

seatIds
->Belt.List.fromArray
->sortListAscending
->setSlidingWindowList(list{})
->keepEmptyPair
->getEmptySeatIdsList
->Belt.List.toArray
->concatSeatId
->Js.log
