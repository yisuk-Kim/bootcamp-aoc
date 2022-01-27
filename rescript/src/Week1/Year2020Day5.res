open Belt

let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day5.input.txt")

// BFFBBBFLLR
// BFFBBBF   LLR
// 0110001   001
// 10->      10->
// (* 8) +  

// BFFBBBFLLR
// 0110001001

let splitNewLine = (pattern) => Js.String.split("\n", pattern)
let stringToArray = (str) => Js.String.split("",str)

let infoToBinary = (info) => switch info {
    | "B" => 1
    | "R" => 1
    | _ => 0
}


let getSeatId = (num) => num -> Belt.Array.reduceWithIndex(0, (acc, v, i) => {
    let exp = Belt.Array.length(num) - (i+1)
    let increment = Js.Math.pow_int(~base=2, ~exp=exp)
    acc + v * increment
})


let getMaxId = (ids) => Js.Math.maxMany_int(ids)

let handleOption = (opt) => switch opt {
| None => 0
| Some(num) => num
}

let getMaxId2 = (ids) => ids->Belt.SortArray.Int.stableSort
                            ->Belt.Array.reverse
                            ->Belt.Array.get(0)
                            ->handleOption

let seatIds = input -> splitNewLine
            -> Belt.Array.map((x) => x->stringToArray) 
            -> Belt.Array.map((xs)=> xs 
                -> Belt.Array.map((x) => x->infoToBinary)
            )
            -> Belt.Array.map((x) => x->getSeatId)
            
seatIds -> getMaxId2
    -> Js.log

// part2
// [3,7,4,2,....]
// [2,3,4,5,7....]
// [(2,3),(3,4),(4,5),(5,7)....]
// => find, keep => 차가 1이 아닌 원소를

let getMinId = (ids) => Js.Math.minMany_int(ids)

let maxId = seatIds -> getMaxId2
let minId = seatIds -> getMinId

let getEmptyId = (ids) => {
    let seatIdRange = Belt.Array.range(minId, maxId)
    seatIdRange -> Belt.Array.keep((x) => !Js.Array.includes(x, ids))
}

let setSlidingWindow = (ids) => ids -> Belt.Array.mapWithIndex((i, x) => {
    switch ids[i+1] {
    | None => (x,x+1)
    | Some(num) => (x, num)
    }
})

let getEmptySeatIds = (idPairs) => idPairs -> Belt.Array.map((xs) => {
    let (x,y) = xs
    Belt.Array.range(x+1, y-1)
})

let getDiff = (idPair) => {
    let (x,y) = idPair
    y-x
}

let concatSeatId = (idPairs) => idPairs -> Belt.Array.concatMany

seatIds -> Belt.SortArray.Int.stableSort
    -> setSlidingWindow
    -> getEmptySeatIds
    -> concatSeatId
    -> Js.log
