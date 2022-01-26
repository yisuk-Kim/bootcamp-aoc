let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day5.input.txt")

// BFFBBBFLLR
// BFFBBBF   LLR
// 0110001   001
// 10->      10->
// (* 8) +  

// BFFBBBFLLR
// 0110001001

let splitInput = (pattern) => Js.String.split("\n", pattern)

let inputArray = input -> splitInput
                    -> Belt.Array.map((x) => Js.String.split("",x))

let infoToNum = (info) => info -> Belt.Array.map((xs) => 
    switch xs {
    | "B" => 1
    | "R" => 1
    | _ => 0
    }
)

let getSeatId = (num) => num -> Belt.Array.reduceWithIndex(0, (acc, v, i) => {
    let exp = Belt.Array.length(num) - (i+1)
    let increment = Js.Math.pow_int(~base=2, ~exp=exp)
    acc + v * increment
})

let getMaxId = (ids) => Js.Math.maxMany_int(ids)


let seatIds = inputArray 
            ->Belt.Array.map((x)=> x->infoToNum)
            ->Belt.Array.map((x) => x->getSeatId)
            
seatIds -> getMaxId 
    -> Js.log

// part2
// [3,7,4,2,....]
// [2,3,4,5,7....]
// [(2,3),(3,4),(4,5),(5,7)....]
// => find, keep => 차가 1이 아닌 원소를

let getMinId = (ids) => Js.Math.minMany_int(ids)

let maxId = seatIds -> getMaxId
let minId = seatIds -> getMinId

let getEmptyId = (ids) => {
    let seatIdRange = Belt.Array.range(minId, maxId)
    seatIdRange -> Belt.Array.keep((x) => !Js.Array.includes(x, ids))
}

seatIds -> getEmptyId
        -> Js.log
