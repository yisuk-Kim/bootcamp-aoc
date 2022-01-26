let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day5.input.txt")

let splitInput = (pattern) => Js.String.split("\n", pattern)

let inputArray = input -> splitInput
                    -> Belt.Array.map((x) => Js.String.split("",x))


let splitRowColumn = (input) =>  Belt.Array.partition(input, (x) => (x == "F" || x == "B"))

let infoToNum = (info, upCode) => info -> Belt.Array.reduceWithIndex(0, (acc, v, i) => {
 let exp = Belt.Array.length(info) - (i+1)
 let increment = Js.Math.pow_int(~base=2, ~exp=exp)
 if v == upCode {
     acc + increment
 }
 else {
     acc
 }
})

let getSeatNum = (splitInfo) =>  {
    let (rowInfo, columnInfo) = splitInfo
    
    let rowNum = rowInfo -> infoToNum("B")
    let columnNum = columnInfo -> infoToNum("R")
    
    [rowNum, columnNum]
}
            

let getSeatId = (seatNum) => seatNum[0] * 8 + seatNum[1]

// let getMaxId = (ids) => Belt.Array.reduce(ids, 0, (acc, v) => {
//     if v > acc {
//         v
//     } else {
//         acc
//     }
// })

let getMaxId = (ids) => Js.Math.maxMany_int(ids)

let seatIds = inputArray 
            -> Belt.Array.map((x) => x -> splitRowColumn)
            -> Belt.Array.map((x) => x -> getSeatNum)
            -> Belt.Array.map((x) => x -> getSeatId)

seatIds -> getMaxId
        -> Js.log

// part2

let getMinId = (ids) => Js.Math.minMany_int(ids)

let maxId = seatIds -> getMaxId
let minId = seatIds -> getMinId

let getEmptyId = (ids) => {
    let seatIdRange = Belt.Array.range(minId, maxId)
    seatIdRange -> Belt.Array.keep((x) => !Js.Array.includes(x, ids))
}

seatIds -> getEmptyId
        -> Js.log
