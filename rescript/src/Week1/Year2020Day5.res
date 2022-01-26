let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day5.input.txt")
let splitInput = (pattern) => Js.String.split("\n", pattern)

let inputArray = input -> splitInput
                        -> Belt.Array.map((x) => Js.String.split("",x))

inputArray -> Js.log

let getRowInfo = (info) => Belt.Array.slice(info, ~offset=0, ~len=7)

let getRowNum = (rowInfo) => Belt.Array.reduceWithIndex(rowInfo, 0, (acc, v, i) => {
 let exp = Belt.Array.length(rowInfo) - i
 let digit = Js.Math.pow_int(~base=2, ~exp=exp)
 acc + v
})

