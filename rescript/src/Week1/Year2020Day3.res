let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day3.input.txt")

let splitInput = (pattern) => Js.String.split("\n", pattern)

let inputArray = input -> splitInput

let width = Js.String.length(inputArray[0])

let rec getFootprints = (slope, footprints) => {
    let length = footprints -> Array.length
    let lastPos = footprints[length - 1]
    if (lastPos[1] + slope[1]) < Array.length(inputArray)
    {
        let new = Belt.Array.concat(footprints,[[lastPos[0] + slope[0], lastPos[1] + slope[1]]])
        getFootprints(slope, new)
    }
    else
    {
        footprints
    }
}

let setModX = (pos) => Belt.Array.map(pos, ([x,y]) => [mod(x, width),y])

let getTrees = (pos) => Belt.Array.map(pos, ([x,y]) => Js.String.get(inputArray[y],x))

let treeToNumber = (trees) => Array.map((v) => {
    if v == "#"
    {1}
    else
        {0}
}, trees)

let sum = (numbers) => Belt.Array.reduce(numbers, 0, (acc, value) => acc + value)

[3,1] -> getFootprints([[0,0]])
-> setModX
-> getTrees
-> treeToNumber
-> sum
-> Js.log

// part 2

let slope = [[1,1], [3,1], [5,1], [7,1], [1,2]];

let multiply = (numbers) => Belt.Array.reduce(numbers, 1, (acc, value) => acc * value)


let result = slope -> Belt.Array.map((x) => {
    x -> getFootprints([[0,0]])
        -> setModX
        -> getTrees
        -> treeToNumber
        -> sum
    })
    -> multiply
    -> Js.log
