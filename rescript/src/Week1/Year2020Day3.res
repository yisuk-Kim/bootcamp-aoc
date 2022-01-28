open Belt

let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day3.input.txt")

let splitInput = (pattern) => Js.String.split("\n", pattern)

let inputArray = input -> splitInput

// Map의 의미
// apply :: (a -> b) -> a -> b
// getLength :: string -> number
// getLength = (str) => str.length
// map(getLength, ["test", "123"])
// getLength(["test", "123"])
// apply(getLength, "test")
// map :: (a -> b) -> Array<a> -> Array<b>
// map :: (a -> b) -> Option<a> -> Option<b>
// then :: (a -> b) -> Promise<a> -> Promise<b>

// option<a> -> a -> a

let width = inputArray[0] -> Belt.Option.mapWithDefault(0, Js.String.length)

let rec getFootprints = (slope, footprints) => {
    let length = footprints -> Belt.Array.length

    let lastPos = footprints[length - 1]  -> Belt.Option.getWithDefault((0,0))
    
    let (x,y) = lastPos
    let (slopeX, slopeY) = slope
    
    if (y + slopeY) < Array.length(inputArray)
    {
        let new = Belt.Array.concat(footprints,[(x + slopeX, y + slopeY)])
        getFootprints(slope, new)
    }
    else
    {
        footprints
    }
}

let getFootprintsFromOrigin = getFootprints(_, [(0,0)]) 

// Currying
// setModX :: array<array<int>> => int => array<array<int>>
// let setModXWithPos = setModX(pos) :: int => array<array<int>>
// plus = x => y => x + y
// plus3 = plus(3)

let setModX = (d, pos) => {
  let (x,y) = pos
  (mod(x, d),y)
}

let setModWidth = setModX(width)

let getMapInfo = (pos) => {
    let (x,y) = pos
    inputArray[y] -> Belt.Option.mapWithDefault("", (str) => Js.String.get(str, x))
}

let infoToNumber = (tree) => switch tree {
    | "#" => 1
    | _ => 0
}

let sum = (numbers) => numbers -> Belt.Array.reduce(0, (acc, value) => acc + value)

(3,1) -> getFootprintsFromOrigin
    -> Belt.Array.map(setModWidth)
    -> Belt.Array.map(getMapInfo)
    -> Belt.Array.map(infoToNumber)
    -> sum
    -> Js.log


// part 2
let slope = [(1,1), (3,1), (5,1), (7,1), (1,2)];

let multiply = (numbers) => numbers -> Belt.Array.reduce(1, (acc, value) => acc * value)

slope -> Belt.Array.map((x) => 
        x -> getFootprintsFromOrigin
        -> Belt.Array.map(setModWidth)
        -> Belt.Array.map(getMapInfo)
        -> Belt.Array.map(infoToNumber)
        -> sum
    )
    -> multiply
    -> Js.log
