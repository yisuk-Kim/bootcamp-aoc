open Belt

let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day3.input.txt")

let splitInput = (pattern) => Js.String.split("\n", pattern)

let inputArray = input -> splitInput

let width = switch inputArray[0] {
    | None => 0
    | Some(string) => Js.String.length(string)
}

let rec getFootprints = (slope, footprints) => {
    let length = footprints -> Belt.Array.length
    let lastPos = switch footprints[length - 1] {
        | None => (0,0)
        | Some(pos) => pos
    }
    
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

// None -> case handle??
let getMapInfo = (pos) => {
    let (x,y) = pos
    switch inputArray[y] {
        | None => ""
        | Some(string) => Js.String.get(string,x)
    }
}

let infoToNumber = (tree) => switch tree {
    | "#" => 1
    | _ => 0
}

let sum = (numbers) => numbers -> Belt.Array.reduce(0, (acc, value) => acc + value)

(3,1) -> getFootprintsFromOrigin
    -> Belt.Array.map((x) => x->setModWidth)
    -> Belt.Array.map((x) => x->getMapInfo)
    -> Belt.Array.map((x) => x->infoToNumber)
    -> sum
    -> Js.log


// part 2
let slope = [(1,1), (3,1), (5,1), (7,1), (1,2)];

let multiply = (numbers) => numbers -> Belt.Array.reduce(1, (acc, value) => acc * value)

slope -> Belt.Array.map((x) => 
        x -> getFootprintsFromOrigin
        -> Belt.Array.map((x) => x->setModWidth)
        -> Belt.Array.map((x) => x->getMapInfo)
        -> Belt.Array.map((x) => x->infoToNumber)
        -> sum
    )
    -> multiply
    -> Js.log
