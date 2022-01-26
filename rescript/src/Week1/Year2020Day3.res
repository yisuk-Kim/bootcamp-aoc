let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day3.input.txt")

let splitInput = (pattern) => Js.String.split("\n", pattern)

let inputArray = input -> splitInput

let width = Js.String.length(inputArray[0])

let rec getFootprints = (slope, footprints) => {
    let length = footprints -> Belt.Array.length
    let lastPos = footprints[length - 1]
    
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

// Currying
// setModX :: array<array<int>> => int => array<array<int>>
// let setModXWithPos = setModX(pos) :: int => array<array<int>>
// plus = x => y => x + y
// plus3 = plus(3)

let setModX1 = (d, pos) => {
  let (x,y) = pos
  (mod(x, d),y)
}

let setModWidth = setModX1(width)

let getTrees = (pos) => pos -> Belt.Array.map(((x,y)) => Js.String.get(inputArray[y],x))

let treeToNumber = (trees) => trees -> Belt.Array.map((v) => 
    switch v {
    | "#" => 1
    | _ => 0
    }
)

let sum = (numbers) => numbers -> Belt.Array.reduce(0, (acc, value) => acc + value)

(3,1) -> getFootprints([(0,0)])
    -> Belt.Array.map((x) => x->setModWidth)
    -> getTrees
    -> treeToNumber
    -> sum
    -> Js.log

// part 2

let slope = [(1,1), (3,1), (5,1), (7,1), (1,2)];

let multiply = (numbers) => Belt.Array.reduce(numbers, 1, (acc, value) => acc * value)

slope -> Belt.Array.map((x) => {
        x -> getFootprints([(0,0)])
        -> Belt.Array.map((x) => x->setModWidth)
        -> getTrees
        -> treeToNumber
        -> sum
    })
    -> multiply
    -> Js.log
