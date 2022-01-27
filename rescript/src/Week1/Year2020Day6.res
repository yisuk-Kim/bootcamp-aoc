open Belt

let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day6.input.txt")

let splitGroup = (data) => Js.String.split("\n\n", data)
let splitPerson = (data) => Js.String.split("\n",data)
let splitAnswer = (answer) => Js.String.split("",answer)

let answerArrayToSet = (answer) => answer -> Belt.Set.String.fromArray
let getUnion = (answer) => answer -> Belt.Array.reduce(Belt.Set.String.empty,(acc, x) => Belt.Set.String.union(acc,x))
let getSize = (set) => set -> Belt.Set.String.size

let sum = (num) => num -> Belt.Array.reduce(0, (acc, x) => acc + x)

let answerSet = input -> splitGroup 
    -> Belt.Array.map((x) => x->splitPerson) 
    -> Belt.Array.map((xs) => xs
        -> Belt.Array.map((x) => x -> splitAnswer)
    )
    -> Belt.Array.map((xs) => xs
        -> Belt.Array.map((x) => x -> answerArrayToSet)
    )

answerSet -> Belt.Array.map((x) => x -> getUnion)
    -> Belt.Array.map((x) => x -> getSize)
    -> sum
    -> Js.log

// part2
let getIntersection = (answer) => answer -> Belt.Array.reduce(switch answer[0] {
    | None => Belt.Set.String.empty
    | Some(set) => set
}, (acc, v) => Belt.Set.String.intersect(acc,v))

answerSet -> Belt.Array.map((x) => x -> getIntersection)
    -> Belt.Array.map((x) => x -> getSize)
    -> sum
    -> Js.log
