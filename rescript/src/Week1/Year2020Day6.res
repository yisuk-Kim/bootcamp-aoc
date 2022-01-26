open Belt

let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day6.input.txt")

let splitGroup = (data) => Js.String.split("\n\n", data)
let splitPerson = (data) => Js.String.split("\n",data)
let splitAnswer = (answer) => answer -> Belt.Array.map((x) => Js.String.split("",x))

let answerArrayToSet = (answer) => answer -> Belt.Array.map((x) => x->Belt.Set.String.fromArray)
let getUnion = (answer) => answer -> Belt.Array.reduce(Belt.Set.String.empty,(acc, x) => Belt.Set.String.union(acc,x))

let answerSetToArray = (answer) => answer -> Belt.Array.map((x) => x->Belt.Set.String.toArray)

let countQuestionNumber = (question) => Belt.Array.length(question)

let sum = (num) => num -> Belt.Array.reduce(0, (acc, x) => acc + x)

let answerSet = input -> splitGroup 
    -> Belt.Array.map((x) => x->splitPerson) 
    -> Belt.Array.map((x) => x->splitAnswer)
    -> Belt.Array.map((x) => x->answerArrayToSet)

answerSet -> Belt.Array.map((x) => x->getUnion)
    -> answerSetToArray
    -> Belt.Array.map((x) => x->countQuestionNumber)
    -> sum
    -> Js.log

// part2
let getIntersection = (answer) => answer -> Belt.Array.reduce(switch answer[0] {
    | None => Belt.Set.String.empty
    | Some(set) => set
}, (acc, v) => Belt.Set.String.intersect(acc,v))

answerSet -> Belt.Array.map((x) => x->getIntersection)
    -> answerSetToArray
    -> Belt.Array.map((x) => x->countQuestionNumber)
    -> sum
    -> Js.log
