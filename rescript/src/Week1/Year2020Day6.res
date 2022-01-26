let input = Node.Fs.readFileAsUtf8Sync("input/Week1/Year2020Day6.input.txt")

let splitInput = (data) => Js.String.split("\n", data)

let getBlankPos = (data) => {
    data -> Belt.Array.mapWithIndex((i, x) => {
        if x == "" {
            i
        }
        else {
            0
        }
    })
    -> Belt.Array.keep((x) =>  x != 0)
}

let getGroupSize = (front, back) => {
    back - front - 1
}

let splitGroup = (data) => {
    let blankPos = data 
                -> getBlankPos
                -> Belt.Array.concat([Belt.Array.length(data)])
                
    blankPos -> Belt.Array.mapWithIndex((i, x) => {
        if i != 0 {
            let groupSize = getGroupSize(blankPos[i-1], x)
            Belt.Array.slice(data, ~offset=blankPos[i-1]+1, ~len=groupSize)
        }
        else {
            let groupSize = getGroupSize(-1, x)
            Belt.Array.slice(data, ~offset=0, ~len=groupSize)
        }
    })
}

let splitAnswer = (answer) => answer -> Belt.Array.map((x) => Js.String.split("",x))

let getUnion = (answer) => answer -> Belt.Array.reduceWithIndex([],(acc, xs, i) => {
    if i == 0 {
        xs
    }
    else {
        xs -> Belt.Array.keep((x) => !Js.Array.includes(x,acc))
            -> Belt.Array.concat(acc)
    }
})

let countQuestionNumber = (question) => Belt.Array.length(question)

let sum = (num) => num -> Belt.Array.reduce(0, (acc, x) => acc + x)

input -> splitInput
    -> splitGroup
    -> Belt.Array.map((x) => x->splitAnswer)
    -> Belt.Array.map((x) => x->getUnion)
    -> Belt.Array.map((x) => x->countQuestionNumber)
    -> sum
    -> Js.log

// part2

let getIntersection = (answer) => answer ->  Belt.Array.reduceWithIndex([], (acc, xs, i) => {
                                        if i == 0 {                                            
                                            xs
                                        }
                                        else {
                                            acc -> Belt.Array.keep((x) => Js.Array.includes(x,xs))
                                        }
                                    })

input -> splitInput
    -> splitGroup
    -> Belt.Array.map((x) => x->splitAnswer)
    -> Belt.Array.map((x) => x->getIntersection)
    -> Belt.Array.map((x) => x->countQuestionNumber)
    -> sum
    -> Js.log