const fs = require('fs');

// part 1
let input = fs.readFileSync("input/Week1/Year2020Day3.input.txt", "utf8")

let inputArray = input.split("\n");
let width = inputArray[0].length;

const f = (slope) => {
    const arr = [];
    for (let i = slope[1]; i < inputArray.length; i += slope[1]) {
       arr.push([i, i * slope[0]]) 
    }
    return arr;
}

const getTrees = (pos) => pos.map(([x, y]) => inputArray[x][y])

const treeToNumber = (m) => m.map((v) => v === '#' ? 1 : 0)

const sum = xs => xs.reduce((acc, v) => acc + v, 0);

// ([number, number]) => number
const ns = (slope) => sum(treeToNumber(getTrees(f(slope))));


let matchArray = inputArray.map((v, i) => {
    if (v[3*i % width] == "#") {
        return 1;
    }
    return 0;
});

let result = matchArray.reduce((prev, curr) => prev + curr);

console.log(result);


// part 2
// map(([number, number]) => number) => Array<[number, number]> => Array<number>

let slope = [[1,1], [3,1], [5,1], [7,1], [1,2]];
slope.map(ns).reduce((acc, v) => acc * v, 1) // => [4,3,5,]

let matchArray2 = slope.map((slopeValue, index) => 
    inputArray.map((v, i)=> {
        if(i % slopeValue[1] == 0) {
            let numStep = i / slopeValue[1];
            if (v[slopeValue[0] * numStep % width] == "#") {
                return 1;
            }
        }
        return 0;
    })
);

let result2 = matchArray2.map((v, i) => 
        v.reduce((prev, curr) => prev + curr)
    )
    .reduce((prev, curr) => prev * curr);

console.log(result2);