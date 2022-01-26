// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Caml_array = require("rescript/lib/js/caml_array.js");
var Caml_int32 = require("rescript/lib/js/caml_int32.js");

var input = Fs.readFileSync("input/Week1/Year2020Day3.input.txt", "utf8");

function splitInput(pattern) {
  return pattern.split("\n");
}

var inputArray = input.split("\n");

var width = Caml_array.get(inputArray, 0).length;

function getFootprints(slope, _footprints) {
  while(true) {
    var footprints = _footprints;
    var length = footprints.length;
    var lastPos = Caml_array.get(footprints, length - 1 | 0);
    var slopeY = slope[1];
    var y = lastPos[1];
    if ((y + slopeY | 0) >= inputArray.length) {
      return footprints;
    }
    var $$new = Belt_Array.concat(footprints, [[
            lastPos[0] + slope[0] | 0,
            y + slopeY | 0
          ]]);
    _footprints = $$new;
    continue ;
  };
}

function setModX1(d, pos) {
  return [
          Caml_int32.mod_(pos[0], d),
          pos[1]
        ];
}

function setModWidth(param) {
  return setModX1(width, param);
}

function getTrees(pos) {
  return Belt_Array.map(pos, (function (param) {
                return Caml_array.get(inputArray, param[1])[param[0]];
              }));
}

function treeToNumber(trees) {
  return Belt_Array.map(trees, (function (v) {
                if (v === "#") {
                  return 1;
                } else {
                  return 0;
                }
              }));
}

function sum(numbers) {
  return Belt_Array.reduce(numbers, 0, (function (acc, value) {
                return acc + value | 0;
              }));
}

console.log(sum(treeToNumber(getTrees(Belt_Array.map(getFootprints([
                          3,
                          1
                        ], [[
                            0,
                            0
                          ]]), (function (x) {
                        return setModX1(width, x);
                      }))))));

var slope = [
  [
    1,
    1
  ],
  [
    3,
    1
  ],
  [
    5,
    1
  ],
  [
    7,
    1
  ],
  [
    1,
    2
  ]
];

function multiply(numbers) {
  return Belt_Array.reduce(numbers, 1, (function (acc, value) {
                return Math.imul(acc, value);
              }));
}

console.log(multiply(Belt_Array.map(slope, (function (x) {
                return sum(treeToNumber(getTrees(Belt_Array.map(getFootprints(x, [[
                                              0,
                                              0
                                            ]]), (function (x) {
                                          return setModX1(width, x);
                                        })))));
              }))));

exports.input = input;
exports.splitInput = splitInput;
exports.inputArray = inputArray;
exports.width = width;
exports.getFootprints = getFootprints;
exports.setModX1 = setModX1;
exports.setModWidth = setModWidth;
exports.getTrees = getTrees;
exports.treeToNumber = treeToNumber;
exports.sum = sum;
exports.slope = slope;
exports.multiply = multiply;
/* input Not a pure module */
