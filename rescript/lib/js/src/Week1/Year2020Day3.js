// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Caml_int32 = require("rescript/lib/js/caml_int32.js");

var input = Fs.readFileSync("input/Week1/Year2020Day3.input.txt", "utf8");

function splitInput(pattern) {
  return pattern.split("\n");
}

var inputArray = input.split("\n");

var string = Belt_Array.get(inputArray, 0);

var width = string !== undefined ? string.length : 0;

function getFootprints(slope, _footprints) {
  while(true) {
    var footprints = _footprints;
    var length = footprints.length;
    var pos = Belt_Array.get(footprints, length - 1 | 0);
    var lastPos = pos !== undefined ? pos : [
        0,
        0
      ];
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

function getFootprintsFromOrigin(__x) {
  return getFootprints(__x, [[
                0,
                0
              ]]);
}

function setModX(d, pos) {
  return [
          Caml_int32.mod_(pos[0], d),
          pos[1]
        ];
}

function setModWidth(param) {
  return setModX(width, param);
}

function getMapInfo(pos) {
  var string = Belt_Array.get(inputArray, pos[1]);
  if (string !== undefined) {
    return string[pos[0]];
  } else {
    return "";
  }
}

function infoToNumber(tree) {
  if (tree === "#") {
    return 1;
  } else {
    return 0;
  }
}

function sum(numbers) {
  return Belt_Array.reduce(numbers, 0, (function (acc, value) {
                return acc + value | 0;
              }));
}

console.log(sum(Belt_Array.map(Belt_Array.map(Belt_Array.map(getFootprintsFromOrigin([
                          3,
                          1
                        ]), (function (x) {
                        return setModX(width, x);
                      })), getMapInfo), infoToNumber)));

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
                return sum(Belt_Array.map(Belt_Array.map(Belt_Array.map(getFootprintsFromOrigin(x), (function (x) {
                                          return setModX(width, x);
                                        })), getMapInfo), infoToNumber));
              }))));

exports.input = input;
exports.splitInput = splitInput;
exports.inputArray = inputArray;
exports.width = width;
exports.getFootprints = getFootprints;
exports.getFootprintsFromOrigin = getFootprintsFromOrigin;
exports.setModX = setModX;
exports.setModWidth = setModWidth;
exports.getMapInfo = getMapInfo;
exports.infoToNumber = infoToNumber;
exports.sum = sum;
exports.slope = slope;
exports.multiply = multiply;
/* input Not a pure module */
