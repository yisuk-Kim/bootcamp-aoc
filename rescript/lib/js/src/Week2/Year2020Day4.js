// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Belt_Array = require("rescript/lib/js/belt_Array.js");

var input = Fs.readFileSync("input/Week2/Year2020Day4.sample.txt", "utf8");

function parsePassport(input) {
  var splitField = function (data) {
    return data.split(/\s/);
  };
  return Belt_Array.map(input.split("\n\n"), splitField);
}

function countPassport(param) {
  return 0;
}

console.log(parsePassport(input));

exports.input = input;
exports.parsePassport = parsePassport;
exports.countPassport = countPassport;
/* input Not a pure module */
