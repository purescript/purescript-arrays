"use strict";

exports.peekSTArray = function (xs) {
  return function (i) {
    return function () {
      return xs[i];
    };
  };
};

exports.pokeSTArray = function (xs) {
  return function (i) {
    return function (a) {
      return function () {
        xs[i] = a;
        return {};
      };
    };
  };
};
