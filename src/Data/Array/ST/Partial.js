"use strict";

exports.peekSTArrayImpl = function (xs) {
  return function (i) {
    return function () {
      return xs[i];
    };
  };
};

exports.pokeSTArrayImpl = function (xs) {
  return function (i) {
    return function (a) {
      return function () {
        xs[i] = a;
        return {};
      };
    };
  };
};
