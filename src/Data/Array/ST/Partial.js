"use strict";

exports.peekImpl = function (xs) {
  return function (i) {
    return function () {
      return xs[i];
    };
  };
};

exports.pokeImpl = function (xs) {
  return function (i) {
    return function (a) {
      return function () {
        xs[i] = a;
        return {};
      };
    };
  };
};
