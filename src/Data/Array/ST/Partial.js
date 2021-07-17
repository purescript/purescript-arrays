"use strict";

export var peekImpl = function (i) {
  return function (xs) {
    return function () {
      return xs[i];
    };
  };
};

export var pokeImpl = function (i) {
  return function (a) {
    return function (xs) {
      return function () {
        xs[i] = a;
      };
    };
  };
};
