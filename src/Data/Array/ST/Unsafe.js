"use strict";

exports.unsafeFreeze = function (xs) {
  return function () {
    return xs;
  };
};

exports.unsafeThaw = function (xs) {
  return function () {
    return xs;
  };
};
