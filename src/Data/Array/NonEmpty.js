"use strict";

exports.fold1Impl = function (f) {
  return function (xs) {
    var acc = xs[0];
    var len = xs.length;
      for (var i = 1; i < len; i++) {
        acc = f(acc)(xs[i]);
      }
      return acc;
  };
};
