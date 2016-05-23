/* global exports */
"use strict";

exports.unsafeIndexImpl = function (xs) {
  return function (n) {
    return xs[n];
  };
};
