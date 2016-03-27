/* global exports */
"use strict";

// module Data.Array.Partial

exports.unsafeIndexImpl = function (xs) {
  return function (n) {
    return xs[n];
  };
};
