"use strict";

exports.unsafeCons = function(a) {
  return function(arr) {
    arr.unshift(a);
    return arr;
  };
};

exports.unsafeSnoc = function(a) {
  return function(arr) {
    arr.push(a);
    return arr;
  };
};
