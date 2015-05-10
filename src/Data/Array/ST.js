/* global exports */
"use strict";

// module Data.Array.ST

exports.runSTArray = function (f) {
  return f;
};

exports.emptySTArray = function () {
  return [];
};

exports.peekSTArrayImpl = function (just, nothing, arr, i) {
  return function () {
    return i < arr.length ? just(arr[i]) : nothing;
  };
};

exports.pokeSTArrayImpl = function (arr, i, a) {
  return function () {
    var ret = i < arr.length;
    if (ret) arr[i] = a;
    return ret;
  };
};

exports.pushAllSTArrayImpl = function (arr, as) {
  return function () {
    return arr.push.apply(arr, as);
  };
};

exports.spliceSTArrayImpl = function (arr, i, howMany, bs) {
  return function () {
    return arr.splice.apply(arr, [i, howMany].concat(bs));
  };
};

exports.copyImpl = function (arr) {
  return function () {
    return arr.slice();
  };
};

exports.toAssocArray = function (arr) {
  return function () {
    var n = arr.length;
    var as = new Array(n);
    for (var i = 0; i < n; i++) as[i] = { value: arr[i], index: i };
    return as;
  };
};
