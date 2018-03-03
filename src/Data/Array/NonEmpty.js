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

exports.traverse1Impl = function () {
  function Cont(fn) {
    this.fn = fn;
  }

  var ConsCell = function (head, tail) {
    this.head = head;
    this.tail = tail;
  };

  function FinalCell(x) {
    this.val = x;
  }

  function consList(x) {
    return function (xs) {
      return new ConsCell(x, xs);
    };
  }

  function listToArray(list) {
    var arr = [];
    var xs = list;
    while (xs instanceof ConsCell) {
      arr.push(xs.head);
      xs = xs.tail;
    }
    arr.push(xs.val);
    return arr;
  }

  return function (apply) {
    return function (map) {
      return function (f) {
        var buildFrom = function (x, ys) {
          return apply(map(consList)(f(x)))(ys);
        };

        var go = function (acc, currentLen, xs) {
          if (currentLen === 0) {
            return acc;
          } else {
            var last = xs[currentLen - 1];
            return new Cont(function () {
              return go(buildFrom(last, acc), currentLen - 1, xs);
            });
          }
        };

        return function (array) {
          var acc = new FinalCell(array[array.length - 1]);
          var result = go(acc, array.length - 1, array);
          while (result instanceof Cont) {
            result = result.fn();
          }

          return map(listToArray)(result);
        };
      };
    };
  };
}();
