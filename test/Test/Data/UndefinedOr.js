exports.undefined = undefined;

exports.defined = function (x) {
  return x;
};

exports.eqUndefinedOrImpl = function (eq) {
  return function (a) {
    return function (b) {
      return (a === undefined && b === undefined) || eq(a)(b);
    };
  };
};

exports.compareUndefinedOrImpl = function (lt) {
  return function (eq) {
    return function (gt) {
      return function (compare) {
        return function (a) {
          return function (b) {
            if (a === undefined && b === undefined) return eq;
            if (a === undefined) return lt;
            if (b === undefined) return gt;
            return compare(a)(b);
          };
        };
      };
    };
  };
};
