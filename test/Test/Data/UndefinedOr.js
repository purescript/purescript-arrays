const undefinedImpl = undefined;
export {undefinedImpl as undefined};

export function defined(x) {
  return x;
}

export function eqUndefinedOrImpl(eq) {
  return function (a) {
    return function (b) {
      return (a === undefined && b === undefined) || eq(a)(b);
    };
  };
}

export function compareUndefinedOrImpl(lt) {
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
}
