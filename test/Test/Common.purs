module Test.Common where

import Control.Monad.Eff (Eff())

foreign import data Assert :: !

foreign import assert
  """
  function assert(success) {
    return function () {
      if (success) return {};
      throw new Error("Assertion failed");
    };
  }
  """ :: forall e. Boolean -> Eff (assert :: Assert | e) Unit
