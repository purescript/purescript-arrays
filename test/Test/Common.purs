module Test.Common where

import Control.Monad.Eff (Eff())

foreign import data ASSERT :: !

foreign import assert
  """
  function assert(success) {
    return function () {
      if (success) return {};
      throw new Error("Assertion failed");
    };
  }
  """ :: forall e. Boolean -> Eff (assert :: ASSERT | e) Unit
