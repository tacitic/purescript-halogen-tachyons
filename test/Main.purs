module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Halogen.CSS.Tachyons as T

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Compilation of Tachyons succeeded"
