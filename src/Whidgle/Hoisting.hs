{-
 - Whidgle.Hoisting
 -
 - Describes hoisting for Whidgle.
 -}
module Whidgle.Hoisting
( ioHoistWhidgle
) where

import Control.Monad.Reader
import Control.Monad.State

import System.IO.Unsafe

import Whidgle.Types

-- Hoists an IO morphism to a Whidgle one.
-- There's probably a clever mmorphy way to do this but I really only need this
-- operation in one or two places.
ioHoistWhidgle :: (IO a -> IO b) -> (Whidgle a -> Whidgle b)
ioHoistWhidgle ioA x = do
  settings <- ask
  st <- get
  -- the Whidgle type currently doesn't support the lift operation
  Whidgle . lift . lift $ (ioA (runWhidgle settings st x))
