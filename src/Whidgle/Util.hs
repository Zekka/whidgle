{-# LANGUAGE NoMonomorphismRestriction #-}
{-
 - Whidgle.Util
 -
 - Truly miscellaneous functions.
 -}
module Whidgle.Util where

import Control.Monad.Trans

-- putStrLn, already lifted
lputs = liftIO . putStrLn
