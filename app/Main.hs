module Main where

import Graphics.Gloss.Interface.Pure.Game hiding (Vector)
import Linear

import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MVec
import Data.Vector (Vector, MVector)

import Control.Monad.ST
import Data.Foldable (forM_)
import Control.Monad (when)
-- import Debug.Trace

data Ball = Ball { acceleration :: V2 Float, velocity :: V2 Float, position :: V2 Float }

integrate1 :: Float -> Ball -> Ball
integrate1 dt (Ball acc vel pos) = Ball acc vel (pos + dt *^ vel + 0.5 * dt * dt *^ acc)


integrate3 :: Float -> Ball -> Ball
integrate3 dt (Ball acc vel pos) = Ball acc () pos

main :: IO ()
main = return ()