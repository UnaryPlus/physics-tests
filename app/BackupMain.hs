module Main (main) where

import Graphics.Gloss.Interface.Pure.Game hiding (Vector)
import Linear

import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MVec
import Data.Vector (Vector, MVector)

import Control.Monad.ST
import Data.Foldable (forM_)
import Control.Monad (when)
import Control.Applicative (liftA2)
-- import Debug.Trace

data Verlet = Verlet { acceleration :: V2 Float, position :: V2 Float, prevPosition :: V2 Float }

objectRadius, worldRadius, gravityStrength, bigG :: Float
objectRadius = 20
worldRadius = 350
gravityStrength = 50
bigG = gravityStrength * 30000

windowSize :: Int
windowSize = 700

applyGravityDown :: Verlet -> Verlet
applyGravityDown (Verlet _ pos prevPos) = Verlet (V2 0 (-gravityStrength)) pos prevPos

applyGravitySpace :: Vector Verlet -> Verlet -> Verlet
applyGravitySpace vs (Verlet _ pos prevPos) = Verlet acc pos prevPos
  where
    acc = Vec.foldl' (\a (Verlet _ pos' _) -> a + part pos') zero vs
    part pos' = let 
      dir = normalize (pos' - pos)
      mag = bigG / norm (pos' - pos) ^ 2
      in if pos == pos' then zero else mag *^ dir

updateVerlet :: Float -> Verlet -> Verlet
updateVerlet dt (Verlet acc pos prevPos) = Verlet zero (2 *^ pos - prevPos + dt * dt *^ acc) pos

applyConstraint :: Verlet -> Verlet
applyConstraint v@(Verlet acc pos prevPos) = 
  let maxRadius = worldRadius - objectRadius in
  if norm pos > maxRadius
    then Verlet acc (normalize pos ^* maxRadius) prevPos
    else v 

fixCollisions :: MVector s Verlet -> ST s ()
fixCollisions objects = do
  -- TODO: randomize order?
  forM_ [0 .. MVec.length objects - 1] $ \i ->
    forM_ [0 .. i - 1] $ \j -> do
      -- TODO: check that this algorithm is correct
      Verlet acc1 pos1 prevPos1 <- MVec.read objects i
      Verlet acc2 pos2 prevPos2 <- MVec.read objects j
      let err = objectRadius - norm (pos1 - pos2) / 2
      when (err > 0) $ do
        let correction = normalize (pos1 - pos2) ^* err
        let pos1' = pos1 + correction
        let pos2' = pos2 - correction
        MVec.write objects i (Verlet acc1 pos1' prevPos1)
        MVec.write objects j (Verlet acc2 pos2' prevPos2)

stepModelDown :: Float -> Vector Verlet -> Vector Verlet
stepModelDown dt = Vec.modify fixCollisions . fmap (applyConstraint . updateVerlet dt . applyGravityDown)

stepModelSpace :: Float -> Vector Verlet -> Vector Verlet
stepModelSpace dt vs = Vec.modify fixCollisions $ fmap (applyConstraint . updateVerlet dt . applyGravitySpace vs) vs

draw :: Float -> Verlet -> Picture
draw r (Verlet _ (V2 x y) _) = translate x y $ color (light blue) $ circleSolid r

initialModel :: Vector Verlet
initialModel = Vec.empty

handleEvent :: Event -> Vector Verlet -> Vector Verlet
handleEvent event vs =
  case event of
    EventKey (MouseButton LeftButton) Down (Modifiers Up Up Up) (x, y) -> 
      Vec.cons (Verlet zero (V2 x y) (V2 x y)) vs
    EventKey (Char 'r') Down (Modifiers Up Up Up) _ -> Vec.empty
    _ -> vs

main :: IO ()
main = play
  (InWindow "physics test" (windowSize, windowSize) (20, 20))
  black -- background color
  60    -- fps
  initialModel
  (\model -> pictures $ color white (circleSolid worldRadius) : map (draw objectRadius) (Vec.toList model))
  handleEvent
  stepModelSpace -- stepModelDown
