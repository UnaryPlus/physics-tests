module VerletImplicit (implicitDown, implicitSpace) where

import Graphics.Gloss.Interface.Pure.Game hiding (Vector)
import Linear (normalize, (*^), (^*), Metric(norm), V2(..), Additive(zero))

import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MVec
import Data.Vector (Vector, MVector)

import Control.Monad.ST (ST)
import Data.Foldable (forM_)
import Control.Monad (when)

import Simulation (Simulation(..), gravityStrength, objectRadius, worldRadius, Params)
import Control.Lens ((^.))

data Verlet = Verlet 
  { _acceleration :: V2 Float
  , _position :: V2 Float
  , _prevPosition :: V2 Float 
  }

applyGravityDown :: Params -> Verlet -> Verlet
applyGravityDown params (Verlet _ pos prevPos) = Verlet (V2 0 (-params^.gravityStrength)) pos prevPos

applyGravitySpace :: Params -> Vector Verlet -> Verlet -> Verlet
applyGravitySpace params vs (Verlet _ pos prevPos) = Verlet acc pos prevPos
  where
    acc = Vec.foldl' (\a (Verlet _ pos' _) -> a + part pos') zero vs
    part pos' = let 
      dir = normalize (pos' - pos)
      mag = params^.gravityStrength * 30000 / norm (pos' - pos) ^ (2 :: Int)
      in if pos == pos' then zero else mag *^ dir

updateVerlet :: Float -> Verlet -> Verlet
updateVerlet dt (Verlet acc pos prevPos) = Verlet zero (2 *^ pos - prevPos + dt * dt *^ acc) pos

applyConstraint :: Params -> Verlet -> Verlet
applyConstraint params v@(Verlet acc pos prevPos) = 
  let maxRadius = params^.worldRadius - params^.objectRadius in
  if norm pos > maxRadius
    then Verlet acc (normalize pos ^* maxRadius) prevPos
    else v 

fixCollisions :: Params -> MVector s Verlet -> ST s ()
fixCollisions params objects = do
  -- TODO: randomize order? (or fix spinning problem in some other way (but keep current version as an option?))
  forM_ [0 .. MVec.length objects - 1] $ \i ->
    forM_ [0 .. i - 1] $ \j -> do
      -- TODO: check that this algorithm is correct
      Verlet acc1 pos1 prevPos1 <- MVec.read objects i
      Verlet acc2 pos2 prevPos2 <- MVec.read objects j
      let err = params^.objectRadius - norm (pos1 - pos2) / 2
      when (err > 0) $ do
        let correction = normalize (pos1 - pos2) ^* err
        let pos1' = pos1 + correction
        let pos2' = pos2 - correction
        MVec.write objects i (Verlet acc1 pos1' prevPos1)
        MVec.write objects j (Verlet acc2 pos2' prevPos2)

stepModelDown :: Params -> Float -> Vector Verlet -> Vector Verlet
stepModelDown params dt = Vec.modify (fixCollisions params) . fmap (applyConstraint params . updateVerlet dt . applyGravityDown params)

stepModelSpace :: Params -> Float -> Vector Verlet -> Vector Verlet
stepModelSpace params dt vs = Vec.modify (fixCollisions params) $ fmap (applyConstraint params . updateVerlet dt . applyGravitySpace params vs) vs

draw :: Float -> Verlet -> Picture
draw r (Verlet _ (V2 x y) _) = translate x y $ color (light blue) $ circleSolid r

drawModel :: Params -> Vector Verlet -> Picture
drawModel params balls = 
  pictures $ 
    color white (circleSolid (params^.worldRadius)) 
    : map (draw (params^.objectRadius)) (Vec.toList balls)

handleEvent :: Event -> Vector Verlet -> Vector Verlet
handleEvent event vs =
  case event of
    EventKey (MouseButton LeftButton) Down (Modifiers Up Up Up) (x, y) -> 
      Vec.cons (Verlet zero (V2 x y) (V2 x y)) vs
    EventKey (Char 'r') Down (Modifiers Up Up Up) _ -> Vec.empty
    _ -> vs

implicitDown :: Params -> Simulation (Vector Verlet)
implicitDown params = Simulation
  { _name = "Physics test: downward gravity, implicit verlet collision handling"
  , _backgroundColor = black
  , _initialModel = Vec.empty
  , _drawModel = drawModel params
  , _handleEvent = handleEvent
  , _stepModel = stepModelDown params
  }

implicitSpace :: Params -> Simulation (Vector Verlet)
implicitSpace params = Simulation
  { _name = "Physics test: gravitating objects, implicit verlet collision handling"
  , _backgroundColor = black
  , _initialModel = Vec.empty
  , _drawModel = drawModel params
  , _handleEvent = handleEvent
  , _stepModel = stepModelSpace params
  }
