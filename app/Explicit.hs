module Explicit (elasticSpace) where

import Graphics.Gloss.Interface.Pure.Game hiding (Vector)
import Linear (normalize, (*^), Metric(quadrance, norm, dot), V2(..), Additive(zero))

import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MVec
import Data.Vector (Vector)

import Data.Foldable (forM_)
import Control.Monad (when)
import Simulation (gravityStrength, objectRadius, Params, Simulation(..))
import Control.Lens ((^.))

data Ball = Ball 
  { _position :: V2 Float
  , _velocity :: V2 Float
  , _acceleration :: V2 Float 
  }

newPosition :: Float -> Ball -> V2 Float
newPosition dt (Ball pos vel acc) = pos + dt *^ vel + 0.5 * dt * dt *^ acc

getAccelerations :: Params -> Vector (V2 Float) -> Vector (V2 Float)
getAccelerations params ps = fmap (getAcceleration params ps) ps

getAcceleration :: Params -> Vector (V2 Float) -> V2 Float -> V2 Float
getAcceleration params ps pos =
  Vec.foldl' (\acc pos' -> acc + part pos') zero ps
  where
    part pos' 
      | pos' == pos = zero
      | otherwise = let
        dir = normalize (pos' - pos)
        mag = params^.gravityStrength * 30000 / norm (pos' - pos) ^ (2 :: Int)
        in mag *^ dir

updateBall :: Float -> V2 Float -> V2 Float -> Ball -> Ball
updateBall dt pos' acc' (Ball _ vel acc)
  = Ball pos' (vel + 0.5 * dt *^ (acc + acc')) acc'

integrateBalls :: Params -> Float -> Vector Ball -> Vector Ball
integrateBalls params dt balls = let
  ps = fmap (newPosition dt) balls
  as = getAccelerations params ps
  balls' = Vec.zipWith3 (updateBall dt) ps as balls 
  in balls'

colliding :: Params -> Ball -> Ball -> Bool
colliding params (Ball p1 v1 _) (Ball p2 v2 _) = let
  touching = norm (p1 - p2) < 2 * params^.objectRadius
  inward = dot (v1 - v2) (p1 - p2) < 0
  in touching && inward 

-- assumes balls are colliding
handleCollision :: (Ball, Ball) -> (Ball, Ball)
handleCollision (Ball p1 v1 a1, Ball p2 v2 a2) = let
  -- wikipedia formula
  v1' = v1 - (dot (v1 - v2) (p1 - p2) / quadrance (p1 - p2)) *^ (p1 - p2)
  v2' = v2 - (dot (v2 - v1) (p2 - p1) / quadrance (p2 - p1)) *^ (p2 - p1)
  in (Ball p1 v1' a1, Ball p2 v2' a2)

handleCollisions :: Params -> Vector Ball -> Vector Ball
handleCollisions params = Vec.modify $ \balls -> do
  let len = MVec.length balls
  forM_ [0 .. len - 1] $ \i ->
    forM_ [i + 1 .. len - 1] $ \j -> do
      ball1 <- MVec.read balls i
      ball2 <- MVec.read balls j
      when (colliding params ball1 ball2) $ do
        let (ball1', ball2') = handleCollision (ball1, ball2)
        MVec.write balls i ball1'
        MVec.write balls j ball2' 

{-
collectCollisions :: Vector Ball -> Vector [V2 Float] 
collectCollisions balls = Vec.create $ do
  -- is there a good way to do this without ST?
  let len = Vec.length balls
  lists <- MVec.replicate len []
  forM_ [0 .. len - 1] $ \i ->
    forM_ [i + 1 .. len - 1] $ \j -> do
      let ball1 = balls ! i
      let ball2 = balls ! j
      when (colliding ball1 ball2) $ do
        let (v1, v2) = handleCollision (ball1, ball2)
        MVec.modify lists (v1 :) i
        MVec.modify lists (v2 :) j
  return lists

averageCollision :: [V2 Float] -> Ball -> Ball
averageCollision vs (Ball pos vel acc) = Ball pos vel' acc
  where
    vel' =
      if null vs then vel
      else sum vs ^/ fromIntegral (length vs)
-}

draw :: Float -> Ball -> Picture
draw r (Ball (V2 x y) _ _) = translate x y $ color (light blue) $ circleSolid r

drawModel :: Params -> Vector Ball -> Picture
drawModel params = pictures . map (draw (params^.objectRadius)) . Vec.toList

handleEvent :: Event -> Vector Ball -> Vector Ball
handleEvent event vs =
  case event of
    EventKey (MouseButton LeftButton) Down (Modifiers Up Up Up) (x, y) -> 
      Vec.cons (Ball (V2 x y) zero zero) vs
    EventKey (Char 'r') Down (Modifiers Up Up Up) _ -> Vec.empty
    _ -> vs

stepModel :: Params -> Float -> Vector Ball -> Vector Ball
stepModel params dt = handleCollisions params . integrateBalls params dt

elasticSpace :: Params -> Simulation (Vector Ball)
elasticSpace params = Simulation
  { _name = "Physics test: gravitating objects, elastic collisions"
  , _backgroundColor = white
  , _initialModel = Vec.empty
  , _drawModel = drawModel params
  , _handleEvent = handleEvent
  , _stepModel = stepModel params
  }

-- TODO: add inelasticSpace