module Main where

import Graphics.Gloss.Interface.Pure.Game hiding (Vector)
import Linear

import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MVec
import Data.Vector (Vector, MVector, (!))

import Control.Monad.ST
import Data.Foldable (forM_)
import Control.Monad (when)
import qualified Debug.Trace as Debug

objectRadius, worldRadius, gravityStrength, bigG :: Float
objectRadius = 20
worldRadius = 350
gravityStrength = 50
bigG = gravityStrength * 30000

windowSize :: Int
windowSize = 700

data Ball = Ball { position :: V2 Float, velocity :: V2 Float, acceleration :: V2 Float }

newPosition :: Float -> Ball -> V2 Float
newPosition dt (Ball pos vel acc) = pos + dt *^ vel + 0.5 * dt * dt *^ acc

getAccelerations :: Vector (V2 Float) -> Vector (V2 Float)
getAccelerations ps = fmap (getAcceleration ps) ps

getAcceleration :: Vector (V2 Float) -> V2 Float -> V2 Float
getAcceleration ps pos =
  Vec.foldl' (\acc pos' -> acc + part pos') zero ps
  where
    part pos' 
      | pos' == pos = zero
      | otherwise = let
        dir = normalize (pos' - pos)
        mag = bigG / norm (pos' - pos) ^ (2 :: Int)
        in mag *^ dir

updateBall :: Float -> V2 Float -> V2 Float -> Ball -> Ball
updateBall dt pos' acc' (Ball _ vel acc)
  = Ball pos' (vel + 0.5 * dt *^ (acc + acc')) acc'

integrateBalls :: Float -> Vector Ball -> Vector Ball
integrateBalls dt balls = let
  ps = fmap (newPosition dt) balls
  as = getAccelerations ps
  balls' = Vec.zipWith3 (updateBall dt) ps as balls 
  in balls'

colliding :: Ball -> Ball -> Bool
colliding (Ball p1 v1 _) (Ball p2 v2 _) = let
  touching = norm (p1 - p2) < 2 * objectRadius
  inward = dot (v1 - v2) (p1 - p2) < 0
  in touching && inward 

-- assumes balls are colliding
handleCollision :: (Ball, Ball) -> (V2 Float, V2 Float)
handleCollision (Ball p1 v1 _, Ball p2 v2 _) = let
  -- wikipedia formula
  v1' = v1 - (dot (v1 - v2) (p1 - p2) / quadrance (p1 - p2)) *^ (p1 - p2)
  v2' = v2 - (dot (v2 - v1) (p2 - p1) / quadrance (p2 - p1)) *^ (p2 - p1)
  in (v1', v2')

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

handleCollisions :: Vector Ball -> Vector Ball
handleCollisions balls = Vec.zipWith averageCollision (collectCollisions balls) balls

draw :: Float -> Ball -> Picture
draw r (Ball (V2 x y) _ _) = translate x y $ color (light blue) $ circleSolid r

handleEvent :: Event -> Vector Ball -> Vector Ball
handleEvent event vs =
  case event of
    EventKey (MouseButton LeftButton) Down (Modifiers Up Up Up) (x, y) -> 
      Vec.cons (Ball (V2 x y) zero zero) vs
    EventKey (Char 'r') Down (Modifiers Up Up Up) _ -> Vec.empty
    _ -> vs

main :: IO ()
main = play
  (InWindow "physics test" (windowSize, windowSize) (20, 20))
  white -- background color
  60    -- fps
  Vec.empty
  (pictures . map (draw objectRadius) . Vec.toList)
  handleEvent
  (\dt -> handleCollisions . integrateBalls dt)