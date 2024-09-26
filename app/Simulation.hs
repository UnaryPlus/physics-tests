{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Simulation (Params(..), windowSize, objectRadius, worldRadius, gravityStrength, Simulation(..), run) where

import Control.Lens.TH (makeLenses)
import Control.Lens (set, (^.))
import Graphics.Gloss.Interface.Pure.Game (Color, Picture, Event, Display(InWindow), play)
import Text.Read (readMaybe)
import Control.Monad (foldM, (>=>))

data Params = Params
  { _windowSize :: Int 
  , _objectRadius :: Float
  , _worldRadius :: Float
  , _gravityStrength :: Float
  }
$(makeLenses ''Params)

defaultParams :: Params
defaultParams = Params
  { _windowSize = 700
  , _objectRadius = 20
  , _worldRadius = 350
  , _gravityStrength = 50
  }

-- Overkill argument parsing API
-- TODO: make worldRadius = windowSize / 2 by default
data Result a
  = InvalidValue String
  | Pass
  | Success a

instance Semigroup (Result a) where
  (<>) x y = 
    case x of
      Pass -> y
      _ -> x

instance Monoid (Result a) where
  mempty = Pass

mkArgParser :: Read b => [String] -> (b -> a -> a) -> (String, String) -> a -> Result a
mkArgParser flags update (flag, val) params
  | flag `elem` flags =
    case readMaybe val of
      Nothing -> InvalidValue val
      Just val' -> Success (update val' params)
  | otherwise = Pass

argParsers :: [(String, String) -> Params -> Result Params]
argParsers =
  [ mkArgParser ["-windowsize", "-s"] (set windowSize) 
  , mkArgParser ["-objectradius", "-r"] (set objectRadius)
  , mkArgParser ["-worldradius", "-w"] (set worldRadius)
  , mkArgParser ["-gravitystrength", "-gravity", "-g"] (set gravityStrength)
  ]

parseArg :: (String, String) -> Params -> Either String Params
parseArg arg@(flag, _) params = 
  case foldMap (\p -> p arg params) argParsers of
    InvalidValue val -> Left $ "Invalid value: " ++ val
    Pass -> Left $ "Unknown flag: " ++ flag
    Success params' -> Right params'

parseArgs' :: [(String, String)] -> Either String Params
parseArgs' = foldM (flip parseArg) defaultParams

pairs :: [a] -> Either String [(a, a)]
pairs = \case
  [] -> Right []
  [_] -> Left "Invalid number of arguments (should be paired)"
  x:y:xs -> fmap ((x, y):) (pairs xs)

parseArgs :: [String] -> Either String Params
parseArgs = pairs >=> parseArgs'

data Simulation model = Simulation
  { _name :: String
  , _backgroundColor :: Color
  , _initialModel :: model
  , _drawModel :: model -> Picture
  , _handleEvent :: Event -> model -> model
  , _stepModel :: Float -> model -> model
  }
$(makeLenses ''Simulation)

run :: [String] -> (Params -> Simulation model) -> IO ()
run args sim' =
  case parseArgs args of
    Left msg -> putStrLn msg
    Right params ->
      let sim = sim' params in
        play
          (InWindow (sim^.name) (params^.windowSize, params^.windowSize) (20, 20))
          (sim^.backgroundColor)
          60 --fps
          (sim^.initialModel)
          (sim^.drawModel)
          (sim^.handleEvent)
          (sim^.stepModel)  
