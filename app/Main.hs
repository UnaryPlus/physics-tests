{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Simulation
import System.Environment (getArgs)
import VerletImplicit (implicitDown, implicitSpace)
import Explicit (elasticSpace)

main :: IO ()
main = getArgs >>= \case
  [] -> putStrLn "TODO: add documentation"
  name : args ->
    case name of
      -- I wish Haskell had Sigma types :(
      "implicitdown" -> Simulation.run args implicitDown
      "implicitspace" -> Simulation.run args implicitSpace
      "elasticspace" -> Simulation.run args elasticSpace
      _ -> putStrLn "First argument must be one of: implicitdown, implicitspace, elasticspace"