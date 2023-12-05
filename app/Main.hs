module Main where

import App.Prelude
import Day1.Calibration

main :: IO ()
main = do
  inp <- readFile "app/Day1/input.txt"
  let Calibration s = sumCalibrations2 $ lines inp
  print s
