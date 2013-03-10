module Main where

import Test.HUnit
import Math.GSLInterp
import qualified Data.Vector.Unboxed as V
import Data.Maybe (isJust)
import System.Exit
import System.Random (randomRIO)

eps :: Double
eps = 1e-15

epsEqual 
  :: Double 
  -> Double 
  -> Bool
epsEqual x y = r < eps
  where r = if m == 0 then 0 else abs (x - y) / m
        m = max x y

testInit 
  :: String
  -> Maybe InterpStructPtr 
  -> Test
testInit msg s = 
  let check = isJust s in TestCase (assertBool msg check)

testInterpEval
  :: String
  -> Maybe InterpStructPtr
  -> Double
  -> Double
  -> Test
testInterpEval msg s x y = 
  let check = 
        case s of 
          Just p  -> epsEqual y $ interpEval p x
          Nothing -> False
  in TestCase (assertBool msg check)

interpTestSuite :: IO ()
interpTestSuite = do
  -- create the test data
  let nx = 21
      dx = 2.0 * pi / (nx - 1)
      xs = V.map (dx*) $ V.fromList [0..nx - 1]
      ys = V.map sin xs
      vf = 0.0                         -- out-of-bounds fill value
      xf = 3.0 * pi                    -- out-of-bounds value to check
  it <- randomRIO (0,truncate nx - 1)  -- abcissa index to check
  -- initialize the interpolant
  s <- interpInit xs ys vf
  -- run the tests and exit appropriately
  counts <- runTestTT $ TestList 
    [ testInit       "Test: initialization" s
    , testInterpEval "Test: interpolation property" s (xs V.! it) (ys V.! it)
    , testInterpEval "Test: fill value" s xf vf]
  if failures counts > 0 
    then exitFailure
    else exitSuccess

main :: IO ()
main = interpTestSuite
