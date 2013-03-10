module Main where

import Test.HUnit
import Math.GSLInterp
import qualified Data.Vector.Unboxed as V
import Data.Maybe (isJust)
import System.Exit

eps :: Double
eps = 1e-15

epsEqual 
  :: Double 
  -> Double 
  -> Bool
epsEqual x y = r < eps
  where r = abs (x - y) / max x y

testInit 
  :: Maybe InterpStructPtr 
  -> Test
testInit s = let check = isJust s in TestCase (assertBool "initialization" check)

testInterp 
  :: Maybe InterpStructPtr
  -> Double
  -> Double
  -> Test
testInterp s x y = 
  let check = 
        case s of 
          Just p  -> epsEqual y $ interpEval p x
          Nothing -> False
  in TestCase (assertBool "Test: Interpolation property" check)

interpTestSuite :: IO ()
interpTestSuite = do
  -- create the test data
  let x  = 2.5
      nx = 21
      it = 10
      dx = 2.0 * pi / (nx - 1)
      xs = V.map (dx*) $ V.fromList [0..nx - 1]
      ys = V.map sin xs
  -- initialize the interpolant
  s <- interpInit xs ys
  -- run the tests and exit appropriately
  counts <- runTestTT $ TestList [testInit s, testInterp s (xs V.! it) (ys V.! it)]
  if failures counts > 0 
    then exitFailure
    else exitSuccess

main :: IO ()
main = interpTestSuite
