
import GSLInterp
import Text.Printf
import qualified Data.Vector.Unboxed as V

test :: IO ()
test = do
  let x  = 2.5
  let nx = 11
  let dx = 2.0 * pi / (nx - 1)
  let xs = V.map (dx*) $ V.fromList [0..nx - 1]
  let ys = V.map sin xs
  s <- interpInit xs ys
  case s of 
    Just p  -> sequence_
      [ printf "%+e ~ %+e\n" (interpEval p x) (sin x)
      , printf "%+e ~ %+e\n" (interpEvalDeriv p x) (cos x)
      , printf "%+e ~ %+e\n" (interpEvalSecondDeriv p x) (- (sin x))
      , printf "%+e ~ %+e\n" (interpEvalInteg p x (x + 0.1)) (- (cos (x + 0.1) - cos x))
      , interpFree p]
    Nothing -> return ()

main :: IO ()
main = test
