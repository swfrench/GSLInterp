{-# LANGUAGE ForeignFunctionInterface #-}

-- | Cubic-spline interpolation with the GSL
--
-- All @Vector@ types are @Unboxed@ @Double@s - conversion to @Storable@ @CDouble@s is performed internally.
--
-- TODO: Possible improvements include wrapping the 'InterpStructPtr' in a
-- @ForeignPtr@ and registering the 'interpFree' function as a finalizer.
-- For now, I prefer to handle the deallocation myself. In addition, leaving
-- the 'InterpStructPtr' as a plain @Ptr@ type alias keeps the evaluation
-- functions out of the @IO@ Monad (no requirement to use @withForeignPtr@)
-- or alternatively from using @unsafeForeignPtrToPtr@ excessively.
module GSLInterp
( InterpStruct (..)
, InterpStructPtr
, interpInit
, interpFree
, interpEval
, interpEvalDeriv
, interpEvalSecondDeriv
, interpEvalInteg
) where

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr hiding (unsafeForeignPtrToPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed  as V
import Control.Monad (unless)
import Text.Printf


-- | Unit-like type representing foreign interpolant structure
data InterpStruct = InterpStruct

-- | Convenient type alias for pointer to foreign interpolant structure
type InterpStructPtr = Ptr InterpStruct


-- FFI interfaces for wrapper code (C)

foreign import ccall "gsl_interp_init_wrapper" gslInterpInit
  :: Ptr CDouble
  -> Ptr CDouble
  -> CInt
  -> IO InterpStructPtr

foreign import ccall "gsl_interp_free_wrapper" gslInterpFree
  :: InterpStructPtr
  -> IO ()

foreign import ccall "gsl_interp_eval_wrapper" gslInterpEval
  :: InterpStructPtr
  -> CDouble
  -> CDouble

foreign import ccall "gsl_interp_eval_deriv_wrapper" gslInterpEvalDeriv
  :: InterpStructPtr
  -> CDouble
  -> CDouble

foreign import ccall "gsl_interp_eval_deriv2_wrapper" gslInterpEvalSecondDeriv
  :: InterpStructPtr
  -> CDouble
  -> CDouble

foreign import ccall "gsl_interp_eval_integ_wrapper" gslInterpEvalInteg
  :: InterpStructPtr
  -> CDouble
  -> CDouble
  -> CDouble

-- Helpers used below

-- Converts an Unboxed Vector of Doubles to a Storable Vector of CDoubles
toStorable
  :: V.Vector Double
  -> S.Vector CDouble
toStorable = S.map realToFrac . S.convert

-- Converts a function of type CDouble -> CDouble to one of Double -> Double
wrapDouble
  :: (CDouble -> CDouble)
  -> Double
  -> Double
wrapDouble f = realToFrac . f . realToFrac

-- Converts a function of type CDouble -> CDouble -> CDouble to one of Double -> Double -> Double
wrapDouble3
  :: (CDouble -> CDouble -> CDouble)
  -> Double
  -> Double
  -> Double
wrapDouble3 f x = wrapDouble $ f (realToFrac x)

-- | Initialization of a GSL cubic-spline interpolant
--
-- Fails on: (1) memory-allocation failure within the C-based wrapper; or (2) unequal dimension of user-supplied abscissa and ordinate vectors.
interpInit
  :: V.Vector Double              -- ^ Unboxed @Vector@ of abscissae
  -> V.Vector Double              -- ^ Unboxed @Vector@ of ordinates
  -> IO (Maybe InterpStructPtr)   -- ^ @Maybe@ pointer to the resulting interpolant structure (returns @Nothing@ on failure)
interpInit xs ys = do
  -- convert to S.Vector CDouble, extract ForeignPtrs
  let (fpx,nx) = S.unsafeToForeignPtr0 . toStorable $ xs
      (fpy,ny) = S.unsafeToForeignPtr0 . toStorable $ ys
  -- check for dimension mismatch
  if nx /= ny
    then printf "Error [interpInit]: vector dimension mismatch: %i /= %i\n" nx ny >> return Nothing
    else do
      -- initialize foreign interpolant state structure
      interp <- gslInterpInit (unsafeForeignPtrToPtr fpx) (unsafeForeignPtrToPtr fpy) (fromIntegral ny)
      -- ensure the ForeignPtrs live until _after_ call to gslInterpInit
      touchForeignPtr fpx
      touchForeignPtr fpy
      -- check for failure and return
      if interp == nullPtr
        then printf "Error [interpInit]: init returned null pointer\n" >> return Nothing
        else return . Just $ interp

-- | Free storage associated with the foreign interpolant structure
interpFree
  :: InterpStructPtr
  -> IO ()
interpFree p = unless (p == nullPtr) (gslInterpFree p)

-- | Evaluate the cubic spline interpolant associated with the supplied interpolant structure
interpEval
  :: InterpStructPtr
  -> Double
  -> Double
interpEval = wrapDouble . gslInterpEval

-- | Evaluate the first derivative of the cubic spline interpolant associated with the supplied interpolant structure
interpEvalDeriv
  :: InterpStructPtr
  -> Double
  -> Double
interpEvalDeriv = wrapDouble . gslInterpEvalDeriv

-- | Evaluate the second derivative of the cubic spline interpolant associated with the supplied interpolant structure
interpEvalSecondDeriv
  :: InterpStructPtr
  -> Double
  -> Double
interpEvalSecondDeriv = wrapDouble . gslInterpEvalSecondDeriv

-- | Evaluate the defintie integral of the cubic spline interpolant associated with the supplied interpolant structure within the specified range
interpEvalInteg
  :: InterpStructPtr
  -> Double
  -> Double
  -> Double
interpEvalInteg = wrapDouble3 . gslInterpEvalInteg
