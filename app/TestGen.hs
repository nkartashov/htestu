{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import System.Random ( newStdGen, next )
import System.Random.TF ( newTFGen )
-- import qualified System.Random.PCG.Fast.Pure as PCG
import qualified System.Random.MWC as MWC
import System.Random.Mersenne.Pure64 ( newPureMT )
-- import Control.Monad ( join )
import System.Random ( RandomGen )
import Test.HTestU (TestResult, runBatteryToResults, c_smallCrush)
import Test.HTestU.Streaming (nextStreamFromGen)
import Test.HTestU.Wrapping (Battery)

import Control.Monad.Primitive
import System.IO.Unsafe


-- Actions for running batteries on a newly generated instance of a PRNG

runCrush :: (RandomGen g) =>
             Battery -> IO g -> IO [TestResult]
runCrush crush gen = (\g -> runBatteryToResults nextStreamFromGen g crush ) `fmap`
                      gen

runSmallCrushStd :: IO [TestResult]
runSmallCrushStd = runCrush c_smallCrush newStdGen

runSmallCrushTF :: IO [TestResult]
runSmallCrushTF = runCrush c_smallCrush newTFGen

-- runSmallCrushPCG :: IO [TestResult]
-- runSmallCrushPCG = runCrush c_smallCrush (join $ fmap PCG.save PCG.create)

data MWCRNG = MWCRNG (MWC.Gen (PrimState IO))

instance RandomGen MWCRNG where
  next g@(MWCRNG gen) = unsafeDupablePerformIO $
   do v <- MWC.uniform gen
      return (v, g)

runSmallCrushMWC :: IO [TestResult]
runSmallCrushMWC = do
  s <- MWC.create
  runCrush c_smallCrush (return $ MWCRNG s)

runSmallCrushMT :: IO [TestResult]
runSmallCrushMT = runCrush c_smallCrush newPureMT

main :: IO ()
main = do
  rStd <- runSmallCrushStd
  putStrLn "Standard"
  putStrLn $ show rStd
  rTF <- runSmallCrushTF
  putStrLn "TF"
  putStrLn $ show rTF
  -- rPCG <- runSmallCrushPCG
  -- putStrLn "PCG"
  -- putStrLn $ show rPCG
  rMWC <- runSmallCrushMWC
  putStrLn "MWC"
  putStrLn $ show rMWC
  rMT <- runSmallCrushMT
  putStrLn "MT"
  putStrLn $ show rMT
