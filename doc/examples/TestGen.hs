{-# OPTIONS_GHC -Wall #-}

module TestGen ( main ) where

import System.Random ( newStdGen )
import System.Random.TF ( newTFGen )
import System.Random.SplitMix.Gen ( newSplitMix64 )
import System.Random.PCG.Fast.Pure (save, create)

import Control.Monad ( join )
import System.Random ( RandomGen )
import Test.HTestU (TestResult, runBatteryToResults, c_smallCrush,
                    c_crush)
import Test.HTestU.Streaming (nextStreamFromGen)
import Test.HTestU.Wrapping (Battery)

-- Actions for running batteries on a newly generated instance of a PRNG

runCrush :: (RandomGen g) =>
             Battery -> IO g -> IO [TestResult]
runCrush crush gen = (\g -> runBatteryToResults nextStreamFromGen g crush ) `fmap`
                      gen

runSmallCrushStd :: IO [TestResult]
runSmallCrushStd = runCrush c_smallCrush newStdGen

runSmallCrushTF :: IO [TestResult]
runSmallCrushTF = runCrush c_smallCrush newTFGen

runSmallCrushSM :: IO [TestResult]
runSmallCrushSM = runCrush c_smallCrush newSplitMix64

runSmallCrushPCG :: IO [TestResult]
runSmallCrushPCG = runCrush c_smallCrush (join $ fmap save create)

main :: IO ()
main = do
  rStd <- runSmallCrushStd
  putStrLn "Standard"
  putStrLn $ show rStd
  rTF <- runSmallCrushTF
  putStrLn "TF"
  putStrLn $ show rTF
  rSM <- runSmallCrushSM
  putStrLn "SM"
  putStrLn $ show rSM
  rPCG <- runSmallCrushPCG
  putStrLn "PCG"
  putStrLn $ show rPCG
