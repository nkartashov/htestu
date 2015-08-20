module TestGen where

import System.Random (RandomGen, next, split)
import Test.HTestU (runBatteryToResults, c_smallCrush, c_crush)
import Test.HTestU.Streaming (nextStreamFromGen)

-- | Pretend that this a data type of a PRNG
data TestGen

-- | This is an action to generate an instance of a PRNG
newTestGen :: IO TestGen
newTestGen = undefined

-- | Dummy instance of 'RandomGen' class
instance RandomGen TestGen where
  next = undefined
  split = undefined

-- | Action for running batteries on a newly generated instance of a PRNG
runCrush :: Battery -> IO [TestResult]
runCrush crush = (\g -> runBatteryToResults nextStreamFromGen g crush ) `fmap` newTestGen

-- | Action for running smallCrush on a newly generated instance of a PRNG
runSmallCrush :: IO [TestResult]
runSmallCrush = runCrush c_smallCrush
