module HTestU.Wrapping where

import System.Random
import Data.IORef
import System.IO.Unsafe

wrapForPassing :: RandomGen g => (g -> [Int]) -> g  -> IO Int
wrapForPassing streamer gen = nextIntFromStreamRef newIntStreamGen
  where newIntStreamGen = unsafePerformIO $ newIORef $ streamer gen

nextIntFromStreamRef :: IORef [Int] -> IO Int
nextIntFromStreamRef streamRef = do
  (newRandomBits : newStream) <- readIORef streamRef
  writeIORef streamRef newStream
  return newRandomBits

intStreamFromRandomGen :: RandomGen g => g -> [Int]
intStreamFromRandomGen gen = newInt : intStreamFromRandomGen newGen
  where (newInt, newGen) = next gen

intStreamFromRandomSplitGen :: RandomGen g => g -> [Int]
intStreamFromRandomSplitGen gen = intertwineStreams (intStreamFromRandomGen leftGen) $ intStreamFromRandomGen rightGen
  where (leftGen, rightGen) = split gen

intertwineStreams :: [a] -> [a] -> [a]
intertwineStreams (x : xs) (y : ys) = x : y : intertwineStreams xs ys
