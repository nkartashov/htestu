module Test.HTestU.Streaming
 (RandomStream,
 nextStreamFromGen,
 splitNextStreamFromGen) where

import System.Random (RandomGen, next, split)
import Data.List (iterate)

type RandomStream = [Int]

nextStreamFromGen :: RandomGen g => g -> RandomStream
nextStreamFromGen gen = map fst $ iterate wrappedNext firstStreamElement
  where firstStreamElement = next gen

wrappedNext :: RandomGen g => (Int, g) -> (Int, g)
wrappedNext (_, newGen) = next newGen

splitNextStreamFromGen :: RandomGen g => g -> RandomStream
splitNextStreamFromGen gen = intertwineStreams (nextStreamFromGen leftGen) $ nextStreamFromGen rightGen
  where (leftGen, rightGen) = split gen

intertwineStreams :: [a] -> [a] -> [a]
intertwineStreams (x : xs) (y : ys) = x : y : intertwineStreams xs ys
