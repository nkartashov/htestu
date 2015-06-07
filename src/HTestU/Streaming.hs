module HTestU.Streaming
 (RandomStream,
 nextStreamFromGen,
 splitNextStreamFromGen) where

import System.Random (RandomGen, next, split)

type RandomStream = [Int]

nextStreamFromGen :: RandomGen g => g -> RandomStream
nextStreamFromGen gen = newInt : nextStreamFromGen newGen
  where (newInt, newGen) = next gen

splitNextStreamFromGen :: RandomGen g => g -> RandomStream
splitNextStreamFromGen gen = intertwineStreams (nextStreamFromGen leftGen) $ nextStreamFromGen rightGen
  where (leftGen, rightGen) = split gen

intertwineStreams :: [a] -> [a] -> [a]
intertwineStreams (x : xs) (y : ys) = x : y : intertwineStreams xs ys
