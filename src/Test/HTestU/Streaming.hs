module Test.HTestU.Streaming
 ( RandomStream
 , nextStreamFromGen
 , splitNextStreamFromGen
 , leftSplitStreamFromGen
 , rightSplitStreamFromGen) where

import System.Random (RandomGen, next, split)
import Data.List (iterate)
import Data.Tuple (swap)

type RandomStream = [Int]

nextStreamFromGen :: RandomGen g => g -> RandomStream
nextStreamFromGen gen = map fst $ iterate wrappedNext firstStreamElement
  where firstStreamElement = next gen

splitStreamFromGen :: RandomGen g => ((g, g) -> (g, g)) -> g -> RandomStream
splitStreamFromGen choose gen = let (old, new) = choose $ split gen
                                    (value, _) = next new
                                     in value : splitStreamFromGen choose new

goLeft = id
goRight = swap

leftSplitStreamFromGen :: RandomGen g => g -> RandomStream
leftSplitStreamFromGen = splitStreamFromGen goLeft

rightSplitStreamFromGen :: RandomGen g => g -> RandomStream
rightSplitStreamFromGen = splitStreamFromGen goRight

wrappedNext :: RandomGen g => (Int, g) -> (Int, g)
wrappedNext (_, newGen) = next newGen

splitNextStreamFromGen :: RandomGen g => g -> RandomStream
splitNextStreamFromGen gen = intertwineStreams (nextStreamFromGen leftGen) $ nextStreamFromGen rightGen
  where (leftGen, rightGen) = split gen

intertwineStreams :: [a] -> [a] -> [a]
intertwineStreams (x : xs) (y : ys) = x : y : intertwineStreams xs ys
