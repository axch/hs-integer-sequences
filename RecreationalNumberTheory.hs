module RecreationalNumberTheory where

import Prelude hiding (ceiling)
import Data.List
import Data.Maybe
import Language.Haskell.TH (listE)

import NumberTheoryMeta

-- The presumption is that a is an ordered and enumerable type, whose
-- enumeration is compatible with its ordering.  Betweens a extends a
-- with elements representing the spaces in the ordering of a between
-- successive elements of a: After x is "between" x and (succ x).
data Betweens a = Exactly a
                | After a
  deriving (Eq, Show)

instance (Ord a) => Ord (Betweens a) where
    (Exactly x) `compare` (Exactly y) = x `compare` y
    (Exactly x) `compare` (After y) | x == y = LT
                                    | otherwise = x `compare` y
    (After x) `compare` (Exactly y) | x == y = GT -- Is there no function to reverse the sense of a Ordering?
                                    | otherwise = x `compare` y
    (After x) `compare` (After y) = x `compare` y

instance (Enum a) => (Enum (Betweens a)) where
    toEnum k | k `mod` 2 == 0 = Exactly $ toEnum $ k `div` 2
             | otherwise = After $ toEnum $ k `div` 2
    fromEnum (Exactly x) = 2 * fromEnum x
    fromEnum (After x) = 2 * fromEnum x + 1

ceiling :: (Enum a) => Betweens a -> a
ceiling (Exactly x) = x
ceiling (After x) = succ x

-- generator
-- inverter
-- tester
-- counter
-- streamer
-- upStreamer
-- downStreamer
-- upRanger
-- downRanger

data PartialSequence ind a
    = PartialSequence { generator :: Maybe (ind -> a)
                      , inverter :: Maybe (a -> Betweens ind)
                      , tester :: Maybe (a -> Bool)
                      , counter :: Maybe (a -> a -> ind)
                      , streamer :: Maybe (() -> [a])
                      , upStreamer :: Maybe (a -> [a])
                      , downStreamer :: Maybe (a -> [a])
                      , upRanger :: Maybe (a -> a -> [a])
                      , downRanger :: Maybe (a -> a -> [a])
                      }

empty :: PartialSequence ind a
empty = PartialSequence Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data View ind a
    = Generator (ind -> a)
    | Inverter (a -> Betweens ind)
    | Tester (a -> Bool)
    | Counter (a -> a -> ind)
    | Streamer (() -> [a])
    | UpStreamer (a -> [a])
    | DownStreamer (a -> [a])
    | UpRanger (a -> a -> [a])
    | DownRanger (a -> a -> [a])

addView :: PartialSequence ind a -> View ind a -> PartialSequence ind a
addView s (Generator f) = s{generator = Just f}
addView s (Inverter f) = s{inverter = Just f}
addView s (Tester f) = s{tester = Just f}
addView s (Counter f) = s{counter = Just f}
addView s (Streamer f) = s{streamer = Just f}
addView s (UpStreamer f) = s{upStreamer = Just f}
addView s (DownStreamer f) = s{downStreamer = Just f}
addView s (UpRanger f) = s{upRanger = Just f}
addView s (DownRanger f) = s{downRanger = Just f}

data Sequence ind a
    = Sequence { kth :: (ind -> a)
               , root :: (a -> Betweens ind)
               , is :: (a -> Bool)
               , count :: (a -> a -> ind)
               , the :: (() -> [a])
               , from :: (a -> [a])
               , downFrom :: (a -> [a])
               , fromTo :: (a -> a -> [a])
               , downFromTo :: (a -> a -> [a])
               }

freeze :: PartialSequence ind a -> Sequence ind a
freeze PartialSequence { generator = Just kth
                       , inverter = Just root
                       , tester = Just is
                       , counter = Just count
                       , streamer = Just the
                       , upStreamer = Just from
                       , downStreamer = Just downFrom
                       , upRanger = Just fromTo
                       , downRanger = Just downFromTo
                       }
    = Sequence { kth = kth
               , root = root
               , is = is
               , count = count
               , the = the
               , from = from
               , downFrom = downFrom
               , fromTo = fromTo
               , downFromTo = downFromTo
               }
freeze _ = error "Freezing incomplete sequence definition"

invertByBinarySearch :: (Integral ind, Ord a) => (ind -> a) -> (a -> Betweens ind)
invertByBinarySearch f n = up 0 where
    up i = if n <= (f next) then
               search i next
           else
               up next
        where next = 2 * i + 1
    search low high = if low == mid then
                          if n == f high then
                              Exactly high
                          else
                              After low
                      else
                          if n <= f mid then
                              search low mid
                          else
                              search mid high
        where mid = (low + high) `div` 2

-- The Single Steps

generatorToInverter :: (Integral ind, Ord a) => (ind -> a) -> (a -> Betweens ind)
generatorToInverter = invertByBinarySearch

inverterToGenerator :: (Ord ind, Integral a) => (a -> Betweens ind) -> (ind -> a)
 -- In this use case, result should always be Exactly, so ceiling is OK.
inverterToGenerator inv = ceiling . invertByBinarySearch inv . Exactly

inverterToTester :: (a -> Betweens ind) -> a -> Bool
inverterToTester inv n = case inv n of
                           Exactly _ -> True
                           After _ -> False
testerToUpRanger t low high = filter t [low..high]
testerToDownRanger t low high = filter t [high,(high-1)..low]
testerToUpStreamer t low = filter t [low..]
downRangerToDownStreamer r high = r 0 high
upStreamerToStreamer u () = u 1
streamerToGenerator s k = (s ()) `genericIndex` k
generatorToStreamer g () = map g [1..]
streamerToUpStreamer s low = dropWhile (< low) (s ())
upStreamerToUpRanger u low high = takeWhile (< high) (u low)
upRangerToUpStreamer u low = concatMap interval $ iterate next low
    where next n = n+1 `max` 2*n
          interval n = u n (next n)
upRangerToDownRanger u low high = reverse $ u (low + 1) (high + 1) -- Add 1 to fix inclusivity
upRangerToTester u n = not $ null $ u n (n + 1)
downStreamerToDownRanger d low high = takeWhile (> low) (d high)
downRangerToUpRanger d low high = reverse $ d (low - 1) (high - 1) -- Subtract 1 to fix inclusivity
downRangerToTester d n = not $ null $ d (n - 1) n

-- TODO The three binary steps from numbers-meta.scm

upRangerToCounter u low high = genericLength $ u low high
downRangerToCounter d low high = genericLength $ d (low - 1) (high - 1) -- Subtract 1 to fix inclusivity
inverterToCounter :: (Enum ind, Num ind) => (a -> Betweens ind) -> a -> a -> ind
inverterToCounter inv low high = ceiling (inv high) - ceiling (inv low)
counterToTester c n = c n (n+1) > 0
counterToInverter c n = if nCount == 0 then
                            After (c 0 n)
                        else
                            Exactly (c 0 n + 1)
    where nCount = c n (n+1)

-- $(transformer 'inverterToTester) is expected to produce this (as a
-- lambda expression, and without the type declaration):
-- inverterToTesterS :: PartialSequence ind a -> Maybe (PartialSequence ind a)
-- inverterToTesterS s@PartialSequence { inverter = (Just inv), tester = Nothing } =
--     Just s{ tester = Just $ inverterToTester inv }
-- inverterToTesterS _ = Nothing

transforms :: (Integral ind, Integral a) => [PartialSequence ind a -> Maybe (PartialSequence ind a)]
transforms = $(listE $ map transformer
  [ 'inverterToTester
  , 'inverterToCounter
  , 'counterToTester
  , 'counterToInverter
  , 'generatorToStreamer
  , 'downRangerToDownStreamer
  , 'upStreamerToStreamer
  , 'upStreamerToUpRanger
  , 'downStreamerToDownRanger
  , 'upRangerToTester
  , 'downRangerToTester
  , 'upRangerToCounter
  , 'downRangerToCounter
  -- TODO Binary transforms go here?
  , 'generatorToInverter
  -- TODO Order of these five?
  , 'streamerToUpStreamer
  , 'testerToUpRanger
  , 'testerToDownRanger
  , 'testerToUpStreamer
  , 'upRangerToUpStreamer

  , 'upRangerToDownRanger
  , 'downRangerToUpRanger
  , 'streamerToGenerator
  , 'inverterToGenerator -- Forces (Integral a) rather than (Num a, Ord a, Enum a)
  ])

define :: (Integral ind, Integral a) => [View ind a] -> Sequence ind a
define = freeze . complete . build where
    build = foldl' addView empty
    complete s = case listToMaybe $ catMaybes $ map ($ s) transforms of
                   Nothing -> s
                   (Just s') -> complete s'

square = define [Generator (\k -> k * k)]
squares = square

-- TODO Define sequences
-- TODO Test thoroughly
-- TODO Figure out how to get high-quality Core from this
