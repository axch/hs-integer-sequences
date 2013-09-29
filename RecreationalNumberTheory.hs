module RecreationalNumberTheory where

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
                      , counter :: Maybe (a -> a -> Integer)
                      , streamer :: Maybe (() -> [a])
                      , upStreamer :: Maybe (ind -> [a])
                      , downStreamer :: Maybe (ind -> [a])
                      , upRanger :: Maybe (ind -> ind -> [a])
                      , downRanger :: Maybe (ind -> ind -> [a])
                      }

empty :: PartialSequence ind a
empty = PartialSequence Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data View ind a
    = Generator (ind -> a)
    | Inverter (a -> Betweens ind)
    | Tester (a -> Bool)
    | Counter (a -> a -> Integer) -- Is there a good way to generalize this like genericLength?
    | Streamer (() -> [a])
    | UpStreamer (ind -> [a])
    | DownStreamer (ind -> [a])
    | UpRanger (ind -> ind -> [a])
    | DownRanger (ind -> ind -> [a])

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
               , count :: (a -> a -> Integer)
               , the :: (() -> [a])
               , from :: (ind -> [a])
               , downFrom :: (ind -> [a])
               , fromTo :: (ind -> ind -> [a])
               , downFromTo :: (ind -> ind -> [a])
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

inverterToTester :: (a -> Betweens ind) -> a -> Bool
inverterToTester inv n = case inv n of
                           Exactly _ -> True
                           After _ -> False

-- $(transformer "inverterToTester") is expected to produce this:
-- inverterToTesterS :: PartialSequence ind a -> Maybe (PartialSequence ind a)
-- inverterToTesterS s@PartialSequence { inverter = (Just inv), tester = Nothing } =
--     Just s{ tester = Just $ inverterToTester inv }
-- inverterToTesterS _ = Nothing

transforms :: [PartialSequence ind a -> Maybe (PartialSequence ind a)]
transforms = $(listE $ map transformer ['inverterToTester])

define :: [View ind a] -> Sequence ind a
define = freeze . complete . build where
    build = foldl' addView empty
    complete s = case listToMaybe $ catMaybes $ map ($ s) transforms of
                   Nothing -> s
                   (Just s') -> complete s'

square = define [Generator (\k -> k * k)]
squares = square
