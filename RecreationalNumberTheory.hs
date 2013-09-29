module RecreationalNumberTheory where

import Data.List
import Data.Maybe

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

data ViewType = GeneratorT | InverterT | TesterT | CounterT
              | StreamerT | UpStreamerT | DownStreamerT | UpRangerT | DownRangerT
  deriving (Eq, Show)

viewType :: View ind a -> ViewType
viewType (Generator _) = GeneratorT
viewType (Inverter _) = InverterT
viewType (Tester _) = TesterT
viewType (Counter _) = CounterT
viewType (Streamer _) = StreamerT
viewType (UpStreamer _) = UpStreamerT
viewType (DownStreamer _) = DownStreamerT
viewType (UpRanger _) = UpRangerT
viewType (DownRanger _) = DownRangerT

data Sequence ind a
    = Sequence { kth :: (ind -> a)
               , root :: (a -> Betweens ind)
               , is :: (a -> Bool)
               , count :: (a -> a -> Integer)
               , the :: (() -> [a])
               , from :: (a -> [a])
               , downFrom :: (a -> [a])
               , fromTo :: (a -> a -> [a])
               , downFromTo :: (a -> a -> [a])
               }

makeSequence :: [View ind a] -> Sequence ind a
makeSequence views = Sequence { kth = fromJust $ find (\v -> viewType v == GeneratorT) views
                              , root = fromJust $ find (\v -> viewType v == InverterT) views
                              , is = fromJust $ find (\v -> viewType v == TesterT) views
                              , count = fromJust $ find (\v -> viewType v == CounterT) views
                              , the = fromJust $ find (\v -> viewType v == StreamerT) views
                              , from = fromJust $ find (\v -> viewType v == UpStreamerT) views
                              , downFrom = fromJust $ find (\v -> viewType v == DownStreamerT) views
                              , fromTo = fromJust $ find (\v -> viewType v == UpRangerT) views
                              , downFromTo = fromJust $ find (\v -> viewType v == DownRangerT) views
                              }

transforms :: [View ind a -> Maybe (View ind a)]
transforms = []

define :: [View ind a] -> Sequence ind a
define views = makeSequence $ loop views transforms where
    loop views [] = views
    loop views (t:ts) = case attempt t views of
                          (Just new) -> loop (new:views) transforms
                          Nothing -> loop views ts
    attempt t views = listToMaybe $ filter (`notThere` views) $ catMaybes $ map t views
    notThere view views = viewType view `notElem` (map viewType views)

square = define [Generator (\k -> k * k)]
squares = square
