module RecreationalNumberTheory where

type Index = Integer -- Is there a good way to generalize this to e.g., Int indexes?

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

data View a
    = Generator (Index -> a)
    | Inverter (a -> Betweens Index)
    | Tester (a -> Bool)
    | Counter (a -> a -> Integer) -- Is there a good way to generalize this like genericLength?
    | Streamer (() -> [a])
    | UpStreamer (Index -> [a])
    | DownStreamer (Index -> [a])
    | UpRanger (Index -> Index -> [a])
    | DownRanger (Index -> Index -> [a])

data Sequence a
    = Sequence { kth :: (Index -> a)
               , root :: (a -> Betweens Index)
               , is :: (a -> Bool)
               , count :: (a -> a -> Integer)
               , the :: (() -> [a])
               , from :: (a -> [a])
               , downFrom :: (a -> [a])
               , fromTo :: (a -> a -> [a])
               , downFromTo :: (a -> a -> [a])
               }

define :: [View a] -> Sequence a
define = undefined

square = define [Generator (\k -> k * k)]
squares = square
