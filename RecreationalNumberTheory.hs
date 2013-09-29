module RecreationalNumberTheory where

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

define :: [View ind a] -> Sequence ind a
define = undefined

square = define [Generator (\k -> k * k)]
squares = square
