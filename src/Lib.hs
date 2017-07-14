module Lib where


-- |
-- returns a single element list that contains the argument.
--
-- >>> singletonList 1
-- [1]
--
singletonList :: a -> [a]
singletonList = take 1 . repeat

