module TinyParser where

import Data.Char
import Control.Monad.State


data ParseFailed =
    NotEnoughLength |
    ConditionUnsatisfied
    deriving (Show, Eq)


type Parser = StateT String (Either ParseFailed) String


-- |
-- a parser which parse an any char.
--
-- >>> runStateT anyChar "\nbc"
-- Right ("\n","bc")
--
anyChar :: Parser
anyChar = StateT anyChar where
    anyChar (x:xs) = Right ([x], xs)
    anyChar xs = Left NotEnoughLength


-- |
-- generate a parser which parse an char that matches the predicate.
--
-- >>> let a2eParser = satisfy (`elem` ['a'..'e'])
-- >>> runStateT a2eParser "ef"
-- Right ("e","f")
-- >>> runStateT a2eParser "fg"
-- Left ConditionUnsatisfied
--
satisfy :: (Char -> Bool) -> Parser
satisfy pred = StateT satisfy where
    satisfy (x:xs)
      | pred x = Right ([x], xs)
      | otherwise = Left ConditionUnsatisfied
    satisfy _ = Left NotEnoughLength


-- |
-- a parser which parse /[0-9]/.
--
-- >>> runStateT digit "0"
-- Right ("0","")
-- >>> runStateT digit "/"
-- Left ConditionUnsatisfied
-- >>> runStateT digit ":"
-- Left ConditionUnsatisfied
--
digit :: Parser
digit = satisfy isDigit


-- |
-- generate a parser which parse a specified char.
--
-- >>> let aParser = char 'a'
-- >>> runStateT aParser "a"
-- Right ("a","")
-- >>> runStateT aParser "A"
-- Left ConditionUnsatisfied
--
char :: Char -> Parser
char c = satisfy (== c)


-- |
-- parse `1 + 1`.
--
-- >>> runStateT parse1plus1 "abcd"
-- Left ConditionUnsatisfied
--
-- >>> runStateT parse1plus1 "1+12"
-- Right ("1+1","2")
--
-- >>> runStateT parse1plus1 "1+"
-- Left NotEnoughLength
--
parse1plus1 :: Parser
parse1plus1 = do
    lhs <- char '1'
    plus <- char '+'
    rhs <- char '1'
    return (lhs ++ plus ++ rhs)

