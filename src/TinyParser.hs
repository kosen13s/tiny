module TinyParser where

import Data.Char


data ParseFailed =
    NotEnoughLength |
    ConditionUnsatisfied
    deriving (Show, Eq)

type Parser = String -> Either ParseFailed (String, String)

-- |
-- a parser which parse an any char.
--
-- >>> runStateT anyChar "\nbc"
-- Right ("\n","bc")
--
anyChar :: Parser
anyChar (x:xs) = Right ([x], xs)
anyChar _ = Left NotEnoughLength


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
satisfy pred (x:xs)
  | pred x = Right ([x], xs)
  | otherwise = Left ConditionUnsatisfied

satisfy _ _ = Left NotEnoughLength


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

