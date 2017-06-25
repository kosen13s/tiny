module TinyParser where

import Data.Char
import Control.Monad.State
import Data.Either


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
-- a parser which parse /[1-9]/.
--
-- >>> runStateT nonZeroDigit "0"
-- Left ConditionUnsatisfied
-- >>> runStateT nonZeroDigit "9"
-- Right ("9","")
-- >>> runStateT nonZeroDigit ":"
-- Left ConditionUnsatisfied
--
nonZeroDigit :: Parser
nonZeroDigit = satisfy isNonZeroDigit where
    isNonZeroDigit c = isDigit c && c /= '0'


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
-- express 0 or more times repeatation of the specified pattern.
--
-- >>> let p = closure digit
-- >>> evalStateT p "13s"
-- Right "13"
-- >>> isLeft $ evalStateT p "kosen13s"
-- True
--
closure :: Parser -> Parser
closure p = StateT $ \s -> closure ("", s) where
    closure (a, s) =
        case runStateT p s of
          Left _ -> Right (a, s)
          Right (a', s') -> closure (a ++ a', s')

