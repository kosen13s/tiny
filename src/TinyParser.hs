module TinyParser where

import Data.Char
import Control.Monad.State
import Data.Either


data ParseFailed =
    ParseFailed [ParseFailed] |
    NotEnoughLength |
    ConditionUnsatisfied
    deriving (Show, Eq)


type Parser = StateT String (Either ParseFailed) String


-- |
-- a parser which does nothing.
--
-- >>> runStateT epsilon "0"
-- Right ("","0")
--
epsilon :: Parser
epsilon = return ""


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
-- generate a parser which parse a specified string.
--
-- >>> let abcdParser = string "abcd"
-- >>> runStateT abcdParser "abcde"
-- Right ("abcd","e")
-- >>> runStateT abcdParser "abdc"
-- Left ConditionUnsatisfied
--
string :: String -> Parser
string = concatnate . map char


-- |
-- express any of specified parsers.
--
-- >>> let p = choice [digit, char 'a', string "ab"]
-- >>> evalStateT p "0a"
-- Right "0"
-- >>> evalStateT p "abc"
-- Right "a"
-- >>> isLeft $ runStateT p "b"
-- True
--
choice :: [Parser] -> Parser
choice = foldl1 (<|>)


-- |
-- express that patterns appear seaquentially.
--
-- >>> let p = concatnate [digit, char '+', digit]
-- >>> evalStateT p "1+2i"
-- Right "1+2"
-- >>> isLeft $ evalStateT p "1+i"
-- True
--
concatnate :: [Parser] -> Parser
concatnate = foldl1 (<.>)


-- |
-- express 0 or more times repeatation of the specified pattern.
--
-- >>> let p = closure digit
-- >>> evalStateT p "13s"
-- Right "13"
-- >>> evalStateT p "kosen13s"
-- Right ""
--
closure :: Parser -> Parser
closure p = p <.> closure p <|> epsilon


-- |
-- express 0 or 1 times appearance of the specified pattern.
--
-- >>> let p = optional digit
-- >>> runStateT p  "0"
-- Right ("0","")
-- >>> runStateT p "a"
-- Right ("","a")
--
optional :: Parser -> Parser
optional p = p <|> epsilon


-- |
-- express the former or the latter.
--
-- >>> let p = char 'a' <|> closure (char 'a')
-- >>> evalStateT p "aaa"
-- Right "a"
--
(<|>) :: Parser -> Parser -> Parser
(StateT a) <|> (StateT b) =
    StateT $ \s -> a s <|> b s where
        Left a <|> Left b = Left $ ParseFailed [a, b]
        Left _ <|> b = b
        a <|> _ = a


-- |
-- express the latter following the former.
--
-- >>> let p = char '-' <.> digit
-- >>> evalStateT p "-12"
-- Right "-1"
--
(<.>) :: Parser -> Parser -> Parser
a <.> b = do
    p1 <- a
    p2 <- b
    return (p1 ++ p2)


infixl 6 <|>
infixl 7 <.>

