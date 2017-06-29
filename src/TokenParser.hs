module TokenParser where

import TinyParser
import Control.Monad.State hiding (fail)
import Data.Either


type TokenParser = Parser Token


data Token = Token
    { mCategory :: Category
    , mValue :: String
    } deriving Show


data Category =
    Literal | Identifier | Keyword | UnaryOperator | BinaryOperator
        deriving (Eq, Show)


-- |
-- generate a token parser which returns the single token with specified parameters.
-- You can use this function for binding `BasicParser` to `tokenParser`
--
-- >>> let p = tokenParser Literal =<< digit
-- >>> evalStateT p "3"
-- Right [Token {mCategory = Literal, mValue = "3"}]
--
tokenParser :: Category -> String -> TokenParser
tokenParser category value = return [Token category value]


-- |
-- parse /0|[1-9][0-9]*/.
--
-- >>> mValue . head <$> evalStateT naturalNumber "0.1"
-- Right "0"
-- >>> mValue . head <$> evalStateT naturalNumber "1024/65536"
-- Right "1024"
-- >>> isLeft $ runStateT naturalNumber "N0 number"
-- True
--
naturalNumber :: TokenParser
naturalNumber = tokenParser Literal =<< char '0' <|> nonZeroDigit <.> closure digit


-- |
-- parse a literal.
-- TODO: may include
--      - string literals
--      - array literals
--      - dictionary literals
--      - function(block) literals
--      and more.
--
literal :: TokenParser
literal = naturalNumber


-- |
-- parse an identifier
-- TODO: implement this function.
--
identifier :: TokenParser
identifier = TinyParser.fail


factor :: TokenParser
factor = choice factors where
    factors = [literal, identifier]


term :: TokenParser
term = closure prefixOperator <.> factor <.> closure postfixOperator


-- |
-- parse expressions.
--
expression :: TokenParser
expression = term <.> optional (binaryOperator <.> expression)


-- |
-- parse binary operators.
-- TODO: add other operators.
--
binaryOperator :: TokenParser
binaryOperator = tokenParser BinaryOperator =<< choice operatorParsers where
    operatorParsers = map string ["+", "-", "*", "/"]


-- |
-- parse prefix unary operators.
-- TODO: add other operators.
--
prefixOperator :: TokenParser
prefixOperator = tokenParser UnaryOperator =<< choice operatorParsers where
    operatorParsers = [string "-"]


-- |
-- parse postfix unary operators.
-- TODO: add operators.
-- TBD: how parse non-simple operators such as call operator `(...)` and index operator `[...]`.
--
postfixOperator :: TokenParser
postfixOperator = TinyParser.fail

