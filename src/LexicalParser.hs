module LexicalParser where

import TinyParser
import Control.Monad.State hiding (fail)
import Lib

-- $setup
-- >>> import Test.QuickCheck
-- >>> import Data.Either
-- >>> let parseOK = liftM isRight . runStateT


type LexicalParser = Parser Token


data Token = Token
    { mCategory :: Category
    , mValue :: String
    } | Parenthesized [Token]
        deriving (Eq, Show)


data Category =
    Literal | Identifier | Keyword | UnaryOperator | BinaryOperator
        deriving (Eq, Show)


-- |
-- generate a token parser which returns the single token with specified parameters.
-- You can use this function for binding `BasicParser` to `LexicalParser`
--
-- >>> let p = lexicalParser Literal =<< digit
-- >>> evalStateT p "3"
-- Right [Token {mCategory = Literal, mValue = "3"}]
--
lexicalParser :: Category -> String -> LexicalParser
lexicalParser category value = return [Token category value]


-- |
-- parse a parenthesized expression and **returns inner of '(' and ')'**.
--
-- >>> let mValueInP = \(Parenthesized ts) -> map mValue ts
-- >>> map mValueInP . fst . either undefined id $ runStateT expression "(1+1)"
-- [["1","+","1"]]
--
parenthesized :: LexicalParser
parenthesized = return . singletonList . Parenthesized =<< between (char '(') (char ')') expression


-- |
-- parse /0|[1-9][0-9]*/.
--
-- >>> mValue . head <$> evalStateT naturalNumber "0.1"
-- Right "0"
-- >>> mValue . head <$> evalStateT naturalNumber "1024/65536"
-- Right "1024"
-- >>> parseOK naturalNumber "N0 number"
-- False
--
naturalNumber :: LexicalParser
naturalNumber = lexicalParser Literal =<< char '0' <|> nonZeroDigit <.> closure digit


-- |
-- parse a literal.
-- TODO: may include
--      - string literals
--      - array literals
--      - dictionary literals
--      - function(block) literals
--      and more.
--
-- >>> parseOK literal "0"
-- True
-- >>> parseOK literal "-1"
-- False
-- >>> parseOK literal "(1+2*3)"
-- True
-- >>> parseOK literal "\"\""
-- False
--
literal :: LexicalParser
literal = naturalNumber <|> parenthesized


-- |
-- parse an identifier
-- TODO: implement this function.
--
identifier :: LexicalParser
identifier = TinyParser.fail


-- |
-- parse a token which can become a element of SyntaxTree/`Leaf`.
--
-- prop> liftM2 (==) (parseOK factor) (parseOK literal)
--
factor :: LexicalParser
factor = choice factors where
    factors = [literal, identifier]


-- |
-- parse a couple of unary operators and a `factor` associated with it.
--
-- >>> parseOK term "----------1"
-- True
-- >>> parseOK term "0"
-- True
--
term :: LexicalParser
term = closure prefixOperator <.> factor <.> closure postfixOperator


-- |
-- parse expressions.
--
-- >>> parseOK expression "1*(2+3)"
-- True
-- >>> parseOK expression "0"
-- True
--
expression :: LexicalParser
expression = term <.> optional (binaryOperator <.> expression)


-- |
-- parse binary operators.
-- TODO: add other operators.
--
-- >>> evalStateT binaryOperator "-"
-- Right [Token {mCategory = BinaryOperator, mValue = "-"}]
--
binaryOperator :: LexicalParser
binaryOperator = lexicalParser BinaryOperator =<< choice operatorParsers where
    operatorParsers = map string ["+", "-", "*", "/"]


-- |
-- parse prefix unary operators.
-- TODO: add other operators.
--
-- >>> evalStateT prefixOperator "-"
-- Right [Token {mCategory = UnaryOperator, mValue = "-"}]
--
prefixOperator :: LexicalParser
prefixOperator = lexicalParser UnaryOperator =<< choice operatorParsers where
    operatorParsers = [string "-"]


-- |
-- parse postfix unary operators.
-- TODO: add operators.
-- TBD: how parse non-simple operators such as call operator `(...)` and index operator `[...]`.
--
postfixOperator :: LexicalParser
postfixOperator = TinyParser.fail

