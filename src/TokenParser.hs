module TokenParser where

import TinyParser
import Control.Monad.State


-- |
-- Parse /0|[1-9][0-9]*/.
--
-- >>> runStateT naturalNumber "0abc"
-- Right ("0","abc")
-- >>> runStateT naturalNumber "1024kib"
-- Right ("1024","kib")
-- >>> runStateT naturalNumber "N0 number"
-- Left (ParseFailed [ConditionUnsatisfied,ConditionUnsatisfied])
--
naturalNumber :: Parser
naturalNumber = char '0' <|> nonZeroDigit <.> closure digit


-- |
-- Parse /-?0|[1-9][0-9]*/.
--
integer :: Parser
integer = optional (char '-') <.> naturalNumber


-- |
-- Parse integer and floating point numbers.
-- TODO: Implement about floating point numbers.
-- TBD: the kind of predefined numbers tiny uses.
--
number :: Parser
number = integer


-- |
-- Parse expressions.
-- NOTE: Now this function parses only four-arithmetical-operation.
-- TBD: the kind of expressions.
--
expression :: Parser
expression = number <.> binaryOperator <.> number


-- |
-- Parse binaryOperators.
-- TBD: the kind of predefined operators.
-- TBD: whether a programmer can define his or her own operators.
--
binaryOperator :: Parser
binaryOperator = choice $ map string operators where
    operators = ["+", "-", "*", "/"]

