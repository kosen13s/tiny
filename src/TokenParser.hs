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
naturalNumber = char '0' <|> do
    h <- nonZeroDigit
    t <- closure digit
    return (h ++ t)

