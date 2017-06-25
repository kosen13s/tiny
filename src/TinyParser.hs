module TinyParser where


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

