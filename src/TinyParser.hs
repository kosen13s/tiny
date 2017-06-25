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

