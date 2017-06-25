module TinyParser where


data ParseFailed =
    NotEnoughLength |
    ConditionUnsatisfied
    deriving (Show, Eq)

type Parser = String -> Either ParseFailed (String, String)

