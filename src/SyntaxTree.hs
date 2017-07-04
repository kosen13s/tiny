module SyntaxTree where


import LexicalParser
import Control.Monad.State


data Tree a =
    Empty |
    Leaf a |
    Unary a (Tree a) |
    Binary (Tree a) a (Tree a)
        deriving (Eq, Show)


type LexicalTree = Tree String


-- |
-- modify a lexical tree from an array of tokens.
-- >>> modifyTree (Unary "-" (Leaf "2")) [(Token BinaryOperator "+"), (Token Literal "1")]
-- Binary (Unary "-" (Leaf "2")) "+" (Leaf "1")
modifyTree :: LexicalTree -> [Token] -> LexicalTree
modifyTree tree [] = tree
modifyTree _ ((Token Literal v):xs) = modifyTree (Leaf v) xs
modifyTree _ ((Token UnaryOperator op):x:xs) = modifyTree (Unary op (lexicalTree [x])) xs
modifyTree left ((Token BinaryOperator op):xs) = Binary left op (lexicalTree xs)


-- |
-- generate a lexical tree from an array of tokens.
-- >>> let p = lexicalTree <$> expression
-- >>> evalStateT p "1+2"
-- Right (Binary (Leaf "1") "+" (Leaf "2"))
-- >>> evalStateT p "-1+2"
-- Right (Binary (Unary "-" (Leaf "1")) "+" (Leaf "2"))
-- >>> evalStateT p "1+-2"
-- Right (Binary (Leaf "1") "+" (Unary "-" (Leaf "2")))
-- >>> evalStateT p "--3"
-- Right (Leaf "3")
lexicalTree :: [Token] -> LexicalTree
lexicalTree = modifyTree Empty
