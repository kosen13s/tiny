module SyntaxTree where


import LexicalParser
import Control.Monad.State


data Tree a =
    Empty |
    Leaf a |
    Unary a (Tree a) |
    Binary (Tree a) a (Tree a)
        deriving (Eq, Show)


type TreeState = (Tree String -> Tree String, [Token])


type LexicalTree = Tree String


leaf :: [Token] -> TreeState
leaf ((Token Literal v):xs) = (const (Leaf v), xs)


unary :: [Token] -> TreeState
unary ((Token UnaryOperator op):xs) = (const unaryTree, ys) where
    (child, ys) = unary xs
    unaryTree = Unary op (child Empty)
unary xs = leaf xs


binary :: [Token] -> TreeState
binary ((Token BinaryOperator op):xs) = (\left -> Binary left op rightTree, ys) where
    (right, ys) = unary xs
    rightTree = right Empty
binary xs = unary xs


-- |
-- modify a lexical tree from an array of tokens.
-- >>> modifyTree (Unary "-" (Leaf "2")) [(Token BinaryOperator "+"), (Token Literal "1")]
-- Binary (Unary "-" (Leaf "2")) "+" (Leaf "1")
modifyTree :: LexicalTree -> [Token] -> LexicalTree
modifyTree tree [] = tree
modifyTree tree xs = modifyTree (next tree) ys where
    (next, ys) = binary xs


-- |
-- generate a lexical tree from an array of tokens.
lexicalTree :: [Token] -> LexicalTree
lexicalTree = modifyTree Empty
