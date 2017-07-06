module SyntaxTree where


import LexicalParser
import Control.Monad.State


data Tree a =
    Empty |
    Leaf a |
    Unary a (Tree a) |
    Binary (Tree a) a (Tree a)
        deriving (Eq, Show)


type TreeState a = (Tree a -> Tree a, [Token])


type LexicalTreeState = TreeState String


type LexicalTree = Tree String


leaf :: [Token] -> LexicalTreeState
leaf ((Token Literal v):xs) = (const (Leaf v), xs)


unary :: [Token] -> LexicalTreeState
unary ((Token UnaryOperator op):xs) = (const unaryTree, ys) where
    (child, ys) = unary xs
    unaryTree = Unary op (child Empty)
unary xs = leaf xs


binary :: [Token] -> LexicalTreeState
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
