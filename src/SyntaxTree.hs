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


type TreeState a = (Tree a -> Tree a, [Token])


type LexicalTreeState = TreeState String


priority :: String -> Int
priority "+" = 1
priority "-" = 1
priority "*" = 2
priority "/" = 2


leaf :: [Token] -> LexicalTreeState
leaf ((Token Literal v):xs) = (const (Leaf v), xs)


unary :: [Token] -> LexicalTreeState
unary (x@(Token UnaryOperator op):y:xs) = (const unaryTree, ys) where
    isAnnihilated = x == y
    (child, ys) = construct (if isAnnihilated then xs else y:xs)
    childTree = child Empty
    unaryTree = if isAnnihilated then childTree else Unary op childTree


binary :: [Token] -> LexicalTreeState
binary ((Token BinaryOperator op):xs) = (\left -> roll (Binary left op rightTree), ys) where
    (right, ys) = construct xs
    rightTree = right Empty


construct :: [Token] -> LexicalTreeState
construct xs@((Token Literal _):_) = leaf xs
construct xs@((Token UnaryOperator _):_) = unary xs
construct xs@((Token BinaryOperator _):_) = binary xs


roll :: LexicalTree -> LexicalTree
roll tree@(Binary (Binary lleft lop lright) pop pright)
    | priority lop < priority pop = Binary lleft lop (Binary lright pop pright)
    | otherwise = tree
roll tree = tree


-- |
-- modify a lexical tree from an array of tokens.
-- >>> modifyTree (Unary "-" (Leaf "2")) [(Token BinaryOperator "+"), (Token Literal "1")]
-- Binary (Unary "-" (Leaf "2")) "+" (Leaf "1")
modifyTree :: LexicalTree -> [Token] -> LexicalTree
modifyTree tree [] = tree
modifyTree tree xs = modifyTree (next tree) ys where
    (next, ys) = construct xs


-- |
-- generate a lexical tree from an array of tokens.
lexicalTree :: [Token] -> LexicalTree
lexicalTree = modifyTree Empty
