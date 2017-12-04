module SyntaxTree where


import LexicalParser
import Control.Monad.State


data Tree a =
    Empty |
    LiteralLeaf a |
    IdentifierLeaf a |
    Unary a (Tree a) |
    Binary (Tree a) a (Tree a)
        deriving (Eq, Show)


type LexicalTree = Tree String


type TreeState a = (Tree a -> Tree a, [Token])


type LexicalTreeState = TreeState String


-- |
-- returns given operator's priority
--
priority :: String -> Int
priority "+" = 1
priority "-" = 1
priority "*" = 2
priority "/" = 2


-- |
-- generate a leaf with state from the head of a token array
--
-- >>> let (f, _) = leaf [Token Literal "1"]
-- >>> f Empty
-- LiteralLeaf "1"
--
leaf :: [Token] -> LexicalTreeState
leaf ((Token Literal v):xs) = (const (LiteralLeaf v), xs)
leaf ((Token Identifier v):xs) = (const (IdentifierLeaf v), xs)


-- |
-- generate an unary tree with state from a token array
--
-- >>> let (f, _) = unary [Token UnaryOperator "-", Token Literal "1"]
-- >>> f Empty
-- Unary "-" (LiteralLeaf "1")
--
unary :: [Token] -> LexicalTreeState
unary (x@(Token UnaryOperator op):y:xs) = (const unaryTree, ys) where
    isAnnihilated = x == y
    (child, ys) = construct (if isAnnihilated then xs else y:xs)
    childTree = child Empty
    unaryTree = if isAnnihilated then childTree else Unary op childTree


-- |
-- generate a binary tree with state from a token array
--
-- >>> let (f,_) = binary [Token BinaryOperator "+", Token UnaryOperator "-", Token Literal "1"]
-- >>> f (LiteralLeaf "1")
-- Binary (LiteralLeaf "1") "+" (Unary "-" (LiteralLeaf "1"))
--
binary :: [Token] -> LexicalTreeState
binary ((Token BinaryOperator op):xs) = (\left -> roll (Binary left op rightTree), ys) where
    (right, ys) = construct xs
    rightTree = right Empty


-- |
-- construct a syntax tree with state from a token array
--
-- >>> let (f, _) = construct [Parenthesized [Token Literal "1"]]
-- >>> f Empty
-- Unary "()" (LiteralLeaf "1")
--
construct :: [Token] -> LexicalTreeState
construct xs@((Token Literal _):_) = leaf xs
construct xs@((Token UnaryOperator _):_) = unary xs
construct xs@((Token BinaryOperator _):_) = binary xs
construct ((Parenthesized xs):ys) = (const (Unary "()" (lexicalTree xs)), ys)


-- |
-- roll a binary tree in accordance with priorities of operators
--
-- >>> roll (Binary (Binary (LiteralLeaf "3") "+" (LiteralLeaf "2")) "*" (LiteralLeaf "4"))
-- Binary (LiteralLeaf "3") "+" (Binary (LiteralLeaf "2") "*" (LiteralLeaf "4"))
--
roll :: LexicalTree -> LexicalTree
roll tree@(Binary (Binary lleft lop lright) pop pright)
    | priority lop < priority pop = Binary lleft lop (Binary lright pop pright)
    | otherwise = tree
roll tree = tree


-- |
-- modify a lexical tree from an array of tokens.
--
-- >>> modifyTree (Unary "-" (LiteralLeaf "2")) [(Token BinaryOperator "+"), (Token Literal "1")]
-- Binary (Unary "-" (LiteralLeaf "2")) "+" (LiteralLeaf "1")
--
modifyTree :: LexicalTree -> [Token] -> LexicalTree
modifyTree tree [] = tree
modifyTree tree xs = modifyTree (next tree) ys where
    (next, ys) = construct xs


-- |
-- generate a lexical tree from an array of tokens.
--
-- >>> lexicalTree [(Token Literal "18"), (Token BinaryOperator "*"), (Token Literal "4")]
-- Binary (LiteralLeaf "18") "*" (LiteralLeaf "4")
--
lexicalTree :: [Token] -> LexicalTree
lexicalTree = modifyTree Empty
