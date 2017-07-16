import Test.Hspec
import TinyParser
import LexicalParser
import SyntaxTree
import Control.Monad.State


main :: IO ()
main = hspec $ do
    describe "addition for 2 single digit number" $ do
        it "successes parsing \"1+2 the rest\"." $
            runStateT parse1plus1 "1+2 the rest" `shouldBe` Right ("1+2", " the rest")
        it "fails parsing \"10+1\" because 10 is 2 digit." $
            runStateT parse1plus1 "10+1" `shouldBe` Left ConditionUnsatisfied
        it "fails parsing \"5+\" because input too short." $
            runStateT parse1plus1 "5+" `shouldBe` Left NotEnoughLength

    describe "four-arithmetical-operation" $ do
        let parse = either (const []) (map mCategory) . evalStateT expression
        it "parses addition." $
            parse "-123+456"
                `shouldBe` [UnaryOperator, Literal, BinaryOperator, Literal]
        it "parses subtraction." $
            parse "0---0"
                `shouldBe` [Literal, BinaryOperator, UnaryOperator, UnaryOperator, Literal]
        it "parses multiplication." $
            parse "2*123456789012234567890"
                `shouldBe` [Literal, BinaryOperator, Literal]
        it "parses division." $
            parse "0/-0"
                `shouldBe` [Literal, BinaryOperator, UnaryOperator, Literal]
        it "doesn't parse imcomplete binary operation." $
            parse "+20*20" `shouldBe` []

    describe "construct syntax trees" $ do
        let tree = either (const Empty) lexicalTree
        it "construct a leaf." $
            tree (Right [(Token Literal "12")])
                `shouldBe` (Leaf "12")
        it "construct an unary tree." $
            tree (Right [(Token UnaryOperator "-"), (Token Literal "1")])
                `shouldBe` (Unary "-" (Leaf "1"))
        it "optimize an unary tree." $
            tree (Right [(Token UnaryOperator "-"), (Token UnaryOperator "-"), (Token Literal "1")])
                `shouldBe` (Leaf "1")
        it "construct a binary tree." $
            tree (Right [(Token Literal "12"), (Token BinaryOperator "+"), (Token Literal "3")])
                `shouldBe` (Binary (Leaf "12") "+" (Leaf "3"))
        it "construct a binary tree includes an unary tree." $
            tree (Right [(Token Literal "12"), (Token BinaryOperator "+"), (Token UnaryOperator "-"), (Token Literal "3")])
                `shouldBe` (Binary (Leaf "12") "+" (Unary "-" (Leaf "3")))
        it "construct a binary tree includes a binary tree." $
            tree (Right [(Token Literal "12"), (Token BinaryOperator "+"), (Token Literal "4"), (Token BinaryOperator "-"), (Token Literal "3")])
                `shouldBe` (Binary (Binary (Leaf "12") "+" (Leaf "4")) "-" (Leaf "3"))
        it "construct multiple and division preferentially." $
            tree (Right [(Token Literal "10"), (Token BinaryOperator "-"), (Token Literal "2"), (Token BinaryOperator "*"), (Token Literal "5")])
                `shouldBe` (Binary (Leaf "10") "-" (Binary (Leaf "2") "*" (Leaf "5")))
        it "construct parenthesized expressions preferentially." $
            tree (Right [(Parenthesized [(Token Literal "1"), (Token BinaryOperator "-"), (Token Literal "2")]), (Token BinaryOperator "*"), (Token Literal "8")])
                `shouldBe` (Binary (Unary "()" (Binary (Leaf "1") "-" (Leaf "2"))) "*" (Leaf "8"))


parse1plus1 :: BasicParser
parse1plus1 = do
    lhs <- digit
    plus <- char '+'
    rhs <- digit
    return (lhs ++ plus ++ rhs)

