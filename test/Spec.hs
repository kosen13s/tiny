import Test.Hspec
import TinyParser
import TokenParser
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


parse1plus1 :: BasicParser
parse1plus1 = do
    lhs <- digit
    plus <- char '+'
    rhs <- digit
    return (lhs ++ plus ++ rhs)

