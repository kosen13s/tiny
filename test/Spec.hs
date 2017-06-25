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
        it "parses addition." $
            evalStateT expression "-123+456" `shouldBe` Right "-123+456"
        it "parses subtraction." $
            evalStateT expression "0--0" `shouldBe` Right "0--0"
        it "parses multiplication." $
            evalStateT expression "2*12345678901234567890" `shouldBe` Right "2*12345678901234567890"
        it "parses division." $
            evalStateT expression "0/-0" `shouldBe` Right "0/-0"

parse1plus1 :: Parser
parse1plus1 = do
    lhs <- digit
    plus <- char '+'
    rhs <- digit
    return (lhs ++ plus ++ rhs)

