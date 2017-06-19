import Test.Hspec
import TinyParser
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

parse1plus1 :: Parser
parse1plus1 = do
    lhs <- digit
    plus <- char '+'
    rhs <- digit
    return (lhs ++ plus ++ rhs)

