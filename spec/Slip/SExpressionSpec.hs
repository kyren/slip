module Slip.SExpressionSpec (spec) where

import Data.Either
import Test.Hspec
import Slip.SExpression

testString :: Spec
testString = it "parses strings" $ do
  readSExpression "\"foo\"" `shouldBe` Right (SString "foo")
  readSExpression "\"\"foo\"" `shouldSatisfy` isLeft

testNumber :: Spec
testNumber = it "parses numbers" $ do
  readSExpression "5" `shouldBe` Right (SNumber 5)
  readSExpression "5a" `shouldSatisfy` isLeft

testSpacedList :: Spec
testSpacedList = it "parses spaced lists" $ do
  readSExpression "(a b c \"d\")" `shouldBe` Right (SList [SSymbol "a", SSymbol "b", SSymbol "c", SString "d"])
  readSExpression "(a (b 3)) " `shouldBe` Right (SList [SSymbol "a", SList [SSymbol "b", SNumber 3]])
  readSExpression " (a (b c))) " `shouldSatisfy` isLeft

testDottedList :: Spec
testDottedList = it "parses dotted lists" $ do
  readSExpression " (a b . c ) " `shouldBe` Right (SDottedList [SSymbol "a", SSymbol "b"] (SSymbol "c"))
  readSExpression " (a . b )" `shouldBe` Right (SDottedList [SSymbol "a"] (SSymbol "b"))
  readSExpression "( a b . c d) " `shouldSatisfy` isLeft

testExpressionSequence :: Spec
testExpressionSequence = it "parses expression sequences" $ do
  readSExpressionSequence " ( a b) 5 (c . d ) " `shouldBe` Right [SList [SSymbol "a", SSymbol "b"], SNumber 5, SDottedList [SSymbol "c"] (SSymbol "d")]
  readSExpressionSequence " 5 6 7 (8)" `shouldBe` Right [SNumber 5, SNumber 6, SNumber 7, SList [SNumber 8]]

spec :: Spec
spec = describe "parsing" $ do
  testString
  testNumber
  testSpacedList
  testDottedList
  testExpressionSequence
