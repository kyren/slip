module Slip.InterpreterSpec (spec) where

import Test.Hspec
import Slip.Interpreter
import Slip.Conversion

testExpressions :: Spec
testExpressions = it "evaluates simple expressions" $ do
  runScheme "(+ 1 2)" `shouldBe` Right (3 :: Integer)
  runScheme "(- 10 (+ 1 2))" `shouldBe` Right (7 :: Integer)
  runScheme "(> 2 3)" `shouldBe` Right False
  runScheme "(> 3 2)" `shouldBe` Right True
  runScheme "(|| #t #f)" `shouldBe` Right True
  runScheme "(&& #t #f)" `shouldBe` Right False

testConditionals :: Spec
testConditionals = it "evaluates conditional expressions" $ do
  runScheme "(if #t 10 20)" `shouldBe` Right (10 :: Integer)
  runScheme "(if #f 10 20)" `shouldBe` Right (20 :: Integer)
  runScheme "(if (> 3 2) 10 20)" `shouldBe` Right (10 :: Integer)

testEquality :: Spec
testEquality = it "eq? eqv? and equals? work correctly" $ do
  runScheme "(eq? 10 10)" `shouldBe` Right True
  runScheme "(eq? \"foo\" \"foo\")" `shouldBe` Right True
  runScheme "(eq? \"foo\" \"bar\")" `shouldBe` Right False
  runScheme "(equal? 10 10)" `shouldBe` Right True
  runScheme "(equal? 10 \"10\")" `shouldBe` Right True
  runScheme "(equal? 10 \"20\")" `shouldBe` Right False

testListOps :: Spec
testListOps = it "cons car cdr work correctly" $ do
  runScheme "(cdr '(1 2 3))" `shouldBe` Right ([2, 3] :: [Integer])
  runScheme "(car (cdr '(a b c)))" `shouldBe` Right (FrozenSchemeSymbol "b")
  runScheme "(cons 1 '(2 3))" `shouldBe` Right ([1, 2, 3] :: [Integer])
  runScheme "(cons 1 '())" `shouldBe` Right ([1] :: [Integer])

testBasicAssignment :: Spec
testBasicAssignment = it "handles basic variables and assignment correctly" $ do
  runScheme "(define x 3) x" `shouldBe` Right (3 :: Integer)
  runScheme "(define x 3) (+ x 2)" `shouldBe` Right (5 :: Integer)
  runScheme "(define x 3) (define y 4) (+ x y)" `shouldBe` Right (7 :: Integer)
  runScheme "(define x 3) (set! x 4) x" `shouldBe` Right (4 :: Integer)

testBasicFunctions :: Spec
testBasicFunctions = it "handles defining simple functions" $ do
  runScheme "(define (f x y) (+ x y)) (f 5 10)" `shouldBe` Right (15 :: Integer)
  runScheme "(define (factorial x) (if (= x 1) 1 (* x (factorial (- x 1))))) (factorial 5)" `shouldBe` Right (120 :: Integer)

testVariableBinding :: Spec
testVariableBinding = it "handles binding variables from different scopes" $ do
  runScheme "(define x 1) (define (inc) (set! x (+ x 1))) (inc) (inc) (inc) x" `shouldBe` Right (4 :: Integer)
  runScheme "(define counter ((lambda () (define i 0) (lambda () (set! i (+ i 1)))))) (counter) (counter) (counter)" `shouldBe` Right (3 :: Integer)
  runScheme "(define t 1) ((lambda () (define t 2))) t" `shouldBe` Right (1 :: Integer)
  runScheme "(define t 1) ((lambda () (set! t 2))) t" `shouldBe` Right (2 :: Integer)

spec :: Spec
spec = describe "parsing" $ do
  testExpressions
  testConditionals
  testEquality
  testListOps
  testBasicAssignment
  testBasicFunctions
  testVariableBinding
