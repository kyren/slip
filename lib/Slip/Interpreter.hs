module Slip.Interpreter
  ( readSchemeValue
  , readSchemeValueSequence
  , evalSchemeProgram
  , evalSchemeStatement
  , runScheme
  , defaultSchemeEnvironment
  ) where

import Data.List
import Control.Monad.Except
import Control.Monad.ST
import Text.Read (readMaybe)
import Slip.Core
import Slip.SExpression
import Slip.Conversion

schemeValueFromSExpression :: SExpression -> SchemeValue s
schemeValueFromSExpression (SSymbol "#t") = SchemeBoolean True
schemeValueFromSExpression (SSymbol "#f") = SchemeBoolean False
schemeValueFromSExpression (SSymbol symbol) = SchemeSymbol symbol
schemeValueFromSExpression (SString string) = SchemeString string
schemeValueFromSExpression (SNumber num) = SchemeNumber num
schemeValueFromSExpression (SList exprs) = SchemeList (map schemeValueFromSExpression exprs)
schemeValueFromSExpression (SDottedList leftExprs rightExpr) = SchemeDottedList (map schemeValueFromSExpression leftExprs) (schemeValueFromSExpression rightExpr)
schemeValueFromSExpression (SQuotedExpression expr) = SchemeList [SchemeSymbol "quote", schemeValueFromSExpression expr]

readSchemeValue :: String -> Either String (SchemeValue s)
readSchemeValue expr =
  case readSExpression expr of
    Left err -> Left err
    Right sexpr -> Right $ schemeValueFromSExpression sexpr

readSchemeValueSequence :: String -> Either String [SchemeValue s]
readSchemeValueSequence exprs =
  case readSExpressionSequence exprs of
    Left err -> Left err
    Right sexprs -> Right $ map schemeValueFromSExpression sexprs

evalSchemeProgram :: String -> ST s (Either String (SchemeValue s))
evalSchemeProgram sexprs = runExceptT $ do
  env <- lift defaultSchemeEnvironment
  case readSchemeValueSequence sexprs of
    Left err -> throwError err
    Right [] -> throwError "Cannot evaluate scheme program with no statements"
    Right stmts -> do
      res <- mapM (eval env) stmts
      return $ last res

evalSchemeStatement :: SchemeEnvironment s -> String -> ST s (Either String (SchemeValue s))
evalSchemeStatement env s = runExceptT $
  case readSchemeValue s of
    Left err -> throwError err
    Right stmt -> eval env stmt

runScheme :: FromScheme a => String -> Either String a
runScheme s = runST $ do
  r <- evalSchemeProgram s
  return $ case r of
    Left err -> Left err
    Right v ->
      case fromScheme v of
        Just cv -> Right cv
        Nothing -> Left "Cannot convert result"

defaultSchemeEnvironment :: ST s (SchemeEnvironment s)
defaultSchemeEnvironment = makeEnvironment Nothing baseEnv
  where
    baseEnv =
      [ ("+", SchemePrimitiveFunction $ numericBinOp (+))
      , ("-", SchemePrimitiveFunction $ numericBinOp (-))
      , ("*", SchemePrimitiveFunction $ numericBinOp (*))
      , ("/", SchemePrimitiveFunction $ numericBinOp div)
      , ("mod", SchemePrimitiveFunction $ numericBinOp mod)
      , ("quotient", SchemePrimitiveFunction $ numericBinOp quot)
      , ("remainder", SchemePrimitiveFunction $ numericBinOp rem)
      , ("=", SchemePrimitiveFunction $ numBoolBinop (==))
      , ("<", SchemePrimitiveFunction $ numBoolBinop (<))
      , (">", SchemePrimitiveFunction $ numBoolBinop (>))
      , ("/=", SchemePrimitiveFunction $ numBoolBinop (/=))
      , (">=", SchemePrimitiveFunction $ numBoolBinop (>=))
      , ("<=", SchemePrimitiveFunction $ numBoolBinop (<=))
      , ("&&", SchemePrimitiveFunction $ boolBoolBinop (&&))
      , ("||", SchemePrimitiveFunction $ boolBoolBinop (||))
      , ("string=?", SchemePrimitiveFunction $ strBoolBinop (==))
      , ("string<?", SchemePrimitiveFunction $ strBoolBinop (<))
      , ("string>?", SchemePrimitiveFunction $ strBoolBinop (>))
      , ("string<=?", SchemePrimitiveFunction $ strBoolBinop (<=))
      , ("string>=?", SchemePrimitiveFunction $ strBoolBinop (>=))
      , ("car", SchemePrimitiveFunction car)
      , ("cdr", SchemePrimitiveFunction cdr)
      , ("cons", SchemePrimitiveFunction cons)
      , ("eq?", SchemePrimitiveFunction eqv)
      , ("eqv?", SchemePrimitiveFunction eqv)
      , ("equal?", SchemePrimitiveFunction equal)
      ]

numericBinOp :: (Integer -> Integer -> Integer) -> [SchemeValue s] -> SchemeExcept (SchemeValue s)
numericBinOp op vals = do
  l <- mapM unpackNum vals
  case l of
    [] -> throwError "Numeric operation applied with no arguments"
    (first : rest) -> return $ SchemeNumber $ foldl' op first rest

boolBinop :: (SchemeValue s -> SchemeExcept a) -> (a -> a -> Bool) -> [SchemeValue s] -> SchemeExcept (SchemeValue s)
boolBinop unpacker op args =
  case args of
    [larg, rarg] -> do
      left <- unpacker larg
      right <- unpacker rarg
      return $ SchemeBoolean $ left `op` right
    _ -> throwError "Argument count not 2 in binary op"

numBoolBinop :: (Integer -> Integer -> Bool) -> [SchemeValue s] -> SchemeExcept (SchemeValue s)
numBoolBinop  = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [SchemeValue s] -> SchemeExcept (SchemeValue s)
strBoolBinop  = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [SchemeValue s] -> SchemeExcept (SchemeValue s)
boolBoolBinop = boolBinop unpackBool

unpackBool :: SchemeValue s -> SchemeExcept Bool
unpackBool (SchemeBoolean b) = return b
unpackBool _ = throwError "Value not convertible to Boolean"

unpackNum :: SchemeValue s -> SchemeExcept Integer
unpackNum (SchemeString str) =
  case readMaybe str of
    Just i -> return i
    Nothing -> throwError $ "Error parsing numeric value: " ++ str
unpackNum (SchemeNumber i) = return i
unpackNum (SchemeList [i]) = unpackNum i
unpackNum _ = throwError "Non-numeric value"

unpackStr :: SchemeValue s -> SchemeExcept String
unpackStr (SchemeString s) = return s
unpackStr (SchemeNumber s) = return $ show s
unpackStr (SchemeBoolean s)   = return $ show s
unpackStr _ = throwError "Value not convertible to String"

car :: [SchemeValue s] -> SchemeExcept (SchemeValue s)
car [SchemeList (x : _)] = return x
car [SchemeDottedList (x : _) _] = return x
car _ = throwError "car must be called on pair"

cdr :: [SchemeValue s] -> SchemeExcept (SchemeValue s)
cdr [SchemeList (_ : xs)] = return $ SchemeList xs
cdr [SchemeDottedList [_] x] = return x
cdr [SchemeDottedList (_ : xs) x] = return $ SchemeDottedList xs x
cdr _ = throwError "cdr must be called on pair"

cons :: [SchemeValue s] -> SchemeExcept (SchemeValue s)
cons [x1, SchemeList []] = return $ SchemeList [x1]
cons [x, SchemeList xs] = return $ SchemeList $ x : xs
cons [x, SchemeDottedList xs xlast] = return $ SchemeDottedList (x : xs) xlast
cons [x1, x2] = return $ SchemeDottedList [x1] x2
cons _ = throwError "cons must be called with 2 arguments"

areEquiv :: SchemeValue s -> SchemeValue s -> Bool
areEquiv (SchemeBoolean arg1) (SchemeBoolean arg2) = arg1 == arg2
areEquiv (SchemeNumber arg1) (SchemeNumber arg2) = arg1 == arg2
areEquiv (SchemeString arg1) (SchemeString arg2) = arg1 == arg2
areEquiv (SchemeSymbol arg1) (SchemeSymbol arg2) = arg1 == arg2
areEquiv (SchemeDottedList xs x) (SchemeDottedList ys y) = areEquiv (SchemeList $ xs ++ [x]) (SchemeList $ ys ++ [y])
areEquiv (SchemeList arg1) (SchemeList arg2) = (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
  where eqvPair (x1, x2) = areEquiv x1 x2
areEquiv _ _ = False

eqv :: [SchemeValue s] -> SchemeExcept (SchemeValue s)
eqv [a, b] = return $ SchemeBoolean $ areEquiv a b
eqv _ = throwError "eqv must be called with 2 arguments"

unpackEquals :: (Eq a) => SchemeValue s -> SchemeValue s -> (SchemeValue s -> SchemeExcept a) -> SchemeExcept Bool
unpackEquals a b unpacker = do
    r <- res
    case r of
      Left _ -> return False
      Right val -> return val
  where
    res = lift $ runExceptT $ do
      ua <- unpacker a
      ub <- unpacker b
      return $ ua == ub

equal :: [SchemeValue s] -> SchemeExcept (SchemeValue s)
equal [a, b] = do
  let e1 = areEquiv a b
  e2 <- unpackEquals a b unpackBool
  e3 <- unpackEquals a b unpackNum
  e4 <- unpackEquals a b unpackStr
  return $ SchemeBoolean $ e1 || e2 || e3 || e4
equal _ = throwError "equal must be called with 2 arguments"
