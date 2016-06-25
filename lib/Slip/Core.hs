module Slip.Core
  ( SchemeValue(..)
  , SchemeEnvironment
  , SchemeExcept
  , SchemeExceptST
  , makeEnvironment
  , eval
  ) where

import Data.Maybe
import Control.Monad.Except
import Control.Monad.ST
import qualified Data.HashTable.ST.Cuckoo as HTC
import qualified Data.HashTable.Class as HT

data SchemeValue s
  = SchemeBoolean Bool
  | SchemeNumber Integer
  | SchemeString String
  | SchemeSymbol String
  | SchemeList [SchemeValue s]
  | SchemeDottedList [SchemeValue s] (SchemeValue s)
  | SchemePrimitiveFunction ([SchemeValue s] -> SchemeExcept (SchemeValue s))
  | SchemeFunction [String] (Maybe String) [SchemeValue s] (SchemeEnvironment s)

type HashTable s k v = HTC.HashTable s k v
type SchemeValueHashTable s = HashTable s String (SchemeValue s)

data SchemeEnvironment s = SchemeEnvironment
  { parent :: Maybe (SchemeEnvironment s)
  , bindings :: SchemeValueHashTable s
  }

type SchemeExcept = Except String
type SchemeExceptST s = ExceptT String (ST s)

liftExcept :: SchemeExcept a -> SchemeExceptST s a
liftExcept a =
  case runExcept a of
    Left err -> throwError err
    Right val -> return val

findBinding :: SchemeEnvironment s -> String -> ST s (Maybe (SchemeValue s, SchemeEnvironment s))
findBinding env name = do
  mv <- HT.lookup (bindings env) name
  case mv of
    Just v -> return $ Just (v, env)
    Nothing ->
      case parent env of
        Just p -> findBinding p name
        Nothing -> return Nothing

getVar :: SchemeEnvironment s -> String -> SchemeExceptST s (SchemeValue s)
getVar env name = do
  p <- lift $ findBinding env name
  case p of
    Just (v, _) -> return v
    Nothing -> throwError $ "Access to unbound variable " ++ name

setVar :: SchemeEnvironment s -> String -> SchemeValue s -> SchemeExceptST s ()
setVar env name val = do
  p <- lift $ findBinding env name
  case p of
    Just (_, e) -> lift $ HT.insert (bindings e) name val
    Nothing -> throwError $ "Setting unbound variable " ++ name

defineVar :: SchemeEnvironment s -> String -> SchemeValue s -> SchemeExceptST s ()
defineVar env name val = do
  mv <- lift $ HT.lookup (bindings env) name
  case mv of
    Just _ -> throwError $ "Defining an already bound variable " ++ name
    Nothing -> lift $ HT.insert (bindings env) name val

makeEnvironment :: Maybe (SchemeEnvironment s) -> [(String, SchemeValue s)] -> ST s (SchemeEnvironment s)
makeEnvironment p b = do
  b' <- HT.fromList b
  return $ SchemeEnvironment p b'

eval :: SchemeEnvironment s -> SchemeValue s -> SchemeExceptST s (SchemeValue s)
eval _ val@(SchemeBoolean _) = return val
eval _ val@(SchemeNumber _) = return val
eval _ val@(SchemeString _) = return val
eval env (SchemeSymbol var) = getVar env var
eval _ (SchemeList [SchemeSymbol "quote", val]) = return val
eval env (SchemeList [SchemeSymbol "if", predicate, consequent, alternate]) = do
  result <- eval env predicate
  case result of
    SchemeBoolean False -> eval env alternate
    _ -> eval env consequent
eval env (SchemeList [SchemeSymbol "set!", SchemeSymbol var, form]) = do
  r <- eval env form
  setVar env var r
  return r
eval env (SchemeList [SchemeSymbol "define", SchemeSymbol var, form]) = do
  r <- eval env form
  defineVar env var r
  return r
eval env (SchemeList (SchemeSymbol "define" : SchemeList (SchemeSymbol var : params) : body)) = do
  func <- makeNormalFunc env params body
  defineVar env var func
  return func
eval env (SchemeList (SchemeSymbol "define" : SchemeDottedList (SchemeSymbol var : params) varargs : body)) = do
  func <- makeVarArgs varargs env params body
  defineVar env var func
  return func
eval env (SchemeList (SchemeSymbol "lambda" : SchemeList params : body)) =
  makeNormalFunc env params body
eval env (SchemeList (SchemeSymbol "lambda" : SchemeDottedList params varargs : body)) =
  makeVarArgs varargs env params body
eval env (SchemeList (SchemeSymbol "lambda" : varargs@(SchemeSymbol _) : body)) =
  makeVarArgs varargs env [] body
eval env (SchemeList (func : args)) = do
  evaledFunction <- eval env func
  evaledArgs <- mapM (eval env) args
  apply evaledFunction evaledArgs
eval _ _ = throwError "Unrecognized special form"

apply :: SchemeValue s -> [SchemeValue s] -> SchemeExceptST s (SchemeValue s)
apply (SchemePrimitiveFunction func) args = liftExcept $ func args
apply (SchemeFunction params varargs body closure) args =
    if num params /= num args && isNothing varargs
       then throwError "Improper number of arguments given to function"
       else lift (makeEnvironment (Just closure) $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = last <$> mapM (eval env) body
    bindVarArgs arg env = case arg of
        Just argName -> lift $ makeEnvironment (Just env) [(argName, SchemeList remainingArgs)]
        Nothing -> return env
apply _ _ = throwError "Unrecognized special form"

makeFunc :: Maybe String -> SchemeEnvironment s -> [SchemeValue s] -> [SchemeValue s] -> SchemeExceptST s (SchemeValue s)
makeFunc varargs env params body = do
    argNames <- mapM getArg params
    return $ SchemeFunction argNames varargs body env
  where
    getArg :: SchemeValue s -> SchemeExceptST s String
    getArg (SchemeSymbol arg) = return arg
    getArg _ = throwError "Bad argument form"

makeNormalFunc :: SchemeEnvironment s -> [SchemeValue s] -> [SchemeValue s] -> SchemeExceptST s (SchemeValue s)
makeNormalFunc = makeFunc Nothing

makeVarArgs :: SchemeValue s -> SchemeEnvironment s -> [SchemeValue s] -> [SchemeValue s] -> SchemeExceptST s (SchemeValue s)
makeVarArgs (SchemeSymbol varargs) env params body = makeFunc (Just varargs) env params body
makeVarArgs _ _ _ _ = throwError "Bad argument form"
