import Control.Monad
import Control.Monad.ST
import System.Environment
import System.IO
import Slip.Core
import Slip.Conversion
import Slip.Interpreter

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

printSchemeValue :: SchemeValue s -> IO ()
printSchemeValue v =
  case fromScheme v :: Maybe FrozenSchemeValue of
    Just fv -> print fv
    Nothing -> return ()

runOne :: String -> IO ()
runOne expr = do
  res <- stToIO $ evalSchemeProgram expr
  case res of
    Left err -> putStrLn $ "Error: " ++ err
    Right v -> printSchemeValue v
  return ()

runRepl :: IO ()
runRepl = do
    env <- stToIO defaultSchemeEnvironment
    runMany env
  where
    runMany env = do
      expr <- readPrompt "Scheme>>> "
      unless (expr == "quit") $ do
        res <- stToIO $ evalSchemeStatement env expr
        case res of
          Left err -> putStrLn $ "Error: " ++ err
          Right v -> printSchemeValue v
        runMany env

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl
    [arg] -> runOne arg
    _ -> putStrLn "Program takes only 0 or 1 argument"
