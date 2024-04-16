{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PrettyPrintInterpreter where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO   
import Interpreter (ExprAlg(..))

newtype Eval a = Eval { runEval :: a }

-- Define a newtype wrapper for representing programs as Text
newtype HaskellText a = HaskellText { runHaskellText :: T.Text }

-- Interpret the EDSL to compute a valid Haskell representation
instance ExprAlg HaskellText where
    boolLit b = HaskellText $ if b then T.pack "True" else T.pack "False"
    ifThenElse (HaskellText c) (HaskellText t) (HaskellText e) = HaskellText $ T.concat [T.pack "if ", c, T.pack "\n then ", t, T.pack "\n else ", e]
    intLit n = HaskellText $ T.pack $ show n
    pair (HaskellText a) (HaskellText b) = HaskellText $ T.concat [T.pack "(", a, T.pack ", ", b,T.pack ")"]
    fst' (HaskellText p) = HaskellText $ T.concat [T.pack "fst ", p]
    snd' (HaskellText p) = HaskellText $ T.concat [T.pack "snd ", p]
    add (HaskellText t1) (HaskellText t2) = HaskellText $ T.concat [T.pack "(", t1, T.pack " + ", t2, T.pack ")"]
    minus (HaskellText t1) (HaskellText t2) = HaskellText $ T.concat [T.pack "(", t1, T.pack " - ", t2, T.pack ")"]
    mul (HaskellText t1) (HaskellText t2) = HaskellText $ T.concat [T.pack "(", t1, T.pack " x ", t2, T.pack ")"]
    lam f = HaskellText $ T.concat [
      T.pack "lambda \\x ->\n{\n", 
      T.concat $ map (\line -> T.pack "  " `T.append` line `T.append` T.pack "\n") $ T.lines $ runHaskellText $ f (HaskellText $ T.pack "x"),
      T.pack "}"]
    app (HaskellText t1) (HaskellText t2) = HaskellText $ T.concat [T.pack "(", t1, T.pack " ", t2, T.pack ")"]
    lessthan (HaskellText t1) (HaskellText t2) = HaskellText $ T.concat [T.pack "(", t1, T.pack " < ", t2,T.pack ")"]
    equality (HaskellText t1) (HaskellText t2) = HaskellText $ T.concat [T.pack "(", t1, T.pack " == ", t2, T.pack ")"]
    fix f = HaskellText $ T.concat [
      T.pack "fix {\\f -> \n",
      T.concat $ map (\line -> T.pack "  " `T.append` line `T.append` T.pack "\n") $ T.lines $ runHaskellText $ f (HaskellText $ T.pack "f"),
      T.pack "}\n"]

runHaskellTextMultiline :: HaskellText a -> IO ()
runHaskellTextMultiline (HaskellText txt) = TIO.putStrLn txt

exampleExpr2 :: ExprAlg repr => repr Int
exampleExpr2 = ifThenElse (lessthan (intLit 3) (intLit 2)) (intLit 3) (intLit 2)
test2example :: IO ()
test2example = runHaskellTextMultiline exampleExpr2

-- Example usage
example :: HaskellText (Int -> Int)
example = fix $ \f -> lam $ \x ->
    ifThenElse (lessthan x (intLit 2))
        (intLit 1)
        (add (app f (minus x (intLit 1))) (app f (minus x (intLit 2))))


testexample :: IO ()
testexample = runHaskellTextMultiline example

-- Test case
main :: IO ()
main = do
  putStrLn "Pretty print version of if (3 < 2) then 3 else 2 :"
  test2example
  putStrLn "Pretty print version of f(x) -> if x < 2 then 1 else f(x -1) + f(x -2) :"
  testexample



