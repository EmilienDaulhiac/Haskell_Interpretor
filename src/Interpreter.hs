module Interpreter where

import Test.HUnit

class ExprAlg repr where
  intLit :: Int -> repr Int
  add :: repr Int -> repr Int -> repr Int
  minus :: repr Int -> repr Int -> repr Int
  mul :: repr Int -> repr Int -> repr Int
  boolLit :: Bool -> repr Bool
  ifThenElse :: repr Bool -> repr a -> repr a -> repr a
  lessthan :: repr Int -> repr Int -> repr Bool
  equality :: repr Int -> repr Int -> repr Bool
  pair :: repr a -> repr b -> repr (a, b)
  fst' :: repr (a, b) -> repr a
  snd' :: repr (a, b) -> repr b
  lam :: (repr a -> repr b) -> repr (a -> b)
  app :: repr (a -> b) -> repr a -> repr b
  fix :: (repr a -> repr a)  -> repr a

newtype Eval a = Eval { runEval :: a }

instance ExprAlg Eval where
  intLit = Eval
  add (Eval a) (Eval b) = Eval (a + b)
  minus (Eval a) (Eval b) = Eval (a - b)
  mul (Eval a) (Eval b) = Eval (a * b)
  boolLit = Eval
  ifThenElse (Eval cond) (Eval t) (Eval f) = Eval (if cond then t else f)
  lessthan (Eval a) (Eval b) = Eval (a < b)
  equality (Eval a) (Eval b) = Eval (a == b)
  pair (Eval a) (Eval b) = Eval (a, b)
  fst' (Eval (a, _)) = Eval a
  snd' (Eval (_, b)) = Eval b
  lam f = Eval $ \x -> runEval (f (Eval x))
  app (Eval f) (Eval a) = Eval (f a)
  fix f = let Eval g = f (Eval g) in Eval g
