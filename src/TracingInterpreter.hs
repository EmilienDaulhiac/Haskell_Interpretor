{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module TracingInterpreter where 

class ExprAlgTrace repr where
  intLitTrace :: Int -> repr Int
  addTrace :: repr Int -> repr Int -> repr Int
  minusTrace :: repr Int -> repr Int -> repr Int
  mulTrace :: repr Int -> repr Int -> repr Int
  boolLitTrace :: Bool -> repr Bool
  ifThenElseTrace :: repr Bool -> repr a -> repr a -> repr a
  lessthanTrace :: repr Int -> repr Int -> repr Bool
  equalityTrace :: repr Int -> repr Int -> repr Bool
  pairTrace :: (Show a, Show b) => repr a -> repr b -> repr (a, b)
  fstTrace' :: (Show a, Show b) => repr (a, b) -> repr a
  sndTrace' :: (Show a, Show b) => repr (a, b) -> repr b
  lambdaTrace :: (Show a, Show b) => (repr a -> repr b) -> repr (a -> b)
  applyTrace :: (Show a, Show b) => repr (a -> b) -> repr a -> repr b
  fixTrace :: (Show a) => repr (a -> a) -> repr a

newtype Trace a = Trace { runTrace :: (String, a) }

instance Functor Trace where
  fmap f (Trace (s, x)) = Trace (s, f x)

instance Applicative Trace where
  pure x = Trace ("", x)
  (Trace (s1, f)) <*> (Trace (s2, x)) = Trace (s1 ++ s2, f x)

instance Monad Trace where
  return = pure
  (Trace (s1, x)) >>= f =
    let (Trace (s2, y)) = f x
    in Trace (s1 ++ s2, y)

instance ExprAlgTrace Trace where
  intLitTrace n = Trace ("intLit " ++ show n ++ "\n", n)
  addTrace e1 e2 = do
    n1 <- e1
    n2 <- e2
    Trace ("add (" ++ show n1 ++ ") (" ++ show n2 ++ ")\n", n1 + n2)
  minusTrace e1 e2 = do
    n1 <- e1
    n2 <- e2
    Trace ("minus (" ++ show n1 ++ ") (" ++ show n2 ++ ")\n", n1 - n2)
  mulTrace e1 e2 = do
    n1 <- e1
    n2 <- e2
    Trace ("mul (" ++ show n1 ++ ") (" ++ show n2 ++ ")\n", n1 * n2)
  boolLitTrace b = Trace ("boolLit " ++ show b ++ "\n", b)
  ifThenElseTrace cond e1 e2 = do
    c <- cond
    if c then e1 else e2
  lessthanTrace e1 e2 = do
    n1 <- e1
    n2 <- e2
    Trace ("lessthan (" ++ show n1 ++ ") (" ++ show n2 ++ ")\n", n1 < n2)
  equalityTrace e1 e2 = do
    n1 <- e1
    n2 <- e2
    Trace ("equality (" ++ show n1 ++ ") (" ++ show n2 ++ ")\n", n1 == n2)
  pairTrace e1 e2 = do
    v1 <- e1
    v2 <- e2
    Trace ("pair (" ++ show v1 ++ ") (" ++ show v2 ++ ")\n", (v1, v2))
  fstTrace' e = do
    p <- e
    Trace ("fst' (" ++ show p ++ ")\n", fst p)
  sndTrace' e = do
    p <- e
    Trace ("snd' (" ++ show p ++ ")\n", snd p)
  lambdaTrace f = Trace ("lambda\n", \x -> let (_, res) = runTrace (f (pure x)) in res)
  applyTrace fun arg = do
    f <- fun
    a <- arg
    Trace ("apply\n", f a)
  fixTrace f = do
    func <- f
    Trace ("fix\n", fixTraceHelper func)
      where
        fixTraceHelper g = let (_, res) = runTrace (fmap g (pure (fixTraceHelper g))) in res

-- Example expression: if 3 < 4 then (5 + 6) else (7 * 8)
exampleExpr :: ExprAlgTrace repr => repr Int
exampleExpr = ifThenElseTrace (lessthanTrace (intLitTrace 3) (intLitTrace 4))
                          (addTrace (intLitTrace 5) (intLitTrace 6))
                          (mulTrace (intLitTrace 7) (intLitTrace 8))
runTracing :: (String, Int)
runTracing = runTrace exampleExpr

examplepairTraceExpr :: ExprAlgTrace repr => repr Int
examplepairTraceExpr = fstTrace' (pairTrace (minusTrace (intLitTrace 3) (intLitTrace 3)) (intLitTrace 4))
runTracingpairTrace :: (String, Int)
runTracingpairTrace = runTrace examplepairTraceExpr


examplelambdaTraceExpr :: ExprAlgTrace repr => repr Int
examplelambdaTraceExpr = applyTrace (lambdaTrace (\x -> addTrace x x)) (intLitTrace 5)
runTracinglambdaTrace :: (String, Int)
runTracinglambdaTrace = runTrace examplelambdaTraceExpr

main :: IO ()
main = do
  let (trace, result) = runTracing
  putStrLn $ "Trace:\n" ++ trace
  putStrLn $ "Result: " ++ show result

  let (tracepairTrace, resultpairTrace) = runTracingpairTrace
  putStrLn $ "Trace (examplepairTraceExpr):\n" ++ tracepairTrace
  putStrLn $ "Result (examplepairTraceExpr): " ++ show resultpairTrace

  let (tracelambdaTrace, resultlambdaTrace) = runTracinglambdaTrace
  putStrLn $ "Trace (examplelambdaTraceExpr):\n" ++ tracelambdaTrace
  putStrLn $ "Result (examplelambdaTraceExpr): " ++ show resultlambdaTrace