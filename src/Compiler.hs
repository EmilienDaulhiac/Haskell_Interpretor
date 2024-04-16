{-# LANGUAGE TemplateHaskell #-}

module Compiler where 

import Language.Haskell.TH

newtype C a = C { unC :: ExpQ }

liftC :: Q Exp -> C a
liftC q = C q

add :: C Int -> C Int -> C Int
add (C a) (C b) = C [| $(a) + $(b) |]

minus :: C Int -> C Int -> C Int
minus (C a) (C b) = C [| $(a) - $(b) |]

multiply :: C Int -> C Int -> C Int
multiply (C a) (C b) = C [| $(a) * $(b) |]

equality :: C Int -> C Int -> C Bool
equality (C a) (C b) = C [| $(a) == $(b) |]

lessthan :: C Int -> C Int -> C Bool
lessthan (C a) (C b) = C [| $(a) < $(b) |]

ifC :: C Bool -> C a -> C a -> C a
ifC (C cond) (C trueExpr) (C falseExpr) = C [| if $(cond) then $(trueExpr) else $(falseExpr) |]

pair :: C a -> C b -> C (a, b)
pair (C a) (C b) = C [| ($(a), $(b)) |]

first :: C (a, b) -> C a
first (C p) = C [| fst $(p) |]

second :: C (a, b) -> C b
second (C p) = C [| snd $(p) |]

fixC :: (C a -> C a) -> C a
fixC f = C [| fix (\x -> $(unC $ f (C [| x |]))) |]

lambdaC :: (C a -> C b) -> C (a -> b)
lambdaC f = C [| \x -> $(unC $ f (C [| x |])) |]

applyC :: C (a -> b) -> C a -> C b
applyC (C f) (C x) = C [| $(f) $(x) |]

factorial :: C Int -> C Int
factorial n = applyC fact n
  where
    fact = fixC $ \fact' ->
      lambdaC $ \n' ->
        ifC (lessthan n' (liftC [| 1 |]))
          (liftC [| 1 |])
          (multiply n' (applyC fact' (minus n' (liftC [| 1 |]))))

main :: IO ()
main = do
  let x = liftC [| 3 |]
      y = liftC [| 4 |]
      resultAdd = unC $ add x y
      resultMul = unC $ multiply x y
      resultequal =  unC $ equality x y
      resultlessThan =  unC $ lessthan x y
  putStrLn "Result of addition:"
  print =<< runQ resultAdd
  putStrLn "Result of multiplication:"
  print =<< runQ resultMul
  putStrLn "Is x equal to y?"
  print =<< runQ resultequal
  putStrLn "Is x less than y?"
  print =<< runQ resultlessThan
  let x = liftC [| 3 |]
      y = liftC [| 4 |]
      z = liftC [| "hello" |]
      pairXY = pair x y
      pairXZ = pair x z
      firstResult = unC $ first pairXY
      secondResult = unC $ second pairXZ
  putStrLn "Result of pairing x and y:"
  print =<< runQ (unC pairXY)
  putStrLn "Result of pairing x and z:"
  print =<< runQ (unC pairXZ)
  putStrLn "First element of (x, y):"
  print =<< runQ firstResult
  putStrLn "Second element of (x, z):"
  print =<< runQ secondResult
  let num = liftC [| 5 |]
      result = unC $ factorial num
  putStrLn "Result of factorial:"
  print =<< runQ result