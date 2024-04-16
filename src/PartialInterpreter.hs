module PartialInterpreter where

import Control.Applicative (liftA2)

type Env = [(String,  PevalExpr)]

data  PevalExpr
  = ConstPart Int
  | BoolConstPart Bool
  | VarPart String
  | PrimPart Op [ PevalExpr]
  | IfPart  PevalExpr  PevalExpr  PevalExpr
  | PairPart  PevalExpr  PevalExpr
  | FirstPart  PevalExpr
  | SecondPart  PevalExpr
  | LambdaPart [String]  PevalExpr
  | ApplyPart  PevalExpr [ PevalExpr]
  | FixPart  PevalExpr
  deriving Show

data Op = AddPart | SubtractPart | MultiplyPart | DividePart | LessThanPart | EqualPart deriving (Eq, Show)

peval :: Prog -> Maybe PevalExpr
peval (fdefs, mainExpr) = peval' mainExpr (map (\(name, expr) -> (name, expr)) fdefs)

  where
    peval' ::  PevalExpr -> Env -> Maybe PevalExpr
    peval' (ConstPart v) _ = Just (ConstPart v)
    peval' (BoolConstPart b) _ = Just (BoolConstPart b)
    peval' (VarPart s) env =
      case lookup s env of
        Just e -> peval' e env
        Nothing -> Nothing
    peval' (PrimPart op es) env =
      let rs = sequenceA [peval' e env | e <- es] in
        rs >>= \rs' ->
          if all isVal rs'
            then Just (ConstPart (prim op (map getVal rs')))
            else Just (PrimPart op rs')
    peval' (IfPart e0 e1 e2) env =
      let r0 = peval' e0 env in
        r0 >>= \r0' ->
          if isVal r0'
            then if getBool (getVal r0')
              then peval' e1 env
              else peval' e2 env
            else IfPart <$> r0 <*> peval' e1 env <*> peval' e2 env
    peval' (PairPart e1 e2) env =
      let v1 = peval' e1 env
          v2 = peval' e2 env
      in liftA2 PairPart v1 v2
    peval' (FirstPart e) env =
      peval' e env >>= \v ->
        case v of
          PairPart v1 _ -> Just v1
          _ -> Nothing
    peval' (SecondPart e) env =
      peval' e env >>= \v ->
        case v of
          PairPart _ v2 -> Just v2
          _ -> Nothing
    peval' (LambdaPart args body) _ = Just (LambdaPart args body)
    peval' (ApplyPart func args) env =
      let funcVal = peval' func env
          argVals = mapM (\arg -> peval' arg env) args
      in
        funcVal >>= \funcVal' ->
          argVals >>= \argVals' ->
            case funcVal' of
              LambdaPart params body ->
                peval' body (zip params argVals' ++ env)
              _ -> Nothing
    peval' (FixPart e) env =
      let v = peval' e env
      in v >>= \v' ->
        case v' of
          LambdaPart params body ->
            peval' (ApplyPart (LambdaPart params body) [FixPart (LambdaPart params body)]) env
          _ -> Nothing

prim :: Op -> [Int] -> Int
prim AddPart [i1, i2] = i1 + i2
prim SubtractPart [i1, i2] = i1 - i2
prim MultiplyPart [i1, i2] = i1 * i2
prim DividePart [i1, i2] = i1 `div` i2
prim LessThanPart [i1, i2] = if i1 < i2 then 1 else 0
prim EqualPart [i1, i2] = if i1 == i2 then 1 else 0

isVal ::  PevalExpr -> Bool
isVal (ConstPart _) = True
isVal (BoolConstPart _) = True
isVal _ = False

getVal ::  PevalExpr -> Int
getVal (ConstPart v) = v
getVal _ = error "Not an integer value"

getBool :: Int -> Bool
getBool 0 = False
getBool _ = True

type Prog = ([(String,  PevalExpr)],  PevalExpr)

instance Eq PevalExpr where
  (ConstPart v1) == (ConstPart v2) = v1 == v2
  (BoolConstPart b1) == (BoolConstPart b2) = b1 == b2
  (VarPart s1) == (VarPart s2) = s1 == s2
  (PrimPart op1 es1) == (PrimPart op2 es2) = op1 == op2 && es1 == es2
  (IfPart e11 e12 e13) == (IfPart e21 e22 e23) = e11 == e21 && e12 == e22 && e13 == e23
  (PairPart e1 e2) == (PairPart e3 e4) = e1 == e3 && e2 == e4
  (FirstPart e1) == (FirstPart e2) = e1 == e2
  (SecondPart e1) == (SecondPart e2) = e1 == e2
  (LambdaPart args1 body1) == (LambdaPart args2 body2) = args1 == args2 && body1 == body2
  (ApplyPart func1 args1) == (ApplyPart func2 args2) = func1 == func2 && args1 == args2
  (FixPart e1) == (FixPart e2) = e1 == e2
  _ == _ = False


main :: IO ()
main = do
  let prog = ( [ ("addition", LambdaPart ["x", "y"] (PrimPart AddPart [VarPart "x", VarPart "y"]))
               , ("main", LambdaPart [] (ApplyPart (VarPart "addition") [ConstPart 3, ConstPart 4]))
               ]
             , ApplyPart (VarPart "main") []
             )
  putStrLn "Program before partial evaluation:"
  print prog
  putStrLn "Program after partial evaluation:"
  print (peval prog)

  let pairProg = ( [ ("pairTest", LambdaPart [] (PairPart (VarPart "x") (ConstPart 6)))]
                 , PairPart (FirstPart (ApplyPart (VarPart "pairTest") [])) (SecondPart (ApplyPart (VarPart "pairTest") []))
                 )
  putStrLn "PairPart program before partial evaluation:"
  print pairProg
  putStrLn "PairPart program after partial evaluation:"
  print (peval pairProg)






