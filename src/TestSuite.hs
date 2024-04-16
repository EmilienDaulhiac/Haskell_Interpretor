module TestSuite where 

import Test.HUnit
import Data.Text ()
import qualified Data.Text as T
import Data.Text.IO ()
import Interpreter (runEval, ExprAlg, ExprAlg(..))
import PrettyPrintInterpreter (runHaskellText)
import HaskellInterpreter (runHaskellRepr)
import LengthCounterInterpreter (rungetLength)
import TracingInterpreter (runTrace, ExprAlgTrace(..))
import PartialInterpreter (Prog, PevalExpr(..), Op(..), peval)

-- Example expressions
exampleExpr1 :: ExprAlg repr => repr Int
exampleExpr1 = add (intLit 2) (mul (intLit 3) (intLit 4))
test1example :: Int 
test1example = runEval exampleExpr1
testAddition :: Test
testAddition = TestCase $ assertEqual "2 + (3 * 4) should be 14" 14 test1example

exampleExpr2 :: ExprAlg repr => repr Int
exampleExpr2 = ifThenElse (lessthan (intLit 3) (intLit 2)) (intLit 3) (intLit 2)
test2example :: Int 
test2example = runEval exampleExpr2
testIfThenElse1 :: Test
testIfThenElse1 = TestCase $ assertEqual "if 3 < 2 then 3 else 2 should be 2" 2 test2example

exampleExpr3 :: ExprAlg repr => repr Int
exampleExpr3 = ifThenElse (equality (intLit 3) (intLit 2)) (intLit 3) (intLit 2)
test3example :: Int 
test3example = runEval exampleExpr3
testIfThenElse2 :: Test
testIfThenElse2 = TestCase $ assertEqual "if 3 == 2 then 3 else 2 should be 2" 2 test3example

exampleExpr4 :: ExprAlg repr => repr Int
exampleExpr4 = fst' (pair (intLit 1) (intLit 2))
test4example :: Int 
test4example = runEval exampleExpr4
testPairProjection :: Test
testPairProjection = TestCase $ assertEqual "fst (1, 2) should be 1" 1 test4example

-- Example Length representation
exampleLength :: ExprAlg repr => repr Int
exampleLength = ifThenElse (lessthan (intLit 3) (intLit 5)) (intLit 1) (intLit 0)
test7 :: Int 
test7 = rungetLength exampleLength
testlength :: Test
testlength = TestCase $ assertEqual "Length of if 3 > 5 then 1 else 0 should be 6 " 6 test7

-- Example haskell representation
exampleHaskellRepresentation :: ExprAlg repr => repr Int
exampleHaskellRepresentation = ifThenElse (lessthan (intLit 3) (intLit 5)) (intLit 1) (intLit 0)
test4 :: T.Text
test4 = runHaskellRepr exampleHaskellRepresentation
testhaskellcond :: Test
testhaskellcond = TestCase $ assertEqual "haskell representation of if 3 < 2 then 1 else 0" "(if (3 < 5) then 1 else 0)" (T.unpack test4)

-- Example pretty printed
examplePrettyPrintedProgram :: ExprAlg repr => repr Int
examplePrettyPrintedProgram = add (intLit 3) (intLit 4)
test5 :: T.Text
test5 = runHaskellText examplePrettyPrintedProgram
testprettyaddition :: Test
testprettyaddition = TestCase $ assertEqual "pretty printed add (3) (4) should be (3 + 4)" "(3 + 4)" (T.unpack test5)

examplePrettyPrintedProgram2 :: ExprAlg repr => repr (Int,Int)
examplePrettyPrintedProgram2 = pair (mul (intLit 3) (intLit 18)) (minus (intLit 20) (intLit 5))
test6 :: T.Text
test6 = runHaskellText examplePrettyPrintedProgram2
testprettypair :: Test
testprettypair = TestCase $ assertEqual "pretty printed pair (mul (3) (18) (minus (20) (5)) should be ((3 x 18) , (20 - 5))" "((3 x 18), (20 - 5))" (T.unpack test6)

-- Example Tracer Interpreter
exampleExprTrace1 :: ExprAlgTrace repr => repr Int
exampleExprTrace1 = ifThenElseTrace (lessthanTrace (intLitTrace 3) (intLitTrace 4))
                          (addTrace (intLitTrace 5) (intLitTrace 6))
                          (mulTrace (intLitTrace 7) (intLitTrace 8))
testTrace1 :: Int
testTrace1 = snd (runTrace exampleExprTrace1)
testcaseTrace1 :: Test
testcaseTrace1 = TestCase $ assertEqual "if 3<4 then 5+6 else 7*8 should return 11" 11 testTrace1

exampleExprTrace2 :: ExprAlgTrace repr => repr Int
exampleExprTrace2 = fstTrace' (pairTrace (minusTrace (intLitTrace 3) (intLitTrace 3)) (intLitTrace 4))
testTrace2 ::  Int
testTrace2 = snd( runTrace exampleExprTrace2)
testcaseTrace2 :: Test
testcaseTrace2 = TestCase $ assertEqual "first element of pair (3-3, 4) should be 0" 0 testTrace2

exampleExprTrace3 :: ExprAlgTrace repr => repr Int
exampleExprTrace3 = applyTrace (lambdaTrace (\x -> addTrace x x)) (intLitTrace 5)
testTrace3 ::  Int
testTrace3 = snd (runTrace exampleExprTrace3)
testcaseTrace3 :: Test
testcaseTrace3 = TestCase $ assertEqual "5.apply(lambda x: x+x) should be 10" 10 testTrace3

-- Partial Interpreter 
examplePartial1 :: Prog
examplePartial1 = ( [ ("addition", LambdaPart ["x", "y"] (PrimPart AddPart [VarPart "x", VarPart "y"]))
               , ("main", LambdaPart [] (ApplyPart (VarPart "addition") [ConstPart 3, ConstPart 4]))
               ]
             , ApplyPart (VarPart "main") []
             )
testPartial1 ::  Maybe PevalExpr
testPartial1 = peval examplePartial1
testcasePartial1 :: Test
testcasePartial1 = TestCase $ assertEqual "3 + 4 should be 7" (Just (ConstPart 7)) testPartial1

examplePartial2 :: Prog
examplePartial2 = ( [ ("pairTest", LambdaPart [] (PairPart (PrimPart AddPart [ConstPart 8, ConstPart 6]) (ConstPart 6)))]
                 , FirstPart (ApplyPart (VarPart "pairTest") [])
                 )
testPartial2 ::  Maybe PevalExpr
testPartial2 = peval examplePartial2
testcasePartial2 :: Test
testcasePartial2 = TestCase $ assertEqual "First element of (8+6,6) should be 14" (Just (ConstPart 14)) testPartial2

tests :: Test
tests = TestList [ TestLabel "Addition" testAddition
                 , TestLabel "If-Then-Else 1" testIfThenElse1
                 , TestLabel "If-Then-Else 2" testIfThenElse2
                 , TestLabel "Pair Projection" testPairProjection
                 , TestLabel "Pretty addition" testprettyaddition
                 , TestLabel "Pretty pair" testprettypair
                 , TestLabel "Length test" testlength
                 , TestLabel "Haskell Representation of If Then Else" testhaskellcond
                 , TestLabel "Tracing interpreter ifthenelse test" testcaseTrace1
                 , TestLabel "Tracing interpreter pair test" testcaseTrace2
                 , TestLabel "Tracing interpreter lambda and apply test" testcaseTrace3
                 , TestLabel "Partial evaluator add test" testcasePartial1
                 , TestLabel "Partial evaluator pair test" testcasePartial2]
