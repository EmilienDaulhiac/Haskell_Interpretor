module TestSuite where 

import Test.HUnit
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (readFile)
import Interpreter (runEval, ExprAlg, ExprAlg(..))
import PrettyPrintInterpreter (runHaskellText)
import HaskellInterpreter (BooleanAlgebra(..), 
                            IntAlgebra(..), 
                            OrdAlgebra(..), 
                            LambdaAlgebra(..),
                            FixAlgebra(..), 
                            runHaskellRepr,
                            HaskellRepr, 
                            OrdAlgebra(..),
                            PairAlgebra(..) )

-- Example expressions
exampleExpr1 :: ExprAlg repr => repr Int
exampleExpr1 = add (intLit 2) (mul (intLit 3) (intLit 4))
test1example = runEval exampleExpr1
testAddition :: Test
testAddition = TestCase $ assertEqual "2 + (3 * 4) should be 14" 14 test1example

exampleExpr2 :: ExprAlg repr => repr Int
exampleExpr2 = ifThenElse (lessthan (intLit 3) (intLit 2)) (intLit 3) (intLit 2)
test2example = runEval exampleExpr2
testIfThenElse1 :: Test
testIfThenElse1 = TestCase $ assertEqual "if 3 < 2 then 3 else 2 should be 2" 2 test2example

exampleExpr3 :: ExprAlg repr => repr Int
exampleExpr3 = ifThenElse (equality (intLit 3) (intLit 2)) (intLit 3) (intLit 2)
test3example = runEval exampleExpr3
testIfThenElse2 :: Test
testIfThenElse2 = TestCase $ assertEqual "if 3 == 2 then 3 else 2 should be 2" 2 test3example

exampleExpr4 :: ExprAlg repr => repr Int
exampleExpr4 = fst' (pair (intLit 1) (intLit 2))
test4example = runEval exampleExpr4
testPairProjection :: Test
testPairProjection = TestCase $ assertEqual "fst (1, 2) should be 1" 1 test4example

-- Example haskell representation
exampleHaskellRepresentation :: (BooleanAlgebra repr, IntAlgebra repr, OrdAlgebra repr) => repr Int
exampleHaskellRepresentation = hask_ifThenElse (hask_lessThan (hask_int 3) (hask_int 5)) (hask_int 1) (hask_int 0)
test4 = runHaskellRepr exampleHaskellRepresentation
testhaskellcond :: Test
testhaskellcond = TestCase $ assertEqual "haskell representation of if 3 < 2 then 1 else 0" "(if (3 < 5) then 1 else 0)" (T.unpack test4)

-- Example pretty printed
examplePrettyPrintedProgram :: ExprAlg repr => repr Int
examplePrettyPrintedProgram = add (intLit 3) (intLit 4)
test5 = runHaskellText examplePrettyPrintedProgram
testprettyaddition :: Test
testprettyaddition = TestCase $ assertEqual "pretty printed add (3) (4) should be (3 + 4)" "(3 + 4)" (T.unpack test5)

examplePrettyPrintedProgram2 :: ExprAlg repr => repr (Int,Int)
examplePrettyPrintedProgram2 = pair (mul (intLit 3) (intLit 18)) (minus (intLit 20) (intLit 5))
test6 = runHaskellText examplePrettyPrintedProgram2
testprettypair :: Test
testprettypair = TestCase $ assertEqual "pretty printed pair (mul (3) (18) (minus (20) (5)) should be ((3 x 18) , (20 - 5))" "((3 x 18), (20 - 5))" (T.unpack test6)

tests :: Test
tests = TestList [ TestLabel "Addition" testAddition
                 , TestLabel "If-Then-Else 1" testIfThenElse1
                 , TestLabel "If-Then-Else 2" testIfThenElse2
                 , TestLabel "Pair Projection" testPairProjection
                 , TestLabel "Pretty addition" testprettyaddition
                 , TestLabel "Pretty pair" testprettypair
                 , TestLabel "Haskell Representation of If Then Else" testhaskellcond]
