module LengthCounterInterpreter where 
    
import Interpreter (ExprAlg(..))

newtype LengthCounter a = LengthCounter { rungetLength :: Int }

instance ExprAlg LengthCounter where
    intLit _ = LengthCounter 1
    add e1 e2 = LengthCounter $ rungetLength e1 + rungetLength e2 + 1
    minus e1 e2 = LengthCounter $ rungetLength e1 + rungetLength e2 + 1
    boolLit _ = LengthCounter 1
    ifThenElse e1 e2 e3 = LengthCounter $ rungetLength e1 + rungetLength e2 + rungetLength e3 + 1
    lessthan e1 e2 = LengthCounter $ rungetLength e1 + rungetLength e2 + 1
    equality e1 e2 = LengthCounter $ rungetLength e1 + rungetLength e2 + 1
    pair e1 e2 = LengthCounter $ rungetLength e1 + rungetLength e2 + 1
    fst' e = LengthCounter $ rungetLength e + 1
    snd' e = LengthCounter $ rungetLength e + 1
    lam _ = LengthCounter 1
    app e1 e2 = LengthCounter $ rungetLength e1 + rungetLength e2 + 1
    fix _ = LengthCounter 1