{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module HaskellInterpreter where 


import Data.Text (Text)
import qualified Data.Text as T
import Interpreter (ExprAlg(..))

-- Instance for interpreting expressions to Haskell representation
newtype HaskellRepr a = HaskellRepr { runHaskellRepr :: Text }

-- Interpret the EDSL to compute a valid Haskell representation
instance ExprAlg HaskellRepr where
    boolLit b = HaskellRepr $ if b then T.pack "True" else T.pack "False"
    ifThenElse cond trueBranch falseBranch = 
            HaskellRepr $ T.pack "(if " <> runHaskellRepr cond <> T.pack " then " <> runHaskellRepr trueBranch <> T.pack " else " <> runHaskellRepr falseBranch <> T.pack ")"
    pair a b = HaskellRepr $ T.pack "(" <> runHaskellRepr a <> T.pack ", " <> runHaskellRepr b <> T.pack ")"
    fst' p = HaskellRepr $ T.pack "fst " <> runHaskellRepr p
    snd' p = HaskellRepr $ T.pack "snd " <> runHaskellRepr p
    lam f = HaskellRepr $ T.pack "\\x -> " <> runHaskellRepr (f (HaskellRepr "x"))
    app f a = HaskellRepr $ T.pack "(" <> runHaskellRepr f <> T.pack " " <> runHaskellRepr a <> T.pack ")"
    intLit n = HaskellRepr $ T.pack (show n)
    add a b = HaskellRepr $ T.pack "(" <> runHaskellRepr a <> T.pack " + " <> runHaskellRepr b <> T.pack ")"
    mul a b = HaskellRepr $ T.pack "(" <> runHaskellRepr a <> T.pack " * " <> runHaskellRepr b <> T.pack ")"
    minus a b = HaskellRepr $ T.pack "(" <> runHaskellRepr a <> T.pack " - " <> runHaskellRepr b <> T.pack ")"
    lessthan a b = HaskellRepr $ T.pack "(" <> runHaskellRepr a <> T.pack " < " <> runHaskellRepr b <> T.pack ")"
    equality a b = HaskellRepr $ T.pack "(" <> runHaskellRepr a <> T.pack " == " <> runHaskellRepr b <> T.pack ")"
    fix f = HaskellRepr $ T.pack "fix " <> runHaskellRepr (f (HaskellRepr "x"))


