{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module PrettyPrintInterpreter where 
    
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.IO (stdout)
import Interpreter (ExprAlg(..))


-- Define a newtype wrapper for representing programs as Text
newtype HaskellText a = HaskellText { runHaskellText :: T.Text }

-- Interpret the EDSL to compute a valid Haskell representation
instance ExprAlg HaskellText where
    boolLit b = HaskellText $ if b then T.pack "True" else T.pack "False"
    ifThenElse (HaskellText c) (HaskellText t) (HaskellText e) = HaskellText $ T.concat [T.pack "(if ", c, T.pack " then ", t, T.pack " else ", e, T.pack ")"]
    intLit n = HaskellText $ T.pack $ show n
    pair (HaskellText a) (HaskellText b) = HaskellText $ T.concat [T.pack "(", a, T.pack ", ", b,T.pack ")"]
    fst' (HaskellText p) = HaskellText $ T.concat [T.pack "fst ", p]
    snd' (HaskellText p) = HaskellText $ T.concat [T.pack "snd ", p]
    add (HaskellText t1) (HaskellText t2) = HaskellText $ T.concat [T.pack "(", t1, T.pack " + ", t2, T.pack ")"]
    minus (HaskellText t1) (HaskellText t2) = HaskellText $ T.concat [T.pack "(", t1, T.pack " - ", t2, T.pack ")"]
    mul (HaskellText t1) (HaskellText t2) = HaskellText $ T.concat [T.pack "(", t1, T.pack " x ", t2, T.pack ")"]
    lam f = HaskellText $ T.concat [T.pack "\\x -> ", runHaskellText $ f (HaskellText $ T.pack "x")]
    app (HaskellText t1) (HaskellText t2) = HaskellText $ T.concat [T.pack "(", t1, T.pack " ", t2, T.pack ")"]
    lessthan (HaskellText t1) (HaskellText t2) = HaskellText $ T.concat [T.pack "(", t1, T.pack " < ", t2,T.pack ")"]
    equality (HaskellText t1) (HaskellText t2) = HaskellText $ T.concat [T.pack "(", t1, T.pack " == ", t2, T.pack ")"]
    fix f = HaskellText $ T.concat [T.pack "fix (\\f -> ", runHaskellText $ f (HaskellText $ T.pack "f"), T.pack ")"]



