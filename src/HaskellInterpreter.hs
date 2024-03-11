{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module HaskellInterpreter where 


import Data.Text (Text)
import qualified Data.Text as T

-- Type class for booleans
class BooleanAlgebra repr where
    hask_bool :: Bool -> repr Bool
    hask_ifThenElse :: repr Bool -> repr a -> repr a -> repr a

-- Type class for pairs and projections
class PairAlgebra repr where
    hask_pair :: repr a -> repr b -> repr (a, b)
    hask_fst' :: repr (a, b) -> repr a
    hask_snd' :: repr (a, b) -> repr b

-- Type class for lambda abstraction and application
class LambdaAlgebra repr where
    hask_lambda :: (repr a -> repr b) -> repr (a -> b)
    hask_apply :: repr (a -> b) -> repr a -> repr b

-- Type class for integers, addition, multiplication, and unary minus
class IntAlgebra repr where
    hask_int :: Int -> repr Int
    hask_add :: repr Int -> repr Int -> repr Int
    hask_multiply :: repr Int -> repr Int -> repr Int
    hask_minus :: repr Int -> repr Int

-- Type class for ordering or equality on integers
class OrdAlgebra repr where
    hask_lessThan :: repr Int -> repr Int -> repr Bool
    hask_equals :: repr Int -> repr Int -> repr Bool

-- Type class for fixed point computation
class FixAlgebra repr where
    hask_fix :: (repr a -> repr a) -> repr a

-- Instance for interpreting expressions to Haskell representation
newtype HaskellRepr a = HaskellRepr { runHaskellRepr :: Text }

instance BooleanAlgebra HaskellRepr where
   hask_bool b = HaskellRepr $ if b then T.pack "True" else T.pack "False"
   hask_ifThenElse cond trueBranch falseBranch = 
        HaskellRepr $ T.pack "(if " <> runHaskellRepr cond <> T.pack " then " <> runHaskellRepr trueBranch <> T.pack " else " <> runHaskellRepr falseBranch <> T.pack ")"

instance PairAlgebra HaskellRepr where
    hask_pair a b = HaskellRepr $ T.pack "(" <> runHaskellRepr a <> T.pack ", " <> runHaskellRepr b <> T.pack ")"
    hask_fst' p = HaskellRepr $ T.pack "fst " <> runHaskellRepr p
    hask_snd' p = HaskellRepr $ T.pack "snd " <> runHaskellRepr p

instance LambdaAlgebra HaskellRepr where
    hask_lambda f = HaskellRepr $ T.pack "\\x -> " <> runHaskellRepr (f (HaskellRepr "x"))
    hask_apply f a = HaskellRepr $ T.pack "(" <> runHaskellRepr f <> T.pack " " <> runHaskellRepr a <> T.pack ")"

instance IntAlgebra HaskellRepr where
    hask_int n = HaskellRepr $ T.pack (show n)
    hask_add a b = HaskellRepr $ T.pack "(" <> runHaskellRepr a <> T.pack " + " <> runHaskellRepr b <> T.pack ")"
    hask_multiply a b = HaskellRepr $ T.pack "(" <> runHaskellRepr a <> T.pack " * " <> runHaskellRepr b <> T.pack ")"
    hask_minus a = HaskellRepr $ T.pack "(-" <> runHaskellRepr a <> T.pack ")"

instance OrdAlgebra HaskellRepr where
    hask_lessThan a b = HaskellRepr $ T.pack "(" <> runHaskellRepr a <> T.pack " < " <> runHaskellRepr b <> T.pack ")"
    hask_equals a b = HaskellRepr $ T.pack "(" <> runHaskellRepr a <> T.pack " == " <> runHaskellRepr b <> T.pack ")"

instance FixAlgebra HaskellRepr where
    hask_fix f = HaskellRepr $ T.pack "fix " <> runHaskellRepr (f (HaskellRepr "x"))


