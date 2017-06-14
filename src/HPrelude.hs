{-# language OverloadedStrings #-}

module HPrelude where

import           Data.Text (Text)
import qualified Data.Text as T
import           Ty

-- data BaseUniv = BaseUniv String

-- pSucc :: Term
-- pSucc = Term "succ" (TArr TInt TInt)
--         (Prim1 (\(LitInt i) -> LitInt (succ i)))

-- pIsZero :: Term
-- pIsZero = Term "iszero" (TArr TInt TBool)
--         (Prim1 (\(LitInt i) -> LitBool (i == 0)))
