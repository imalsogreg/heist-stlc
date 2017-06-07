module Ty where

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language

-- Primitive value
data Term = Term
    { termSymbol :: T.Text
    , termTy     :: Ty
    , termFun    :: PrimVal
    }

data Ty = TInt | TBool | THtml | TFun Ty Ty | TFun' [Ty] Ty

data Lit = LitInt  Int
         | LitBool Bool
         | LitHtml T.Text

data PrimVal = Prim0 Lit
             | Prim1 (Lit -> Lit)
               -- ^ Simple Function
             | Prim  [(T.Text, Lit) -> Lit]
               -- ^ Named args Function

term :: Parser Term
term = undefined
