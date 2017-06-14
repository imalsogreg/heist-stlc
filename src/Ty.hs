{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}

module Ty where

import           Control.Monad             (msum)
import           Data.Functor.Identity
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import qualified Data.Text.Prettyprint.Doc as P
import           GHC.Generics              (Generic)
import           GHC.TypeLits
import qualified Generics.SOP as SOP
import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language
import           Text.Parsec.String
import qualified Text.Parsec.Token         as Tok


------------------------------------------------------------------------------
-- | Our Simply typed LC's type system
data Ty = TInt
        | TBool
        | THtml
          -- ^ Valid Html (fully applied with no free variables)
        | TNominal TyName
          -- ^ Nominal user-defined types (e.g. "LegalFooter", "Header")
        | TArr Ty Ty
          -- ^ Function
        | TRecord (M.Map FieldName Ty)
          -- ^ Record types are products with named fields
        -- | TProd [(Ty, Ty)]
        -- | TSum [(T.Text, [Ty])]
        deriving (Eq, Show, Generic)

instance SOP.Generic Ty

newtype TyName = TyName { unTyName :: T.Text }
    deriving (Eq, Ord, Show, Generic)

newtype FieldName = FieldName { unFieldName :: T.Text}
    deriving (Eq, Ord, Show, Generic)

instance P.Pretty TyName where
    pretty (TyName n) = P.pretty n

------------------------------------------------------------------------------
-- | Parsing

parseType :: T.Text -> Either ParseError Ty
parseType = parse (tyE Nothing <* eof) "string" . T.unpack

parens :: Parser a -> Parser a
parens = Tok.parens haskell

reserved :: String -> ParsecT String u Identity ()
reserved = Tok.reservedOp haskell

identifier :: Parser String
identifier = Tok.identifier haskell

tyE :: Maybe [T.Text] -> Parser Ty
tyE univ = buildExpressionParser table (tyTerm univ)
     <?> "type expression"

tyTerm :: Maybe [T.Text] -> Parser Ty
tyTerm univ = parens (tyE univ)
     <|> try (TBool <$ string "Bool")
     <|> try (TInt  <$ string "Int")
     <|> try (THtml <$ string "Html")
     <|> fmap (TRecord . M.fromList) (tyRecord univ)
     <|> try (tyNominal univ)
     <|> tyE univ
     <?> "simple type"


tyNominal :: Maybe [T.Text] -> Parser Ty
tyNominal Nothing = fmap (TNominal . TyName . T.pack) identifier
tyNominal (Just univ) = msum $
    fmap (fmap (TNominal . TyName . T.pack) . string . T.unpack) univ


-- TODO fix parser to allow nested records
tyRecord :: Maybe [T.Text] -> Parser [(FieldName, Ty)]
tyRecord univ =
    char '{' *>
    sepBy tyField (spaces >> char ',' >> spaces)
    <* char '}'
  where tyField = do
            spaces
            n <- spaces *> fmap T.pack identifier
            spaces >> string "::" >> spaces
            t <- tyE univ <* spaces
            return (FieldName n, t)


table :: [[Operator String u Identity Ty]]
table =  [[binary "->" TArr AssocRight]]

binary
    :: String
    -> (a -> a -> a)
    -> Assoc
    -> Operator String u Identity a
binary name fun assoc =
    Infix (do{ spaces >> reserved name >> spaces; return fun}) assoc


------------------------------------------------------------------------------
-- | Pretty printing

instance P.Pretty Ty where
    pretty (TArr arg res) = prettyArg P.<+> "->" P.<+> P.pretty res
      where prettyArg = case arg of
                TArr _ _ -> P.parens (P.pretty arg)
                _        -> P.pretty arg
    pretty TInt  = P.pretty ("Int"  :: T.Text)
    pretty TBool = P.pretty ("Bool" :: T.Text)
    pretty THtml = P.pretty ("Html" :: T.Text)
    pretty (TNominal t) = P.pretty t
    pretty (TRecord fs) =
        P.encloseSep P.lbrace P.rbrace P.comma (prettyRecord <$> M.toList fs)
      where
        prettyRecord (FieldName nm,ty) =
            P.pretty nm P.<+> "::" P.<+> P.pretty ty
