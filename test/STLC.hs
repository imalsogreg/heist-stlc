{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module STLC where

import Data.Char
import Data.Monoid
import Data.Ord (comparing)
import Data.List (nubBy)
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc as P
import Data.Traversable
import Generics.SOP.Arbitrary
import Test.QuickCheck.Arbitrary
import Test.QuickCheck
import Test.Hspec
import Ty

instance Arbitrary Ty where
    arbitrary = oneof [pure TInt, pure TBool, pure THtml
                      ,TNominal <$> arbitrary, sized arbArrow
                      ,sized arbRecord]
      where
        arbArrow s = TArr
                     <$> resize (s `div` 2) arbitrary
                     <*> resize (s `div` 2) arbitrary
        arbRecord s = fmap (TRecord . M.fromList) $ do
            nRec :: Int <- fmap (max 1 . min s . (`mod` 5))
                           arbitrarySizedNatural
            forM (("f_" <>) . T.pack . show <$> [1..nRec]) $ \n -> do
                fTy <- resize (s `div` 5) arbNoRecord
                return (FieldName n, fTy)
    shrink = genericShrink

instance Arbitrary TyName where
    arbitrary = TyName . ("My" <>) . T.filter isAlphaNum . T.pack
                <$> arbitrary


instance Arbitrary FieldName where
    arbitrary = FieldName . ("f" <>) . T.filter isAlphaNum . T.pack
                <$> arbitrary

spec :: Spec
spec = do
    it "Satisfies (parse . print === id) property" $ property $
        \x -> (parseType . T.pack . show . P.pretty) x
              == Right (x :: Ty)
