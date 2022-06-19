{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.Coerce
import qualified Data.Set as Set
import Lib
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

-- Test:
-- forall e1 e1 x, x not in (free e1), (\x -> e1) e2 = e1

newtype VarStr = VarStr {_getVarStr :: String}
  deriving newtype (Show)

instance Arbitrary VarStr where arbitrary = coerce var

var :: Gen String
var = elements [show x | x <- [1 :: Int .. 7]]

instance Arbitrary Expr where
  arbitrary = sized f
    where
      f 0 = Var <$> arbitrary
      f n =
        let r = f (div n 3 * 2)
         in oneof [App <$> r <*> r, Lam <$> var <*> r, Var <$> var]
  shrink (Var str) = Var <$> shrink str
  shrink (App f x) = [f, x]
  shrink (Lam str body) = body : (Lam str <$> shrink body)
  shrink (Typed e _) = [e]

instance Arbitrary Context where
  arbitrary = sized (\n -> chooseInt (0, n)) >>= f
    where
      f 0 = pure Empty
      f n =
        oneof
          [ TyCons <$> var <*> f (n - 1),
            TrCons <$> var <*> arbitrary <*> f (n - 1)
          ]
  shrink = fmap fromList . shrink . toList
    where
      toList Empty = []
      toList (TyCons str ctx) = Left str : toList ctx
      toList (TrCons str ty ctx) = Right (str, ty) : toList ctx

      fromList = foldr (either TyCons (uncurry TrCons)) Empty

instance Arbitrary Type where
  arbitrary = sized f
    where
      f 0 = TBase <$> var
      f n =
        let r = f (div n 2)
         in oneof [TBase <$> var, TFun <$> r <*> r]

main :: IO ()
main =
  hspec $
    describe "dependent-types-tutorial-test" $ do
      prop "abstract and apply" $ \e1 e2 (VarStr str) -> Set.notMember str (free e1) ==> eval e1 == eval (App (Lam str e1) e2)
      prop "id" $ \e (VarStr str) -> eval e == eval (Lam str (Var str) `App` e)
      prop "const" $ \ea eb (VarStr a) (VarStr b) -> a /= b && Set.notMember b (free ea) ==> eval ea == eval ((Lam a (Lam b (Var a)) `App` ea) `App` eb)
      describe "id type example" $ do
        let ctx = TrCons "y" "α" $ TyCons "α" Empty
            idExpr = Lam "x" "x"
            idTyp = TFun "α" "α"
            expr = App (Typed idExpr idTyp) "y"
        it "hasType" (hasType ctx expr "α")
        it "infers" (infer ctx expr == Just "α")
      describe "const type example" $ do
        let ctx = TyCons "β" $ TrCons "y" "α" $ TyCons "α" Empty
            idExpr = Lam "x" "x"
            constTyp = TFun (TFun "β" "β") (TFun "α" (TFun "β" "β"))
            constExpr = Lam "a" (Lam "b" "a")
            expr = App (App (Typed constExpr constTyp) idExpr) "y"
        it "hasType" (hasType ctx expr (TFun "β" "β"))
        it "infers" (infer ctx expr `shouldBe` Just (TFun "β" "β"))
      prop "infer infers correct type" $ \ctx expr ->
        case infer ctx expr of
          Just ty -> hasType ctx expr ty
          Nothing -> True
