{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Control.Applicative
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

instance Arbitrary ExprC where
  arbitrary = sized f
    where
      f 0 = ExprI . Var <$> arbitrary
      f n =
        resize (div n 3 * 2) $
          oneof
            [ Lam <$> var <*> arbitrary,
              ExprI <$> arbitrary
            ]
  shrink (Lam str body) = body : (Lam str <$> shrink body)
  shrink (ExprI expr) = ExprI <$> shrink expr

instance Arbitrary ExprI where
  arbitrary = sized f
    where
      f 0 = Var <$> arbitrary
      f n =
        resize (div n 3 * 2) $
          oneof
            [ liftA2 App arbitrary arbitrary,
              liftA2 Typed arbitrary (liftA2 TFun arbitrary arbitrary),
              Var <$> var
            ]
  shrink (Var str) = Var <$> shrink str
  shrink (App f x) = f : tail (liftA2 App (f : shrink f) (x : shrink x))
  shrink (Typed e t) = tail $ liftA2 Typed (e : shrink e) (t : shrink t)

instance Arbitrary Context where
  arbitrary = sized (\n -> chooseInt (0, n)) >>= f
    where
      f 0 = pure Empty
      f n =
        oneof
          [ HasKind <$> var <*> f (n - 1),
            HasType <$> var <*> arbitrary <*> f (n - 1)
          ]
  shrink = fmap fromList . shrink . toList
    where
      toList Empty = []
      toList (HasKind str ctx) = Left str : toList ctx
      toList (HasType str ty ctx) = Right (str, ty) : toList ctx

      fromList = foldr (either HasKind (uncurry HasType)) Empty

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
      it "apply ignored argument" $
        evalI (App (Typed (Lam "poup" "soup") undefined) "poup")
          === evalI "soup"
      it "id" $
        evalI (App (Typed (Lam "x" "x") undefined) "soup")
          === evalI "soup"
      it "const" $
        let const = Typed (Lam "x" (Lam "y" "x")) undefined
         in evalI (App (App const "soup") "snot") === evalI "soup"
      it "flip const" $
        let const = Typed (Lam "x" (Lam "x" "x")) undefined
         in evalI (App (App const "snot") "soup") === evalI "soup"

-- it "id" $
--   evalI (App (Typed _ _) _)
--     === evalI "soup"

-- prop "abstract and apply" $ \e1 e2 (VarStr str) ->
--   Set.notMember str (freeC e1) ==> evalC e1 == evalC (ExprI $ App (Lam str e1) e2)
-- prop "id" $ \e (VarStr str) -> eval e == evalC (Lam str (Var str) `App` e)
-- prop "const" $ \ea eb (VarStr a) (VarStr b) -> a /= b && Set.notMember b (free ea) ==> eval ea == eval ((Lam a (Lam b (Var a)) `App` ea) `App` eb)
-- describe "id type example" $ do
--   let ctx = HasType "y" "α" $ HasKind "α" Empty
--       idExpr = Lam "x" "x"
--       idTyp = TFun "α" "α"
--       expr = App (Typed idExpr idTyp) "y"
--   it "hasType" (hasType ctx expr "α")
--   it "infers" (infer ctx expr == Just "α")
-- describe "const type example" $ do
--   let ctx = HasKind "β" $ HasType "y" "α" $ HasKind "α" Empty
--       idExpr = Lam "x" "x"
--       constTyp = TFun (TFun "β" "β") (TFun "α" (TFun "β" "β"))
--       constExpr = Lam "a" (Lam "b" "a")
--       expr = App (App (Typed constExpr constTyp) idExpr) "y"
--   it "hasType" (hasType ctx expr (TFun "β" "β"))
--   it "infers" (infer ctx expr `shouldBe` Just (TFun "β" "β"))
-- prop "infer infers correct type" $ \ctx expr ->
--   case infer ctx expr of
--     Just ty -> hasType ctx expr ty
--     Nothing -> True
