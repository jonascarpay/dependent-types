{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import qualified Data.Set as Set
import Lib
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

-- Test:
-- forall e1 e1 x, x not in (free e1), (\x -> e1) e2 = e1

newtype VarStr = VarStr {getVarStr :: String}
  deriving newtype (Show)

instance Arbitrary VarStr where
  arbitrary = elements [VarStr (show x) | x <- [1 :: Int .. 7]]

instance Arbitrary Expr where
  arbitrary = sized f
    where
      var = getVarStr <$> arbitrary
      f 0 = Var <$> arbitrary
      f n =
        let n' = div n 3 * 2
         in oneof
              [ App <$> f n' <*> f n',
                Lam <$> var <*> f n',
                Var <$> var
              ]
  shrink (Var str) = Var <$> shrink str
  shrink (App f x) = [f, x]
  shrink (Lam str body) = body : (Lam str <$> shrink body)
  shrink (Typed e _) = [e]

main :: IO ()
main =
  hspec $
    describe "dependent-types-tutorial-test" $ do
      prop "abstract and apply" $ \e1 e2 (VarStr str) -> Set.notMember str (free e1) ==> eval e1 == eval (App (Lam str e1) e2)
      prop "id" $ \e (VarStr str) -> eval e == eval (Lam str (Var str) `App` e)
      prop "const" $ \ea eb (VarStr a) (VarStr b) -> a /= b && Set.notMember b (free ea) ==> eval ea == eval ((Lam a (Lam b (Var a)) `App` ea) `App` eb)
