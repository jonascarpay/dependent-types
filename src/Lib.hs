{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString (..))

data Type
  = TBase String
  | TFun Type Type
  deriving (Eq, Show)

instance IsString Type where fromString = TBase

data ExprI
  = Typed ExprC Type
  | Var String
  | App ExprI ExprC
  deriving (Eq, Show)

data ExprC
  = ExprI ExprI
  | Lam String ExprC
  deriving (Eq, Show)

instance IsString ExprI where fromString = Var

instance IsString ExprC where fromString = ExprI . Var

data Value
  = Neutral Neutral
  | VLam String Value
  deriving (Eq, Show)

data Neutral
  = NVar String
  | NApp Neutral Value
  deriving (Eq, Show)

data Context
  = Empty
  | HasKind String Context
  | HasType String Type Context
  deriving (Show)

freeC :: ExprC -> Set String
freeC (Lam x body) = Set.delete x $ freeC body
freeC (ExprI expr) = freeI expr

freeI :: ExprI -> Set String
freeI (Var str) = Set.singleton str
freeI (App f x) = freeI f <> freeC x
freeI (Typed e _) = freeC e

evalC :: ExprC -> Value
evalC (Lam str body) = VLam str (evalC body)
evalC (ExprI expr) = evalI expr

evalI :: ExprI -> Value
evalI (App f x) = case evalI f of
  VLam str body -> subst str (evalC x) body
  Neutral f' -> Neutral $ NApp f' (evalC x)
evalI (Typed e _) = evalC e
evalI (Var str) = Neutral (NVar str)

subst :: String -> Value -> Value -> Value
subst var sub = goV
  where
    goN :: Neutral -> Value
    goN (NVar str)
      | str == var = sub
      | otherwise = Neutral (NVar str)
    goN (NApp f x) = case goN f of
      VLam str body -> subst str (goV x) body
      Neutral f' -> Neutral (NApp f' (goV x))

    goV :: Value -> Value
    goV (VLam str body)
      | str == var = VLam str body
      | otherwise = VLam str (goV body)
    goV (Neutral nexpr) = goN nexpr

valid :: Context -> Bool
valid Empty = True
valid (HasKind _ ctx) = valid ctx
valid (HasType _ τ ctx) = valid ctx && isType ctx τ

isType :: Context -> Type -> Bool
isType ctx (TBase str) = isBaseIn ctx
  where
    isBaseIn Empty = False
    isBaseIn (HasKind str' ctx') = str == str' || isBaseIn ctx'
    isBaseIn (HasType str' _ ctx') = str /= str' && isBaseIn ctx'
isType ctx (TFun τ τ') = isType ctx τ && isType ctx τ'

check :: Context -> ExprC -> Type -> Bool
check ctx (Lam str body) (TFun tf tx) = check (HasType str tf ctx) body tx
check ctx (Lam str body) _ = False
check ctx (ExprI expr) ty = infer ctx expr == Just ty

infer :: Context -> ExprI -> Maybe Type
infer ctx (Typed expr ty)
  | isType ctx ty && check ctx expr ty = Just ty
  | otherwise = Nothing
infer ctx (Var str) = go ctx
  where
    go Empty = Nothing
    go (HasKind str' ctx')
      | str == str' = Nothing
      | otherwise = go ctx'
    go (HasType str' ty ctx')
      | str == str' = pure ty
      | otherwise = go ctx'
infer ctx (App f x) = case infer ctx f of
  Just (TFun τ τ') | check ctx x τ -> Just τ'
  _ -> Nothing
infer ctx (Lam _ _) = Nothing -- Unannotated lambda expression

-- >>> infer (HasType "y" "a" $ HasKind "a" Empty) ()
