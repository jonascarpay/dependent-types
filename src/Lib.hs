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

data Expr
  = Typed Expr Type
  | Var String
  | App Expr Expr
  | Lam String Expr
  deriving (Eq, Show)

instance IsString Expr where fromString = Var

data Value
  = Neutral Neutral
  | VLam String Value
  deriving (Eq, Show)

data Neutral
  = NVar String
  | NApp Neutral Value
  deriving (Eq, Show)

free :: Expr -> Set String
free (Var str) = Set.singleton str
free (App f x) = free f <> free x
free (Lam x body) = Set.delete x $ free body
free (Typed e _) = free e

eval :: Expr -> Value
eval (App f x) = case eval f of
  VLam str body -> subst str (eval x) body
  Neutral f' -> Neutral $ NApp f' (eval x)
eval (Typed e _) = eval e
eval (Var str) = Neutral (NVar str)
eval (Lam str body) = VLam str (eval body)

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

-- >>> eval $ (Lam "const" (Lam "id" (App "const" "id" `App` "y")) `App` Lam "a" (Lam "b" "a")) `App` Lam "y" "y"
-- VLam "y" (Neutral (NVar "y"))

data Context
  = Empty
  | TyCons String Context
  | TrCons String Type Context
  deriving (Show)

valid :: Context -> Bool
valid Empty = True
valid (TyCons _ ctx) = valid ctx
valid (TrCons x τ ctx) = valid ctx && wellFormed ctx τ

wellFormed :: Context -> Type -> Bool
wellFormed ctx (TBase str) = isBaseIn ctx
  where
    isBaseIn Empty = False
    isBaseIn (TyCons str' ctx') = str == str' || isBaseIn ctx'
    isBaseIn (TrCons str' _ ctx') = str /= str' && isBaseIn ctx'
wellFormed ctx (TFun τ τ') = wellFormed ctx τ && wellFormed ctx τ'

hasType :: Context -> Expr -> Type -> Bool
hasType ctx (Lam str body) (TFun tf tx) = hasType (TrCons str tf ctx) body tx
hasType ctx expr ty = infer ctx expr == Just ty

infer :: Context -> Expr -> Maybe Type
infer ctx (Typed expr ty)
  | wellFormed ctx ty && hasType ctx expr ty = Just ty
  | otherwise = Nothing
infer ctx (Var str) = go ctx
  where
    go Empty = Nothing
    go (TyCons str' ctx')
      | str == str' = Nothing
      | otherwise = go ctx'
    go (TrCons str' ty ctx')
      | str == str' = pure ty
      | otherwise = go ctx'
infer ctx (App f x) = case infer ctx f of
  Just (TFun τ τ') | hasType ctx x τ -> Just τ'
  _ -> Nothing
infer ctx (Lam _ _) = Nothing -- Unannotated lambda expression

-- >>> infer (TrCons "y" "a" $ TyCons "a" Empty) ()
