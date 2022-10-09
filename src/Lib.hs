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

data Context
  = Empty
  | HasKind String Context
  | HasType String Type Context
  deriving (Show)

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

valid :: Context -> Bool
valid Empty = True
valid (HasKind _ ctx) = valid ctx
valid (HasType x τ ctx) = valid ctx && isType ctx τ

isType :: Context -> Type -> Bool
isType ctx (TBase str) = isBaseIn ctx
  where
    isBaseIn Empty = False
    isBaseIn (HasKind str' ctx') = str == str' || isBaseIn ctx'
    isBaseIn (HasType str' _ ctx') = str /= str' && isBaseIn ctx'
isType ctx (TFun τ τ') = isType ctx τ && isType ctx τ'

typeCheck :: Context -> Expr -> Type -> Bool
typeCheck ctx (Lam str body) (TFun tf tx) = typeCheck (HasType str tf ctx) body tx
typeCheck ctx expr ty = infer ctx expr == Just ty

infer :: Context -> Expr -> Maybe Type
infer ctx (Typed expr ty)
  | isType ctx ty && typeCheck ctx expr ty = Just ty
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
  Just (TFun τ τ') | typeCheck ctx x τ -> Just τ'
  _ -> Nothing
infer ctx (Lam _ _) = Nothing -- Unannotated lambda expression

-- >>> infer (HasType "y" "a" $ HasKind "a" Empty) ()
