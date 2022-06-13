{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString (..))

data Type
  = TUnit
  | TFun Type Type
  deriving (Eq, Show)

data Expr
  = Typed Expr Type
  | Var String
  | App Expr Expr
  | Lam String Expr
  deriving (Eq, Show)

instance IsString Expr where
  fromString = Var

data VExpr
  = NExpr NExpr
  | VLam String VExpr
  deriving (Eq, Show)

data NExpr
  = NVar String
  | NApp NExpr VExpr
  deriving (Eq, Show)

free :: Expr -> Set String
free (Var str) = Set.singleton str
free (App f x) = free f <> free x
free (Lam x body) = Set.delete x $ free body
free (Typed e t) = free e

eval :: Expr -> VExpr
eval (App f x) = case eval f of
  VLam str body -> subst str (eval x) body
  NExpr f' -> NExpr $ NApp f' (eval x)
eval (Typed e _) = eval e
eval (Var str) = NExpr (NVar str)
eval (Lam str body) = VLam str (eval body)

substN :: String -> VExpr -> NExpr -> NExpr
substN var sub = go
  where
    go = undefined

subst :: String -> VExpr -> VExpr -> VExpr
subst var sub = goV
  where
    goN :: NExpr -> VExpr
    goN (NVar str)
      | str == var = sub
      | otherwise = NExpr (NVar str)
    goN (NApp f x) = case goN f of
      VLam str body -> subst str (goV x) body
      NExpr f' -> NExpr (NApp f' (goV x))

    goV :: VExpr -> VExpr
    goV (VLam str body)
      | str == var = VLam str body
      | otherwise = VLam str (goV body)
    goV (NExpr nexpr) = goN nexpr

-- >>> eval $ (Lam "const" (Lam "id" (App "const" "id" `App` "y")) `App` Lam "a" (Lam "b" "a")) `App` Lam "y" "y"
-- VLam "x" (NExpr (NVar "x"))
