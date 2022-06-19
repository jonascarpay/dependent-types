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

substN :: String -> Value -> Neutral -> Neutral
substN var sub = go
  where
    go = undefined

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
