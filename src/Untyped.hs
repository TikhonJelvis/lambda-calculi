-- | A very simple interpreter for the untyped λ-calculus.
module Untyped where

import Text.Printf (printf)

data Name = Global String
          | Local Int String
          deriving (Eq)

instance Show Name where
  show (Glboal name)  = name
  show (Local _ name) = name

-- | A term in the λ-calculus:
data Term = Var Name
          | Term :@: Term
          | Lam Name Term
          deriving (Eq)

instance Show Term where
  show (Var name)     = show name
  show (e₁ :@: e₂)    = show e₁ ++ " " ++ show e₂
  show (Lam arg body) = printf "λ%s.%s" (show arg) (show body)
