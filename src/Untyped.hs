-- | A very simple interpreter for the untyped λ-calculus.
module Untyped where

import Text.Printf (printf)

-- | A name can either be a local variable or a global
--   definition. Local variables are annotated with their binding
--   depth (ie deBruijn indexing).
data Name = Global String
          | Local Int String
          deriving (Eq)

instance Show Name where
  show (Global name)  = name
  show (Local _ name) = name

-- | A term in the untyped λ-calculus:
data Term = Var Name
          | Term :@: Term
          | Lam Name Term
          deriving (Eq)

instance Show Term where
  show (Var name)       = show name
  show (Lam arg body)   = printf "λ%s. %s" (show arg) (show body)
  show (e₁ :@: e₂)      = printf "%s %s" (fn e₁) (arg e₂)
    where fn λ@Lam{}      = printf "(%s)" $ show λ
          fn e            = show e
          arg app@(_:@:_) = printf "(%s)" $ show app
          arg e           = fn e 


