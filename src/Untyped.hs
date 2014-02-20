{-# LANGUAGE LambdaCase #-}

-- | A very simple interpreter for the untyped λ-calculus.
module Untyped where

import qualified Data.Map    as Map
import           Data.Map    (Map)
import           Data.Maybe  (fromMaybe)

import           Text.Printf (printf)

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

-- | The global environment maps global names to values. It's separate
--   from locally bound names because those use deBruijn indexing.
type Globals = Map String Term

-- | No names defined in global scope.
empty :: Globals
empty = Map.empty

-- | We can just store our locally bound variables in a stack and
--   index with !!
type Locals = [Term]

-- | Evaluate the term, using eager evaluation (for now).
-- 
--   Free variables are left unevaluated, so @foo@ would evaluate to
--   itself.
eval :: Globals -> Term -> Term
eval globals = go []
  where go locals = \case
          var@(Var (Global name))   -> fromMaybe var $ Map.lookup name globals
          var@(Var (Local index _))
            | index < length locals -> locals !! index
            | otherwise             -> var
          λ@Lam {}                  -> λ -- we do not evaluate under lambdas
          (Lam _ body) :@: term     -> go (go locals term : locals) body
          fn :@: term               -> go locals $ go locals fn :@: term
