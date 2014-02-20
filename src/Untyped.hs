{-# LANGUAGE LambdaCase #-}

-- | A very simple interpreter for the untyped λ-calculus.
module Untyped (Globals, Locals, Term (..), Name (..), empty, eval, markLocals, parseTerm) where

import           Control.Applicative ((<*), (*>), (<*>), (<$>), (<$))

import           Data.List           (elemIndex)
import qualified Data.Map            as Map
import           Data.Map            (Map)
import           Data.Maybe          (fromMaybe)

import qualified Text.Parsec         as P
import           Text.Parsec         ((<|>), (<?>))
import           Text.Parsec.String  (Parser)
import           Text.Printf         (printf)

-- | A name can either be a local variable or a global
--   definition. Local variables are annotated with their binding
--   depth (ie deBruijn indexing).
data Name = Global String
          | Local Int String
          deriving (Eq)

instance Show Name where
  show (Global name)  = name
  show (Local _ name) = name

-- | A valid variable name is made up of alphanumeric characters and
--   underscores.
-- 
--   Variables get parsed as globals, with a separate pass doing all
--   the bookkeeping for local variables.
ident :: Parser Name
ident = Global <$> P.many1 (P.alphaNum <|> P.char '_') <* P.spaces <?> "variable name"



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

symbol :: String -> Parser String
symbol c    = P.string c <* P.spaces

term, atom, variable, lambda, parens :: Parser Term

-- | Parses a term, does not handle local variables.
term =  application <|> atom
  where application = foldl1 (:@:) <$> P.many1 atom

atom =  lambda  
    <|> variable
    <|> parens

variable  = Var <$> ident
lambda    = λ *> (Lam <$> (ident <* symbol ".") <*> term)
  where λ = symbol "λ" <|> symbol "\\"
parens    = symbol "(" *> term <* symbol ")"

-- | Goes through a term keeping track of scopes and annotates locally
--   bound variables with their deBruijn index.
markLocals :: Term -> Term
markLocals = go []
  where go scope = \case
          var@(Var (Global name)) -> fromMaybe var $ local <$> elemIndex name scope
            where local index = Var $ Local index name
          var@Var{}     -> var
          Lam name body -> Lam name $ go (show name : scope) body
          e₁ :@: e₂     -> go scope e₁ :@: go scope e₂

-- | Try to parse a λ-calculus term, returning either the result or a
--   parse error.
parseTerm :: String -> String -> Either P.ParseError Term
parseTerm source str = markLocals <$> P.parse (P.spaces *> term <* P.eof) source str



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
          (Lam _ body) :@: expr     -> go (go locals expr : locals) body
          fn :@: expr               -> go locals $ go locals fn :@: expr

-- | Parses and then evaluate a λ-calculus term. Might return a parse
--   error.
run :: Globals -> String -> Either P.ParseError String
run globals str = show . eval globals <$> parseTerm "" str
