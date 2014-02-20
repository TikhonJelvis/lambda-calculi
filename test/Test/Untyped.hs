module Test.Untyped where

import           Control.Applicative ((<$>), (<*>))

import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import           Untyped

tests = testGroup "Untyped" [prettyPrinting, parsing]

instance QC.Arbitrary Name where
  arbitrary = QC.oneof [Local <$> QC.arbitrary <*> QC.arbitrary, Global <$> QC.arbitrary]

prettyPrinting = testGroup "Pretty Printing"
 [ testCase "Local Names" $ do
      show x @?= "x"
      show y @?= "y"
 , testCase "Global Names" $ do
      show foo @?= "foo"
      show bar @?= "bar"
 , QC.testProperty "Names" $
   \ num name -> show (Local num name) == name && show (Global name) == name
 , testCase "Variables" $ do
      show x'   @?= "x"
      show y'   @?= "y"
      show foo' @?= "foo"
      show bar' @?= "bar"
 , QC.testProperty "Variables" $
   \ var -> show (Var var) == show var
 , testCase "Variable Application" $ do
     show (f' :@: x')     @?= "f x"
     show (foo' :@: bar') @?= "foo bar"
 , testCase "Lambda Application" $ do
     show (f' :@: Lam x x')           @?= "f (λx. x)"
     show (foo' :@: Lam x bar')       @?= "foo (λx. bar)"
     show (Lam x x' :@: x')           @?= "(λx. x) x"
     show (Lam x bar' :@: foo')       @?= "(λx. bar) foo"
     show (Lam x bar' :@: Lam y foo') @?= "(λx. bar) (λy. foo)"
 , testCase "Application Application" $ do
     show (f' :@: x' :@: y')                           @?= "f x y"
     show (f' :@: (x' :@: y'))                         @?= "f (x y)"
     show (Lam x x' :@: (f' :@: x'))                   @?= "(λx. x) (f x)"
     show ((Lam x x') :@: Lam x bar' :@: Lam y foo')   @?= "(λx. x) (λx. bar) (λy. foo)"
     show ((Lam x x') :@: (Lam x bar' :@: Lam y foo')) @?= "(λx. x) ((λx. bar) (λy. foo))"
 ]
  where vars@[x, y, f, foo, bar] =
          [ Local 0 "x"
          , Local 10 "y"
          , Global "f"
          , Global "foo"
          , Global "bar"
          ]
        [x', y', f', foo', bar'] = Var <$> vars
        
parsing = testGroup "Parsing"
  [ testCase "variables" $ do
       show (parseTerm "" "x")         @?= "Right x"
       show (parseTerm "" "y")         @?= "Right y"
       show (parseTerm "" "foo")       @?= "Right foo"
       show (parseTerm "" "bar")       @?= "Right bar"
       show (parseTerm "" "fooBar")    @?= "Right fooBar"
       show (parseTerm "" "foo_bar")   @?= "Right foo_bar"
       show (parseTerm "" "Foo_bar")   @?= "Right Foo_bar"
       show (parseTerm "" "__Foo_bar") @?= "Right __Foo_bar"
       show (parseTerm "" "__")        @?= "Right __"
       show (parseTerm "" "   x")      @?= "Right x"
       show (parseTerm "" "x   ")      @?= "Right x"
       show (parseTerm "" "   x   ")   @?= "Right x"

  , testCase "lambdas" $ do
       show (parseTerm "" "(λ x.y)")          @?= "Right λx. y"
       show (parseTerm "" "λ x.y")            @?= "Right λx. y"
       show (parseTerm "" "(λ x.y) z")        @?= "Right (λx. y) z"
       show (parseTerm "" "λ x.y z")          @?= "Right λx. y z"
       show (parseTerm "" "z (λ x.y)")        @?= "Right z (λx. y)"
       show (parseTerm "" "(λ x.y) (λ y. z)") @?= "Right (λx. y) (λy. z)"
       show (parseTerm "" "λ x. λ y. z")      @?= "Right λx. λy. z"
       show (parseTerm "" "λ x. (λ y. z)")    @?= "Right λx. λy. z"

  , testCase "applications" $ do
       show (parseTerm "" "f x")              @?= "Right f x"
       show (parseTerm "" "f x y")            @?= "Right f x y"
       show (parseTerm "" "(f x) y")          @?= "Right f x y"
       show (parseTerm "" "f x y z")          @?= "Right f x y z"
       show (parseTerm "" "(f x) y z")        @?= "Right f x y z"
       show (parseTerm "" "(f x y) z")        @?= "Right f x y z"
       show (parseTerm "" "((f x) y) z")      @?= "Right f x y z"
       show (parseTerm "" "f (g y)")          @?= "Right f (g y)"
       show (parseTerm "" "f (g y) x")        @?= "Right f (g y) x"
       show (parseTerm "" "f x (g y)")        @?= "Right f x (g y)"
       show (parseTerm "" "f (x g y)")        @?= "Right f (x g y)"
       show (parseTerm "" "f λx .x")          @?= "Right f (λx. x)"
       show (parseTerm "" "f (λx .x)")        @?= "Right f (λx. x)"
       show (parseTerm "" "(λx .x) x")        @?= "Right (λx. x) x"
       show (parseTerm "" "λx .x x")          @?= "Right λx. x x"
       show (parseTerm "" "(λx .x) (λ y. y)") @?= "Right (λx. x) (λy. y)"
       show (parseTerm "" "(λx .x) λ y. y")   @?= "Right (λx. x) (λy. y)"
       show (parseTerm "" "λx .x (λ y. y)")   @?= "Right λx. x (λy. y)"
       show (parseTerm "" "λx .x λ y. y")     @?= "Right λx. x (λy. y)"
  ]

