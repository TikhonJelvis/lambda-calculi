module Test.Untyped where

import           Control.Applicative ((<$>), (<*>))

import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import           Untyped

tests = testGroup "Untyped" [prettyPrinting]

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
        
