module Main where

import Scheme
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import Data.Map
import Control.Monad.State

main :: IO ()
main = defaultMainWithOpts
       [ testCase "car" testCar
       ] mempty

testCar :: Assertion
testCar = do
  let (r, env) = runState (car (HeapPointer 1) >>= dereference) $ SchemeEnvironment (Heap (fromList [(HeapPointer 0, SchemeInteger 1), (HeapPointer 1, SchemeCons (HeapPointer 0) SchemeNil)]) (HeapPointer 2)) SchemeNil SchemeNil
  r @?= SchemeInteger 1
