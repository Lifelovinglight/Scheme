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
       [ testCase "car" testCar,
         testCase "cdr" testCdr,
         testCase "primitiveCons" testPrimitiveCons
       ] mempty

testCar :: Assertion
testCar = do
  let (r, env) = runState (car (HeapPointer 1) >>= dereference) $
                 SchemeEnvironment
                 (Heap (fromList [(HeapPointer 0, SchemeInteger 1),
                                  (HeapPointer 1, SchemeCons
                                                  (HeapPointer 0)
                                                  SchemeNil)])
                  (HeapPointer 2)) SchemeNil SchemeNil
  r @?= SchemeInteger 1
  
testCdr :: Assertion
testCdr = do
  let (r, env) = runState (cdr (HeapPointer 1) >>= dereference) $
                 SchemeEnvironment
                 (Heap (fromList [(HeapPointer 0, SchemeInteger 1),
                                  (HeapPointer 1, SchemeCons
                                                  SchemeNil
                                                  (HeapPointer 0))])
                  (HeapPointer 2)) SchemeNil SchemeNil
  r @?= SchemeInteger 1
  
testPrimitiveCons :: Assertion
testPrimitiveCons = do
  let (r, (SchemeEnvironment (Heap heap index) env cont)) =
        runState (primitiveCons (HeapPointer 0) (HeapPointer 1)) $
        SchemeEnvironment
        (Heap (fromList [(HeapPointer 0, SchemeInteger 1),
                         (HeapPointer 1, SchemeInteger 2)])
         (HeapPointer 2)) SchemeNil SchemeNil
  r @?= HeapPointer 2
  index @?= HeapPointer 3
  (toList heap) @?= [(HeapPointer 0, SchemeInteger 1),
                     (HeapPointer 1, SchemeInteger 2),
                     (HeapPointer 2, SchemeCons
                                     (HeapPointer 0)
                                     (HeapPointer 1))]

-- testAssoc :: Assertion
-- testAssoc = do
