-- | Test suite for Scheme module.
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
         testCase "primitiveCons" testPrimitiveCons,
         testCase "assoc" testAssoc
       ] mempty

-- | Test for car.
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

-- | Test for cdr.
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

-- | Test for primitive cons.
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

-- | Test for assoc.
testAssoc :: Assertion
testAssoc = do
  let (r, (SchemeEnvironment (Heap heap index) env cont)) =
        runState (assoc (HeapPointer 5) >>= dereference) $
        SchemeEnvironment
        (Heap (fromList [(HeapPointer 0, SchemeInteger 1),
                         (HeapPointer 1, SchemeInteger 2),
                         (HeapPointer 2, SchemeCons
                                         (HeapPointer 0)
                                         (HeapPointer 1)),
                         (HeapPointer 3, SchemeCons
                                         (HeapPointer 2)
                                         SchemeNil),
                         (HeapPointer 4, SchemeInteger 1),
                         (HeapPointer 5, SchemeCons
                                         (HeapPointer 4)
                                         (HeapPointer 6)),
                         (HeapPointer 6, SchemeCons
                                         (HeapPointer 3)
                                         SchemeNil)])
         (HeapPointer 7)) SchemeNil SchemeNil
  r @?= SchemeInteger 2
