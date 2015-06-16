-- | A simple non-standard scheme implementation.
module Scheme where

import Data.Map as Map
import Control.Monad.State as State
-- import System.IO
import Text.Parsec
-- import Text.Parsec.String
-- import Data.List
import Data.Char

-- | Any legal Scheme token.
data SchemeValue = SchemeInteger Integer
                 | SchemeSymbol String
                 | SchemeCons HeapPointer HeapPointer
                 deriving (Show, Eq)
                          
-- | A pointer to the heap.
data HeapPointer = HeapPointer Integer
                 | SchemeNil
                 deriving (Show, Eq)
                          
instance Ord HeapPointer where
  compare SchemeNil SchemeNil = EQ
  compare SchemeNil b = LT
  compare a SchemeNil = GT
  compare (HeapPointer a) (HeapPointer b) = compare a b
  
-- | The type of a char parser w/ the scheme heap as user state.
  -- Should return a pointer to the element parsed.
type SchemeParser = Parsec String Heap HeapPointer

data Heap = Heap { heap :: Map HeapPointer SchemeValue,
                   heapIndex :: HeapPointer }
          deriving (Show)
                   
newHeap :: Heap
newHeap = Heap (fromList []) (HeapPointer 0)

addToHeap :: SchemeValue -> Heap -> Heap
addToHeap v (Heap h o@(HeapPointer i)) =
  Heap (Map.insert o v h) (HeapPointer (1 + i))

schemeParser :: SchemeParser
schemeParser = try schemeIntegerParser <|>
               try schemeCharParser <|>
               try schemeSymbolParser <|>
               ((char '(') >> schemeListParser)

-- | Parser for scheme S-expressions.
schemeListParser :: SchemeParser
schemeListParser = do
  _ <- skipMany space
  car <- try schemeIntegerParser <|>
         try schemeCharParser <|>
         try schemeSymbolParser <|>
         try ((char '(') >> schemeListParser) <|>
         schemeNilParser
  case car of
   SchemeNil -> return SchemeNil
   (HeapPointer a) -> do
     cdr <- try schemeListParser <|>
            schemeNilParser
     (Heap h i) <- getState
     modifyState (addToHeap (SchemeCons car cdr))
     return i

-- | Parser for the end of a list.
schemeNilParser :: SchemeParser
schemeNilParser = (char ')') >> return SchemeNil
               
-- | Parser for scheme integers.
schemeIntegerParser :: SchemeParser
schemeIntegerParser = do
  (Heap h i) <- getState
  skipMany space
  token <- many1 digit
  modifyState $ (addToHeap $ SchemeInteger . (read :: String -> Integer) $
                 token)
  return i

-- | Parser for scheme chars.
schemeCharParser :: SchemeParser
schemeCharParser = do
  (Heap h i) <- getState
  skipMany space
  string "#\\"
  token <- letter
  modifyState (addToHeap $ SchemeInteger . fromIntegral . ord $ token)
  return i

-- | Parser for scheme symbols.
schemeSymbolParser :: SchemeParser
schemeSymbolParser = do
  (Heap h i) <- getState
  skipMany space
  token <- many1 letter
  modifyState (addToHeap $ SchemeSymbol token)
  return i

schemeTopLevelParser :: Parsec String Heap (Heap, HeapPointer)
schemeTopLevelParser = do
  e <- schemeParser
  t <- getState
  return (t, e)
  
-- | The environment containing the runtime data.
data SchemeEnvironment = SchemeEnvironment { runtimeHeap :: Heap,
                                             environment :: HeapPointer,
                                             continuation :: HeapPointer }
                       deriving (Show)
                                
-- | The scheme runtime state monad.
type SchemeMonad a = State.State SchemeEnvironment a

-- | Allocate an atomic value on the heap.
allocSchemeAtomic :: SchemeValue -> SchemeMonad HeapPointer
allocSchemeAtomic val = do
  (Heap heap' index'@(HeapPointer indexVal)) <- gets runtimeHeap
  state' <- get
  put $ state' { runtimeHeap = Heap (Map.insert index' val heap')
                               (HeapPointer $ indexVal + 1) }
  return index'

-- | Dereference a heap pointer.
dereference :: HeapPointer -> SchemeMonad SchemeValue
dereference SchemeNil = return $ SchemeSymbol "nil"
dereference pointer = do
  (Heap heap' _) <- gets runtimeHeap
  let maybeAtom = Map.lookup pointer heap'
  case maybeAtom of
   Nothing -> error "Dangling pointer."
   (Just atom) -> return atom
   
-- | A function with this type can be used as a scheme primitive function
  -- by apply.
type SchemePrimitive = HeapPointer -> SchemeMonad HeapPointer

-- | Allocate a cons on the heap
primitiveCons :: HeapPointer -> HeapPointer -> SchemeMonad HeapPointer
primitiveCons carp cdrp = do
  allocSchemeAtomic $ SchemeCons carp cdrp

cons :: SchemePrimitive
cons argp = do
  carp <- car argp
  cdrp <- (\argp -> car argp >>= cdr) argp
  primitiveCons carp cdrp
  
car :: SchemePrimitive
car e = do
  c <- dereference e
  case c of
   (SchemeCons p _) -> return p
   otherwise -> error $ "Car applied to non-cons: " ++ (show c)
   
cdr :: SchemePrimitive
cdr e = do
  c <- dereference e
  case c of
   (SchemeCons _ p) -> return p
   otherwise -> error $ "Cdr applied to non-cons: " ++ (show c)

-- | Construct a closure.
lambda :: SchemePrimitive
lambda argp = do
  formalArguments <- car argp
  functionBody <- cdr argp
  env <- gets environment
  primitiveCons formalArguments functionBody >>= primitiveCons env

-- | Evaluate a list of statements and return the result of the last one.
begin :: SchemePrimitive
begin argp = do
  r <- car argp >>= eval
  n <- cdr argp
  case n of
   SchemeNil -> return r
   otherwise -> begin n
  
-- | Perform function application.
apply :: SchemePrimitive
apply argp = do
  funp <- car argp
  fun <- dereference funp
  case fun of
   (SchemeSymbol "nil") -> error "Attempted to apply NIL."
   (SchemeSymbol "lamba") -> lambda funp
   --(SchemeSymbol "define") -> define cdr'
   otherwise -> do
     (closure:args) <- evalList funp
     formals <- car closure
     closureEnvironment <- cdr closure >>= car
     closureBody <- cdr closure >>= cdr >>= car
     s <- get
     env <- gets environment
     put $ s { environment = closureEnvironment,
               continuation = env }
     bindFormals formals args
     r <- begin closureBody
     s <- get
     envn <- gets continuation
     put $ s { environment = envn }
     return r
     where evalList :: HeapPointer -> SchemeMonad [HeapPointer]
           evalList argp = do
             e <- car argp
             r <- eval e
             n <- cdr argp
             case n of
              SchemeNil -> return []
              otherwise -> do
                rn <- evalList n
                return $ r : rn
           bindFormals :: HeapPointer -> [HeapPointer] -> SchemeMonad ()
           bindFormals SchemeNil [] = return ()
           bindFormals SchemeNil _ = error "Too many arguments to function."
           bindFormals _ [] = error "Too few arguments to function."
           bindFormals formalsp (arg:argpv) = do
             argp <- car formalsp
             primitiveDefine argp arg
             nf <- cdr formalsp
             bindFormals nf argpv

-- | Primitive define.
primitiveDefine :: HeapPointer -> HeapPointer -> SchemeMonad ()
primitiveDefine symbolp valp = do
  env <- gets environment
  s <- get
  nc <- primitiveCons symbolp valp
  nenv <- primitiveCons nc env
  put $ s { environment = nenv}
  return ()
                
-- | Looks up a value in an association list.
assoc :: SchemePrimitive
assoc argp = do
  valp <- car argp
  consp <- cdr argp
  assocPair <- car consp
  k <- car assocPair
  if valp == k
    then cdr assocPair
    else do
    nextPair <- cdr consp
    if nextPair == SchemeNil
      then return SchemeNil
      else assoc nextPair
     
resolve :: SchemePrimitive
resolve argp = do
  env <- gets environment
  res <- assoc env
  val <- dereference res
  case res of
   SchemeNil -> error $ "Unbound symbol: " ++ (show res)
   otherwise -> return res
   
eval :: SchemePrimitive
eval SchemeNil = return $ SchemeNil
eval a = do
  s <- dereference a
  case s of
   (SchemeSymbol _) -> resolve a
   (SchemeInteger _) -> return a
   (SchemeCons _ _) -> apply a

read' :: String -> SchemeMonad HeapPointer
read' str = do
  h <- gets runtimeHeap
  let r = runParser schemeTopLevelParser h "" str
  case r of
   (Right (newHeap, e)) -> do
     s <- get
     put $ s { runtimeHeap = newHeap }
     return e
   (Left e) -> error $ "Parser error: " ++ (show e)
 
example str = do
  let (Right (heap, heapPointer)) = runParser schemeTopLevelParser (Heap (fromList []) (HeapPointer 0)) "" str
  runState (eval heapPointer) $ SchemeEnvironment heap SchemeNil SchemeNil
