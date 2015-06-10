-- A simple non-standard scheme implementation.
module Scheme where

import Data.Map as Map
import Control.Monad.State as State
import System.IO
import Text.Parsec
import Text.Parsec.String
import Data.List

-- | Any legal Scheme token.
data SchemeToken = SchemeInteger Integer
                 | SchemeChar Char
                 | SchemeSymbol String
                 | SchemeSyntaxTree [SchemeToken]
                   
instance Show SchemeToken where
  show (SchemeInteger n) = show n
  show (SchemeChar c) = "#\\" ++ (:[]) c
  show (SchemeSymbol str) = str
  show (SchemeSyntaxTree ls) = "(" ++
                         (concat . intersperse " " $ (Prelude.map show ls))
                         ++ ")" 

-- | Parser for scheme expressions.
schemeParser :: Parser SchemeToken
schemeParser = try schemeIntegerParser <|>
               try schemeCharParser <|>
               try schemeSymbolParser <|>
               schemeListParser

-- | Parser for scheme integers.
schemeIntegerParser :: Parser SchemeToken
schemeIntegerParser = skipMany space >>
                      many1 digit >>=
                      return . SchemeInteger . (read :: String -> Integer)

-- | Parser for scheme chars.
schemeCharParser :: Parser SchemeToken
schemeCharParser = skipMany space >>
                   string "#\\" >>
                   letter >>=
                   return . SchemeChar

-- | Parser for scheme symbols.
schemeSymbolParser :: Parser SchemeToken
schemeSymbolParser = skipMany space >>
                     many1 letter >>=
                     return . SchemeSymbol

-- | Parser for S-expressions.
schemeListParser :: Parser SchemeToken
schemeListParser = do
  skipMany space
  _ <- char '('
  tokenList <- many schemeParser
  _ <- char ')'
  return $ SchemeSyntaxTree tokenList

-- | The environment containing the runtime data.
data SchemeEnvironment = SchemeEnvironment { heap :: Heap,
                                             hindex :: Integer,
                                             winding :: [Integer] }
                       deriving (Show)

-- | A heap containing runtime data types.
newtype Heap = Heap (Map Integer SchemeDatatype)
             deriving (Show)

-- | A map of bindings from symbols to pointers to the heap.
newtype Bindings = Bindings (Map String Integer)
                 deriving (Show)

-- | Runtime data types.
data SchemeDatatype = SchemeHeapInteger Integer
                    | SchemeHeapChar Char
                    | SchemeHeapSymbol String
                    | SchemeHeapCons Integer Integer
                    | SchemeHeapEnvironment Bindings
                    | SchemeLambda Integer [String] SchemeToken
                    deriving (Show)

-- | The scheme runtime state monad.
type SchemeMonad a = State.State SchemeEnvironment a

-- | Look up a symbol binding.
lookupSymbol :: SchemeToken -> SchemeMonad SchemeDatatype
lookupSymbol (SchemeSymbol s) = do
  c <- gets winding
  p <- mapM lookupHeap c
  let r = Map.lookup s (Data.List.foldl1 Map.union $ (Prelude.map (\(SchemeHeapEnvironment (Bindings e)) -> e) $ p))
  case r of
   (Just r) -> lookupHeap r
   Nothing -> do
     e <- get
     error $ "Symbol " ++ s ++ "not found" ++ (show e)
     
lookupSymbol t = error $ (show t) ++ " is not a symbol"

-- | Look up a heap pointer.
lookupHeap :: Integer -> SchemeMonad SchemeDatatype
lookupHeap n = do
  (Heap t) <- gets heap
  let r = Map.lookup n t
  case r of
   (Just r) -> return r
   Nothing -> error $ "Dangling pointer at " ++ (show n)

-- | Evaluate scheme expressions.
eval :: SchemeToken -> SchemeMonad SchemeDatatype
eval (SchemeInteger n) = return $ SchemeHeapInteger n
eval (SchemeChar c) = return $ SchemeHeapChar c
eval t@(SchemeSymbol c) = lookupSymbol t
eval t@(SchemeSyntaxTree _) = apply t

-- | Evaluate a "body" of expressions, returning the final one.
evalList :: SchemeToken -> SchemeMonad SchemeDatatype
evalList (SchemeSyntaxTree (t : [])) = do
  r <- eval t
  w <- gets winding
  c <- get
  if length w /= 0
    then do
    put $ c { winding = tail w }
    return r
    else return r
evalList (SchemeSyntaxTree (t : ax)) = eval t >> evalList (SchemeSyntaxTree ax)

-- | Perform function application.
apply :: SchemeToken -> SchemeMonad SchemeDatatype
apply (SchemeSyntaxTree (fn : args)) = do
  case fn of
   (SchemeSymbol "define") -> do
     if length args == 2
       then do
       r <- eval (args !! 1)
       define (args !! 0) r
       return $ SchemeHeapCons 0 0
       else error "Wrong number of arguments to define"
   (SchemeSymbol "lambda") -> do
     if length args >= 2
       then do
       closure <- allocateEnvironment
       return $ SchemeLambda closure (makeFormalParams (args !! 0)) (SchemeSyntaxTree (tail args))
       else error "Wrong number of arguments to lambda"
   (SchemeSymbol "add") -> do
     if length args >= 2
       then do
       e <- mapM eval args
       return $ SchemeHeapInteger $ addition e
       else error "Wrong number of arguments to add"
   otherwise -> do
     n <- eval fn
     case n of
      (SchemeLambda environment formalParams ast) -> do
        enter environment
        v <- mapM eval args
        bindFormalParams formalParams v
        evalList ast
      otherwise -> error $ (show n) ++ "is not a function" 
       
apply _ = error "Not a function call"

-- | Create a new environment on the heap
allocateEnvironment :: SchemeMonad Integer
allocateEnvironment = do
  (Heap s) <- gets heap
  i <- gets hindex
  a <- get
  put $ a { heap = Heap $ Map.insert i (SchemeHeapEnvironment $ Bindings $ fromList []) s,
      hindex = 1 + i }
  return i

-- | Primitive addition.
addition :: [SchemeDatatype] -> Integer
addition [] = 0
addition ((SchemeHeapInteger n) : nx) = n + addition nx
addition _ = error "Not an integer in addition"

-- | Primitive define.
define :: SchemeToken -> SchemeDatatype -> SchemeMonad ()
define (SchemeSymbol a) v = do
  (b : bx) <- gets winding
  (Heap h) <- gets heap
  d <- gets hindex
  t <- get
  (SchemeHeapEnvironment (Bindings n)) <- lookupHeap b
  let e = SchemeHeapEnvironment $ Bindings $ Map.insert a d n
  put $ t { heap = Heap $ Map.insert b e $ Map.insert d v h,
            hindex = (1 + d) }
    
define a _ = error $ "Not a symbol: " ++ (show a)

-- | Verify list of formal parameters in lambda expressions.
makeFormalParams :: SchemeToken -> [String]
makeFormalParams (SchemeSyntaxTree []) = []
makeFormalParams (SchemeSyntaxTree ((SchemeSymbol a) : ax)) = a : makeFormalParams (SchemeSyntaxTree ax)
makeFormalParams _ = error "Error in formal parameter list"

-- | Bind formal parameters.
bindFormalParams :: [String] -> [SchemeDatatype] -> SchemeMonad ()
bindFormalParams formals params = do
  if length formals == length params
    then do
    mapM_ (\(a, b) -> define (SchemeSymbol a) b) $ zip formals params
    else error "Wrong number of arguments to function"

-- | Enter an environment.
enter :: Integer -> SchemeMonad ()
enter bn = do
  t <- gets winding
  a <- get 
  put $ a { winding = bn : t }

example str = runState (evalList $ (\(Right a) -> a) $ parse schemeParser "" str) $ SchemeEnvironment (Heap $ fromList [(1, SchemeHeapEnvironment $ Bindings $ fromList [])]) 2 [1]
