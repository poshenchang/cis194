-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Week05.Calc where

import Week05.ExprT as E
import Week05.Parser
import Week05.StackVM as VM
import qualified Data.Map as M

-- Exercise 1

-- evaluate given expression
-- eval (E.Mul (E.Add (E.Lit 2) (E.Lit 3)) (E.Lit 4)) == 20
eval :: ExprT -> Integer
eval (E.Lit x) = x
eval (E.Add x y) = eval x + eval y
eval (E.Mul x y) = eval x * eval y

-- Exercise 2

-- evaluate arithmetic expressions given as a String,
-- producing Nothing for inputs which are not well-formed expressions,
-- and Just n for well-formed inputs that evaluate to n.
-- evalStr "(2+3)*4" == Just 20
-- evalStr "2+3*4" == Just 14
-- evalStr "2+3*" == Nothing
evalStr :: String -> Maybe Integer
evalStr = (eval <$>) . parseExp E.Lit E.Add E.Mul

-- Exercise 3

-- abstractify the properties of ExprT with a type class Expr
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = E.Lit
    add  = E.Add
    mul = E.Mul

-- Exercise 4

-- Make instances of Expr for each of the following types

-- Integer
-- works like the original calculator
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

-- Bool
-- every literal value less than or equal to 0 is 
-- interpreted as False, and all positive Integers
-- are interpreted as True; “addition” is logical or,
-- “multiplication” is logical and.
instance Expr Bool where
    lit = (>= 0)
    add = (||)
    mul = (&&)

-- MinMax
-- “addition” is taken to be the max function, while
-- “multiplication” is the min function

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax $ max x y
    mul (MinMax x) (MinMax y) = MinMax $ min x y

-- Mod7
-- all values should be in the ranage 0 . . . 6, and
-- all arithmetic is done modulo 7; for example,
-- 5 + 3 = 1.
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit = Mod7 . (`mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 ((x+y) `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7 ((x*y) `mod` 7)

-- testExp :: Maybe Integer == Just (-7)
-- testExp :: Maybe Bool    == Just True
-- testExp :: Maybe MinMax  == Just (MinMax 5)
-- testExp :: Maybe Mod7    == Just (Mod7 7)
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

-- Exercise 5

-- create an instance of the Expr type class for Program
instance Expr Program where
    lit x = [PushI x]
    add x y = x ++ y ++ [VM.Add]
    mul x y = x ++ y ++ [VM.Mul]

-- a compiler for arithmetic expressions, based on the architecture
-- provided in module StackVM
compile :: String -> Maybe Program
compile = parseExp lit add mul

-- Exercise 6

-- instances of HasVars have some notion of named variables.
-- add (lit 3) (var "x") :: (Expr a, HasVars a) => a
class HasVars a where
    var :: String -> a

-- new data type VarExprT which is the same as ExprT
-- but with an extra constructor for variables.
-- add (lit 3) (var "x") :: VarExprT == VarAdd (VarLit 3) (Var "x")
data VarExprT = VarLit Integer
              | VarAdd VarExprT VarExprT
              | VarMul VarExprT VarExprT
              | Var String
    deriving (Show, Eq)

instance HasVars VarExprT where
    var = Var

instance Expr VarExprT where
    lit = VarLit
    add = VarAdd
    mul = VarMul

-- interpret expressions containing variables, given a suitable mapping
-- from variables to values.
-- withVars [("x", 6)] $ add (lit 3) (var "x") == Just 9
-- withVars [("x", 6)] $ add (lit 3) (var "y") == Nothing
-- withVars [("x", 6), ("y", 3)] 
--   $ mul (var "x") (add (var "y") (var "x")) == Just 54
instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit x _ = Just x
    add f g m = (+) <$> f m <*> g m
    mul f g m = (*) <$> f m <*> g m

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs