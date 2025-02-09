module Week08.Party where

import Data.Monoid
import Data.Tree
import Data.List (sort)
import Week08.Employee
import qualified Data.Graph as Data

-- Exercise 1

-- adds an Employee to the GuestList
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)

instance Semigroup GuestList where
    (GL e1s f1) <> (GL e2s f2) = GL (e1s ++ e2s) (f1+f2)

instance Monoid GuestList where
    mempty = GL [] 0

-- returns whichever GuestList is more fun
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2

-- fold for type Data.Tree
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x ts) = f x (map (treeFold f) ts)

-- Exercise 3

-- recursion on computing a GuestList pair on computing:
-- 1. maximal guest list if we invite the boss
-- 2. maximal guest list if we don't invite the boss
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e xs = (glCons e (mconcat $ map snd xs), mconcat $ map fst xs)

-- Exercise 4

-- takes a company hierarchy as input and outputs a fun-maximizing guest list
maxFun :: Tree Employee -> GuestList
maxFun = uncurry max . treeFold nextLevel

-- Exercise 5

inFile :: FilePath
inFile = "Week08/company.txt"

-- show guest list as readable format.
-- employee names are sorted by lexicographical order.
printGL :: GuestList -> String
printGL (GL xs y) = unlines $ ("Total fun: " ++ show y) :
                    sort (map empName xs)

-- main data processing logic
processData :: String -> String
processData = printGL . maxFun . read

main :: IO ()
main = do
       companyData <- readFile inFile
       putStrLn $ processData companyData