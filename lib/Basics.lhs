
\section{The most basic library}\label{sec:Basics}

This section describes a module which we will import later on.

\begin{code}
module Basics where

import Control.Monad
import System.Random
import qualified Data.Map as Map
import Control.Monad.State
import Data.List (union)
import Data.Functor.Identity (Identity, runIdentity)

thenumbers :: [Integer]
thenumbers = [1..]

somenumbers :: [Integer]
somenumbers = take 10 thenumbers

randomnumbers :: IO [Integer]
randomnumbers = replicateM 10 $ randomRIO (0,10)
\end{code}

We can interrupt the code anywhere we want.

\begin{code}
funnyfunction :: Integer -> Integer
funnyfunction 0 = 42
\end{code}

Even in between cases, like here.
It's always good to cite something \cite{Knuth11CombAlg}.

\begin{code}
funnyfunction n | even n    = funnyfunction (n-1)
                | otherwise = n*100
\end{code}

Something to reverse lists.

\begin{code}
myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]
\end{code}

That's it, for now.

\begin{code}

-- Define VarMap as a type alias for Map.Map String Int
type VarMap = Map.Map String Int

-- Define the data type for lambda calculus expressions
data Expr = Var Int           -- Using Int to encode variables
          | Abs Int Expr      -- Abstractions also use Ints for variable names
          | App Expr Expr     -- Application remains unchanged
          deriving (Eq, Show)

-- Function to perform substitution
substitute :: Int -> Expr -> Expr -> Expr
substitute x with expr = case expr of
    Var y -> if y == x then with else Var y
    Abs y body -> if y == x then Abs y body else Abs y (substitute x with body)
    App e1 e2 -> App (substitute x with e1) (substitute x with e2)

-- Function to perform eta reduction
etaReduce :: Expr -> (Expr, Bool)
etaReduce (Abs x (App f (Var y)))
    | x == y && x `notElem` freeVars f = (f, True)
    | otherwise = let (e, changed) = etaReduce (App f (Var y))
                  in (Abs x e, changed)
etaReduce (Var x) = (Var x, False)
etaReduce (Abs x e) = let (e', changed) = etaReduce e in (Abs x e', changed)
etaReduce (App e1 e2) = let (e1', changed1) = etaReduce e1
                            (e2', changed2) = etaReduce e2
                        in (App e1' e2', changed1 || changed2)

-- Function to perform beta reduction with a limit to recursion depth
betaReduce :: Expr -> Int -> (Expr, Bool)
betaReduce expr 0 = (expr, False) -- Stop reducing after depth limit reached
betaReduce expr depth = case expr of
  Var _ -> (expr, False)
  Abs x e -> let (e', changed) = betaReduce e (depth - 1) in (Abs x e', changed)
  App (Abs x body) arg -> let result = substitute x arg body
                              (reducedResult, changed) = betaReduce result (depth - 1)
                          in (reducedResult, True)
  App e1 e2 -> let (e1', changed1) = betaReduce e1 (depth - 1)
                   (e2', changed2) = betaReduce e2 (depth - 1)
               in (App e1' e2', changed1 || changed2)

-- Generates a new variable name not in the list
freshVar :: [Int] -> Int -> Int
freshVar vars base = head $ filter (`notElem` vars) $ map (\i -> base + i) [1..]

-- -- Function to perform alpha conversion to avoid capture
-- alphaConvert :: Int -> Expr -> Expr
-- alphaConvert x (Abs y body)
--     | x == y = let newY = freshVar (freeVars body) y
--                in Abs newY (substitute y (Var newY) body)
--     | otherwise = Abs y (alphaConvert x body)
-- alphaConvert x (App e1 e2) = App (alphaConvert x e1) (alphaConvert x e2)
-- alphaConvert _ expr = expr

alphaConvert :: Int -> Expr -> StateT VarMap Identity Expr
alphaConvert x (Abs y body)
    | x == y = do
        let newY = freshVar (freeVars body) y
        modify (Map.insert (show y) newY)  -- Assuming y is an Int and needs to be converted to String
        body' <- alphaConvert x body
        return $ Abs newY body'
    | otherwise = do
        body' <- alphaConvert x body
        return $ Abs y body'
alphaConvert x (App e1 e2) = do
    e1' <- alphaConvert x e1
    e2' <- alphaConvert x e2
    return $ App e1' e2'
alphaConvert _ expr = return expr

-- Example of running alphaConvert with an initial empty VarMap
runAlphaConvert :: Expr -> Expr
runAlphaConvert expr = runIdentity (evalStateT (alphaConvert 1 expr) Map.empty)


-- Collects all free variables in an expression, now returns a list of Int
freeVars :: Expr -> [Int]
freeVars (Var x) = [x]
freeVars (Abs x e) = filter (/= x) (freeVars e)
freeVars (App e1 e2) = freeVars e1 `union` freeVars e2

-- Helper function to print and reduce expressions
printAndReduce :: Expr -> IO ()
printAndReduce expr = do
    putStrLn "Original Expression:"
    print expr
    let (reducedExprBeta, betaChanged) = betaReduce expr 10  -- Reduced depth for safety in Omega
    when betaChanged $ do
      putStrLn "After Beta Reduction:"
      print reducedExprBeta
    let (reducedExprEta, etaChanged) = etaReduce reducedExprBeta
    when etaChanged $ do
      putStrLn "After Eta Reduction:"
      print reducedExprEta

\end{code}
