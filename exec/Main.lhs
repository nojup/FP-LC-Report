
\section{Wrapping it up in an exectuable}\label{sec:Main}

We will now use the library form Section \ref{sec:Basics} in a program.

\begin{code}
module Main where

import Basics

main :: IO ()
main = do
  -- Example 1: Identity Function
  putStrLn "Example 1: Identity Function"
  let identity = Abs 1 (Var 1)
  let expr1 = App identity (Var 2)
  printAndReduce expr1

  -- Example 2: Combination of Functions
  putStrLn "\nExample 2: Combination of Functions"
  let function = Abs 1 (Abs 2 (App (Var 1) (Var 2)))
  let expr2 = App (App function (Var 3)) (Var 4)
  printAndReduce expr2

  -- Example 3: Omega Combinator (non-terminating)
  putStrLn "\nExample 3: Omega Combinator"
  let omega = App (Abs 1 (App (Var 1) (Var 1))) (Abs 1 (App (Var 1) (Var 1)))
  printAndReduce omega

  -- Example 4: Church Numerals (Two and Three)
  putStrLn "\nExample 4: Church Numerals (Two applied to Three)"
  let two = Abs 1 (Abs 2 (App (Var 1) (App (Var 1) (Var 2))))
  let three = Abs 1 (Abs 2 (App (Var 1) (App (Var 1) (App (Var 1) (Var 2)))))
  let expr4 = App two three
  printAndReduce expr4

  -- Example 5: S and K Combinators
  putStrLn "\nExample 5: S and K Combinators"
  let sComb = Abs 1 (Abs 2 (Abs 3 (App (App (Var 1) (Var 3)) (App (Var 2) (Var 3)))))
  let kComb = Abs 1 (Abs 2 (Var 1))
  let expr5 = App (App sComb kComb) identity
  printAndReduce expr5
\end{code}

We can run this program with the commands:

\begin{verbatim}
stack build
stack exec myprogram
\end{verbatim}

The output of the program is something like this:

\begin{verbatim}
Hello!
[1,2,3,4,5,6,7,8,9,10]
[100,100,300,300,500,500,700,700,900,900]
[1,3,0,1,1,2,8,0,6,4]
[100,300,42,100,100,100,700,42,500,300]
GoodBye
\end{verbatim}
