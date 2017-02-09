
\section{Simple Tests}
\label{sec:simpletests}

We now use QuickCheck

\begin{code}
module Main where

import Basics

import Text.Printf (printf)
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import System.Exit (exitSuccess, exitFailure)

tests :: [(String, IO Result)]
tests =
  [ ("funnytest1", quickCheckResult (\n -> funnyfunction n `elem` [42, n*100, (n-1)*100]) )
  , ("reversetest", quickCheckResult (\str -> myreverse (myreverse str) == (str::String)) )
  ]

main :: IO ()
main  = do
  results <- mapM (\(s,a) -> printf "\n%-20s: " s >> a) tests
  if all isSuccess results
    then exitSuccess
    else exitFailure
\end{code}

To run this, use \texttt{stack clean; stack test --coverage}. In particular
this will generate a nice report using hpc. Look for ``The coverage report
for ... is available at ... .html'' and open this file in your browser. See
\url{https://wiki.haskell.org/Haskell_program_coverage} for more examples and
information how to read the report.
