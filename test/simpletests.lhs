
\section{Simple Tests}
\label{sec:simpletests}

We now use the library QuickCheck to randomly generate input for our functions
and test some properties.

\begin{code}
module Main where

import Basics

import Text.Printf (printf)
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import System.Exit (exitSuccess, exitFailure)
\end{code}

The tuples in the following list consist of a name for the test and a
quickCheckResult applied to a property. For example, we check that the output
of funnyfunction is one of three possibilities. The second test checks that
reversing twice gives the same string back.

% TODO: add tests using thenumbers, somenumbers, randomnumbers

\begin{code}
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

To run this, use \verb|stack clean && stack test --coverage|. In particular
this will generate a nice report using hpc. Look for ``The coverage report
for ... is available at ... .html'' and open this html file in your browser.
See \url{https://wiki.haskell.org/Haskell_program_coverage} for more examples
and information how to read the report.
