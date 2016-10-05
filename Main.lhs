
\section{Wrapping it up in an exectuable}
\label{sec:Main}

We will now use the library form Section \ref{sec:Basics} in a program.

\begin{code}
module Main where

import Basics

main :: IO ()
main = do
  putStrLn "Hello!"
  print somenumbers
  print (map funnyfunction somenumbers)
  putStrLn "GoodBye"
\end{code}

The output of the program is:

\begin{showCode}
Hello!
[1,2,3,4,5,6,7,8,9,10]
[100,100,300,300,500,500,700,700,900,900]
GoodBye
\end{showCode}

Note that the above \texttt{showCode} block is only shown, but it gets
ignored by the Haskell compiler.
