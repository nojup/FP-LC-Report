
\section{The most basic library}
\label{sec:Basics}

This section describes a module which we will import later on.

\begin{code}
module Basics where

thenumbers :: [Integer]
thenumbers = [1..]

somenumbers :: [Integer]
somenumbers = take 10 thenumbers
\end{code}

We can interrupt the code anywhere we want.

\begin{code}
funnyfunction :: Integer -> Integer
funnyfunction 0 = 42
\end{code}

Even in between cases, like here.
It's always good to cite something \cite{Knuth11CombAlg}.

\begin{code}
funnyfunction n | even n     = funnyfunction (n-1)
                | otherwise = n*100
\end{code}

That's it, for now.
