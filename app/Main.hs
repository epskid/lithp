module Main (main) where

import Lex

main :: IO ()
main = print $ lexer "(print \"\" (+ -seeveen67 '(1 2 3) 7) -6.7 -123)"
