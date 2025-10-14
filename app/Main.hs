module Main (main) where

import Lex

main :: IO ()
main = print $ lexer "(print \"hello\" (+ seeveen '(1 2 3) 7) -6.7 -123)"
