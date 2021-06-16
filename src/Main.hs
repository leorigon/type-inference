module Main where

import Lambda (infer)
import Parser (parseExpr)

main = do
  putStrLn "---------------------------------------------------------"
  putStrLn "Insert the name of the file with a Lambda expression!"
  putStrLn "---------------------------------------------------------\n"
  fileName <- getLine
  putStrLn "---------------------------------------------------------\n"
  body <- readFile fileName
  putStrLn ("Lambda Expression:\n" ++ body)
  let parsed = parseExpr body
  putStrLn ("Parsed Expression:\n" ++ show parsed)
  let result = infer parsed
  putStrLn ("Typed Expression:\n" ++ show result)