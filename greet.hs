module Main where
import System.IO

greet = do 
    putStrLn "What is your name?"
    name <- getLine
    putStrLn $ "Hi, " ++ name
    
main = greet
