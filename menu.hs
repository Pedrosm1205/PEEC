module Main where

import Grammars
import System.IO (hFlush, stdout)


menu :: IO ()
menu = do
    putStrLn "Write a type: " 
    input <- getLine
    let type_input = myParser input
    hFlush stdout
    putStrLn "1. occT"
    putStrLn "2. polarities"
    putStrLn "3. rules m => k.n"
    putStrLn "4. rules m => p0n1...ns"
    putStrLn "5. pre-grammar rules"
    putStrLn "6. Exit"
    putStr "Enter your choice: "
    hFlush stdout  
    choice <- getLine
    case choice of
        "1" -> do
            let output = occT type_input 0
            print output
            menu  
        "2" -> do
            let output = polarities (occT type_input 0)
            print output
            menu
        "3" -> do
            let output = pre_grammar1 (occT type_input 0) (polarities (occT type_input 0))
            print output
            menu 
        "4" -> do
            let output = pre_grammar2 (occT type_input 0) (polarities (occT type_input 0))
            print output
            menu 
        "5" -> do
            let output = pre_grammar type_input
            print output
            menu         
        "6" -> putStrLn "Exit"
        _   -> do
            putStrLn "Invalid choice, try again."
            menu

main :: IO ()
main = menu
