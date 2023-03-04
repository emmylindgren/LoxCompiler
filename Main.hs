module Main (main) where

import Scanner (scanTokens)
import Parser(parse)
import Interpreter(interpret)

main :: IO ()
{-
main =
    let filePath = "C:\\Users\\Emmy\\Documents\\Programspråk\\OU1\\loxsource.lox" in
        readFile filePath >>= \s -> printTokens s
-}
main =
    let filePath = "C:\\Users\\Emmy\\Documents\\Programspråk\\OU1\\anotherloxsource.lox" in
        readFile filePath >>= \s -> printProgram s

printTokens :: String -> IO ()
printTokens s = mapM_ print $ scanTokens s

printDeclarations :: String -> IO ()
printDeclarations s = print $ parse $ scanTokens s

printProgram :: String -> IO ()
printProgram s = mapM_ print $ interpret $ parse $ scanTokens s