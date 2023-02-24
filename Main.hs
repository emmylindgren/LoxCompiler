module Main (main) where

import Scanner (scanTokens)
import Parser(parse)

main :: IO ()
{-
main =
    let filePath = "C:\\Users\\Emmy\\Documents\\Programspråk\\OU1\\loxsource.lox" in
        readFile filePath >>= \s -> printTokens s
-}
main =
    let filePath = "C:\\Users\\Emmy\\Documents\\Programspråk\\OU1\\loxsourceTest.lox" in
        readFile filePath >>= \s -> printDeclarations s

printTokens :: String -> IO ()
printTokens s = mapM_ print $ scanTokens s

printDeclarations :: String -> IO ()
printDeclarations s = print $ parse $ scanTokens s