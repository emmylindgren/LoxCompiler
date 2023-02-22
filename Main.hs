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
    let filePath = "C:\\Users\\Emmy\\Documents\\Programspråk\\OU1\\loxsourceEasy.lox" in
        readFile filePath >>= \s -> printTokens s

printTokens :: String -> IO ()
printTokens s = print $ parse $ scanTokens s
--Nedan för att printa ifall man får tillbaka en lista av scanTokens
--printTokens s = mapM_ print $ parse $ scanTokens s