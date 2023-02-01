module Main (main) where
import Scanner (scanTokens)

main :: IO ()
main =
    let filePath = "C:\\Users\\Emmy\\Documents\\Programspråk\\OU1\\loxsource.lox" in
        readFile filePath >>= \s -> printTokens s
{-
main :: IO ()
main = printTokens "12"-}

printTokens :: String -> IO ()
printTokens s = mapM_ print $ scanTokens s