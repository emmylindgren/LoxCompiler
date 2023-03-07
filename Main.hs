module Main (main) where
    
import Scanner (scanTokens)
import Parser(parse)
import Interpreter(interpret)
import System.IO
import System.Environment
import System.IO.Error
import Control.Exception
{-
-- The following code is for taking the filename in as args! Works.
main :: IO ()
main = interpretLoxFile `catch` errorHandler

interpretLoxFile :: IO ()  
interpretLoxFile = do 
        (fileName:_) <- getArgs
        file <- openFile fileName ReadMode  
        contents <- hGetContents file  
        printProgram contents  

errorHandler :: IOError -> IO ()  
errorHandler e  
    | isDoesNotExistError e = putStrLn "The file does not exist!"  
    | otherwise = ioError e
-}

main =
    let filePath = "C:\\Users\\Emmy\\Documents\\Programspråk\\OU1\\anotherloxsource.lox" in
        readFile filePath >>= \s -> printProgram s


printProgram :: String -> IO ()
printProgram s = mapM_ putStrLn $ interpret $ parse $ scanTokens s

--- Used for testing the previous sections ---
printTokens :: String -> IO ()
printTokens s = mapM_ print $ scanTokens s

printDeclarations :: String -> IO ()
printDeclarations s = print $ parse $ scanTokens s