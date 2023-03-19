module Main (main) where  
import Scanner (scanTokens)
import Parser(parse)
import Interpreter(interpret)
import System.IO
import System.Environment
import System.IO.Error
import Control.Exception
{-|
    Author: Emmy Lindgren
    id19eln
    Date: 2023-03-19
-}
main :: IO ()
main = interpretLoxFile `catch` errorHandler

interpretLoxFile :: IO ()  
interpretLoxFile = do 
        args <- getArgs
        let fileName = checkFileName args
        file <- openFile fileName ReadMode  
        contents <- hGetContents file  
        printProgram contents  

errorHandler :: IOError -> IO ()  
errorHandler e  
    | isDoesNotExistError e = putStrLn "The file does not exist!"  
    | otherwise = ioError e

checkFileName :: [FilePath] -> FilePath
checkFileName (fileName:_) = fileName 
checkFileName _ = error "You need to enter a file name."

printProgram :: String -> IO ()
printProgram s = mapM_ putStrLn $ interpret $ parse $ scanTokens s

--- Used for testing the previous sections ---
{-
main =
    let filePath = "C:\\Users\\Emmy\\Documents\\Programspråk\\OU1\\anotherloxsource.lox" in
        readFile filePath >>= \s -> printProgram s
-}

printTokens :: String -> IO ()
printTokens s = mapM_ print $ scanTokens s

printDeclarations :: String -> IO ()
printDeclarations s = print $ parse $ scanTokens s