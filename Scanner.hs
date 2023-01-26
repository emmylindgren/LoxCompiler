module Scanner (scanTokens) where

import Data.Maybe
import Data.Char

import Tokens

--Main function.
scanTokens :: [Char] -> [Token]
scanTokens x = runTokens x 1

runTokens :: [Char] -> Int -> [Token]
runTokens [] _ = []
runTokens (x:xs) line = case x of 
    '(' -> addToken LEFT_PAREN [x] line xs
    ')' -> addToken RIGHT_PAREN [x] line xs
    '{' -> addToken LEFT_BRACE [x] line xs
    '}' -> addToken RIGHT_BRACE [x] line xs
    ',' -> addToken COMMA [x] line xs
    '.' -> addToken DOT [x] line xs
    '-' -> addToken MINUS [x] line xs
    '+' -> addToken PLUS [x] line xs
    ';' -> addToken SEMICOLON [x] line xs
    '*' -> addToken STAR [x] line xs
    '!' -> if nextMatches '=' xs 
        then addToken BANG_EQUAL [x] line (tail xs)
        else addToken BANG [x] line (tail xs)
    -- Next one to tackle is =, look at the java file. 
    _  -> [] --Some kind of error handling here


nextMatches:: Char -> [Char] -> Bool
nextMatches c [] = False
nextMatches c (x:xs) = c == x

--Denna används ej nu! Kanske behövs sen? 
createToken:: TokenType -> [Char] -> Int -> Token
createToken t content = TOKEN t content NONE 

addToken :: TokenType -> [Char] -> Int -> [Char] -> [Token]
addToken t content line rest = TOKEN t content NONE line : runTokens rest line 
--TOKEN tokentype [x] NONE line
-- 1: Gå genom character för char och utvärdera...
-- ska jag använda fold?
-- Eller kalla på scantokens tills vi har en tom lista..
-- känns ju faktiskt najs tror jag? Gör ett token om de går
-- sen skicka vidare till sig själv. 
-- kan väl dock va snabbare med fold i guess :) 