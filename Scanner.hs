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
    '(' -> addSingleToken LEFT_PAREN
    ')' -> addSingleToken RIGHT_PAREN
    '{' -> addSingleToken LEFT_BRACE
    '}' -> addSingleToken RIGHT_BRACE
    ',' -> addSingleToken COMMA
    '.' -> addSingleToken DOT
    '-' -> addSingleToken MINUS
    '+' -> addSingleToken PLUS
    ';' -> addSingleToken SEMICOLON
    '*' -> addSingleToken STAR
    '!' -> if nextMatches '=' xs 
        then addDoubleToken BANG_EQUAL
        else addSingleToken BANG
    '=' -> if nextMatches '=' xs
        then addDoubleToken EQUAL_EQUAL
        else addSingleToken EQUAL
    '<' -> if nextMatches '=' xs
        then addDoubleToken LESS_EQUAL
        else addSingleToken LESS
    '>' -> if nextMatches '=' xs 
        then addDoubleToken GREATER_EQUAL
        else addSingleToken GREATER 
    '/' -> if nextMatches '/' xs 
        then comment xs line
        else addSingleToken SLASH
    ' ' -> noToken 
    '\r'-> noToken
    '\t'-> noToken 
    '\n'-> runTokens xs (line + 1)
    '"'-> let (subString,restString,lineCount) = string xs line
          in 
            TOKEN STRING "" (STR subString) line : runTokens restString lineCount
    -- next one is: isDigit :) look at the java file. Default case? 
    x 
        | isDigit x -> let (num,restString) = number (x:xs)
                in
                TOKEN NUMBER "" (NUM (read num :: Float)) line : runTokens restString line
        | otherwise -> error "fel"
    where 
        addSingleToken x = addToken x line xs
        addDoubleToken x = addToken x line (tail xs)
        noToken = runTokens xs line

addToken :: TokenType -> Int -> [Char] -> [Token]
addToken t line rest = TOKEN t "" NONE line : runTokens rest line 

nextMatches:: Char -> [Char] -> Bool
nextMatches c [] = False
nextMatches c (x:xs) = c == x

-- Function for advancing a [Char] until reaching the end of a comment. 
-- Calls runTokens with the rest of [Char] when done.
-- Comments are not added to tokens as it is not meaningfull for 
-- the parser later on. 
comment:: [Char] -> Int -> [Token]
comment [] line = runTokens [] line
comment (x:xs) line = if x == '\n'
    then runTokens xs (line + 1)
    else comment xs line

-- Function for collecting strings out of a [Char].
-- Returns: triple containing the substring, 
-- the rest of the [Char] to be examined, and the linecount.
string:: [Char] -> Int -> ([Char],[Char],Int)
string [] line = error ("Unterminated string, at line: "++ show line)
string (x:xs) line = case x of 
    '"' -> ([],xs,line)
    '\n' -> advance (line+1)
    _ -> advance line
    where 
        advance line = 
            let (subString,rest,lineCount) = string xs line
                in (x:subString,rest,lineCount) 

-- Function for collecting a Float out of a [Char].
-- Returns: pair containing substring with the Float, 
-- and the rest of the [Char] to be examined.
number :: [Char] -> ([Char],[Char])
number [] = ([],[])
number (x:xs) = case x of 
    '.' -> case xs of 
        [] -> ([],x:xs)
        xs -> if isDigit (head xs)
                then let decimals = takeWhile isDigit xs
                    in (x : decimals, drop (length decimals) xs)
                else ([],x:xs)
    x 
        | isDigit x -> let (num, rest) = number xs
                in (x:num,rest)
        | otherwise -> ([],x:xs)