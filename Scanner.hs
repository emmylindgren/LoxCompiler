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
    '"'-> string xs line
    x 
        | isDigit x -> number (x:xs) line
        | isAlpha x || x == '_'-> identifier (x:xs) line
        | otherwise -> error ("Unexpected value: "++ show x ++" at line "++ show line)
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


-- Function for collecting a string Token from [Char].
-- Returns a list of Tokens following the string token, 
-- with the string token appended at the head.
string :: [Char] -> Int -> [Token]
string list line = 
    let (subString,rest,newLine) = getString list line
        in TOKEN STRING "" (STR subString) line : runTokens rest newLine
    where 
        getString:: [Char] -> Int -> ([Char],[Char],Int)
        getString [] line = error ("Unterminated string, at line: "++ show line)
        getString (x:xs) line = case x of 
            '"' -> ([],xs,line)
            '\n' -> advance (line+1)
            _ -> advance line
            where 
                advance line = 
                    let (subString,rest,lineCount) = getString xs line
                        in (x:subString,rest,lineCount) 

-- Function for collecting a Float Token out of a [Char].
-- Returns a list of Tokens following the float token, 
-- with the float token appended at the head.
number :: [Char] -> Int -> [Token]
number list line = 
    let (numString,restString) = getNumber list
        in TOKEN NUMBER "" (NUM (read numString :: Float)) line : runTokens restString line
        where
            getNumber :: [Char] -> ([Char],[Char])
            getNumber [] = ([],[])
            getNumber (x:xs) = case x of 
                '.' -> case xs of 
                    [] -> ([],x:xs)
                    xs -> if isDigit (head xs)
                            then let decimals = takeWhile isDigit xs
                                in (x : decimals, drop (length decimals) xs)
                            else ([],x:xs)
                x 
                    | isDigit x -> let (num, rest) = getNumber xs
                            in (x:num,rest)
                    | otherwise -> ([],x:xs)


identifier :: [Char] -> Int -> [Token]
identifier list line = 
    let (name,rest) = getName list
        in case getKeyWord name of 
            IDENTIFIER -> TOKEN IDENTIFIER "" (ID name) line : runTokens rest line
            FALSE -> TOKEN FALSE "" FALSE_LIT line : runTokens rest line
            TRUE -> TOKEN TRUE "" TRUE_LIT line : runTokens rest line
            NIL -> TOKEN NIL "" NIL_LIT line : runTokens rest line
            x -> TOKEN x "" NONE line : runTokens rest line
    where
        getName :: [Char] -> ([Char],[Char]) 
        getName [] = ([],[])
        getName (x:xs) 
            | isAlphaNum x || x == '_' = 
                let (name,rest) = getName xs
                    in (x:name,rest)
            | otherwise = ([],x:xs)
        getKeyWord :: [Char] -> TokenType
        getKeyWord x = case x of 
            "and" -> AND 
            "class" -> CLASS
            "else" -> ELSE
            "false" -> FALSE
            "for" -> FOR
            "fun" -> FUN
            "if" -> IF
            "nil" -> NIL
            "or" -> OR
            "print" -> PRINT
            "return" -> RETURN
            "super" -> SUPER
            "this" -> THIS
            "true" -> TRUE
            "var" -> VAR
            "while" -> WHILE
            _ -> IDENTIFIER