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
    -- Bryta ut strängskapandet kanske? Känns också som en ganska ful lösning? Snyggare? 
    -- KOLLA GENOM DETTA ORDENTLIGT! 
    '"'-> TOKEN STRING "" (STR (fst stringAndRest)) line : runTokens (snd stringAndRest) line
        where stringAndRest = string xs line
    -- next one is: isDigit :) look at the java file. Default case? 
    _  -> error "fel"
    where 
        addSingleToken x = addToken x line xs
        addDoubleToken x = addToken x line (tail xs)
        noToken = runTokens xs line


nextMatches:: Char -> [Char] -> Bool
nextMatches c [] = False
nextMatches c (x:xs) = c == x

-- Comments are not added to tokens, it's not meaningfull for 
-- the parser later on. 
comment:: [Char] -> Int -> [Token]
comment [] line = runTokens [] line
comment (x:xs) line = if x == '\n'
    then runTokens xs (line + 1)
    else comment xs line

-- Problem? Inte så snyggt? Kanske bryta upp det i två funktioner annars?
string:: [Char] -> Int -> ([Char],[Char])
string [] line = error ("Unterminated string, at line: "++ show line)
string (x:xs) line = if x == '"'
    then ([],xs)
    else (x : fst stringAndRest, snd stringAndRest)
    where stringAndRest = string xs line

-- Eller så: 
-- så länge vi ej stöter på " så returnerar vi den strängen + nästa omgång m xs.
-- 

addToken :: TokenType -> Int -> [Char] -> [Token]
addToken t line rest = TOKEN t "" NONE line : runTokens rest line 

--TOKEN tokentype [x] NONE line
-- 1: Gå genom character för char och utvärdera...
-- ska jag använda fold?
-- Eller kalla på scantokens tills vi har en tom lista..
-- känns ju faktiskt najs tror jag? Gör ett token om de går
-- sen skicka vidare till sig själv. 
-- kan väl dock va snabbare med fold i guess :) 