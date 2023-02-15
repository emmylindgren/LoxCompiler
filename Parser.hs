module Parser (parse) where

import Tokens
-- Detta nedan måste ändras. Tittar mer på de senare! 
data Tree = Expression 
  deriving Show

--Literal ska ju kunna va en sträng eller ett nummer I guess, kan använda de literals
-- som redan är definierade? 
data Expression = Literal 
                | Unary {operator::Token,right::Expression}
                | Binary {left::Expression, operator::Token,right::Expression}
                | Grouping Expression 
  deriving (Show)
{- Nedan från kap 6 iaf, de är funktionerna som kommer behövas.
 De ska också returnera ett träd samt resterande del av tokenlistan.
 Just nu är trädet expression från dessa! 
 Tror det kommer vara så mEEN sen kmr expr komma från statement osv
 Då kommer alla måsta utgå från samma typ? Vet inte riktigt ännu.
expression :: [Token] -> (Expression, [Token])
equality :: [Token] -> (Expression, [Token])
comparison :: [Token] -> (Expression, [Token])
term       :: [Token] -> (Expression, [Token])
factor     :: [Token] -> (Expression, [Token])
unary :: [Token] -> (Expression, [Token])
primary :: [Token] -> (Expression, [Token])

Sen kommer vi ha några som returnerar statements? 
Så kmr trädet = statement | Expression?? 

Exempelträd från annan grammatik:
data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String
  deriving Show
-}
--Detta lär returnera ett träd sedan som är byggt av expressions osv 
-- but for now, expression:) 
parse :: [Token] -> Expression
parse x = let (tree,rest) = expression x 
        in 
          if null rest 
            then tree
            else error "Unexpected leftover tokens" --jobba ut denna mer? ska de bli fel här ens?  


expression :: [Token] -> (Expression,[Token])
expression = equality  

--OBSOBS! Tänk på när listan är slut, när vi stöter på EOF, hanterar vi de fallet ens? 
equality :: [Token] -> (Expression,[Token])
equality x = let (equalityExpr,rest) = comparison x 
    in binaryCheck rest equalityExpr [BANG_EQUAL,EQUAL_EQUAL] comparison

comparison :: [Token] -> (Expression,[Token])
comparison x = let (comparisonExpr,rest) = term x
          in binaryCheck rest comparisonExpr [GREATER,GREATER_EQUAL,LESS,LESS_EQUAL] term

term :: [Token] -> (Expression,[Token])
term x = let (factorExpr,rest) = factor x 
    in binaryCheck rest factorExpr [MINUS,PLUS] factor

factor :: [Token] -> (Expression,[Token])
factor x = let (unaryExpr,rest) = unary x 
    in binaryCheck rest unaryExpr [SLASH,STAR] unary

-- Gjort denna som en hjälpfunktion men vet inte riktigt. Blev ganska rörig faktiskt.
-- Applicerat på term och factor. Det bör funka. Men tänk på EOF när tar vi hand om de? 
binaryCheck ::[Token] -> Expression -> [TokenType] -> ([Token] -> (Expression,[Token])) 
              -> (Expression,[Token])
binaryCheck (x:xs) leftExpr tokenMatches exprType =
  if x `match` tokenMatches
    then let (rightExpr,rest') = exprType xs
      in binaryCheck rest' Binary{left = leftExpr, operator = x, right = rightExpr} tokenMatches exprType
    else (leftExpr,x:xs) 

match :: Token -> [TokenType] -> Bool 
match t matches = let tokenType = getTokenType t in 
  tokenType `elem` matches 

getTokenType :: Token -> TokenType
getTokenType (TOKEN t _ _ _) = t 

unary :: [Token] -> (Expression,[Token])
unary (x:xs) = if x `match` [BANG,MINUS]
  then let (unaryExpr,rest) = unary xs 
    in (Unary{operator = x, right= unaryExpr}, rest)
  else primary 

--next up: primary

{-
Gamla equality osv. Ifall bättre så? 
equality :: [Token] -> (Expression,[Token])
equality x =  let (expr,rest)= comparison x 
        in equalityCheck rest expr
        where 
          equalityCheck :: [Token] -> Expression -> (Expression, [Token]) 
          -- Antar att vi också måste ta hand om det tomma fallet? När listan är tom alltså?
          -- Hmmm, men EOF-token är ju sist så de bör alltid finnas? Kan 
          -- jag anta att de alltid finns? Då returnera när man stöter på de? De blir ju tokentype X = EOF?
          -- Kan tänka mig att de kan skapa nån slags hjälpfunktion för detta eventuellt.
          -- Det kanske också är ordnat som de ser ut just nu? Pga de matchar ju ej bang equal eller equal 
          --equal?
          equalityCheck  (x:xs) leftExpr = 
            if getTokenType x == BANG_EQUAL || getTokenType x == EQUAL_EQUAL
            then let (rightExpr,rest') = comparison xs
              in equalityCheck  rest' Binary{left = leftExpr, operator = x, right = rightExpr} -- oklart hur vi gör ett binary expression ännu
            else (leftExpr,x:xs)

comparison :: [Token] -> (Expression,[Token])
comparison x = let (expr,rest) = term x
          in comparisonCheck rest expr 
          where 
            comparisonCheck :: [Token] -> Expression -> (Expression,[Token])
            comparisonCheck (x:xs) leftExpr = let tokenType = getTokenType x in
              if tokenType == GREATER || tokenType == GREATER_EQUAL || tokenType == LESS ||tokenType == LESS_EQUAL 
                then let (rightExpr,rest') = term xs
                  in comparisonCheck rest' Binary{left = leftExpr, operator = x, right = rightExpr}
                else (leftExpr,x:xs)

term :: [Token] -> (Expression,[Token])
term x = let (expr,rest) = factor x 
    in termCheck rest expr
    where 
      termCheck :: [Token] -> Expression -> (Expression,[Token])
      termCheck (x:xs) leftExpr = let tokenType = getTokenType x in
        if tokenType == MINUS || tokenType == PLUS 
          then let (rightExpr,rest') = factor xs
            in termCheck rest' Binary{left = leftExpr, operator = x, right = rightExpr}
          else (leftExpr,x:xs) 

factor :: [Token] -> (Expression,[Token])
factor x = let (expr,rest) = unary x 
    in factorCheck rest expr
    where 
      factorCheck :: [Token] -> Expression -> (Expression,[Token])
      factorCheck (x:xs) leftExpr = let tokenType = getTokenType x in
        if tokenType == SLASH || tokenType == STAR 
          then let (rightExpr,rest') = unary xs
            in factorCheck rest' Binary{left = leftExpr, operator = x, right = rightExpr}
          else (leftExpr,x:xs) 
--}