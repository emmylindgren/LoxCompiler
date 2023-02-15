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

-- The rule equality       → comparison ( ( "!=" | "==" ) comparison )* ; 
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
--Next up: Term :) 
getTokenType :: Token -> TokenType
getTokenType (TOKEN t _ _ _) = t 