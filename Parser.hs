module Parser (parse) where

import Tokens
{- Måste ju ha nån datatyp A som beskrivet i specen, lista av statements? 
data Tree = Expression
  deriving Show
-}
data Statement = ExpressionStmt Expression 
                | PrintStmt Expression 
{-
  Reglerna: (som de ser ut just nu, början av kap 8)
  program        → statement* EOF ;

  statement      → exprStmt
                | printStmt ;

  exprStmt       → expression ";" ;
  printStmt      → "print" expression ";" ;
-}
--Använder just nu de literaler som redan är definierade i Tokens.hs
data Expression = Literal Literal
                | Unary {operator::Token,right::Expression}
                | Binary {left::Expression, operator::Token,right::Expression}
                | Grouping Expression
  deriving (Show)

--Detta lär returnera ett träd sedan som är byggt av statement osv 
-- but for now, expression:) Detta blir fel! Vi har ju kvar EOF just nu så de är svårt
-- så får Non-exhaustive patterns när de körs.
parse :: [Token] -> Expression
parse x = let (tree,rest) = expression x
        in
          if null rest
            then tree
            else error "Unexpected leftover tokens." -- ska de bli fel här ens? Just nu blir alltid EOF
            -- som kmr tillbaka så :)   

statement :: [Token] -> (Statement,[Token])
statement tokens@(x:xs) = case getTokenType x of 
  PRINT -> printStmt tokens
  _ -> exprStmt tokens

--next up: printStmt!

exprStmt :: [Token] -> (Statement,[Token])
exprStmt x = let (expr,first:rest) = expression x 
    in if first `match` [SEMICOLON]
      then (ExpressionStmt expr,rest)
      else error ("Error in function ExprStmt. Expected ';' after expression on line "++ show (getTokenLine first)) 

expression :: [Token] -> (Expression,[Token])
expression = equality

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
-- Det bör funka, mindre rörigt än om alla gör samma. Men tänk på EOF när tar vi hand om de? 
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

getTokenLiteral :: Token -> Literal
getTokenLiteral (TOKEN _ _ l _) = l

getTokenLine :: Token -> Int
getTokenLine (TOKEN _ _ _ l) = l

unary :: [Token] -> (Expression,[Token])
unary (x:xs) = if x `match` [BANG,MINUS]
  then let (unaryExpr,rest) = unary xs
    in (Unary{operator = x, right= unaryExpr}, rest)
  else primary (x:xs)

-- Obs! Bör vi göra nåt ifall token inte matchar någon av fallen? Bör vi
-- kasta fel då? EOF här? 
primary :: [Token] ->(Expression,[Token])
primary (x:xs) = case getTokenType x of
  FALSE -> saveTokenLiteral
  TRUE -> saveTokenLiteral
  NIL -> saveTokenLiteral
  NUMBER -> saveTokenLiteral
  STRING -> saveTokenLiteral
  LEFT_PAREN -> let (expr,first:rest) = expression xs
      in if first `match` [RIGHT_PAREN]
        then (Grouping expr, rest)
        else error ("Error in function Primary. Expected ')' after grouping on line "++ show (getTokenLine first))
  where 
    saveTokenLiteral = (Literal (getTokenLiteral x), xs)