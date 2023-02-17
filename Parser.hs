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
-- som den gör nu. Vet dock inte om det är så superrätt men. 
-- Jag tänker också att den ska returnera något av typen A, men kan man kanske 
-- göra de till en datatyp som har en lista med statements? 
parse :: [Token] -> [Statement]
parse t@(x:xs) = if x `match` [EOF]
  then []
  else let (stmt,rest) = statement t
    in stmt : parse rest

-- Lite frågetecken här. Tror dock printstmt bara ska få resten av listan, han skriver de i boken iaf 
-- men tror exprstmt behöver hela? gör så i boken också just nu. 
statement :: [Token] -> (Statement,[Token])
statement tokens@(x:xs) = case getTokenType x of 
  PRINT -> printStmt xs
  _ -> exprStmt tokens

printStmt:: [Token] -> (Statement,[Token])
printStmt x = let (printexpr,first:rest) = expression x
    in if first `match` [SEMICOLON]
      then (PrintStmt printexpr,rest)
      else loxError "Error in function PrintStmt. Expected ';' after value on line " first

exprStmt :: [Token] -> (Statement,[Token])
exprStmt x = let (expr,first:rest) = expression x 
      in if first `match` [SEMICOLON]
      then (ExpressionStmt expr,rest)
      else loxError "Error in function ExprStmt. Expected ';' after expression on line " first

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

{-
  Helperfunction to look for and if found parse one or several binary expressions. 
  It takes four arguments: 
  [Token] (the tokens to be parsed),  'Expression': the left hand expression in binary if it is to be created.
  [TokenType] a list of the tokentypes allowed to create the binary expression.
  [Token] -> (Expression,[Token]) the function creating the expression to be on the 
  right hand side of the binary expression.

  Returns a tuple containing the binary expression (with any more binary expressions nested) and 
  the rest of the tokenlist. 
-}
binaryCheck ::[Token] -> Expression -> [TokenType] -> ([Token] -> (Expression,[Token]))
              -> (Expression,[Token])
binaryCheck (x:xs) leftExpr tokenMatches exprType =
  if x `match` tokenMatches
    then let (rightExpr,rest') = exprType xs
          in binaryCheck rest' Binary{left = leftExpr, operator = x, right = rightExpr} tokenMatches exprType
    else (leftExpr,x:xs)

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
        else loxError "Error in function Primary. Expected ')' after grouping on line " first
  where 
    saveTokenLiteral = (Literal (getTokenLiteral x), xs)

-------------- Helper functions -------------- 
match :: Token -> [TokenType] -> Bool
match t matches = let tokenType = getTokenType t 
                    in tokenType `elem` matches

getTokenType :: Token -> TokenType
getTokenType (TOKEN t _ _ _) = t

getTokenLiteral :: Token -> Literal
getTokenLiteral (TOKEN _ _ l _) = l

getTokenLine :: Token -> Int
getTokenLine (TOKEN _ _ _ l) = l

{-
  Function to throw an error in parsing state, 
  It takes two arguments, one of type [Char] (the string)
  and one of type 'Token', the token of which threw an error. 
-} 
loxError :: [Char] -> Token -> error 
loxError string tok = error (string ++ " on line "++ show (getTokenLine tok))