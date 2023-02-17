module Parser (parse) where

import Tokens
{- Måste ju ha nån datatyp A som beskrivet i specen = lista av statements. 
Typ enligt nedan :) 
-}
data Tree = Tree [Statement] 
  deriving Show
--Expression är maybe i init pga den kan va "null"
data Statement = ExpressionStmt Expression 
                | PrintStmt Expression 
                | VarDec{name::Token,initializer::Maybe Expression}
        deriving (Show)
{-
  Reglerna: (som de ser ut just nu, mitten av kap 8)
program        → declaration* EOF ;

declaration    → varDecl
               | statement ;

statement      → exprStmt
               | printStmt ;
-}
--Använder just nu de literaler som redan är definierade i Tokens.hs
data Expression = Literal Literal
                | Unary {operator::Token,right::Expression}
                | Variable {varname::Token}
                | Assign {varAssignname::Token, value::Expression}
                | Binary {left::Expression, operator::Token,right::Expression}
                | Grouping Expression
  deriving (Show)

--Detta lär returnera ett träd sedan som är byggt av declaration (Statement) osv 
-- som den gör nu. Vet dock inte om det är så superrätt men. 
-- Jag tänker också att den ska returnera något av typen A, men kan man kanske 
-- göra de till en datatyp som har en lista med statements? 
parse :: [Token] -> [Statement]
parse t@(x:xs) = if x `match` [EOF]
  then []
  else let (declarationStmt,rest) = declaration t
    in declarationStmt : parse rest

declaration ::[Token] -> (Statement,[Token])
declaration tokens@(x:xs) = case getTokenType x of 
  VAR -> varDeclaration xs
  _ -> statement tokens


varDeclaration :: [Token] -> (Statement,[Token])
varDeclaration tokens@(x:xs) = if x `match` [IDENTIFIER]
  then let (init,first:rest) = getInitializer xs
    in if first `match` [SEMICOLON]
      then (VarDec{name = x, initializer = init}, rest) 
      else loxError "Expect ';' after variable declaration in function varDeclaration " x 
  else loxError "Expect variable name in function varDeclaration" x
  where 
    getInitializer :: [Token] -> (Maybe Expression,[Token])
    getInitializer t@(first:rest) = if first `match` [EQUAL]
      then let (expr, rest') = expression rest
        in (Just expr,rest')
      else (Nothing, t)

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
expression = assignment

-- Vet ej om denna funkar som den ska. Kolla genom. 
assignment:: [Token] -> (Expression,[Token])
assignment t = let (expr,first:rest) = equality t
            in if first `match` [EQUAL]
              then let(val,rest') = assignment rest
              in if checkifVariable expr
                then (Assign{varAssignname = varname expr, value = val}, rest')
                else loxError "Invalid assignment target in function assigment" first
            else (expr,first:rest)

-- Denna är otroligt oklar :) Har testat den med en variable och då fick jag sant.
-- testade parsa : [TOKEN NUMBER "" (NUM 1.0) 1,TOKEN PLUS "" NONE 1,TOKEN NUMBER "" (NUM 2.0) 1,TOKEN EQUAL "" NONE 1,TOKEN NUMBER "" (NUM 3.0) 1, TOKEN SEMICOLON "" NONE 1,TOKEN EOF "" NONE 1] 
-- då fick jag fel.. ska de bli fel då? Men får iaf falskt från denna funktion då. Rimligtvis ja? 1 + 2 = 3 är väl inte nåt som ska godkännas?
-- Detta funkar [TOKEN IDENTIFIER "" (ID "Hej") 1,TOKEN EQUAL "" NONE 1,TOKEN NUMBER "" (NUM 3.0) 1,TOKEN SEMICOLON "" NONE 1,TOKEN EOF "" NONE 1]
-- Tror den funkar? 
checkifVariable :: Expression -> Bool
checkifVariable (Variable _) = True 
checkifVariable _ = False 

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
  IDENTIFIER -> (Variable{varname = x},xs)
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