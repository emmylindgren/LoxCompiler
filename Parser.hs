module Parser (parse) where
import Tokens
import Data.Maybe (isNothing, fromJust)
{-|
    Author: Emmy Lindgren
    id19eln
    Date: 2023-02-XX
-}

data Program = PROGRAM [Declaration]
instance Show Program where
    show (PROGRAM decs) = (show $ length decs) ++ "\n" ++ (unlines $ map show decs)

data Declaration = VarDec{name::Token,initializer::Maybe Expression}
                  | Statement Statement
instance Show Declaration where
  show (VarDec name init) = "V DEC -> " ++ getIdentifierName name ++
    if isNothing init
      then ";"
      else  "=" ++ show (fromJust init) ++ ";"
  show (Statement s) = show s

data Statement = ExpressionStmt Expression
                | IfStmt {condition::Expression, thenBranch::Statement,
                          elseBranch:: Maybe Statement}
                | PrintStmt Expression
                | WhileStmt {condition::Expression, body::Statement}
                | BlockStmt [Declaration]
instance Show Statement where
  show (ExpressionStmt e) = show e ++ ";"
  show (IfStmt cond thenBranch elseBranch) = "if(" ++show cond ++") "
    ++ show thenBranch ++ if isNothing elseBranch
      then ""
      else "else " ++ show (fromJust elseBranch)
  show (PrintStmt e) = "print " ++ show e ++ ";"
  show (WhileStmt cond body) = "while(" ++ show cond ++ ")" ++ show body
  show (BlockStmt declarations) = "{" ++ concatMap show declarations ++ "}"

data Expression = Literal Literal
                | Logical {left::Expression,operator::Token,right::Expression}
                | Unary {operator::Token,right::Expression}
                | Variable {varname::Token}
                | Assign {varAssignname::Token, value::Expression}
                | Binary {left::Expression, operator::Token,right::Expression}
                | Grouping Expression
instance Show Expression where
  show (Literal l) = case l of
    STR s -> show s
    NUM n -> show n
    literalType  -> show literalType
  show (Logical left op right) = "("++ show left ++ getOperator op ++ show right ++ ")"
  show (Unary op right) = "(" ++ getOperator op ++ show right ++ ")"
  show (Variable t) = getIdentifierName t
  show (Assign name val) = getIdentifierName name ++ " = "++ show val
  show (Binary left op right) = "(" ++ show left ++ getOperator op ++ show right ++ ")"
  show (Grouping expr) = "(" ++ show expr ++ ")"

{-
  Function for parsing a list of tokens to a Program. 
  A program is a list of declarations.
-}
parse :: [Token] -> Program
parse tokens = let decs = getDeclarations tokens
      in PROGRAM decs
{-
  Function for parsing declarations from a list of tokens. 
  If first token in list is "EOF" then there are no tokens to parse declarations
  from. Otherwise a declaration is fetched using the list and appended to the head 
  of the list of the rest declarations.
-}
getDeclarations :: [Token] -> [Declaration]
getDeclarations t@(x:xs) = if x `match` [EOF]
  then []
  else let (declarationStmt,rest) = declaration t
    in declarationStmt : getDeclarations rest
{-
  Function for parsing a declaration. 
  A declaration is either a variable declaration or a statement.
-}
declaration ::[Token] -> (Declaration,[Token])
declaration tokens@(x:xs) = case getTokenType x of
  VAR -> varDeclaration xs
  _ -> let (stmt,rest) = statement tokens in (Statement stmt,rest)
{-
  Function for parsing a variable declaration. 
  A variable declaration is a identifier-token optionally followed 
  by "=" and a initializer. If no initializer is added then it is set to nothing.
  A variable declaration must be followed by a ";".
-}
varDeclaration :: [Token] -> (Declaration,[Token])
varDeclaration tokens@(x:xs) = if x `match` [IDENTIFIER]
  then let (init,first:rest) = getInitializer xs
    in if first `match` [SEMICOLON]
      then (VarDec{name = x, initializer = init}, rest)
      else loxError "Error in function varDeclaration. Expected ';' after variable declaration" x
  else loxError "Error in function varDeclaration. Expected variable name" x
  where
    getInitializer :: [Token] -> (Maybe Expression,[Token])
    getInitializer t@(first:rest) = if first `match` [EQUAL]
      then let (expr, rest') = expression rest
        in (Just expr,rest')
      else (Nothing, t)
{-
  Function for parsing a statement. 
  A statement is a for-, if-, print-, while-, block- or 
  expression statement. 
-}
statement :: [Token] -> (Statement,[Token])
statement tokens@(x:xs) = case getTokenType x of
  FOR -> forStmt xs
  IF -> ifStmt xs
  PRINT -> printStmt xs
  WHILE -> whileStmt xs
  LEFT_BRACE -> blockStmt xs
  _ -> exprStmt tokens
{-
  Function for parsing a for-statement. 
  A for-statement is a initializer, a stopcondition and a increment all 
  within "()". Initializer and stopcondition has to be followed by ";" and 
  all three clauses can be omitted, but not the ";". 
  This is followed by a body in the form of a statement. 

  The for-statement is created using the while-statement datatype. 
-}
forStmt:: [Token] -> (Statement,[Token])
forStmt tokens@(x:xs) = if x `match` [LEFT_PAREN]
  then (createForLoop body incr cond init,bodyRest)
  else loxError "Error in function ForStmt. Expected '(' after 'for'" x
  where
    (init,initRest) = getInit xs
    (cond,condRest) = getCond initRest
    (incr,incrRest) = getIncr condRest
    (body,bodyRest) = statement incrRest
    getInit:: [Token] -> (Maybe Declaration,[Token])
    getInit tokens@(x:xs) = case getTokenType x of
      SEMICOLON -> (Nothing,xs)
      VAR -> let (init,rest) = varDeclaration xs in (Just init, rest)
      _ -> let (init,rest) = exprStmt xs in (Just (Statement init),rest)
    getCond:: [Token] -> (Maybe Expression,[Token])
    getCond tokens@(x:xs) = if x `match` [SEMICOLON]
      then (Nothing,xs)
      else let (cond,first:rest) = expression tokens in
        if first `match` [SEMICOLON]
          then (Just cond,rest)
          else loxError "Error in function ForStmt. Expected ';' after loop condition" first
    getIncr:: [Token] -> (Maybe Expression,[Token])
    getIncr tokens@(x:xs) = if x `match` [RIGHT_PAREN]
      then (Nothing,xs)
      else let (incr,f:rest) = expression tokens in
        if f `match` [RIGHT_PAREN]
          then (Just incr,rest)
          else loxError "Error in function ForStmt. Expected ')' after for clauses" f
{-
  Helperfunction for creating a for-statement.  
  If there is a initializer then a block is created starting with the initializer expression. 
  A while-expression is then added to the block containing the condition if there is one. If there is
  no condition then True is added as the condition.
  The body is added to the while-expression and the increment-expression is added last in the body
  as to increment it every iteration. 
  If there is no initializer then no block is created, just the while-expression.
-}
createForLoop :: Statement -> Maybe Expression -> Maybe Expression -> Maybe Declaration -> Statement
createForLoop body incr cond init = checkInit (checkCond (checkIncr body incr) cond) init
  where
    checkIncr:: Statement -> Maybe Expression -> Statement
    checkIncr b i = if isNothing i
      then b
      else BlockStmt [Statement b, Statement (ExpressionStmt (fromJust i))]
    checkCond :: Statement -> Maybe Expression -> Statement
    checkCond b c = if isNothing c
      then WhileStmt{condition= Literal TRUE_LIT,body=b}
      else WhileStmt{condition= fromJust c,body=b}
    checkInit :: Statement -> Maybe Declaration -> Statement
    checkInit b i = if isNothing i
      then b
      else BlockStmt [fromJust i,Statement b]
{-
  Function for parsing a if-statement. 
  A if-statement is a expression representing the condition, followed by 
  a statement representing the "then"-body of the statement optionally followed by 
  "else" and another statement representing the "else"-body. 
-}
ifStmt:: [Token] -> (Statement,[Token])
ifStmt tokens@(x:xs) = 
  (IfStmt{condition=expr,thenBranch=thenBranch,elseBranch=elseBranch},elseRest)
  where
    (expr,exprRest) = getExpr tokens
    (thenBranch,thenRest) = statement exprRest
    (elseBranch,elseRest) = getElse thenRest
    getExpr :: [Token] -> (Expression,[Token])
    getExpr(x:xs) = if x `match` [LEFT_PAREN]
      then let (expr,first:rest) = expression xs
          in if first `match` [RIGHT_PAREN]
            then (expr,rest)
            else loxError "Error in function ifStmt. Expected ')' after if condition" first
      else loxError "Error in function ifStmt. Expected '(' after 'if'" x
    getElse :: [Token] -> (Maybe Statement,[Token])
    getElse (first:xs) = if first `match` [ELSE]
      then let (stmt,rest) = statement xs in (Just stmt,rest)
      else (Nothing,first:xs)
{-
  Function for parsing a print-statement. 
  A print-statement is a expression representing what is to 
  be printed, followed by ";".
-}
printStmt:: [Token] -> (Statement,[Token])
printStmt tokens@(x:xs)= let (printexpr,first:rest) = expression tokens
    in if first `match` [SEMICOLON]
      then (PrintStmt printexpr,rest)
      else loxError "Error in function PrintStmt. Expected ';' after value" x
{-
  Function for parsing a while-statement. 
  A while-statement is a expression as condition inbetween "()"
  followed by a body made up of a statement.
-}
whileStmt :: [Token] ->(Statement,[Token])
whileStmt tokens@(x:xs) = if x `match` [LEFT_PAREN]
    then let (expr,first:rest) = expression xs
      in if first `match` [RIGHT_PAREN]
        then let (stmt, rest') = statement rest
          in (WhileStmt{condition=expr,body=stmt},rest')
        else loxError "Error in function WhileStmt. Expected ')' after condition" x
    else loxError "Error in function WhileStmt. Expected '(' after 'while'" x
{-
  Function for parsing block statements.
  A block statement is a list of declarations followed by a }. 
  It takes one argument: [Token] (the tokens to be parsed).

  Returns a tuple containing the block statement (with declarations nested) and 
  the rest of the tokenlist. 
-}
blockStmt :: [Token] -> (Statement,[Token])
blockStmt token = let (declarations,first:rest) = getDeclarations token
          in if first `match` [RIGHT_BRACE]
            then (BlockStmt declarations,rest)
            else loxError "Error in function BlockStmt. Expected '}' after block" first
  where
    getDeclarations :: [Token] -> ([Declaration],[Token])
    getDeclarations tokens@(x:xs) = if not (x `match` [RIGHT_BRACE] || x `match` [EOF])
      then let (firstdec,rest') = declaration tokens in
        let(restOfDec,rest'') = getDeclarations rest' in (firstdec:restOfDec,rest'')
      else ([],tokens)
{-
  Function for parsing a expression-statement. 
  A expression statement is an expression followed by ";".
-}
exprStmt :: [Token] -> (Statement,[Token])
exprStmt x = let (expr,first:rest) = expression x
      in if first `match` [SEMICOLON]
      then (ExpressionStmt expr,rest)
      else loxError "Error in function ExprStmt. Expected ';' after expression" (head x)
{-
  Function for parsing expressions. 
  An expression is a assignment. 
-}
expression :: [Token] -> (Expression,[Token])
expression = assignment
{-
  Function for parsing assignment expressions.
  A assignment expression is either an assignment or a logic-or-expression.
  
  Assigment is a identifiertoken (Variable) followed by a '=' and a value 
  of type expression. Throws error if left hand side is not a Variable, and 
  therefore not a valid assignmenttarget. 
  
  If the first token is not followed by a '=', the tokens are passed on to function
  orExpr to make a logic-or-expression. 
-}
assignment:: [Token] -> (Expression,[Token])
assignment t = let (expr,first:rest) = orExpr t
            in if first `match` [EQUAL]
              then let(val,rest') = assignment rest
              in if checkifVariable expr
                then (Assign{varAssignname = varname expr, value = val}, rest')
                else loxError "Error in function Assigment. Invalid assignment target" first
            else (expr,first:rest)
    where
      checkifVariable :: Expression -> Bool
      checkifVariable (Variable _) = True
      checkifVariable _ = False
{-
  Function for parsing logic-or-expression as a logical expression. 
  A logic-or-expression is a logic-and-expression followed by
  zero or more "or" each followed another logic-and-expression.
-}
orExpr ::[Token] -> (Expression,[Token])
orExpr x = let (orExpr,rest) = andExpr x
            in logicalCheck rest orExpr [OR] andExpr
{-
  Function for parsing logic-and-expression as a logical expression.
  A logic-and-expression is a equality-expression followed by
  zero or more "and" each followed another equality-expression.
-}
andExpr ::[Token] -> (Expression,[Token])
andExpr x = let (orExpr,rest) = equality x
            in logicalCheck rest orExpr [AND] equality
{-
  Function for parsing equality-expression as a binary expression.
  A equality-expression is a comparison-expression followed by
  zero or more "!=" or "==" each followed by another comparison-expression.
-}
equality :: [Token] -> (Expression,[Token])
equality x = let (equalityExpr,rest) = comparison x
              in binaryCheck rest equalityExpr [BANG_EQUAL,EQUAL_EQUAL] comparison
{-
  Function for parsing comparison-expression as a binary expression.
  A comparison-expression is a term-expression followed by
  zero or more ">",">=","<" or "<=" each followed by another term-expression.
-}
comparison :: [Token] -> (Expression,[Token])
comparison x = let (comparisonExpr,rest) = term x
                in binaryCheck rest comparisonExpr [GREATER,GREATER_EQUAL,LESS,LESS_EQUAL] term
{-
  Function for parsing term-expression as a binary expression.
  A term-expression is a factor-expression followed by
  zero or more "-" or "+" each followed by another factor-expression.
-}
term :: [Token] -> (Expression,[Token])
term x = let (factorExpr,rest) = factor x
          in binaryCheck rest factorExpr [MINUS,PLUS] factor
{-
  Function for parsing factor-expression as a binary expression.
  A term-expression is a unary-expression followed by
  zero or more "/" or "*" each followed by another unary-expression.
-}
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
{-
  Helperfunction to look for and if found parse one or several logical expressions. 
  It takes four arguments: 
  [Token] (the tokens to be parsed),  'Expression': the left hand expression in logical if it is to be created.
  [TokenType] a list of the tokentypes allowed to create the logical expression.
  [Token] -> (Expression,[Token]) the function creating the expression to be on the 
  right hand side of the logical expression.

  Returns a tuple containing the logical expression (with any more logical expressions nested) and 
  the rest of the tokenlist. 
-}
logicalCheck ::[Token] -> Expression -> [TokenType] -> ([Token] -> (Expression,[Token]))
              -> (Expression,[Token])
logicalCheck (x:xs) leftExpr tokenMatches exprType =
  if x `match` tokenMatches
    then let (rightExpr,rest') = exprType xs
          in logicalCheck rest' Logical{left = leftExpr, operator = x, right = rightExpr} tokenMatches exprType
    else (leftExpr,x:xs)

{-
  Function for parsing unary-expression as a Unary expression.
  A unary-expression is a "!" or "-" followed by
  another unary-expression.
  If next token does not match "!" or "-" it must be a primary expression instead.
-}
unary :: [Token] -> (Expression,[Token])
unary tokens@(x:xs) = if x `match` [BANG,MINUS]
  then let (unaryExpr,rest) = unary xs
        in (Unary{operator = x, right= unaryExpr}, rest)
  else primary tokens
{-
  Function for parsing primary-expressions.
  A primary-expression could be false,true,nil, number,string (as Literals),
  identifers (Variable) or blocks of code (Grouping).
  If next token does not match any of these it's considered to be an unexpected token.
-}
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
  _ -> loxError ("Error in function Primary. Unexpected token " ++ show (getTokenType x)) x
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

getIdentifierName :: Token -> String
getIdentifierName t = case getTokenLiteral t of
                ID s -> s
getOperator :: Token -> String
getOperator t = case getTokenType t of
  MINUS -> " - "
  PLUS -> " + "
  SLASH -> " / "
  STAR -> " * "
  BANG -> " !"
  BANG_EQUAL -> " != "
  EQUAL -> " = "
  EQUAL_EQUAL -> " == "
  GREATER -> " > "
  GREATER_EQUAL -> " >= "
  LESS -> " < "
  LESS_EQUAL -> " <= "
  AND -> " && "
  OR -> " || "

{-
  Function to throw an error in parsing state, 
  It takes two arguments, one of type [Char] (the string containing error info)
  and one of type 'Token', the token of which threw an error. 
-}
loxError :: [Char] -> Token -> error
loxError string tok = error (string ++ " on line "++ show (getTokenLine tok))