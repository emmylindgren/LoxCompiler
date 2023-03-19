module Parser (parse) where
import Tokens
import Data.Maybe (isNothing, fromJust)
import ParserTree
{-|
    Author: Emmy Lindgren
    id19eln
    Date: 2023-02-24
-}
{-
  Function for parsing a list of tokens to a Program. 
  A program is a list of declarations.
-}
parse :: [Token] -> Program
parse tokens = PROGRAM decs
  where decs = getDeclarations tokens
{-
  Function for parsing declarations from a list of tokens. 
  If first token in list is "EOF" then there are no tokens to parse declarations
  from. Otherwise a declaration is fetched using the list and appended to the head 
  of the list with the rest of the declarations.
-}
getDeclarations :: [Token] -> [Declaration]
getDeclarations t@(x:_) = if x `match` [EOF]
  then []
  else declarationStmt : getDeclarations rest
  where (declarationStmt,rest) = declaration t
{-
  Function for parsing a declaration. 
  A declaration is either a variable declaration, function declaration or a statement.
-}
declaration ::[Token] -> (Declaration,[Token])
declaration tokens@(x:xs) = case getTokenType x of
  VAR -> varDeclaration xs
  FUN -> functionDeclaration xs
  _ -> (Statement stmt,rest)
  where (stmt,rest) = statement tokens
{-
  Function for parsing a variable declaration. 
  A variable declaration is a identifier-token optionally followed 
  by "=" and a initializer. If no initializer is added then it is set to nothing.
  A variable declaration must be followed by a ";".
-}
varDeclaration :: [Token] -> (Declaration,[Token])
varDeclaration tokens@(x:xs) = if x `match` [IDENTIFIER]
  then if first `match` [SEMICOLON]
      then (VarDec{name = x, initializer = init}, rest)
      else loxError "VarDeclaration" "Expected ';' after variable declaration" x
  else loxError "VarDeclaration" "Expected variable name" x
  where
    (init,first:rest) = getInitializer xs
    getInitializer :: [Token] -> (Maybe Expression,[Token])
    getInitializer t@(first:rest) = if first `match` [EQUAL]
      then let (expr, rest') = expression rest
        in (Just expr,rest')
      else (Nothing, t)
{-
  Function for parsing a function declaration. 
  A function is the function name (Identifier-token), followed by '()' optionally filled
  with parameters (Tokens) separated with ','. Followed by a block ([Declaration]) containing the 
  function-body. 
-}
functionDeclaration :: [Token] -> (Declaration,[Token])
functionDeclaration tokens@(first:second:xs)
  | not (first `match` [IDENTIFIER]) = loxError "FunctionDeclaration" "Expected function name" first
  | not (second `match` [LEFT_PAREN]) = loxError "FunctionDeclaration" "Expected '(' after function name" first
  | otherwise = (FuncDec{name=first, params = parameters,funcbody= funcBodyBlock},bodyRest)
  where
    (parameters,paramRest) = getParameters xs
    (funcBodyBlock,bodyRest) = getFunctionBody paramRest
    getParameters :: [Token] -> ([Token],[Token])
    getParameters tokens@(x:xs) = if x `match` [RIGHT_PAREN]
      then ([],xs)
      else let(params,first:rest) = collectParams tokens 0
        in if first `match` [RIGHT_PAREN]
          then (params,rest)
          else loxError "FunctionDeclaration" "Expected ')' after parameters" first
    collectParams :: [Token] -> Int -> ([Token],[Token])
    collectParams tokens@(first:second:rest) nrOf
      | nrOf  >= 255 = loxError "FunctionDeclaration" "Can't have more than 255 parameters" first
      | first `match` [IDENTIFIER] && second `match` [COMMA] = 
            let (restOfParams,restRest) = collectParams rest (nrOf+1) in (first:restOfParams,restRest)
      | first `match` [IDENTIFIER] = ([first],second:rest)
      | otherwise = loxError "FunctionDeclaration" "Expected parameter name" first
    getFunctionBody :: [Token] -> ([Declaration],[Token])
    getFunctionBody tokens@(x:xs)
      | not (x `match` [LEFT_BRACE]) = loxError "FunctionDeclaration" "Expected '{' before function body" x
      | otherwise = block xs
{-
  Function for parsing a statement. 
  A statement is a for-, if-, print-,return-, while-, block- or 
  expression statement. 
-}
statement :: [Token] -> (Statement,[Token])
statement tokens@(x:xs) = case getTokenType x of
  FOR -> forStmt xs
  IF -> ifStmt xs
  PRINT -> printStmt xs
  RETURN -> returnStmt tokens
  WHILE -> whileStmt xs
  LEFT_BRACE -> let (b,rest) = block xs
              in (BlockStmt b,rest)
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
  else loxError "ForStmt" "Expected '(' after 'for'" x
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
          else loxError "ForStmt" "Expected ';' after loop condition" first
    getIncr:: [Token] -> (Maybe Expression,[Token])
    getIncr tokens@(x:xs) = if x `match` [RIGHT_PAREN]
      then (Nothing,xs)
      else let (incr,f:rest) = expression tokens in
        if f `match` [RIGHT_PAREN]
          then (Just incr,rest)
          else loxError "ForStmt" "Expected ')' after for clauses" f
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
            else loxError "IfStmt" "Expected ')' after if condition" first
      else loxError "IfStmt" "Expected '(' after 'if'" x
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
      else loxError "PrintStmt" "Expected ';' after value" x
{-
  Function for parsing a return-statement. 
  A return-statement the fun-keyword optionally followed by an expression as a value 
  to return. Always have to end with ';'.
-}
returnStmt :: [Token] -> (Statement,[Token])
returnStmt tokens@(returnToken:second:xs)
  | second `match` [SEMICOLON] = (ReturnStmt{keyWord=returnToken,returnValue=Nothing},xs)
  | first `match` [SEMICOLON] = (ReturnStmt{keyWord=returnToken,returnValue=Just returnExpr},rest)
  | otherwise = loxError "ReturnStmt" "Expected ';' after returnvalue" returnToken
  where 
    (returnExpr,first:rest) = expression (second:xs)
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
        else loxError "WhileStmt" "Expected ')' after condition" x
    else loxError "WhileStmt" "Expected '(' after 'while'" x
{-
  Function for parsing a block to make a block statement.
  A block statement is a list of declarations followed by a }. 
-}
block :: [Token] -> ([Declaration],[Token])
block token = if first `match` [RIGHT_BRACE]
            then (declarations,rest)
            else loxError "BlockStmt" "Expected '}' after block" first
  where
    (declarations,first:rest) = getDeclarations token
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
exprStmt x = if first `match` [SEMICOLON]
      then (ExpressionStmt expr,rest)
      else loxError "ExprStmt" "Expected ';' after expression" (head x)
      where 
          (expr,first:rest) = expression x
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
assignment t = if first `match` [EQUAL]
          then if checkifVariable expr
              then (Assign{varAssignname = varname expr, value = val}, rest')
              else loxError "Assigment" "Invalid assignment target" first
          else (expr,first:rest)
    where
      (expr,first:rest) = orExpr t
      (val,rest') = assignment rest
      checkifVariable :: Expression -> Bool
      checkifVariable (Variable _) = True
      checkifVariable _ = False
{-
  Function for parsing logic-or-expression as a logical expression. 
  A logic-or-expression is a logic-and-expression followed by
  zero or more "or" each followed another logic-and-expression.
-}
orExpr ::[Token] -> (Expression,[Token])
orExpr x = logicalCheck rest orExpr [OR] andExpr
  where 
    (orExpr,rest) = andExpr x
{-
  Function for parsing logic-and-expression as a logical expression.
  A logic-and-expression is a equality-expression followed by
  zero or more "and" each followed another equality-expression.
-}
andExpr ::[Token] -> (Expression,[Token])
andExpr x = logicalCheck rest eqExpr [AND] equality
  where
    (eqExpr,rest) = equality x
{-
  Function for parsing equality-expression as a binary expression.
  A equality-expression is a comparison-expression followed by
  zero or more "!=" or "==" each followed by another comparison-expression.
-}
equality :: [Token] -> (Expression,[Token])
equality x = binaryCheck rest equalityExpr [BANG_EQUAL,EQUAL_EQUAL] comparison
  where 
    (equalityExpr,rest) = comparison x
{-
  Function for parsing comparison-expression as a binary expression.
  A comparison-expression is a term-expression followed by
  zero or more ">",">=","<" or "<=" each followed by another term-expression.
-}
comparison :: [Token] -> (Expression,[Token])
comparison x = binaryCheck rest comparisonExpr [GREATER,GREATER_EQUAL,LESS,LESS_EQUAL] term
  where 
    (comparisonExpr,rest) = term x
{-
  Function for parsing term-expression as a binary expression.
  A term-expression is a factor-expression followed by
  zero or more "-" or "+" each followed by another factor-expression.
-}
term :: [Token] -> (Expression,[Token])
term x = binaryCheck rest factorExpr [MINUS,PLUS] factor
  where 
    (factorExpr,rest) = factor x
{-
  Function for parsing factor-expression as a binary expression.
  A term-expression is a unary-expression followed by
  zero or more "/" or "*" each followed by another unary-expression.
-}
factor :: [Token] -> (Expression,[Token])
factor x = binaryCheck rest unaryExpr [SLASH,STAR] unary
  where 
    (unaryExpr,rest) = unary x
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
    then binaryCheck rest' Binary{left = leftExpr, operator = x, right = rightExpr} tokenMatches exprType
    else (leftExpr,x:xs)
  where 
    (rightExpr,rest') = exprType xs
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
    then logicalCheck rest' Logical{left = leftExpr, operator = x, right = rightExpr} tokenMatches exprType
    else (leftExpr,x:xs)
  where 
    (rightExpr,rest') = exprType xs
{-
  Function for parsing unary-expression as a Unary expression.
  A unary-expression is a "!" or "-" followed by
  another unary-expression.
  If next token does not match "!" or "-" it must be a call or primary expression instead.
-}
unary :: [Token] -> (Expression,[Token])
unary tokens@(x:xs) = if x `match` [BANG,MINUS]
  then (Unary{operator = x, right= unaryExpr}, rest)
  else call tokens
  where 
    (unaryExpr,rest) = unary xs
{-
  Function for parsing function call-expression as a Call expression.
  A call-expression is a primary expression followed by '(' with zero or at most
  255 parameters separated by ',' followed by a closing ')'. 
  If next token after primary not match "(" it must be a simple primary expression instead.
-}
call :: [Token] -> (Expression,[Token])
call tokens@(x:xs) = checkForFuncCall primaryexpr rest
  where
    (primaryexpr,rest) = primary tokens
    checkForFuncCall :: Expression -> [Token] -> (Expression,[Token])
    checkForFuncCall expr (x:xs) = if x `match` [LEFT_PAREN]
      then let(paramExpr,rest') = finishCall expr xs x
        in checkForFuncCall paramExpr rest'
      else (expr,x:xs)
    finishCall :: Expression -> [Token] -> Token -> (Expression,[Token])
    finishCall expr (x:xs) parenT
      | x `match` [RIGHT_PAREN] = (Call{callee = expr,paren=parenT,arguments=[]},xs)
      | f `match` [RIGHT_PAREN] = (Call{callee = expr,paren=parenT,arguments=firstArg:restArgs},restArgsRest)
      | otherwise = loxError "Call" "Expected ')' after arguments" parenT
      where
          (firstArg, firstArgRest) = expression (x:xs)
          (restArgs, f:restArgsRest) = getArguments firstArgRest 1
    getArguments :: [Token] -> Int -> ([Expression],[Token])
    getArguments tokens@(x:xs) nrOfParams
      | nrOfParams >= 255 = loxError "Call" "Can't have more than 255 arguments" x
      | x `match` [COMMA] = (firstexpr:restOfExpr,restRest)
      | otherwise = ([],tokens)
      where
          (firstexpr, firstRest) = expression xs
          (restOfExpr, restRest) = getArguments firstRest (nrOfParams + 1)
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
        else loxError "Primary"  "Expected ')' after grouping on line " first
  _ -> loxError "Primary" ("Unexpected token "  ++ show (getTokenType x)) x
  where
    saveTokenLiteral = (Literal (getTokenLiteral x), xs)

-------------- Helper functions -------------- 
match :: Token -> [TokenType] -> Bool
match t matches = let tokenType = getTokenType t
                    in tokenType `elem` matches

getTokenLine :: Token -> Int
getTokenLine (TOKEN _ _ _ l) = l
{-
  Function to throw an error in parsing state, 
  It takes two arguments, one of type [Char] (the string containing error info)
  and one of type 'Token', the token of which threw an error. 
-}
loxError :: [Char] -> [Char] -> Token -> error
loxError funcName string tok = error ("Error in function "++ funcName ++". "++ string ++
   " on line "++ show (getTokenLine tok))