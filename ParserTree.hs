module ParserTree where 
import Tokens
import Data.Maybe (isNothing, fromJust)
{-|
    Author: Emmy Lindgren
    id19eln
    Date: 2023-03-01
-}
data Program = PROGRAM [Declaration]
instance Show Program where
    show (PROGRAM decs) = (show $ length decs) ++ "\n" ++ (unlines $ map show decs)

data Declaration = VarDec{name::Token,initializer::Maybe Expression}
                  | Statement Statement
                  | FuncDec{name::Token, params::[Token], funcbody::[Declaration]}
instance Show Declaration where
  show (VarDec name init) = "V DEC -> " ++ getIdentifierName name ++
    if isNothing init
      then ";"
      else  "=" ++ show (fromJust init) ++ ";"
  show (Statement s) = show s
  show (FuncDec name params body) ="F DEC -> "++ getIdentifierName name ++ "("++ parameterString ++ 
    "{" ++ concatMap show body ++ "}" 
    where
      parameterString = if null params
        then ")"
        else getIdentifierName (head params) ++ concatMap getParamString (tail params) ++ ")"
      getParamString:: Token -> [Char]
      getParamString x = "," ++ getIdentifierName x

data Statement = ExpressionStmt Expression
                | IfStmt {condition::Expression, thenBranch::Statement,
                          elseBranch:: Maybe Statement}
                | ReturnStmt {keyWord::Token,returnValue:: Maybe Expression}
                | PrintStmt Expression
                | WhileStmt {condition::Expression, body::Statement}
                | BlockStmt [Declaration]
instance Show Statement where
  show (ExpressionStmt e) = show e ++ ";"
  show (IfStmt cond thenBranch elseBranch) = "if(" ++show cond ++") "
    ++ show thenBranch ++ if isNothing elseBranch
      then ""
      else " else " ++ show (fromJust elseBranch)
  show (ReturnStmt keyWord returnValue) = "return" ++ if isNothing returnValue
    then ";"
    else " " ++ show (fromJust returnValue) ++";"
  show (PrintStmt e) = "print " ++ show e ++ ";"
  show (WhileStmt cond body) = "while(" ++ show cond ++ ")" ++ show body
  show (BlockStmt declarations) = "{" ++ concatMap show declarations ++ "}"

data Expression = Literal Literal
                | Logical {left::Expression,operator::Token,right::Expression}
                | Unary {operator::Token,right::Expression}
                | Variable {varname::Token}
                | Assign {varAssignname::Token, value::Expression}
                | Binary {left::Expression, operator::Token,right::Expression}
                | Call {callee::Expression,paren::Token,arguments::[Expression]}
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
  show (Call callee _ args) = show callee ++"(" ++
    if null args
      then ")"
      else show (head args) ++ concatMap getParamString (tail args) ++ ")"
    where
      getParamString:: Expression -> [Char]
      getParamString x = "," ++ show x
  show (Grouping expr) = "(" ++ show expr ++ ")"

getIdentifierName :: Token -> String
getIdentifierName t = case getTokenLiteral t of
                ID s -> s

getTokenLiteral :: Token -> Literal
getTokenLiteral (TOKEN _ _ l _) = l

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
  
getTokenType :: Token -> TokenType
getTokenType (TOKEN t _ _ _) = t