module Interpreter (interpret) where
import ParserTree
import Tokens
import Data.Maybe (isNothing, fromJust)
import qualified Data.Map.Strict as Map

data Enviroment = ENVIROMENT{variables::[(String,Value)], enclosing::Maybe Enviroment}
data Value = N Float | S String | B Bool | Nil
    deriving Eq
instance Show Value where
    show (N f) = if last (show f) == '0'
        then init $ init $ show f
        else show f
    show (S s) = s
    show (B b) = show b
    show Nil = "Nil"

interpret :: Program -> [String]
interpret (PROGRAM decs) = reverse outList
    where
        (outList,env) = getOutPut decs ([],ENVIROMENT{variables=[],enclosing= Nothing})

getOutPut :: [Declaration] -> ([String],Enviroment) -> ([String],Enviroment)
getOutPut [] listAndEnv = listAndEnv
getOutPut (x:xs) listAndEnv = getOutPut xs (newList,newEnv)
    where
        (newList,newEnv) = execute x listAndEnv


execute :: Declaration -> ([String],Enviroment) -> ([String],Enviroment)
execute (Statement s) listAndEnv = case s of
    ExpressionStmt e -> visitExpressionStmt e listAndEnv
    IfStmt {} -> visitIfStmt s listAndEnv
    PrintStmt e -> visitPrintStmt e listAndEnv
    BlockStmt{} -> visitBlockStmt s listAndEnv
    WhileStmt{} -> visitWhileStmt s listAndEnv
execute dec@VarDec{} listAndEnv = visitVarDec dec listAndEnv

visitExpressionStmt :: Expression -> ([String],Enviroment) -> ([String],Enviroment)
visitExpressionStmt e (list,env) = newListAndEnv
    where
        (newListAndEnv, val) = evaluate e (list,env)

visitPrintStmt :: Expression -> ([String],Enviroment) -> ([String],Enviroment)
visitPrintStmt e listAndEnv = (show val:updatedList,updatedEnv)
    where
        ((updatedList,updatedEnv),val) = evaluate e listAndEnv

--visitVarStmt
visitVarDec :: Declaration -> ([String],Enviroment) -> ([String],Enviroment)
visitVarDec (VarDec name init) (list,env)= updatedEnviromentAndList
    where
        updatedEnviromentAndList = if isNothing init
            then (list,defineVariable env (idName, Nil))
            else (listFromVal,defineVariable envFromVal (idName,val))
        ((listFromVal,envFromVal),val) =  evaluate (fromJust init) (list,env)
        idName = getIdentifierName name

visitWhileStmt :: Statement -> ([String],Enviroment) -> ([String],Enviroment)
visitWhileStmt WhileStmt{condition, body} = runWhile condition body
    where
        runWhile :: Expression -> Statement ->([String],Enviroment) ->([String],Enviroment)
        runWhile cond bod listAndEnv' = if isTruthy conditionVal
            then runWhile cond bod bodyListAndEnv
            else conditionListAndEnv
            where
                (conditionListAndEnv,conditionVal) = evaluate cond listAndEnv'
                bodyListAndEnv = execute (Statement bod) conditionListAndEnv

visitBlockStmt :: Statement -> ([String],Enviroment) -> ([String],Enviroment)
visitBlockStmt (BlockStmt decs) (list,env) = (newList,fromJust enclosing)
    where
        (newList,ENVIROMENT{variables, enclosing}) = executeBlock decs (list, ENVIROMENT{variables=[], enclosing = Just env})
        executeBlock :: [Declaration] -> ([String],Enviroment) -> ([String],Enviroment)
        executeBlock [] listAndEnv = listAndEnv
        executeBlock (x:xs) listAndEnv = executeBlock xs newlistAndEnv
            where
                newlistAndEnv = execute x listAndEnv

visitIfStmt :: Statement -> ([String],Enviroment) -> ([String],Enviroment)
visitIfStmt IfStmt {condition, thenBranch, elseBranch} listAndEnv
  | isTruthy conditionVal = execute (Statement thenBranch) conditionListAndEnv
  | isNothing elseBranch = conditionListAndEnv
  | otherwise = execute (Statement (fromJust elseBranch)) conditionListAndEnv
  where
      (conditionListAndEnv, conditionVal) = evaluate condition listAndEnv

visitAssignmentExpr :: Expression -> ([String],Enviroment) -> (([String],Enviroment),Value)
visitAssignmentExpr Assign{varAssignname,value} listAndEnv@(list,env) = ((newList,newEnv),val)
    where
        newEnv = assignVariable valEnv (getIdentifierName varAssignname,val)
        ((newList,valEnv),val) = evaluate value listAndEnv

visitVariableExpr :: Expression -> ([String],Enviroment) -> (([String],Enviroment),Value)
visitVariableExpr (Variable name) listAndEnv@(list,env) = (listAndEnv,getVariable env (getIdentifierName name))


evaluate :: Expression -> ([String],Enviroment) -> (([String],Enviroment),Value)
evaluate expr@(Literal _) listAndEnv = (listAndEnv,visitLiteralExpr expr)
evaluate expr@(Logical {}) listAndEnv = visitLogicalExpr expr listAndEnv
evaluate expr@(Unary _ _) listAndEnv = visitUnaryExpr expr listAndEnv
evaluate expr@(Variable _) listAndEnv = visitVariableExpr expr listAndEnv
evaluate expr@(Assign{}) listAndEnv = visitAssignmentExpr expr listAndEnv
evaluate expr@(Binary {}) listAndEnv = visitBinaryExpr expr listAndEnv
evaluate expr@(Grouping _) listAndEnv = visitGroupingExpr expr listAndEnv

visitLiteralExpr :: Expression -> Value
visitLiteralExpr (Literal l) = case l of
    NUM n -> N n
    STR s -> S s
    ID s -> S s
    FALSE_LIT -> B False
    TRUE_LIT -> B True
    NIL_LIT -> Nil

visitLogicalExpr :: Expression -> ([String],Enviroment) -> (([String],Enviroment),Value)
visitLogicalExpr Logical {left,operator,right} listAndEnv
  | getTokenType operator == OR = if isTruthy leftVal then leftExpr else rightExpr
  | not $ isTruthy leftVal = leftExpr
  | otherwise = rightExpr
  where
      leftExpr@(leftListAndEnv, leftVal) = evaluate left listAndEnv
      rightExpr@(rightListAndEnv, rightVal)
        = evaluate right leftListAndEnv

visitGroupingExpr :: Expression -> ([String],Enviroment) -> (([String],Enviroment),Value)
visitGroupingExpr (Grouping e) = evaluate e

visitUnaryExpr :: Expression -> ([String],Enviroment)-> (([String],Enviroment),Value)
visitUnaryExpr (Unary op right) listAndEnv = case getTokenType op of
    MINUS -> (newlistAndEnv,N (-rightValNum))
    BANG -> (newlistAndEnv,B (not $ isTruthy rightVal))
    where
        (newlistAndEnv,rightVal) = evaluate right listAndEnv
        rightValNum = getNumberOperand rightVal "visitUnaryExpr" op

visitBinaryExpr :: Expression -> ([String],Enviroment) -> (([String],Enviroment),Value)
visitBinaryExpr (Binary left op right) listAndEnv = case getTokenType op of
    GREATER -> emit $ B (leftValNum > rightValNum)
    GREATER_EQUAL -> emit $ B (leftValNum >= rightValNum)
    LESS -> emit $ B (leftValNum < rightValNum)
    LESS_EQUAL -> emit $ B (leftValNum <= rightValNum)
    BANG_EQUAL -> emit $ B (leftVal /= rightVal)
    EQUAL_EQUAL ->emit $ B (leftVal == rightVal)
    MINUS -> emit $ N (leftValNum - rightValNum)
    SLASH -> emit $ N (leftValNum / rightValNum)
    STAR -> emit $ N (leftValNum * rightValNum)
    PLUS -> if leftVal `isInstanceOf` "NUM" && rightVal `isInstanceOf` "NUM"
        then emit $ N (leftValNum + rightValNum)
        else if leftVal `isInstanceOf` "STRING" && rightVal `isInstanceOf` "STRING"
            then emit $ S (leftValString ++ rightValString)
            else loxError "visitBinary" "Operands must be two numbers or two strings" op
    where
        (leftListAndEnv,leftVal) = evaluate left listAndEnv
        (rightListAndEnv,rightVal) = evaluate right leftListAndEnv
        leftValNum = getNumberOperand leftVal "visitBinary" op
        rightValNum = getNumberOperand rightVal "visitBinary" op
        leftValString = getStringOperand leftVal
        rightValString = getStringOperand rightVal
        emit :: Value -> (([String],Enviroment),Value)
        emit val = (rightListAndEnv,val)

---- Functions for the Enviroment ----
defineVariable :: Enviroment -> (String,Value) -> Enviroment
defineVariable (ENVIROMENT var enc) (name,val) = ENVIROMENT{variables = newVariables,enclosing=enc}
    where
        newVariables = Map.toList (Map.insert name val (Map.fromList var))

assignVariable :: Enviroment -> (String,Value) -> Enviroment
assignVariable (ENVIROMENT var enc) (name,val) = if isNothing (Map.lookup name variableMap)
    then if isNothing enc
        then error ("Undefined variable "++ name)
        else ENVIROMENT{variables = var, enclosing = Just updatedEnclosing}
    else ENVIROMENT{variables= newEnv, enclosing = enc}
    where
        updatedEnclosing = assignVariable (fromJust enc) (name,val)
        variableMap = Map.fromList var
        newEnv = Map.toList (Map.insert name val variableMap)

--felet som kastas här ska väl kastas som alla andra ! FIXA 
getVariable :: Enviroment -> String -> Value
getVariable (ENVIROMENT var enc) name = if isNothing maybeValue
    then if isNothing enc
        then error ("getvariable undefined variable " ++ name)
        else getVariable (fromJust enc) name
    else fromJust maybeValue
    where
        maybeValue = Map.lookup name (Map.fromList var)
---- Functions for the Enviroment ----

---- Helper functions ----
isInstanceOf :: Value -> String -> Bool
isInstanceOf (N _) s = s == "NUM"
isInstanceOf (S _) s = s == "STRING"
isInstanceOf (B _) s = s == "BOOL"
isInstanceOf Nil s = s == "NIL"

isTruthy :: Value -> Bool
isTruthy v = case v of
            Nil -> False
            B bool -> bool
            _ -> True
-- I javakoden heter denna checkNumberOperand men de gör samma sak typ, forutom 
-- min returnerar också. 
getNumberOperand :: Value -> String -> Token -> Float
getNumberOperand (N n) _ _ = n
getNumberOperand _ funcName tok = loxError funcName "Operand must be a number" tok

getStringOperand :: Value -> String
getStringOperand (S s) = s

--Om ett fel uppstår är det ok att ditt program kastar ett exception och avslutar.
-- bör kanske kasta ett exception här? Gör om i så fall! 
loxError :: [Char] -> [Char] -> Token -> error
loxError funcName string tok = error ("Error in function "++ funcName ++". "++ string ++
   " on line "++ show (getTokenLine tok))

getTokenLine :: Token -> Int
getTokenLine (TOKEN _ _ _ l) = l