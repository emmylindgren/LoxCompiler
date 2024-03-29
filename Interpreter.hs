module Interpreter (interpret) where
import ParserTree
import Tokens
import Data.Maybe (isNothing, fromJust)
import qualified Data.Map.Strict as Map
{-|
    Author: Emmy Lindgren
    id19eln
    Date: 2023-03-19
-}
data Enviroment = ENVIROMENT{variables::[(String,Value)], enclosing::Maybe Enviroment, returnVal::Maybe Value}
data Value = N Float | S String | B Bool | Nil
  | Func Fun
  deriving Eq
instance Show Value where
    show (N f) = if last (show f) == '0'
        then init $ init $ show f
        else show f
    show (S s) = s
    show (B b) = show b
    show Nil = "Nil"

data Fun = FUNC{funcname :: Token, funcparams::[Token],funBody::[Declaration],enviroment:: Enviroment}
instance Eq Fun where
  f == p = 
    (getIdentifierName (funcname f) == getIdentifierName (funcname p))&& ( length (funcparams p) == length (funcparams f))
{-
  Function for interpreting a Program to a list of 
  output-strings. A program is a list of declarations.
-}
interpret :: Program -> [String]
interpret (PROGRAM decs) = reverse outList
    where
        (outList,env) = getOutPut decs ([],ENVIROMENT{variables=[],enclosing= Nothing,returnVal = Nothing})
{-
  Function for intepreting a list of declarations to a list of output-strings. 
  It takes two argument, a list of declarations to interpret, and a tuple
  containing a list of output-strings and an Enviroment. The enviroment is to keep track of 
  variables and scope in the program. 
  If the list is empty then there are no declarations to interpret. Otherwise the first 
  declaration is interpreted and the resulting output-string list and enviroment is 
  used for interpreting the rest of the list. 
-}
getOutPut :: [Declaration] -> ([String],Enviroment) -> ([String],Enviroment)
getOutPut [] listAndEnv = listAndEnv
getOutPut (x:xs) listAndEnv = getOutPut xs (newList,newEnv)
    where
        (newList,newEnv) = execute x listAndEnv
{-
  Function for interpreting a declaration. 
  A declaration is either a statement, a variable declaration or a function declaration.
  The output-string list and enviroment is updated through the matching 
  function depending on what kind of declaration it is. 
-}
execute :: Declaration -> ([String],Enviroment) -> ([String],Enviroment)
execute (Statement s) listAndEnv = case s of
    ExpressionStmt e -> visitExpressionStmt e listAndEnv
    IfStmt {} -> visitIfStmt s listAndEnv
    PrintStmt e -> visitPrintStmt e listAndEnv
    WhileStmt{} -> visitWhileStmt s listAndEnv
    BlockStmt{} -> visitBlockStmt s listAndEnv
    ReturnStmt{} -> visitReturnStmt s listAndEnv
execute dec@VarDec{} listAndEnv = visitVarDec dec listAndEnv
execute dec@FuncDec{} listAndEnv = visitFuncDec dec listAndEnv
{-
  Function for interpreting a Expression statement. 
  The output-string list and enviroment is updated through
  evaluating the expression. 
-}
visitExpressionStmt :: Expression -> ([String],Enviroment) -> ([String],Enviroment)
visitExpressionStmt e (list,env) = newListAndEnv
    where
        (newListAndEnv, val) = evaluate e (list,env)
{-
  Function for interpreting a If statement. 
  The output-string list and enviroment is updated through
  evaluating the condition expression. The resulting value is used to determine whether
  to run the then-branch or the optional else-branch. If any of the branches are to run,
  the output-string list and enviroment is updated through executing that branch statement.
-}
visitIfStmt :: Statement -> ([String],Enviroment) -> ([String],Enviroment)
visitIfStmt IfStmt {condition, thenBranch, elseBranch} listAndEnv
  | isTruthy conditionVal = execute (Statement thenBranch) conditionListAndEnv
  | isNothing elseBranch = conditionListAndEnv
  | otherwise = execute (Statement (fromJust elseBranch)) conditionListAndEnv
  where
      (conditionListAndEnv, conditionVal) = evaluate condition listAndEnv
{-
  Function for interpreting a Print statement. 
  The output-string list and enviroment is updated through
  evaluating the expression, and the value resulting from that
  is added to the output-string list. 
-}
visitPrintStmt :: Expression -> ([String],Enviroment) -> ([String],Enviroment)
visitPrintStmt e listAndEnv = (show val:updatedList,updatedEnv)
    where
        ((updatedList,updatedEnv),val) = evaluate e listAndEnv
{-
  Function for interpreting a While statement. 
  The output-string list and enviroment is updated through evaluating the condition
  expression. The resulting value is used to determine whether to run the body. 
  If the body is to run, the output-string list and enviroment is updated through executing
  the body statement, and then the condition is checked again and body is executed until
  it is no longer true. 
-}
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
{-
  Function for interpreting a Block statement.
  A new enviroment containing the enclosing enviroment is created for the block.
  Then the output-string list and new enviroment is updated through executing the block
  body (list of declarations). When the body is executed then the enviroment is set 
  back to the original enviroment and returned with the updated output-string list.
-}
visitBlockStmt :: Statement -> ([String],Enviroment) -> ([String],Enviroment)
visitBlockStmt (BlockStmt decs) (list,env) = (newList,fromJust enclosing)
    where
      (newList,ENVIROMENT{variables,enclosing,returnVal}) =
          executeBlock decs (list, ENVIROMENT{variables=[], enclosing = Just env, returnVal = Nothing})
{-
  Function for executing a list of declarations. 
-}
executeBlock :: [Declaration] -> ([String],Enviroment) -> ([String],Enviroment)
executeBlock [] listAndEnv = listAndEnv
executeBlock (x:xs) listAndEnv = if isNothing (returnVal newEnv)
  then executeBlock xs newlistAndEnv
  else newlistAndEnv
    where
        newlistAndEnv@(newList,newEnv) = execute x listAndEnv
{-
  Function for interpreting a Return statement.
  If the return statement contains any value, the value expression is evaluated and 
  returned. If not, Nil is returned as default. 
-}
visitReturnStmt :: Statement -> ([String],Enviroment) -> ([String],Enviroment)
visitReturnStmt ReturnStmt{keyWord,returnValue} (s,env) = if isNothing returnValue
  then (s,env {returnVal = Just Nil})
  else (newS,newEnv{returnVal = Just return})
  where 
    ((newS,newEnv),return) = evaluate (fromJust returnValue) (s,env)
{-
  Function for interpreting a Variable declaration.
  If the variable declaration has an initializer then the output-string list and 
  enviroment is updated through evaluating the initializer expression, and the value 
  resulting from that is set to correspond to the variable name in the enviroment.
  If the variable declaration lacks initializer then the value is set to lox "nil".  
-}
visitVarDec :: Declaration -> ([String],Enviroment) -> ([String],Enviroment)
visitVarDec (VarDec name init) (list,env)= updatedEnviromentAndList
    where
        updatedEnviromentAndList = if isNothing init
            then (list,defineVariable env (name, Nil))
            else (listFromVal,defineVariable envFromVal (name,val))
        ((listFromVal,envFromVal),val) =  evaluate (fromJust init) (list,env)
{-
  Function for interpreting a Function declaration. 
  The current enviroment is set as the scope of the function, and the function 
  is declared in the enviroment as a Func-Value. 
-}
visitFuncDec :: Declaration -> ([String],Enviroment) -> ([String],Enviroment)
visitFuncDec FuncDec {name, params, funcbody} (list,env) = (list,newEnv)
  where
    newEnv = defineVariable env (name,Func function)
    function = FUNC{funcname = name,funcparams = params, funBody = funcbody, enviroment = newEnv}
{-
  Function for interpreting a Expression.
  Depending on the type of the expression the output-string list and 
  enviroment is updated through calling the matching function.
-}
evaluate :: Expression -> ([String],Enviroment) -> (([String],Enviroment),Value)
evaluate expr@(Literal _) listAndEnv = (listAndEnv,visitLiteralExpr expr)
evaluate expr@(Logical {}) listAndEnv = visitLogicalExpr expr listAndEnv
evaluate expr@(Unary _ _) listAndEnv = visitUnaryExpr expr listAndEnv
evaluate expr@(Variable _) listAndEnv = visitVariableExpr expr listAndEnv
evaluate expr@(Assign{}) listAndEnv = visitAssignmentExpr expr listAndEnv
evaluate expr@(Binary {}) listAndEnv = visitBinaryExpr expr listAndEnv
evaluate expr@(Grouping _) listAndEnv = visitGroupingExpr expr listAndEnv
evaluate expr@(Call{}) listAndEnv = visitCallExpr expr listAndEnv
{-
  Function for interpreting a Literal Expression.
  The function takes an Expression of type literal and returns the 
  corresponding value of that Expression, wrapped in the datatype Value. 
-}
visitLiteralExpr :: Expression -> Value
visitLiteralExpr (Literal l) = case l of
    NUM n -> N n
    STR s -> S s
    ID s -> S s
    FALSE_LIT -> B False
    TRUE_LIT -> B True
    NIL_LIT -> Nil
{-
  Function for interpreting a Logical Expression.
  If the operator is 'or' then the left value is used if it's truthy,
  otherwise the right value is used.
  If it's not the 'or' operator then it must be the 'and' operator,
  in which case left value is used if it's not truthy, otherwise the right is used.
-}
visitLogicalExpr :: Expression -> ([String],Enviroment) -> (([String],Enviroment),Value)
visitLogicalExpr Logical {left,operator,right} listAndEnv
  | getTokenType operator == OR = if isTruthy leftVal then leftExpr else rightExpr
  | not $ isTruthy leftVal = leftExpr
  | otherwise = rightExpr
  where
      leftExpr@(leftListAndEnv, leftVal) = evaluate left listAndEnv
      rightExpr@(rightListAndEnv, rightVal)
        = evaluate right leftListAndEnv
{-
  Function for interpreting a Unary Expression.
  Depending on type of operatior the unary has, different actions are
  applied to the value that results from evaluating the right-expression in the unary.
  If it's the '-' operator then the value is set to be negative, if its '!' then the
  value is negated.
-}
visitUnaryExpr :: Expression -> ([String],Enviroment)-> (([String],Enviroment),Value)
visitUnaryExpr (Unary op right) listAndEnv = case getTokenType op of
    MINUS -> (newlistAndEnv,N (-rightValNum))
    BANG -> (newlistAndEnv,B (not $ isTruthy rightVal))
    where
        (newlistAndEnv,rightVal) = evaluate right listAndEnv
        rightValNum = getNumberOperand rightVal "visitUnaryExpr" op
{-
  Function for interpreting a Variable Expression.
  The value corresponding to the variable name is fetched from the enviroment.
-}
visitVariableExpr :: Expression -> ([String],Enviroment) -> (([String],Enviroment),Value)
visitVariableExpr (Variable name) listAndEnv@(list,env) = (listAndEnv,getVariable env name)
{-
  Function for interpreting a Assignment Expression.
  The value corresponding to the variable name is updated in the enviroment.
-}
visitAssignmentExpr :: Expression -> ([String],Enviroment) -> (([String],Enviroment),Value)
visitAssignmentExpr Assign{varAssignname,value} listAndEnv@(list,env) = ((newList,newEnv),val)
    where
        newEnv = assignVariable valEnv (varAssignname,val)
        ((newList,valEnv),val) = evaluate value listAndEnv
{-
  Function for interpreting a Binary Expression.
  Depending on the type of the operator in the binary, different operations 
  are performed on the resulting values from evaluating the left and right expressions
  in the binary. Typechecking is also performed to make sure operators are 
  used on the appropriate types.
-}
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
{-
  Function for interpreting a Grouping Expression.
  The expression in the grouping is evaluated.
-}
visitGroupingExpr :: Expression -> ([String],Enviroment) -> (([String],Enviroment),Value)
visitGroupingExpr (Grouping e) = evaluate e
{-
  Function for interpreting a Call expression. 
  The callee is evaluated, only functions can be called. Then the function is called 
  with the scope of the enviroment with the parameters from the call. 
-}
visitCallExpr :: Expression -> ([String],Enviroment) -> (([String],Enviroment),Value)
visitCallExpr Call{callee, paren, arguments} listAndEnv = if length argsValues == length (funcparams function)
    then callFunction function argsValues argsListAndEnv
    else loxError "visitCallExpr" ("Expected " ++ show (length $ funcparams function) ++ " arguments but got " ++ show (length argsValues)) paren
  where
    (calleeListAndEnv,calleeValue) = evaluate callee listAndEnv
    function = getFunction calleeValue
    (argsListAndEnv,argsValues) = getArgsValues arguments calleeListAndEnv
    getArgsValues :: [Expression] -> ([String],Enviroment) -> (([String],Enviroment),[Value])
    getArgsValues [] listAndEnv = (listAndEnv,[])
    getArgsValues (x:xs) listAndEnv = (restListAndEnv,firstArgValue:restArgValue)
      where
        (firstListAndEnv,firstArgValue) = evaluate x listAndEnv
        (restListAndEnv,restArgValue) = getArgsValues xs listAndEnv
    getFunction :: Value -> Fun
    getFunction (Func f) = f
    getFunction _ = loxError "visitCallExpr" "Can only call functions and classes" paren
{-
  Helper-function for function call expression. Executes a function call.
  Takes a function, a list of parameters and the current stringlist and enviroment.
  Merges the current enviroment with the functions scope, updating the function scope.
  Then running the function body. Lastly, updating the current enviroment with the 
  enviroment returning from the function body.
-}
callFunction :: Fun -> [Value] -> ([String],Enviroment) -> (([String],Enviroment),Value)
callFunction fun paramVals (list,env) = if isNothing returnVal
  then ((updatedList,resultingEnv),Nil)
  else ((updatedList,resultingEnvWithoutReturn),fromJust returnVal)
  where
    resultingEnvWithoutReturn = resultingEnv {returnVal = Nothing}
    resultingEnv = mergeEnviroments env (fromJust enclosing)
    (updatedList,ENVIROMENT{variables,enclosing,returnVal}) = executeBlock (funBody fun) (list,funEnviromentWithParams)
    funEnviroment = mergeEnviroments (enviroment fun) env
    funEnviromentWithParams =
      addParams ENVIROMENT{variables = [],enclosing = Just funEnviroment, returnVal = Nothing} (funcparams fun) paramVals
    addParams :: Enviroment -> [Token] -> [Value] -> Enviroment
    addParams env [] _ = env
    addParams env (firstName:restNames) (firstVal:restVals) =
      addParams (defineVariable env (firstName,firstVal)) restNames restVals

---- Functions for the Enviroment ----
{-
  Help-function. Merges two enviroments.
  Takes two enviroments, the original to be updated, and updates: the one to update from.
  Updates the variables from the original if they are to be found in the updates.
-}
mergeEnviroments :: Enviroment -> Enviroment -> Enviroment
mergeEnviroments original updates = if isNothing (enclosing original)
  then ENVIROMENT{variables= updateVarList (variables original), enclosing = Nothing, returnVal = returnVal original}
  else ENVIROMENT{variables = updateVarList (variables original), enclosing = Just newEnc,returnVal = returnVal original}
  where
    newEnc = mergeEnviroments (fromJust (enclosing original)) updates
    updateVarList :: [(String,Value)] -> [(String,Value)]
    updateVarList [] = []
    updateVarList (x:xs) = updateValue x : updateVarList xs
    updateValue :: (String,Value) -> (String,Value)
    updateValue (name,val) = if isNothing maybeNewValue
      then (name,val)
      else (name,fromJust maybeNewValue)
      where
        maybeNewValue = getFromUpdatingEnviroment name updates
    getFromUpdatingEnviroment :: String -> Enviroment -> Maybe Value
    getFromUpdatingEnviroment name upd = if isNothing maybeValue
      then if isNothing (enclosing upd)
        then Nothing
        else getFromUpdatingEnviroment name (fromJust (enclosing upd))
      else
        maybeValue
       where
        maybeValue = Map.lookup name (Map.fromList (variables upd))
{-
  Function for defining a variable in the enviroment.
  Takes an enviroment (of which the variable is to be added to) and 
  a key value pair with the variable-name and value.
  If the variable (key) is already present then it is overwritten with 
  the new value.
-}
defineVariable :: Enviroment -> (Token,Value) -> Enviroment
defineVariable (ENVIROMENT var enc returnVal) (nameTok,val) = ENVIROMENT{variables = newVariables,enclosing=enc,returnVal = returnVal}
    where
        newVariables = Map.toList (Map.insert (getIdentifierName nameTok) val (Map.fromList var))
{-
  Function for assigning a variable in the enviroment.
  Takes an enviroment (which contains the variable to be assigned) and 
  a key value pair with the variable-name and the new value.
  If the variable (key) is not present in the enviroment then the enclosing enviroment
  is checked. If there is no enclosing enviroment an error is thrown. 
-}
assignVariable :: Enviroment -> (Token,Value) -> Enviroment
assignVariable (ENVIROMENT var enc returnVal) (nameTok,val) = if isNothing (Map.lookup idName variableMap)
    then if isNothing enc
        then loxError "assignVariable" ("Trying to assign undefined variable "++idName) nameTok
        else ENVIROMENT{variables = var, enclosing = Just updatedEnclosing,returnVal = returnVal}
    else ENVIROMENT{variables= newEnv, enclosing = enc, returnVal = returnVal}
    where
        updatedEnclosing = assignVariable (fromJust enc) (nameTok,val)
        variableMap = Map.fromList var
        newEnv = Map.toList (Map.insert idName val variableMap)
        idName = getIdentifierName nameTok
{-
  Function for fetching a variable from the enviroment.
  Takes an enviroment (which contains the variable) and a string representing 
  the wanted variable name (key). If the variable (key) is not present in the 
  enviroment then the enclosing enviroment is checked. If there is no enclosing enviroment
  an error is thrown. 
-}
getVariable :: Enviroment -> Token -> Value
getVariable (ENVIROMENT var enc returnVal) nameTok = if isNothing maybeValue
    then if isNothing enc
        then loxError "getVariable" ("Trying to get undefined variable "++ idName) nameTok
        else getVariable (fromJust enc) nameTok
    else fromJust maybeValue
    where
        maybeValue = Map.lookup idName (Map.fromList var)
        idName = getIdentifierName nameTok
---- Functions for the Enviroment ----
---- Helper functions ----
{-
  Function for checking if a Value is of a certain Value type.
  Takes a Value and a string representing the certain Value type.
-}
isInstanceOf :: Value -> String -> Bool
isInstanceOf (N _) s = s == "NUM"
isInstanceOf (S _) s = s == "STRING"
isInstanceOf (B _) s = s == "BOOL"
isInstanceOf Nil s = s == "NIL"
isInstanceOf Func {} s = s == "FUN"
{-
  Function for checking the truthiness of a Value.
  All types of Values are True except Nil and False. 
-}
isTruthy :: Value -> Bool
isTruthy v = case v of
            Nil -> False
            B bool -> bool
            _ -> True
{-
  Function for fetching the number out of a Value.
  Takes a value, then a string representing the callee function name and 
  a token needed if an error is to be thrown. Error is thrown when Value is 
  not of type N (Number). 
-}
getNumberOperand :: Value -> String -> Token -> Float
getNumberOperand (N n) _ _ = n
getNumberOperand _ funcName tok = loxError funcName "Operand must be a number" tok
{-
  Function for fetching the string out of a Value.
-}
getStringOperand :: Value -> String
getStringOperand (S s) = s
{-
  Function to throw an error in interpreting state, 
  It takes two arguments, one of type [Char] (the string containing error info)
  and one of type 'Token', the token of which threw an error. 
-}
loxError :: [Char] -> [Char] -> Token -> error
loxError funcName string tok = error ("Error in function "++ funcName ++". "++ string ++
   " on line "++ show (getTokenLine tok))

getTokenLine :: Token -> Int
getTokenLine (TOKEN _ _ _ l) = l