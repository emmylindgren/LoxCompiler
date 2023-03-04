module Interpreter (interpret) where
import ParserTree
import Data.Maybe (isNothing, fromJust)
import Tokens

data Enviroment = ENVIROMENT{variables::[(String,String)], enclosing::Maybe Enviroment}
data Value = N Float | S String | B Bool | Null
    deriving (Show,Eq)

interpret :: Program -> [String]
interpret (PROGRAM (x:xs)) = [show x]
--något sådant? = execute x : execute xs

--evaluate ska utvärdera ett expression verkar de som.
-- återigen, returntyp? Kan inte vara olika.  
-- Det är ju typ sträng men det känns verkligen inte säkert. 
-- i javakoden enligt: 
--   private Object evaluate(Expr expr) {
 --   return expr.accept(this);
--  } och accept va ju som en switchsats vilekn de är. 
evaluate :: Expression -> Value
evaluate expr@(Literal l) = visitLiteralExpr expr
--evaluate (Grouping e) = visitGroupingExpr

--Medan execute ska utvärdera statements. Känns som han dock har statements ist för declarations?
-- kolla upp detta. 
visitLiteralExpr :: Expression -> Value
visitLiteralExpr (Literal l) = case l of
    NUM n -> N n
    STR s -> S s
    ID s -> S s
    FALSE_LIT -> B False
    TRUE_LIT -> B True
    NIL_LIT -> Null

visitGroupingExpr :: Expression -> Value
visitGroupingExpr (Grouping e) = evaluate e

visitUnaryExpr :: Expression -> Value
visitUnaryExpr (Unary op right) = case getTokenType op of
    MINUS -> N (-rightValNum)
    BANG -> B (not $ isTruthy rightVal)
    where
        rightVal = evaluate right
        rightValNum = getNumberOperand rightVal "visitUnaryExpr" op
        isTruthy :: Value -> Bool
        isTruthy v = case v of
            Null -> False
            B bool -> bool
            _ -> True

visitBinary :: Expression -> Value
visitBinary (Binary left op right) = case getTokenType op of
    GREATER -> B (leftValNum > rightValNum)
    GREATER_EQUAL -> B (leftValNum >= rightValNum)
    LESS -> B (leftValNum < rightValNum)
    LESS_EQUAL -> B (leftValNum <= rightValNum)
    BANG_EQUAL -> B (leftVal /= rightVal)
    EQUAL_EQUAL -> B (leftVal == rightVal)
    MINUS -> N (leftValNum - rightValNum)
    SLASH -> N (leftValNum / rightValNum)
    STAR -> N (leftValNum * rightValNum)
    PLUS -> if leftVal `isInstanceOf` "NUM" && rightVal `isInstanceOf` "NUM"
        then N (leftValNum + rightValNum)
        else if leftVal `isInstanceOf` "STRING" && rightVal `isInstanceOf` "STRING"
            then S (leftValString ++ rightValString)
            else loxError "visitBinary" "Operands must be two numbers or two strings" op
    where
        leftVal = evaluate left
        rightVal = evaluate right
        leftValNum = getNumberOperand leftVal "visitBinary" op
        rightValNum = getNumberOperand rightVal "visitBinary" op
        leftValString = getStringOperand leftVal
        rightValString = getStringOperand rightVal

---- Helper functions ----
isInstanceOf :: Value -> String -> Bool
isInstanceOf (N _) s = s == "NUM"
isInstanceOf (S _) s = s == "STRING"
isInstanceOf (B _) s = s == "BOOL"
isInstanceOf Null s = s == "NULL"

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