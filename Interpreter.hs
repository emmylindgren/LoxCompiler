module Interpreter (interpret) where
import ParserTree
import Data.Maybe (isNothing, fromJust)
import Tokens

data Enviroment = ENVIROMENT{variables::[(String,String)], enclosing::Maybe Enviroment}
--vad ska den returnera? Helst inget va? haha 
interpret :: Program -> String
interpret (PROGRAM (x:xs)) = show x
--något sådant? = execute x : execute xs

--In Lox, values are created by literals, computed by expressions, and stored in variables
--Eventuell lösning är väl att man skapar en datatyp som kan va antingen 
-- nummer(float), sträng, boolean eller Nothing typ? 
-- då skulle man ju också kunna göra "typecheck" där 
--typ  enligt nedan. Skulle behövt bättre namn men eftersom jag importerar tokens 
-- så finns redan num,number, string str osv där. 
data LoxObject = N Float | S String | B Bool | Null
    deriving Show

--evaluate ska utvärdera ett expression verkar de som.
-- återigen, returntyp? Kan inte vara olika.  
-- Det är ju typ sträng men det känns verkligen inte säkert. 
evaluate :: Expression -> LoxObject
evaluate h@(Literal l) = visitLiteralExpr h
--evaluate (Grouping e) = visitGroupingExpr

--Medan execute ska utvärdera statements. Känns som han dock har statements ist för declarations?
-- kolla upp detta. 

--- visitLiteralExpr ---
--Använder just nu loxobject datatypen för att representera de olika de kan bli
visitLiteralExpr :: Expression -> LoxObject
visitLiteralExpr (Literal l) = case l of 
    NUM n -> N n
    STR s -> S s
    ID s -> S s
    FALSE_LIT -> B False
    TRUE_LIT -> B True 
    NIL_LIT -> Null

--- visitGroupingExpr --- 
--frågetecken på returvärdet även här. 
{-
visit
visitGroupingExpr (Grouping e) = evaluate e 
-}


--- visitUnaryExp ---
{-
visitUnaryExpr (Unary op right) = case getTokenType op of 
    MINUS -> -rightVal 
    BANG -> not isTruthy right
    where 
        rightVal = evaluate right
-}
--isTruthy måste göras också
-- om det är Nothing vi får (null) så falskt, om de
-- är boolean skicka tillbaka den boolean annars skickar vi sant. Den ska 
-- returnera en bool men vad ska den få in? Kan ju vara vad som helst.Loxobject? 

--nedan saknar en hel del operator alternativ. Kommer kunna returnera 
-- siffra, sträng eller boolean. Måste kontrollera så vi får in nummer och så 
--vidare! 
{-
visitBinary (Binary left op right) = case getTokenType op of 
    MINUS -> N (leftVal - rightVal)
    SLASH -> N (leftVal / rightVal)
    STAR -> N (leftVal * rightVal)
    where 
        leftVal = evaluate left 
        rightVal = evaluate right
-}