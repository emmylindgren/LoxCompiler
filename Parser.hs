module Parser (parse) where
import Tokens


{- Nedan från expressions.hs, förslag på datatypen
data Expr = Num Int
           | Add Expr Expr
           | Sub Expr Expr
           | Mult Expr Expr
           deriving (Show)
-}
--OBS! ALLA datatyper måste definieras om. Nu bara kladd. 
data A = Num Int
           | Add A A
           | Sub A A
           | Mult A A
           deriving (Show)
data Expr
  = IntLit    Int               -- integer constants, leaves of the expression trees
  | BinaryApp BinOp   Expr Expr
  | UnaryApp  UnaryOp Expr

data BinOp
  = MultOp
  | AddOp
  | DivOp
  | SubOp

data UnaryOp
  = NegOp
  | AbsOp

--Påbörjat försökt till parsning enligt boken.
parse :: [Token] -> A
parse x = Num 3

expression :: [Token] -> A
expression x = equality x 

-- The rule equality       → comparison ( ( "!=" | "==" ) comparison )* ; 
equality :: [Token] -> A
equality x =  let (expr,rest)= comparison x 
        in case rest of 
            (x:xs) -> if x == BANG_EQUAL || EQUAL_EQUAL
                        then BinOp expr x (comparison x)
                        else expr