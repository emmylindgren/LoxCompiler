module Parser (parse) where

import Tokens


{- Nedan från expressions.hs, förslag på datatypen
data Expr = Num Int
           | Add Expr Expr
           | Sub Expr Expr
           | Mult Expr Expr
           deriving (Show)

--OBS! ALLA datatyper måste definieras om. Nu bara kladd. 

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String
  deriving Show
-}
data Tree = Expression
  deriving Show

--Literal ska ju kunna va en sträng eller ett nummer I guess 
data Expression = Literal 
                | Unary Token Expression
                | Binary Expression Token Expression
                | Grouping Expression 
  deriving Show


{- Nedan från kap 6 iaf, de är funktionerna som kommer behövas.
 De ska också returnera ett träd samt resterande del av tokenlistan. 
expression :: [Token] -> (Tree, [Token])
equality :: [Token] -> (Tree, [Token])
comparison :: [Token] -> (Tree, [Token])
term       :: [Token] -> (Tree, [Token])
factor     :: [Token] -> (Tree, [Token])
unary :: [Token] -> (Tree, [Token])
primary :: [Token] -> (Tree, [Token])
-}

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
{-
parse :: [Token] -> A
parse x = Num 3 -}
parse :: [Token] -> A
parse x = let (tree,rest) = expression x 
        in 
          if null rest 
            then tree
            else error "Unexpected leftover tokens" --jobba ut denna mer? ska de bli fel här?  


expression :: [Token] -> (A,[Token])
expression = equality  

-- The rule equality       → comparison ( ( "!=" | "==" ) comparison )* ; 
equality :: [Token] -> (A,[Token])
equality x =  let (expr,rest)= comparison x 
        in case rest of 
            (x:xs) -> if x.TokenType == BANG_EQUAL || EQUAL_EQUAL
                        then (BinOp expr x (comparison x), xs)--ska denna ersätta expr nu?)
                        -- plus vi ska väl kalla på equality igen på nåt vis?
                        else (expr,rest)