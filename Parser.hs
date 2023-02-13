module Parser (parse) where
import Tokens


{- Nedan från expressions.hs, förslag på datatypen
data Expr = Num Int
           | Add Expr Expr
           | Sub Expr Expr
           | Mult Expr Expr
           deriving (Show)
-}
data A = Num Int
           | Add A A
           | Sub A A
           | Mult A A
           deriving (Show)

parse :: [Token] -> A
parse x = Num 3