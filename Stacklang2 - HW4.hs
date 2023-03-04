module Stacklang2 where

import Data.Maybe

type Prog = [Cmd]
type Stack = [Either Bool Int]

data Cmd 
        = LDI Int
        |LDB Bool
        | LEQ
        | ADD
        | MULT
        | DUP
        | IFELSE Prog Prog
        deriving Show

semCmd :: Cmd -> Stack -> Maybe Stack

semCmd (LDI n) s =  Just ((Right n):s) 

semCmd (LDB b) s = Just (Left b:s)

semCmd LEQ [] = Nothing
semCmd LEQ ((Right x):[]) = Nothing
semCmd LEQ ((Right x):(Right y):s) = Just ((Left(x<=y)):s)
semCmd LEQ _ = Nothing

semCmd ADD [] = Nothing
semCmd ADD ((Right x):[]) = Nothing
semCmd ADD ((Right x):(Right y):s) = Just ((Right(x+y)):s)
semCmd ADD _ = Nothing

semCmd MULT [] = Nothing
semCmd MULT ((Right x):[]) = Nothing
semCmd MULT ((Right x):(Right y):s) = Just ((Right(x*y)):s)
semCmd MULT _ = Nothing

semCmd DUP [] = Nothing
semCmd DUP ((Right x):s) = Just ((Right x): (Right x):s)
semCmd DUP ((Left x):s) = Just ((Left x): (Left x):s)


semCmd (IFELSE p1 p2) (Left b:s) = run (if b then p1 else p2) s
semCmd (IFELSE _ _) _ = Nothing

run :: Prog -> Stack -> Maybe Stack
run [] s = Just s
--run [] [] = Just []
run (c:cs) s = case semCmd c s of
                  Nothing -> Nothing
                  Just s' -> run cs s'