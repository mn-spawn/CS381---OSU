module Stacklang1 where

type Prog = [Cmd]
type Stack = [Int]

data Cmd 
        = LD Int
        | ADD
        | MULT
        | DUP 
        deriving Show

semCmd :: Cmd -> Stack -> Maybe Stack

semCmd (LD n ) s =  Just (n:s) 

semCmd ADD [] = Nothing
semCmd ADD (x:[]) = Nothing
semCmd ADD (x:y:s) = Just ((x + y):s)

semCmd MULT [] = Nothing
semCmd MULT (x:[]) = Nothing
semCmd MULT (x:y:s) = Just ((x * y):s)

semCmd DUP [] = Nothing
semCmd DUP (x:s) = Just (x:x:s)


run :: Prog -> Stack -> Maybe Stack
run [] s = Just s
--run [] [] = Just []
run (c:cs) s = case semCmd c s of
                  Nothing -> Nothing
                  Just s' -> run cs s'

