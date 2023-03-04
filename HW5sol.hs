module HW5sol where

import Data.Maybe
import HW5types


semCmd :: Cmd -> Stack -> Maybe Stack

--LDI
semCmd (LDI n) s =  Just (I n:s)
--LDB
semCmd (LDB b) s = Just (B b:s)

--LEQ
semCmd LEQ [] = Nothing
semCmd LEQ ((I x):[]) = Nothing
semCmd LEQ ((I x):(I y):s) = Just ((B(x<=y)):s)
semCmd LEQ _ = Nothing

--adds elememts
semCmd ADD [] = Nothing
semCmd ADD ((I x):[]) = Nothing
semCmd ADD ((I x):(I y):s) = Just (I(x+y):s)
semCmd ADD _ = Nothing

--multiplies elements
semCmd MULT [] = Nothing
semCmd MULT ((I x):[]) = Nothing
semCmd MULT (I x:I y:s) = Just (I(x*y):s)
semCmd MULT _ = Nothing


--duplicates an element
semCmd DUP [] = Nothing
semCmd DUP ((I x):s) = Just (I x: I x:s)
semCmd DUP ((B x):s) = Just (B x: B x:s)


--decs top most element
semCmd DEC [] = Nothing
semCmd DEC ((B x):s) = Nothing -- can't dec bool
semCmd DEC (I x:s) = Just (I(x-1):s)


--swaps on stack
semCmd SWAP [] = Nothing
semCmd SWAP [_] = Nothing -- no need to swap itself with itself
semCmd SWAP ((I x):(I y):s) = Just (I y:I x:s)
semCmd SWAP _ = Nothing


--pops off of the stack
semCmd (POP k) s
        | length s < k = Nothing --can't pop off k num of elements
        | otherwise = Just (drop k s) -- remove

semCmd (IFELSE (c:cs) (d:ds)) (B b:s) =
    case semCmd (POP 1) s of
        Just s' ->
            if b then
                case semCmd c s' of
                    Just s'' -> Just $ drop 1 s''
                    Nothing -> Nothing
            else
                case semCmd d s' of
                    Just s'' -> Just $ drop 1 s''
                    Nothing -> Nothing
        Nothing -> Nothing
semCmd (IFELSE _ _) _ = Nothing

--rankC maps a command above to it's specific rank
rankC :: Cmd -> CmdRank
rankC (LDI _) = (0, 1)  --pushes one val onto the stack
rankC (LDB _) = (0, 1)  --pushes one val onto the stack
rankC LEQ = (2, 1)      --removes two values and pushes one
rankC ADD = (2, 1)      -- ADD removes two values and pushes one
rankC MULT = (2, 1)     -- MULT removes two values and pushes one
rankC DUP = (1, 2)      -- DUP removes one value and pushes two
rankC DEC = (1, 1)      --removes one vals and pushes one
rankC SWAP = (2, 2)     --removes two vals and pushes two
rankC (POP n) = (n, 0)  --removes n vals and doesn't push any

rankC (IFELSE p1 p2) =
  let (r1, r2) = sumRanks (p1 ++ p2) (0, 0)
  in (min r1 r2, min 0 (min r1 r2))

sumRanks :: Prog -> CmdRank -> CmdRank
sumRanks [] cr = cr
sumRanks (cmd:cmds) (cr1, cr2) =
  let (r1, r2) = rankC cmd
  in sumRanks cmds (cr1 + r1, cr2 + r2)        

--rank aux function as reccomended in assignment
getRank :: CmdRank -> Rank -> Rank
getRank (a, b) r = (r-a)+b

        
rank :: Prog -> Rank -> Maybe Rank

rank [] r = Just r
rank (x:xs) r = rank xs (getRank (rankC x) r)

--rankP ranks a program
rankP :: Prog -> Rank -> Maybe Rank
rankP [] r = Just r
rankP (x:xs) r = case rank [x] r of
                    Nothing -> Nothing
                    Just r' | r' < 0 -> Nothing
                            | otherwise -> rankP xs r'

--run function
run :: Prog -> Stack -> Result
run [] s = A s
run (c:cs) s = case rankP (c:cs) (length s) of
             Nothing -> RankError
             Just _ -> case semCmd c s of
                                Nothing -> TypeError
                                Just s -> run cs s