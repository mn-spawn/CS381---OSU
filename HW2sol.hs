{-
Name: Madeline Spawn
Email: spawnm@oregonstate.edu
Date:1/30/2023
-}

-- I'm actually really vibing with Haskell :)

module HW2sol where 
import HW2types
import Data.List
sname = "Madeline Spawn"

--Excercise 1

--Part A - MultiSet Bag

ins :: Eq a => a -> Bag a -> Bag a
--base case
ins x [] = [(x,1)]
-- find the set that corresponds to the a value then increment the counter once added else keep
--looking for the proper set to add to.
ins x ((x1,y1):xs) | x == x1 = (x,(y1+1)):xs
                   | otherwise = (x1,y1) : (ins x (xs))


del :: Eq a  => a -> Bag a -> Bag a
del a [] = []
-- if the value is found with more than once instance decrement else if one instance filter it out and remove
del y ((x1,y1):xs) | y == x1 && y1 > 1 = ((y,y1-1):xs)
                   | y == x1 && y1 == 1 = deleteHelp (x1,y1) ((x1,y1):xs)
                   | otherwise  = (x1,y1) : (del y xs)

--filter in this function also from Data List (saw on Piazza forum) in this context anything with a 0 for y1 will taken out
deleteHelp y (x:xs)= filter(\f -> f/=y) xs


bag :: Eq a => [a] -> Bag a
--base case
bag [] = []
--recursive bag building by adding using insert recursively (ins checks and deals with multiples)
bag (x:xs) = ins x (bag xs)



subbag :: Eq a => Bag a -> Bag a -> Bool
--base cases
-- subbag of empty? no
subbag a [] = False
--empty subset of any bag? yes
subbag [] b = True
--check actual valued bags if searching value is found, true else recursively check
--check x and xs for it
subbag (x:xs) a = subbagHelp x a && subbag xs a

--check if number of values is at least contained in to bag otherwise keep looking
subbagHelp x [] = False
subbagHelp (x1,y1) ((x2,y2):xs) | y2 >= y1 = True
                                | otherwise = subbagHelp (x1,y1) xs


isSet :: Eq a => Bag a -> Bool
-- base case empty set
isSet ((x1,y1):[]) | y1 == 1 = True
isSet [] = False
-- check that the "y value" of the set is 1 if not return false else check recursively
isSet ((x1,y1):xs) | y1 == 1 = isSet xs
                   | otherwise = False

size :: Bag a -> Int
--base
size [] = 0
-- size of first element plus size of next element recursively
size ((x1,y1):xs) = y1 + size xs

-- Excercise 2

nodes :: Graph -> [Node]
--base case
nodes [] = []
--saw a classmate mention Data.List on piazza...use nub to throw away any duplicates then return the finished recursed list. 
nodes ((x1,y1):xs) = nub([x1,y1] ++ nodes xs)

suc:: Node -> Graph -> [Node]
suc x [] = []
--return the successor if the node matches the current search node else keep recursively looking
suc x ((x1,y1):xs)
        | x == x1 = [y1] ++ suc x xs
        | otherwise = suc x xs


detach :: Node -> Graph -> Graph
detach x [] = []
-- if the we found a node connected in any direction detach the nodes and if we find the node itseld detach
detach x ((x1,y1):xs) |x == x1 = detach x xs
                      |x == y1 = detach x xs
                      |otherwise = (x1,y1) : detach x xs


cyc :: Int -> Graph
--base case no cycle for one node
cyc 1 = []
--cycle for each node
cyc x = cycHelp x ++ [(x,1)]

cycHelp :: Int -> Graph 
cycHelp 1 = []
cycHelp x = (cycHelp (x-1) ++ [(x-1,x)])


-- Excercise 3
width :: Shape -> Length
--three shapes so three cases
--returns width (radius * 2 for circle)
width(Pt point) = 0
width(Circle point length) = (length * 2)
width(Rect point length1 length2) = length1

bbox :: Shape -> BBox
--bounding on shape
--pt has bounds of one point
bbox(Pt point) = (point, point)
--return point of max x and max y and then min x and min y
bbox(Circle (x,y) length) = (((x-length), (y-length)),((x+length),(y+length)))
bbox(Rect (x,y) length1 length2) = ((x,y), (x+length1, y+length2))


minX :: Shape -> Number
--return the minimum x value of each shape
minX(Pt (x,y)) = x
minX(Circle (x,y) length) = x-length
minX(Rect (x,y) length1 length2) = x

move :: Shape -> Point -> Shape
--this moves the shapes point by values in passed in point
--adds the new values of x and y to previous in point
move(Pt (x,y)) (x2,y2) = (Pt (x+x2,y+y2))
move(Circle (x,y) length) (x2,y2) = (Circle (x+x2,y+y2) length)
move(Rect (x,y) length1 length2) (x2,y2) = (Rect (x+x2,y+y2) length1 length2) 
