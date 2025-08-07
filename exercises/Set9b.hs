module Set9b where
import Mooc.Todo
import Data.List
--------------------------------------------------------------------------------
-- Ex 1
type Row = Int
type Col = Int
type Coord = (Row, Col)
nextRow :: Coord -> Coord
nextRow (i,j) = (i+1, 1)
nextCol :: Coord -> Coord
nextCol (i,j) = (i, j+1)
--------------------------------------------------------------------------------
-- Ex 2
type Size = Int
prettyPrint :: Size -> [Coord] -> String
prettyPrint n qs = unlines [ [ if (r,c) `elem` qs then 'Q' else '.' | c <- [1..n] ] | r <- [1..n] ]
--------------------------------------------------------------------------------
-- Ex 3
sameRow :: Coord -> Coord -> Bool
sameRow (i,j) (k,l) = i == k
sameCol :: Coord -> Coord -> Bool
sameCol (i,j) (k,l) = j == l
sameDiag :: Coord -> Coord -> Bool
sameDiag (i,j) (k,l) = (i - j) == (k - l)
sameAntidiag :: Coord -> Coord -> Bool
sameAntidiag (i,j) (k,l) = (i + j) == (k + l)
--------------------------------------------------------------------------------
-- Ex 4
type Candidate = Coord
type Stack = [Coord]
danger :: Candidate -> Stack -> Bool
danger c qs = any (\q -> sameRow c q || sameCol c q || sameDiag c q || sameAntidiag c q) qs
--------------------------------------------------------------------------------
-- Ex 5
prettyPrint2 :: Size -> Stack -> String
prettyPrint2 n qs = unlines [ [ if (r,c) `elem` qs then 'Q' else if danger (r,c) qs then '#' else '.' | c <- [1..n] ] | r <- [1..n] ]
--------------------------------------------------------------------------------
-- Ex 6
fixFirst :: Size -> Stack -> Maybe Stack
fixFirst n [] = Nothing
fixFirst n (q:qs) =
  let (r,c) = q
  in if c > n then Nothing
     else if danger q qs then fixFirst n ((r,c+1):qs)
          else Just (q:qs)
--------------------------------------------------------------------------------
-- Ex 7
continue :: Stack -> Stack
continue [] = []
continue (q:qs) = nextRow q : q : qs
backtrack :: Stack -> Stack
backtrack [] = []
backtrack (_:[]) = []
backtrack (_:q:qs) = nextCol q : qs
--------------------------------------------------------------------------------
-- Ex 8
step :: Size -> Stack -> Stack
step n s = case fixFirst n s of
  Just s' -> continue s'
  Nothing -> backtrack s
--------------------------------------------------------------------------------
-- Ex 9
finish :: Size -> Stack -> Stack
finish n s = if length s > n then tail s else finish n (step n s)
solve :: Size -> Stack
solve n = finish n [(1,1)]