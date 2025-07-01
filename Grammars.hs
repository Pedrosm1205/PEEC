module Grammars (myParser, occT, polarities, pre_grammar1, pre_grammar2, pre_grammar, Type(..), firstOfThree) where


import qualified Data.Set as Set
import Data.Char (isSpace)

data Type a = Arrow (Type a) (Type a) | Var a    
        deriving (Eq, Show)


splitExp :: String -> Int -> (String,String)
splitExp (')':xs) 0 = ([],xs)
splitExp (')':xs) n = (')':rest1,rest2)
                       where (rest1,rest2) = splitExp xs (n-1)
splitExp ('(':xs) n = ('(':rest1,rest2)
                        where (rest1,rest2) = splitExp xs (n+1)
splitExp (x:xs) n = (x:rest1,rest2)
                       where (rest1,rest2) = splitExp xs n

myParser :: String -> Type Char
myParser ('(':xs) = let (left,right) = splitExp xs 0 
                    in case right of
                        ('-':'>':rest) -> Arrow (myParser left) (myParser (drop 2 right))
                        _              -> myParser left
myParser (x : '-' : '>' : xs) = Arrow (Var x) (myParser xs)
myParser [x] = Var x



printType :: Type Char -> [Char]      
printType (Var x) = [x]
printType (Arrow left right) = "(" ++ (printType left) ++ "->" ++ (printType right) ++ ")"



firstOfThree :: (a, b, c) -> a
firstOfThree (x, _, _) = x

secondOfThree :: (a, b, c) -> b
secondOfThree (_, x, _) = x

lastOfThree :: (a, b, c) -> c
lastOfThree (_, _, x) = x




occT :: Type Char -> Int -> [(Int,Type Char,(Int,Int))]      
occT (Var x) n = [(n,Var x,(n,n))]                          
occT (Arrow left right) n = occT left n ++ occT right (n1+1) ++ [(n2+1,(Arrow left right),(n1,n2))]
                           where 
                            n1 = firstOfThree (head (reverse (occT left n)))
                            n2 = firstOfThree (head (reverse (occT right (n1+1))))




polarities :: [(Int,Type Char,(Int,Int))] -> [(Int,Char)]
polarities [] = []
polarities xs = helpPols xs (length xs - 1) 1 True  

helpPols :: [(Int,Type Char,(Int,Int))] -> Int -> Int -> Bool -> [(Int,Char)]
helpPols xs pos sign flag | lastOfThree (xs !! pos) == (pos,pos) = if sign > 0 || flag then [(pos, if sign > 0 then '+' else '-')] else []
                          | sign > 0 = [(pos, '+')] ++ helpPols xs (fst (lastOfThree (xs !! pos))) (-sign) True ++ helpPols xs (snd (lastOfThree (xs !! pos))) sign True
                          | otherwise = curr ++ helpPols xs (fst (lastOfThree (xs !! pos))) (-sign) False ++ helpPols xs (snd (lastOfThree (xs !! pos))) sign False
                                          where curr = if flag then [(pos, '-')] else []





iguais :: (Int,Type Char,(Int,Int)) -> [(Int,Type Char,(Int,Int))] -> [(Int,Int)]
iguais _ [] = []
iguais t (x:xs) | secondOfThree t == secondOfThree x = (firstOfThree t,firstOfThree x) : iguais t xs
                | otherwise = iguais t xs



equivalent :: [(Int,Type Char,(Int,Int))] -> [(Int,Type Char,(Int,Int))] -> [(Int,Int)]
equivalent [] _ = []
equivalent (x:xs) ys = iguais x ys ++ equivalent xs ys





relationT :: [(Int,Type Char,(Int,Int))] -> [((Int,Int),Int)]
relationT [] = []
relationT (x:xs) | lastOfThree x /= (firstOfThree x,firstOfThree x) = ((snd (lastOfThree x),firstOfThree x),fst (lastOfThree x)) : relationT xs
                 | otherwise = relationT xs 





get :: Int -> [(Int,Char)] -> Char
get _ [] = 'n'
get val (x:xs) | val == (fst x) = snd x
               | otherwise = get val xs


pre_grammar1 :: [(Int,Type Char,(Int,Int))] -> [(Int,Char)] -> [(Int,(Int,Int))]
pre_grammar1 [] _ = []
pre_grammar1  (x:xs) ys | (lastOfThree x /= (firstOfThree x,firstOfThree x)) && get (firstOfThree x) ys == '+' = (firstOfThree x, lastOfThree x) : pre_grammar1  xs ys
                        | otherwise = pre_grammar1  xs ys




findPath :: [((Int,Int),Int)] -> Int -> Int -> Maybe [Int]
findPath edges start end = dfs Set.empty start
   where
    dfs :: Set.Set Int -> Int -> Maybe [Int]
    dfs visited current
      | current == end = Just [end]
      | Set.member current visited = Nothing
      | otherwise =
            let visited' = Set.insert current visited
                nextNodes = [(next,n) | ((u,next),n) <- edges, u == current]
            in tryPaths visited' nextNodes
    
    tryPaths :: Set.Set Int -> [(Int,Int)] -> Maybe [Int]
    tryPaths _ [] = Nothing
    tryPaths visited ((n,d):ns) = 
        case dfs visited n of
            Just path -> Just (d:path)
            Nothing -> tryPaths visited ns


pre_grammar2 :: [(Int,Type Char,(Int,Int))] -> [(Int,Char)] -> [(Int,[Int])]
pre_grammar2 subTipos polars = [ (m,reverse path) | m <- [0..nr-1], get m polars == '+', p0 <- [0..nr-1], get p0 polars == '-', (x,ps) <- equivalent subTipos subTipos, x==m,Just path <- [findPath (relationT subTipos) ps p0]]
    where nr = length subTipos


pre_grammar :: Type Char -> ([(Int,(Int,Int))],[(Int,[Int])])
pre_grammar tipo = (pre_grammar1 subTipos polars,pre_grammar2 subTipos polars)
               where subTipos = occT tipo 0
                     polars   = polarities subTipos 

