data Type a = Arrow (Type a) (Type a) | Var a



-- printType(Arrow  (Arrow  (Arrow  (Var 'o') (Var 'o')  )  (  Arrow  (  Var 'o'  ) (  Var 'o'  )  )  )  (  Arrow  (  Var 'o'  )  (  Var  'o' ) ))


printType :: Type Char -> [Char]
printType (Var x) = [x]
printType (Arrow left right) = "(" ++ (printType left) ++ ")" ++ "->" ++ (printType right)






{-
occT :: Type Char -> Int -> [(Int,(Int,Int))]
occT (Var x) n = [(n,(n,n))]
occT (Arrow left right) n = occT left n ++ occT right (n1+1) ++ [(n2+1,(n1,n2))]
                           where 
                            n1 = fst (head (reverse (occT left n)))
                            n2 = fst (head (reverse (occT right (n1+1))))
-}

firstOfThree :: (a, b, c) -> a
firstOfThree (x, _, _) = x


occT :: Type Char -> Int -> [(Int,[Char],(Int,Int))]
occT (Var x) n = [(n,printType (Var x),(n,n))]
occT (Arrow left right) n = occT left n ++ occT right (n1+1) ++ [(n2+1,printType (Arrow left right),(n1,n2))]
                           where 
                            n1 = firstOfThree (head (reverse (occT left n)))
                            n2 = firstOfThree (head (reverse (occT right (n1+1))))






polarities :: [(Int,(Int,Int))] -> [(Int,Char)]
polarities [] = []
polarities xs = helpPart xs (length xs - 1) 1

helpPart :: [(Int,(Int,Int))] -> Int -> Int -> [(Int,Char)]
helpPart xs pos sign | snd (xs !! pos) == (pos,pos) = [(pos, if sign > 0 then '+' else '-')]
                     | sign > 0 = [(pos, '+')] ++ helpPart xs (fst (snd (xs !! pos))) (-sign) ++ helpPart xs (snd (snd (xs !! pos))) sign 
                     | otherwise = [(pos, '-')] ++ helpPart xs (fst (snd (xs !! pos))) (-sign) ++ helpPart xs (snd (snd (xs !! pos))) sign 





get :: Eq a => a -> [(a,b)] -> b
get val (x:xs) | val == (fst x) = snd x
               | otherwise = get val xs

pre_grammar1 :: [(Int,(Int,Int))] -> [(Int,Char)] -> [(Int,(Int,Int))]
pre_grammar1 [] _ = []
pre_grammar1  (x:xs) ys | (snd x /= (fst x,fst x)) && get (fst x) ys == '+' = (fst x, snd x) : pre_grammar1  xs ys
                        | otherwise = pre_grammar1  xs ys