module Inhabitation where


import Grammars


emptiness :: Type Char -> Bool
emptiness t = check_emptiness (pre_grammar t) (m,[]) []
  where m = firstOfThree (last (occT t 0))

search :: Int -> [Int] -> Bool
search _ [] = False
search m (x:xs) | m==x = True
                | otherwise = search m xs

search_abs :: Int -> [(Int,(Int,Int))] -> Maybe (Int,Int)
search_abs _ [] = Nothing
search_abs m ((m',(k,n)):xs) | m==m' = Just (k,n)
                            | otherwise = search_abs m xs

search_apps :: Int -> [(Int,[Int])] -> [[Int]]
search_apps _  [] = []
search_apps m ((m',x):xs) | m == m' = x:search_apps m xs
                          | otherwise = search_apps m xs                          

check_emptiness :: ([(Int,(Int,Int))],[(Int,[Int])]) -> (Int,[Int]) -> [Int] ->Bool
check_emptiness pre (m,v) cache = if not (search m cache) then 
                                  case search_abs m (fst pre) of
                                  Just (k,n) -> if check_emptiness pre (n,k:v) (m:cache) then True
                                            else or [ and [ check_emptiness pre (n',v) (m:cache) | n' <- ns] | (k:ns) <- search_apps m (snd pre), search k v]
                                  _ -> or [ and [ check_emptiness pre (n',v) (m:cache) | n' <- ns] | (k:ns) <- search_apps m (snd pre), search k v]
                                  else False

test :: Bool
test = emptiness (myParser "((o->o)->o->o)->o->o")
