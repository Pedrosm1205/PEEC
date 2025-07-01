import Grammars

data Lambda = Variable Char | Index Int | Abs Char Lambda | App Lambda Lambda
    deriving (Eq,Show)

m :: Lambda
m = Abs 'x' (Abs 'y' (App (App (Variable 'x') (Abs 'z' (Variable 'y')))  (Variable 'y')))

typeChecking :: Lambda -> Type Char -> Bool
typeChecking term t =
    case occT t 0 of
        [] -> False
        occs ->
            let n = firstOfThree (last occs)
                pre = pre_grammar t
            in help_check term n pre

help_check :: Lambda -> Int -> ([(Int,(Int,Int))],[(Int,[Int])]) -> Bool
help_check (Index x) m pre = 
        case search_pre2 m x (snd pre) of
            Just [] -> True 
            _ -> False 

help_check (Abs x n) m pre = 
    case search_pre1 m (fst pre) of
        Just (k, ni) -> help_check (substitute n k x) ni pre 
        Nothing      -> False
help_check (App n ns) m pre = 
    case getNs n of
        [] -> False
        (Index k : nss) ->
            case search_pre2 m k (snd pre) of
                Just ys -> all (\(ni, nis) -> help_check ni nis pre) (zip (nss ++ [ns]) ys)
                Nothing -> False
        _ -> False  

search_pre1 :: Int -> [(Int, (Int, Int))] -> Maybe (Int, Int)
search_pre1 _ [] = Nothing
search_pre1 m ((ms, pair) : xs) =
    if m == ms then Just pair else search_pre1 m xs

search_pre2 :: Int -> Int -> [(Int, [Int])] -> Maybe [Int]
search_pre2 _ _ [] = Nothing
search_pre2 m k ((ms, ks) : xs) =
    if m == ms && not (null ks) && k == head ks
        then Just (tail ks)
        else search_pre2 m k xs


substitute :: Lambda -> Int -> Char -> Lambda
substitute (Variable ch) k x = if ch == x then (Index k) else Variable ch
substitute (Index x) _ _ = (Index x)
substitute (Abs y n) k x = (Abs y (substitute n k x))
substitute (App m n) k x = App (substitute m k x) (substitute n k x) 


getNs :: Lambda -> [Lambda]
getNs (App n ns) = getNs n ++ [ns]
getNs t = [t]

