module SuffixTrees where 

import Data.List (findIndices, any, drop)
import Data.Ord (comparing)

data SuffixTree = Leaf Int | Node [(String, SuffixTree)]
                    deriving (Eq, Ord, Show)

isPrefix :: String -> String -> Bool
isPrefix xs ys = length xs <= length ys && all (uncurry (==)) zs
    where zs = zip xs ys

removePrefix :: String -> String -> String
removePrefix x = drop (length x)

suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes xs = xs : suffixes (tail xs)

isSubstring :: String -> String -> Bool
isSubstring x y = any (isPrefix x) (suffixes y)

findSubstrings :: String -> String -> [Int]
findSubstrings x y = findIndices (isPrefix x) (suffixes y)

getIndices :: SuffixTree -> [Int]
getIndices (Leaf x) = [x]
getIndices (Node xs) = concatMap (getIndices . snd) xs

-- Only one scan over the string
partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition xs ys = partition' xs ys []

partition' [] ys zs = (reverse zs, [], ys)
partition' xs [] zs = (zs, xs, [])
partition' l@(x:xs) r@(y:ys) zs
    | x == y    = partition' xs ys (x:zs)
    | otherwise = (reverse zs, l, r)

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' s (Leaf i) = [i]
findSubstrings' s (Node xs) = do
    (x, t) <- xs
    case partition x s of
         ("", _, _)  -> []
         (_, _, "")  -> getIndices t
         (_, _, s')  -> findSubstrings' s' t

buildTree :: String -> SuffixTree
buildTree s = foldl (flip insert) (Node []) xs
    where xs = zip (suffixes s) [0..length s-1]

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert _ (Leaf _) = error "Malformed suffix tree, only Leaf encountered"
insert x t = Node $ insert' x t

insert' :: (String, Int) -> SuffixTree -> [(String, SuffixTree)]
insert' (x,i) (Node []) = [(x, Leaf i)]
insert' (x,i) (Node (n@(y,t):ys))
    | y == p && p /= ""   = (y,Node $ insert' (x',i) t):ys
    | y /= p && p /= ""   = split t i:ys
    | otherwise           = n:insert' (x,i) (Node ys)
    where pxy@(p, x', y') = partition x y
          split :: SuffixTree -> Int -> (String, SuffixTree)
          split t i = (p, Node[(y',t), (x',Leaf i)])
