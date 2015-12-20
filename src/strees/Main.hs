module Main where

import SuffixTrees

t1 :: SuffixTree
t1 = Node [("banana", Leaf 0),
           ("a", Node [("na", Node [("na", Leaf 1),
                                    ("", Leaf 3)]),
                       ("", Leaf 5)]),
           ("na", Node [("na", Leaf 2),
                        ("", Leaf 4)])]

t2 :: SuffixTree
t2 = Node [("mississippi", Leaf 0),
           ("i", Node [("ssi", Node [("ssippi", Leaf 1),
                                     ("ppi", Leaf 4)]),
                       ("ppi", Leaf 7),
                       ("", Leaf 10)]),
           ("s", Node [("si", Node [("ssippi", Leaf 2),
                                    ("ppi", Leaf 5)]),
                       ("i", Node [("ssippi", Leaf 3),
                                   ("ppi", Leaf 6)])]),
           ("p", Node [("pi", Leaf 8),
                       ("i", Leaf 9)])]

main = do
            print $ isPrefix "has" "haskell"
            print $ isPrefix "" "haskell"
            print $ isPrefix "haskell" "has"
            print $ isPrefix "ask" "haskell"
            print $ removePrefix "ja" "java"
            print $ removePrefix "" "java"
            print $ suffixes "perl"
            print $ isSubstring "ho" "python"
            print $ isSubstring "thong" "python"
            print $ findSubstrings "an" "banana"
            print $ findSubstrings "s" "mississippi"
            print $ getIndices (Node [("", Leaf 4), ("", Leaf 1)])
            print $ getIndices t1
            print $ partition "happy" "haskell"
            print $ partition "happy" "haskell"
            print $ findSubstrings' "an" t1
            print $ findSubstrings' "s" t2
            print $ buildTree "banana"
            print $ buildTree "mississippi"
            print $ findSubstrings' "an" (buildTree "banana")
            print $ findSubstrings' "s" (buildTree "mississippi")
            print $ repeated (buildTree "mississippi") ""
            print $ longestRepeatedSubstring "mississippi"
            print $ longestRepeatedSubstring "banana"
