module LabelledTransitionSystems where

import Control.Monad (guard)
import Data.Maybe (isJust)
import Data.List (nub)

type Id         = String
type State      = Int
type Transition = ((State, State), Id)
type LTS        = [Transition]
type Alphabet   = [Id]

-- Precondition: `x` exists in `xs`
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp x ((a,b):xs)
    | x == a    = b
    | otherwise = lookUp x xs

states :: LTS -> [State]
states = nub . onlyStates
    where onlyStates :: LTS -> [State]
          onlyStates [] = []
          onlyStates (((s1,s2), _):lts) = 
              [s1,s2] ++ onlyStates lts


transitions :: State -> LTS -> [Transition]
transitions s (t@((s1,_), _):lts)
    | s1 == s   = t:loop
    | otherwise = loop
    where loop = transitions s lts

alphabet :: LTS -> Alphabet
alphabet = nub . onlyAlphabet
    where onlyAlphabet :: LTS -> Alphabet
          onlyAlphabet [] = []
          onlyAlphabet ((_, id):lts) =
              id:onlyAlphabet lts

data Process = STOP
             | Ref Id
             | Prefix Id Process
             | Choice [Process]
             deriving (Eq, Ord, Show)

type ProcessDef = (Id, Process)

actions :: Process -> [Id]
actions STOP = []
actions (Ref _) = []
actions (Prefix id p) = id:actions p
actions (Choice ps) = [id | p <- ps
                          , id <- actions p]

accepts :: [Id] -> [ProcessDef] -> Bool
accepts [] _ = True
accepts _ [] = False
accepts ids pds = all (==True) bs
    where bs = [id `elem` ps | id <- ids]
          ps = concat [id':actions p | (id',p) <- pds]

type StateMap = [((State, State), State)]

composeTransitions :: Transition -> Transition
                   -> Alphabet -> Alphabet
                   -> StateMap -> [Transition]
composeTransitions (s, sa) (t, ta) a1 a2 m
        | sa == ta         = [((id, id2), sa)]
        | inA1 && inA2     = []
        | inA1             = [t1]
        | inA2             = [t2]
        | otherwise        = [t1, t2]
        where (id, id2)    = (genId s, genId t)
              (id3, id4)   = (genId ts', genId st')
              ts'          = (fst t, snd s)
              st'          = (fst s, snd t)
              genId s      = lookUp s m
              inA1         = sa `elem` a2
              inA2         = ta `elem` a1
              t1           = ((id, id3), sa)
              t2           = ((id, id4), ta)

pruneTransitions :: [Transition] -> LTS
pruneTransitions ts = visit 0 [] 
    where 
        visit :: State -> [State] -> [Transition]
        visit s visited
            | s `elem` visited = []
            | otherwise        =
                let next    = transitions s ts
                    updated = s:visited
                    visit' s  = visit s visited
                in concatMap (visit' . snd . fst) next

compose :: LTS -> LTS -> LTS
compose = undefined
