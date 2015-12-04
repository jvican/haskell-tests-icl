module BDDs where

type Index = Int
type Env = [(Index, Bool)]

data BExp = Var Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)

type NodeId = Int
type BDDNode = (NodeId, (Index, NodeId, NodeId))
type BDD = (NodeId, [BDDNode])

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp x ((a, b):xs)
    | x == a    = b
    | otherwise = lookUp x xs

checkSat :: BDD -> Env -> Bool
checkSat (rootId, nodes) e  =
    eval rootId
    where
        eval :: NodeId -> Bool
        eval 0 = False
        eval 1 = True
        eval id = eval $ nextNode (lookUp id nodes)
        nextNode :: (Index, NodeId, NodeId) -> NodeId
        nextNode (ind, il, ir)  =
            if(lookUp ind e) then ir else il

sat :: BDD -> [Env]
sat (rootId, nodes) =
    traverse rootId
    where
        traverse :: NodeId -> [Env]
        traverse 0 = []
        traverse 1 = [[]]
        traverse id =
            let (i, x, y) = lookUp id nodes
                l = traverse x
                r = traverse y
            in addEnv l (i, False) ++ addEnv r (i, True)
        addEnv :: [Env] -> (Index, Bool) -> [Env]
        addEnv [] x = []
        addEnv (xs:xxs) x = (x:xs):xxs

-- One step evaluation - including shortcircuits
simplify :: BExp -> BExp
simplify (And(Var x)(r)) =  if(x) then r else Var False
simplify (And(l)(Var x)) = if(x) then l else Var False
simplify (Or(Var x)(r)) =  if(x) then Var True else r
simplify (Or(l)(Var x)) = if(x) then Var True else l
simplify (Not(Var x)) = Var(not x)
simplify b = b

-- Shannon Boolean restriction
restrict :: BExp -> Index -> Bool -> BExp
restrict ref@(IdRef j) i x = if(i == j) then Var x else ref
restrict (And(l)(r)) i x = simplify $ And(restrict l i x)(restrict r i x)
restrict (Or(l)(r)) i x = simplify $ Or(restrict l i x)(restrict r i x)
restrict (Not(e)) i x = simplify $ Not(restrict e i x)
restrict e i x = e

buildBDD :: BExp -> [Index] -> BDD
buildBDD e xs = buildBDD' (simplify e) 2 xs

buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' (Var False) id xs = (0, [])
buildBDD' (Var True) id xs = (1, [])
buildBDD' (IdRef i) id xs = (id, [(id, (i, 0, 1))])
buildBDD' (Not e) id xs@(_:_) =
    let (id', nodes) = buildBDD' e id xs
    in (id', negate' nodes)
buildBDD' o@(Or(l)(r)) id xs@(_:_) = branch o l r id xs
buildBDD' a@(And(l)(r)) id xs@(_:_) = branch a l r id xs
buildBDD' e id [] = error("BExp malformed")

branch :: BExp -> BExp -> BExp -> NodeId -> [Index] -> BDD
branch e l r id (x:xs) =
    let n = 2 * id
        el = restrict e x False
        er = restrict e x True
        (lid, ls) = buildBDD' el n xs
        (rid, rs) = buildBDD' er (n+1) xs
        node = (id, (x, lid, rid))
    in (id, node:(ls ++ rs))

not' :: NodeId -> NodeId
not' 0 = 1
not' 1 = 0
not' x = x

negate' :: [BDDNode] -> [BDDNode]
negate' [] = []
negate' ((id, (ind, x, y)):xs) = (id, (ind, not' x, not' y)):negate' xs

-- Testing
b1, b2, b3, b4, b5, b6, b7, b8 :: BExp
b1 = Var False
b2 = Not (And (IdRef 1) (Or (Var False) (IdRef 2)))
b3 = And (IdRef 1) (Var True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])
