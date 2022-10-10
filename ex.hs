type Var = String

data Lexp = Lnum Int            -- Constante entière.
          | Lref Var            -- Référence à une variable.
          | Llambda Var Lexp    -- Fonction anonyme prenant un argument.
          | Lcall Lexp Lexp     -- Appel de fonction, avec un argument.
          | Lnil                -- Constructeur de liste vide.
          | Ladd Lexp Lexp      -- Constructeur de liste.
          | Lmatch Lexp Var Var Lexp Lexp -- Expression conditionelle.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lfix [(Var, Lexp)] Lexp
          deriving (Show, Eq)

data Value = Vnum Int
           | Vnil
           | Vcons Value Value
           | Vfun (Value -> Value)

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec _ Vnil = showString "[]"
    showsPrec p (Vcons v1 v2) =
        let showTail Vnil = showChar ']'
            showTail (Vcons v1' v2') =
                showChar ' ' . showsPrec p v1' . showTail v2'
            showTail v = showString " . " . showsPrec p v . showChar ']'
        in showChar '[' . showsPrec p v1 . showTail v2
    showsPrec _ _ = showString "<function>"

type Env = [(Var, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: Env
env0 = [("+", Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x + y)))),
        ("*", Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x * y)))),
        ("/", Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x `div` y)))),
        ("-", Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x - y))))]

env0Var = ["+", "*", "/","-"]
env0Val =   [Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x + y))),
             Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x * y))),
             Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x `div` y))),
             Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x - y)))]





idxList :: [Int]
idxList = [0,1,2,3,4,5] -- x:xs

findIndex'Var :: [Var] -> [Int] -> Var -> Int
findIndex'Var [] _ _ = -1
findIndex'Var env0Var (x:xs) identifiant
    | env0Var !! x == identifiant = x
    | otherwise = findIndex'Var env0Var xs identifiant