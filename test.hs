type Var = String
-- Expressions du code source en forme ASA
data Exp = Enum Int -- Une constante
    | Evar Var -- Une variable
    | Elet Var Exp Exp -- Une expr "let x = e1 in e2"
    | Ecall Exp Exp -- Un appel de fonction

-- Valeurs renvoyées
data Val = Vnum Int -- Un nombre entier
    | Vprim (Val -> Val) -- Une primitive

instance Show Val where
    show (Vnum n) = "Vnum " ++ Prelude.show n
    show (Vprim _) = "<fonction>" 

mkPrim :: (Int -> Int -> Int) -> Val

mkPrim f = Vprim (\(Vnum x) -> Vprim (\(Vnum y) -> Vnum (f x y)))
-- L’environnement initial qui contient toutes les primitives
type Env = [(Var, Val)]
pervasive :: Env
pervasive = [("+", mkPrim (+)),
            ("-", mkPrim (-)),
            ("*", mkPrim (*)),
            ("/", mkPrim div),
            ("x", Vnum 3)]
eval :: Env -> Exp -> Val
eval _ (Enum n) = Vnum n
eval env (Evar v) = find env v
eval env (Elet v e1 e2) =
    let
        expandEnv = ((v, eval env e1) : env)
    in
        eval expandEnv e2
eval env (Ecall f arg) =
    let
        (Vprim valF) = eval env f
        evalArg = eval env arg
    in
        valF evalArg
find :: Env -> Var -> Val
find [] _ = error ("Symbol non trouvé") -- Cas de base
find ((identifiant, valeur) : restEnv) identifiant'
    | identifiant == identifiant' = valeur
    | otherwise = find restEnv identifiant'
