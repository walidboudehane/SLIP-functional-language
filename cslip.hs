-- Auteurs
-- Nom : Walid Boudehane (20206664)
-- Nom : Ithri Kendi     (20222832)

-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Évaluateur
-- - Pretty printer

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Librairie d'analyse syntaxique.
import Data.Char        -- Conversion de Chars de/vers Int et autres
-- import Numeric       -- Pour la fonction showInt
import System.IO        -- Pour stdout, hPutStr
-- import Data.Maybe    -- Pour isJust and fromJust

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3) == (+ . (2 . (3 . ())))
--         ==> Scons (Ssym "+")
--                   (Scons (Snum 2)
--                          (Scons (Snum 3) Snil))
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Scons (Ssym "/")
--       (Scons (Scons (Ssym "*")
--                     (Scons (Scons (Ssym "-")
--                                   (Scons (Snum 68)
--                                          (Scons (Snum 32) Snil)))
--                            (Scons (Snum 5) Snil)))
--              (Scons (Snum 9) Snil))

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                (pChar '\n' <|> eof); return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do { pChar '\''; pSpaces; e <- pSexp;
              return (Scons (Ssym "quote") (Scons e Snil)) }

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp
pList  = do { pChar '('; pSpaces; pTail }
pTail :: Parser Sexp
pTail  = do { pChar ')'; return Snil }
     <|> do { pChar '.'; pSpaces; e <- pSexp; pSpaces;
              pChar ')' <|> error ("Missing ')' after: " ++ show e);
              return e }
     <|> do { e <- pSexp; pSpaces; es <- pTail; return (Scons e es) }

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pList <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) =
    let showTail Snil = showChar ')'
        showTail (Scons e1' e2') =
            showChar ' ' . showSexp' e1' . showTail e2'
        showTail e = showString " . " . showSexp' e . showChar ')'
    in showChar '(' . showSexp' e1 . showTail e2

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire L(ambda)exp(ression)                     --
---------------------------------------------------------------------------

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

-- Première passe simple qui analyse un Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n)        = Lnum n
s2l Snil    = Lnil
s2l (Ssym "nil")    = Lnil

s2l (Ssym s)        = Lref s
s2l (Scons e1 Snil) = s2l e1
s2l (Scons (Ssym "nil") e1) = s2l e1

-- add
s2l (Scons (Ssym "add") (Scons e1 e2)) = Ladd (s2l e1) (s2l e2)

-- list 
s2l (Scons (Ssym "list") (Scons e1 e2)) =
    case e2 of 
        Snil -> Ladd (s2l e1) Lnil
        _    -> Ladd (s2l e1) (s2l (sexpList e2))

-- lambda 
s2l (Scons (Ssym "fn") (Scons (Scons(Ssym s) Snil) (Scons e Snil))) = 
    Llambda s (s2l e)

-- let
s2l (Scons (Ssym "let") (Scons (Scons (Scons (Ssym s) (Scons e1 Snil)) Snil)
    (Scons e2 Snil))) = Lfix [(s, (s2l (sexpand e1)))] (s2l (sexpand e2))

-- match
s2l (Scons (Ssym "match") (Scons e1 (Scons (Scons (Ssym "nil") e2) 
    (Scons (Scons (Scons (Ssym "add") 
    (Scons (Ssym x) (Scons (Ssym y) Snil))) e3) Snil)))) = 
    Lmatch (s2l (sexpand e1)) x y (s2l (sexpand e2)) (s2l (sexpand e3))

-- evaluation de fonction
s2l (Scons e1 e2) = Lcall (s2l e1) (s2l (sexpand e2))


s2l se = error ("Malformed Sexp: " ++ (showSexp se))

-- fonciton auxiliaire pour developper les paires Scons et eliminer le sucre 
-- syntaxique de facon recursive
sexpand :: Sexp -> Sexp
sexpand (Snum n)       = Snum n
sexpand (Ssym "nil")   = Snil
sexpand (Ssym s)       = Ssym s
sexpand Snil           = Snil
sexpand (Scons e Snil) = e
sexpand (Scons e1 e2)  = Scons (sexpand e1) (sexpand e2)

sexpList ::Sexp -> Sexp
sexpList e = (Scons(Ssym "list") e)

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Type des valeurs manipulée à l'exécution.
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


---------------------------------------------------------------------------
-- Représentation intermédiaire Dexp                                     --
---------------------------------------------------------------------------

-- Dexp est similaire à Lexp sauf que les variables sont représentées non
-- pas par des chaînes de caractères mais par des "Indexes de de Bruijn",
-- c'est à dire par leur distance dans l'environnment: la variable la plus
-- récemment déclarée a index 0, l'antérieure 1, etc...
--
-- I.e. Llambda "x" (Llambda "y" (Ladd (Lref "x") (Lref "y")))
-- se traduit par Dlambda (Dlambda (Dadd (Dref 1) (Dref 0)))

type Idx = Int

data Dexp = Dnum Int            -- Constante entière.
          | Dref Idx            -- Référence à une variable.
          | Dlambda Dexp        -- Fonction anonyme prenant un argument.
          | Dcall Dexp Dexp     -- Appel de fonction, avec un argument.
          | Dnil                -- Constructeur de liste vide.
          | Dadd Dexp Dexp      -- Constructeur de liste.
          | Dmatch Dexp Dexp Dexp -- Expression conditionelle.
          
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Dfix [Dexp] Dexp
          deriving (Show, Eq)

env0Var :: [Var]
env0Var = map fst env0

-- Le premier argument contient la liste des variables du contexte.
l2d :: [Var] -> Lexp -> Dexp
l2d _ (Lnum n)              = Dnum n
l2d _ (Lref "nil")          = Dnil
l2d _ Lnil                  = Dnil 
l2d envVar (Lref s)        = Dref (findIndexVar envVar index s)

-- evaluation de fonction
l2d envVar (Lcall e1 e2)    = Dcall (l2d envVar e1) (l2d envVar (lexpand e2))

-- contruction de liste
l2d envVar (Ladd e1 e2)    = Dadd (l2d envVar e1) (l2d envVar (lexpand e2))

-- evaluation d'une fonction lambda
l2d envVar (Llambda s e)   = Dlambda (l2d (s:envVar) e)

-- let
l2d envVar (Lfix [(s, e1)] e2) =

    -- extraction de la variable s et son ajout dans l'environnement
    let letEnv = ((fst (head [(s, e1)]) : envVar))  
        e1'    = (snd (head [(s, e1)]))             -- extraction de e1

    in  Dfix ([l2d letEnv e1']) (l2d letEnv e2) 

-- match
l2d envVar (Lmatch e1 s1 s2 e2 e3) = 
    -- ajout des variables s1 et s2 dans l'environnement
    Dmatch (l2d envVar e1) (l2d envVar e2) (l2d (s2:s1:envVar) e3)

-- fonciton auxiliaire pour developper les paires Lexp de facon recursive
lexpand :: Lexp -> Lexp
lexpand (Lnum n)       = Lnum n
lexpand (Lref "nil")   = Lnil
lexpand (Lref s)       = Lref s
lexpand Lnil           = Lnil
lexpand (Lcall e Lnil) = e
lexpand (Ladd e Lnil)  = e
lexpand (Lcall e1 e2)  = Lcall (lexpand e1) (lexpand e2)
lexpand (Ladd e1 e2)   = Ladd (lexpand e1) (lexpand e2)

index :: Int
index = 0

-- fonction auxiliaire pour trouver l'index d'une variable dans l'environnement
findIndexVar :: [Var] -> Int -> Var -> Int
findIndexVar [] _ _ = -1
findIndexVar env idx identifiant =
    case env of (x:xs) -> if x == identifiant 
                            then idx 
                            else findIndexVar xs (idx + 1) identifiant

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

-- Le premier argument contient la liste des valeurs des variables du contexte,
-- dans le même ordre que ces variables ont été passées à `l2d`.

-- #####################################
-- data Value = Vnum Int
--            | Vnil
--            | Vcons Value Value
--            | Vfun (Value -> Value)
-- #####################################


env0Val :: [Value]
env0Val = map snd env0

eval :: [Value] -> Dexp -> Value
eval _ (Dnum n)       = Vnum n
eval _ (Dnil)         = Vnil
eval envVal (Dref s) = envVal !! s

-- add
eval envVal (Dadd e1 e2) = Vcons (eval envVal e1) (eval envVal e2)

-- evaluation d'un fonction (inspiré de la demonstration 3)
eval envVal (Dcall (Dref f) arg)=
    let
        (Vfun valF) = eval envVal (Dref f)
        evalArg =  tupleArgs arg
        (Vfun valF2) = valF (eval envVal (fst evalArg))
    in 
        valF2 (eval envVal (snd evalArg))

-- evaluation d'un fonction lambda (inspiré de la correction de l'exercice 3.5)
eval envVal (Dcall fun actual) =
    case eval envVal fun of
        Vfun f -> f (eval envVal actual)

eval envVal (Dlambda e) = Vfun (\val -> eval ((val):envVal) e)

-- let
eval envVal (Dfix [e1] e2) = eval ((eval envVal e1):envVal) e2

-- match
eval envVal (Dmatch e1 e2 e3) = case e1 of 
                                    Dnil -> (eval envVal e2)
                                    _    -> (eval ((addToVal e1) ++ envVal) e3)

-- fonction auxiliaire qui prend un Dadd et evalue les expressions presentes 
-- a l'interieur du Dadd et retourne une liste avec les valeurs resultantes
-- de l'evaluation
addToVal :: Dexp -> [Value]
addToVal (Dadd e1 e2) = [(eval env0Val e2), (eval env0Val e1)] 

-- fonction auxiliaire qui nous permet de separer les arguement d'un fonction 
-- afin de les evaluer un par un
tupleArgs :: Dexp -> (Dexp, Dexp)
tupleArgs (Dcall e1 e2) = (e1,e2)
                  
---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = eval (map snd env0) . l2d (map fst env0) . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do s <- readFile filename
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map evalSexp (sexps s))

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

dexpOf :: String -> Dexp
dexpOf = l2d (map fst env0) . s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf