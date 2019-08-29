module Resolution where

import Data.List
import Data.Maybe
import CurrentTime

data Exp = Num Int | Const String | Var String | Plus Exp Exp | Minus Exp Exp |
           Mul Exp Exp | Div Exp Exp | Tuple [Exp] | Func String [Exp]
           deriving (Show, Eq)


data Boolean = Atom String [Exp] | Equals Exp Exp | G Exp Exp |
               L Exp Exp | NE Exp Exp | GE Exp Exp | LE Exp Exp | F
               deriving (Show, Eq)

data Bool3 = Result Bool | Unknown deriving (Show, Eq)

type Fact = Boolean

type Clause = [Boolean]

type LookupTable = [(Exp, Exp)]

type Program = [Clause]

timeWrapper :: String
timeWrapper = "time"

showExp :: Exp -> String
showExp (Num int) = show int

showExp (Const c) = c

showExp (Var v) = v

showExp (Plus exp1 exp2) = "(" ++ (showExp exp1) ++ " + " ++ (showExp exp2) ++ ")"

showExp (Minus exp1 exp2) = "(" ++ (showExp exp1) ++ " - " ++ (showExp exp2) ++ ")"

showExp (Mul exp1 exp2) = "(" ++ (showExp exp1) ++ " * " ++ (showExp exp2) ++ ")"

showExp (Div exp1 exp2) = "(" ++ (showExp exp1) ++ " / " ++ (showExp exp2) ++ ")"

showExp (Tuple exps)
   = "(" ++ (foldl1 (\exp1 exp2 -> exp1 ++ ", " ++ exp2) (map showExp exps)) ++ ")"

showExp (Func str exps)
   = str ++ (showExp (Tuple exps))


showBool :: Boolean -> String
showBool (Atom str exps) = str ++ (showExp (Tuple exps))

showBool (Equals exp1 exp2) = (showExp exp1) ++ " = " ++ (showExp exp2)

showBool (L exp1 exp2) = (showExp exp1) ++ " < " ++ (showExp exp2)

showBool (G exp1 exp2) = (showExp exp1) ++ " > " ++ (showExp exp2)

showBool (NE exp1 exp2) = (showExp exp1) ++ " != " ++ (showExp exp2)

showBool (LE exp1 exp2) = (showExp exp1) ++ " <= " ++ (showExp exp2)

showBool (GE exp1 exp2) = (showExp exp1) ++ " >= " ++ (showExp exp2)

showBool _ = ""

showClause' :: Clause -> String
showClause' clause
  = foldl1 (\bool1 bool2 -> bool1 ++ ", " ++ bool2) (map showBool clause)

showClause :: Clause -> String
showClause [F] = ":-."

showClause [boolean] = (showBool boolean) ++ "."

showClause (F : bs) = ":- " ++ (showClause' bs) ++ "."

showClause (b : bs) = (showBool b) ++ " :- " ++ (showClause' bs) ++ "."

showClause _ = ""

showProg :: Program -> String
showProg program
  = foldl1 (\clause1 clause2 -> clause1 ++ "\n" ++ clause2) (map showClause program)



evalExp :: Exp -> Exp
evalExp (Plus (Num n) (Num n')) = Num (n + n')

evalExp (Minus (Num n) (Num n')) = Num (n - n')

evalExp (Mul (Num n) (Num n')) = Num (n * n')

evalExp (Div (Num n) (Num n')) = Num (n `div` n')

evalExp expression = expression

evalBoolExps :: Boolean -> Boolean
evalBoolExps (Equals exp1 exp2) = Equals (evalExp exp1) (evalExp exp2)

evalBoolExps (L exp1 exp2) = L (evalExp exp1) (evalExp exp2)

evalBoolExps (G exp1 exp2) = G (evalExp exp1) (evalExp exp2)

evalBoolExps (NE exp1 exp2) = NE (evalExp exp1) (evalExp exp2)

evalBoolExps (LE exp1 exp2) = LE (evalExp exp1) (evalExp exp2)

evalBoolExps (GE exp1 exp2) = GE (evalExp exp1) (evalExp exp2)

evalBoolExps (Atom str exps) = Atom str (map evalExp exps)

evalBoolExps boolean = boolean

evalBool :: [Fact] -> Boolean -> Bool3
evalBool _ (Equals (Num n) (Num n')) = Result (n == n')

evalBool _ (L (Num n) (Num n')) = Result (n < n')

evalBool _ (G (Num n) (Num n')) = Result (n > n')

evalBool _ (NE (Num n) (Num n')) = Result (n /= n')

evalBool _ (LE (Num n) (Num n')) = Result (n <= n')

evalBool _ (GE (Num n) (Num n')) = Result (n >= n')

evalBool facts boolean@(Atom _ _)
  | boolean `elem` facts = Result True
  | otherwise = Unknown

evalBool _ boolean = Unknown

validExps :: Exp -> Exp -> Bool
validExps (Func str args) (Func str' args')
  = stringCheck && lengthCheck && argsCheck
  where
    stringCheck = (str == str')
    lengthCheck = ((length args) == (length args))
    argsCheck = and (zipWith validExps args args')

validExps (Func _ _) _ = False

validExps _ (Func _ _) = False

validExps (Tuple args) (Tuple args')
  = lengthCheck && argsCheck
  where
    lengthCheck = ((length args) == (length args))
    argsCheck = and (zipWith validExps args args')

validExps (Tuple _) _ = False

validExps _ (Tuple _) = False

validExps _ _ = True

processExp' :: Exp -> Exp -> LookupTable
processExp' (Num i) (Var v)
  = [(Var v, Num i)]

processExp' (Const c) (Var v)
  = [(Var v, Const c)]

processExp' (Num i) (Plus (Var v) (Num i'))
  = [(Var v, Num  (i - i'))]

processExp' (Num i) (Minus (Var v) (Num i'))
  = [(Var v, Num  (i + i'))]

processExp' (Num i) (Mul (Var v) (Num i'))
  = [(Var v, Num  (i `div` i'))]

processExp' (Num i) (Div (Var v) (Num i'))
  = [(Var v, Num  (i * i'))]

processExp' (Num i) (Plus (Num i') (Var v))
  = [(Var v, Num  (i - i'))]

processExp' (Num i) (Minus (Num i') (Var v))
  = [(Var v, Num  (i' - i))]

processExp' (Num i) (Mul (Num i') (Var v))
  = [(Var v, Num  (i `div` i'))]

processExp' (Num i) (Div (Num i') (Var v))
  = [(Var v, Num  (i' `div` i))]

processExp' (Tuple exps) (Tuple exps')
  = concat (zipWith processExp' exps exps')

processExp' (Func str exps) (Func str' exps')
  = concat (zipWith processExp' exps exps')

processExp' _ _ = []

processExp :: Exp -> Exp -> LookupTable
processExp exp1 exp2
  | validExps exp1 exp2 = processExp' exp1 exp2
  | otherwise = []


generateEqualityLookup :: Clause -> LookupTable
generateEqualityLookup [] = []

generateEqualityLookup ((Equals exp1 exp2) : booleans)
  | not (null (processExp exp1 exp2)) = (processExp exp1 exp2) ++ (generateEqualityLookup booleans)
  | otherwise                         = (processExp exp2 exp1) ++ (generateEqualityLookup booleans)


generateEqualityLookup (_ : booleans)
  = generateEqualityLookup booleans

generateLookup :: Fact -> Clause -> LookupTable
generateLookup _ [] = []

generateLookup fact@(Atom str args) ((Atom str' args') : booleans)
  | str == timeWrapper = []
  | str == str'  =  processExp (Tuple args) (Tuple args')
  | otherwise    =  generateLookup fact booleans

generateLookup fact@(Atom str args) ( _ : booleans)
  =  generateLookup fact booleans

generateLookup _ _ = []

instantiateExp :: LookupTable -> Exp -> Exp
instantiateExp table (Var v)
  | isJust value = fromJust value
  | otherwise    = Var v
  where
    value = lookup (Var v) table

instantiateExp table (Plus exp1 exp2)
  = Plus (instantiateExp table exp1) (instantiateExp table exp2)

instantiateExp table (Minus exp1 exp2)
  = Minus (instantiateExp table exp1) (instantiateExp table exp2)

instantiateExp table (Mul exp1 exp2)
  = Mul (instantiateExp table exp1) (instantiateExp table exp2)

instantiateExp table (Div exp1 exp2)
  = Div (instantiateExp table exp1) (instantiateExp table exp2)

instantiateExp table (Tuple exps)
  = Tuple (map (instantiateExp table) exps)

instantiateExp table (Func str exps)
  = Func str (map (instantiateExp table) exps)

instantiateExp _ expression = expression


instantiateBoolean :: LookupTable -> Boolean -> Boolean
instantiateBoolean table (Atom str exps)
  = Atom str (map (instantiateExp table) exps)

instantiateBoolean table (Equals exp1 exp2)
  = Equals (instantiateExp table exp1) (instantiateExp table exp2)

instantiateBoolean table (G exp1 exp2)
  = G (instantiateExp table exp1) (instantiateExp table exp2)

instantiateBoolean table (L exp1 exp2)
  = L (instantiateExp table exp1) (instantiateExp table exp2)

instantiateBoolean table (NE exp1 exp2)
  = NE (instantiateExp table exp1) (instantiateExp table exp2)

instantiateBoolean table (GE exp1 exp2)
  = GE (instantiateExp table exp1) (instantiateExp table exp2)

instantiateBoolean table (LE exp1 exp2)
  = LE (instantiateExp table exp1) (instantiateExp table exp2)

instantiateBoolean _ F = F


instantiate :: LookupTable -> Clause -> Clause
instantiate table clause = map (instantiateBoolean table) clause

substituteFact :: Clause -> Fact -> Clause
substituteFact clause fact
  = map evalBoolExps clause'
  where
    clause' = instantiate table clause
    table = generateLookup fact clause

substitute' :: Clause -> [Fact] -> [Clause]
substitute' clause [] = [clause]

substitute' clause (fact : facts)
   = (substitute' clause facts) ++ (substitute' (substituteFact clause fact) facts)

substitute :: Clause -> [Fact] ->  [Clause]
substitute clause facts = nub (substitute' clause facts)

substituteEquality :: Clause -> Clause
substituteEquality clause = instantiate (generateEqualityLookup clause) clause

sat :: Clause -> [Fact] -> Bool
sat clause facts = null (filter (\b -> ((evalBool facts b) == (Result False))) clause)

filterTrue :: [Fact] -> Clause -> Clause
filterTrue facts clause
  = filter (\boolean -> (evalBool facts boolean) /= (Result True)) clause

deriveRules :: Clause -> [Fact] -> [Clause]
deriveRules clause facts
  = map (filterTrue facts) clauses
  where
    clauses = substitute clause facts

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _ = False

resolveOnce :: Program -> [Fact] -> (Program, [Fact])
resolveOnce [] facts  = ([], facts)

resolveOnce ([fact] : clauses) facts
  = ([fact] : rules, facts')
  where
    (rules, facts') = resolveOnce clauses facts

resolveOnce (clause : clauses) facts
    = (rules ++ rules', facts')
  where
    rules = deriveRules clause facts
    newFacts = map head (filter isSingleton rules)
    (rules', facts') = resolveOnce clauses (nub (newFacts ++ facts))

resolve''' :: Program -> [Fact] -> Program
resolve''' program facts
  | (and (map (\x -> x `elem` program) program')) && (facts == facts') = program
  | otherwise = resolve''' program' facts'
  where
    (program', facts') = resolveOnce program facts

resolve'' :: Program -> [Fact] -> Program
resolve'' program facts
  = filter (\c -> sat c facts) program'
  where
    program' = (filter (\x -> not (null x)) (resolve''' program facts))

resolve' :: Program -> [Fact] -> Program
resolve' program facts
  = nub (map (filterTrue facts) (map substituteEquality (resolve'' program facts)))

isCurrentTime :: Boolean -> Bool
isCurrentTime (Atom str [Num t]) = not ((str == timeWrapper) && (t < currentTime))

isCurrentTime _ = True

isCurrentClause :: Clause -> Bool
isCurrentClause clause = and (map isCurrentTime clause)

resolve :: Program -> [Fact] -> Program
resolve program facts = filter isCurrentClause (resolve' program facts)

---------------------------------------------------------------------------------------------------
supportedAllocate =
  [Atom "supported"[Func "allocate" [Var "Cust", Var "Item", Var "N", Var "T1"], Var "T"],
  Atom "ant" [Num 1, Tuple [Var "T1", Var "Cust", Var "Item"], Var "T1"],
  Atom "holds" [Func "available" [Var "Item", Var "N"], Minus (Var "T") (Num 1)],
  L (Var "T1")  (Var "T"), L (Var "T") (Plus (Var "T1") (Num 3)),
  Atom "time" [Plus (Var "T") (Num 1)], Atom "number" [Var "N"]]

supportedApologize =
  [Atom "supported" [Func "apologize" [Var "Cust", Var "Item"], Var "T"],
  Atom "ant" [Num 1, Tuple [Var "T1", Var "Cust", Var "Item"], Var "T1"],
  Equals (Var "T")  (Plus (Var "T1") (Num 4)), Atom "time" [Var "T"]]

supportedProcess =
  [Atom "supported" [Func "process" [Var "Cust", Var "Item", Var "T1"], Var "T"],
  Atom "ant" [Num 1, Tuple [Var "T1", Var "Cust", Var "Item"], Var "T1"],
  L (Var "T1")  (Minus (Var "T") (Num 1)), L (Var "T") (Plus (Var "T1") (Num 4)),
  Atom "time" [Var "T"],
  Atom "happens" [Func "allocate" [Var "Cust", Var "Item", Var "N", Var "T1"], Minus (Var "T") (Num 1)]]

consequentClause1 =
  [Atom "cons" [Num 1, Tuple [Var "T", Var "Cust", Var "Item"], Var "T", Plus (Var "T1") (Num 1)],
  Atom "ant" [Num 1, Tuple [Var "T", Var "Cust", Var "Item"], Var "T"],
  Atom "holds" [Func "available" [Var "Item", Var "N"], Minus (Var "T1") (Num 1)],
  Atom "happens" [Func "allocate" [Var "Cust", Var "Item", Var "N", Var "T"], Var "T1"],
  Atom "happens" [Func "process" [Var "Cust", Var "Item", Var "T"], Plus (Var "T1") (Num 1)],
  Atom "time" [Plus (Var "T1") (Num 1)], L (Var "T") (Var "T1"), L (Var "T1") (Plus (Var "T") (Num 3))]

consequentClause2 =
  [Atom "cons" [Num 1, Tuple [Var "T", Var "Cust", Var "Item"], Var "T", Var "T3"],
  Atom "ant" [Num 1, Tuple [Var "T", Var "Cust", Var "Item"], Var "T"],
  Atom "happens" [Func "apologize" [Var "Cust", Var "Item"], Var "T3"],
  Equals (Var "T3") (Plus (Var "T") (Num 4)), Atom "time" [Var "T3"]]

antecedentClause =
  [Atom "ant" [Num 1, Tuple [Var "Ts", Var "Cust", Var "Item"], Var "Ts"],
  Atom "happens" [Func "request" [Var "Cust", Var "Item"], Var "Ts"],
  Atom "time" [Var "Ts"]]

constraint1 =
  [F,
  Atom "happens" [Func "allocate" [Var "Cust", Var "Item", Var "N", Var "T1"], Plus (Var "T") (Num 1)],
  Atom "holds" [Func "available" [Var "Item", Num 0], Var "T"]]

constraint2 =
  [F,
  Atom "holds" [Func "available" [Var "Item", Var "N"], Var "T"],
  Atom "holds" [Func "available" [Var "Item", Var "N1"], Var "T"],
  L (Var "N") (Var "N1")]

facts1 =
  [Atom "happens" [Func "request" [Const "c2", Const "b1"], Num 1],
  Atom "time" [Num 1], Atom "time" [Num 2], Atom "time" [Num 3],
  Atom "holds" [Func "available" [Const "b1", Num 4], Num 0]]

facts2 =
  [Atom "happens" [Func "request" [Const "c1", Const "b1"], Num 2],
  Atom "happens" [Func "allocate" [Const "c2", Const "b1", Num 4, Num 1], Num 2],
  Atom "time" [Num 2], Atom "time" [Num 3], Atom "time" [Num 4],
  Atom "holds" [Func "available" [Const "b1", Num 4], Num 1]]


program = [supportedAllocate,
          supportedProcess,
          supportedApologize,
          consequentClause1,
          consequentClause2,
          antecedentClause,
          constraint1,
          constraint2]
----------------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn (showProg (resolve program facts1))
