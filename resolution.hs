import Data.List
import Data.Maybe

data Exp = Num Int | Var String | Plus Exp Exp | Minus Exp Exp |
           Mul Exp Exp | Div Exp Exp | Tuple [Exp] | Func String [Exp]
           deriving (Show, Eq)


data Boolean = Atom String [Exp] | Equals Exp Exp | G Exp Exp |
               L Exp Exp | NE Exp Exp | GE Exp Exp | LE Exp Exp
               deriving (Show, Eq)

type Fact = Boolean

type Clause = [Boolean]

type LookupTable = [(String, Int)]

type Program = [Clause]

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

evalBool :: [Fact] -> Boolean -> Bool
evalBool _ (Equals (Num n) (Num n')) = n == n'

evalBool _ (L (Num n) (Num n')) = n < n'

evalBool _ (G (Num n) (Num n')) = n > n'

evalBool _ (NE (Num n) (Num n')) = n /= n'

evalBool _ (LE (Num n) (Num n')) = n <= n'

evalBool _ (GE (Num n) (Num n')) = n >= n'

evalBool facts boolean@(Atom _ _) = boolean `elem` facts

evalBool _ boolean = False

processExp :: Exp -> Exp -> LookupTable
processExp (Num i) (Var v)
  = [(v, i)]

processExp (Tuple exps) (Tuple exps')
  = concat (zipWith processExp exps exps')

processExp (Func str exps) (Func str' exps')
  | str == str' = concat (zipWith processExp exps exps')
  | otherwise = []

processExp _ _ = []

generateLookup :: Fact -> Clause -> LookupTable
generateLookup _ [] = []

generateLookup fact@(Atom str args) ((Atom str' args') : booleans)
  | str == str' =  processExp (Tuple args) (Tuple args')
  | otherwise   =  generateLookup fact booleans

generateLookup fact@(Atom str args) ( _ : booleans)
  =  generateLookup fact booleans

generateLookup _ _ = []

instantiateExp :: LookupTable -> Exp -> Exp
instantiateExp table (Var v)
  | isJust value = Num (fromJust value)
  | otherwise    = Var v
  where
    value = lookup v table

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


instantiate :: LookupTable -> Clause -> Clause
instantiate table clause = map (instantiateBoolean table) clause

substituteFact :: Clause -> Fact -> Clause
substituteFact clause fact
  = map evalBoolExps clause'
  where
    clause' = instantiate table clause
    table = generateLookup fact clause

substitute :: Clause -> [Fact] -> Clause
substitute clause facts = foldl substituteFact clause facts

deriveRule :: Clause -> [Fact] -> Clause
deriveRule clause facts
  = filter (\boolean -> not (evalBool facts boolean)) clause'
  where
    clause' = substitute clause facts

resolve :: Program -> [Fact] -> Program
resolve [] _  = []
