module DataTypes where

data Exp = Num Int | Const String | Var String | Plus Exp Exp | Minus Exp Exp |
           Mul Exp Exp | Div Exp Exp | Tuple [Exp] | Func String [Exp]
           deriving (Show, Eq)


data Boolean = Atom String [Exp] | Equals Exp Exp | G Exp Exp | Not Boolean |
               L Exp Exp | NE Exp Exp | GE Exp Exp | LE Exp Exp | F
               deriving (Show, Eq)

data Bool3 = Result Bool | Unknown deriving (Show, Eq)

type Fact = Boolean

type Clause = [Boolean]

type LookupTable = [(Exp, Exp)]

type Program = [Clause]
