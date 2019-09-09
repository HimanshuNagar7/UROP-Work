module Facts where

import DataTypes

facts :: [Fact]
facts = facts2

factsInit = [Atom "time" [Num 0], Atom "time" [Num 1], Atom "time" [Num 2], Atom "time" [Num 3]]

facts1 = [Atom "holds" [Const "p", Num 1], Atom "happens" [Const "a", Num 1], Atom "time" [Num 1], Atom "time" [Num 2], Atom "time" [Num 3], Atom "time" [Num 4]]

facts2 = [Atom "happens" [Const "a", Num 3], Atom "time" [Num 3], Atom "time" [Num 4], Atom "time" [Num 5], Atom "time" [Num 6]]

facts3 = [Atom "happens" [Const "a1", Num 2], Atom "time" [Num 2], Atom "time" [Num 3], Atom "time" [Num 4], Atom "time" [Num 5]
         ,Atom "happens" [Const "a3", Num 2]]
