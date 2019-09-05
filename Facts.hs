module Facts where

import DataTypes

facts :: [Fact]
facts = [Atom "happens" [Const "a", Num 1], Atom "time" [Num 1], Atom "time" [Num 2], Atom "time" [Num 3]]

facts1 =
  [Atom "happens" [Func "request" [Const "c2", Const "b1"], Num 1],
  Atom "time" [Num 1], Atom "time" [Num 2], Atom "time" [Num 3],
  Atom "holds" [Func "available" [Const "b1", Num 4], Num 0]]

facts2 =
  [Atom "happens" [Func "request" [Const "c1", Const "b1"], Num 2],
  Atom "happens" [Func "allocate" [Const "c2", Const "b1", Num 4, Num 1], Num 2],
  Atom "time" [Num 2], Atom "time" [Num 3], Atom "time" [Num 4],
  Atom "holds" [Func "available" [Const "b1", Num 4], Num 1]]
