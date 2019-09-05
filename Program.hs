module Program where

import DataTypes

supportedAllocate =
  [Atom "supported"[Func "allocate" [Var "Cust", Var "Item", Var "N", Var "T1"], Var "T"],
  Atom "ant" [Num 1, Tuple [Var "T1", Var "Cust", Var "Item"], Var "T1"],
  Atom "holds" [Func "available" [Var "Item", Var "N"], Minus (Var "T") (Num 1)],
  L (Var "T1")  (Var "T"), L (Var "T") (Plus (Var "T1") (Num 3)),
  Atom "time" [Plus (Var "T") (Num 1)], Atom "number" [Var "N"],
  Atom "time" [Minus (Var "T") (Num 1)],Atom "time" [Var "T"]]

supportedApologize =
  [Atom "supported" [Func "apologize" [Var "Cust", Var "Item"], Var "T"],
  Atom "ant" [Num 1, Tuple [Var "T1", Var "Cust", Var "Item"], Var "T1"],
  Equals (Var "T")  (Plus (Var "T1") (Num 4)), Atom "time" [Var "T"]]

supportedProcess =
  [Atom "supported" [Func "process" [Var "Cust", Var "Item", Var "T1"], Var "T"],
  Atom "ant" [Num 1, Tuple [Var "T1", Var "Cust", Var "Item"], Var "T1"],
  L (Var "T1")  (Minus (Var "T") (Num 1)), L (Var "T") (Plus (Var "T1") (Num 4)),
  Atom "time" [Var "T"], Atom "time" [Var "T1"],
  Atom "happens" [Func "allocate" [Var "Cust", Var "Item", Var "N", Var "T1"], Minus (Var "T") (Num 1)]]

consequentClause1 =
  [Atom "cons" [Num 1, Tuple [Var "T", Var "Cust", Var "Item"], Var "T", Plus (Var "T1") (Num 1)],
  Atom "ant" [Num 1, Tuple [Var "T", Var "Cust", Var "Item"], Var "T"],
  Atom "holds" [Func "available" [Var "Item", Var "N"], Minus (Var "T1") (Num 1)],
  Atom "happens" [Func "allocate" [Var "Cust", Var "Item", Var "N", Var "T"], Var "T1"],
  Atom "happens" [Func "process" [Var "Cust", Var "Item", Var "T"], Plus (Var "T1") (Num 1)],
  Atom "time" [Plus (Var "T1") (Num 1)], Atom "time" [Minus (Var "T1") (Num 1)], Atom "time" [Var "T1"],
  L (Var "T") (Var "T1"), L (Var "T1") (Plus (Var "T") (Num 3)), Atom "time" [Var "T"]]

consequentClause2 =
  [Atom "cons" [Num 1, Tuple [Var "T", Var "Cust", Var "Item"], Var "T", Var "T3"],
  Atom "ant" [Num 1, Tuple [Var "T", Var "Cust", Var "Item"], Var "T"],
  Atom "happens" [Func "apologize" [Var "Cust", Var "Item"], Var "T3"],
  Equals (Var "T3") (Plus (Var "T") (Num 4)), Atom "time" [Var "T3"],
  Atom "time" [Var "T"]]

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

program :: Program
program =
  [[Atom "ant" [Num 1, Var "Ts"], Atom "happens" [Const "a", Var "Ts"],Atom "time" [Var "Ts"]],

  [Atom "cons" [Num 1,Var "T",Var "Ts"], Atom "ant" [Num 1,Var "T"],
  Atom "happens" [Const "a1", Var "T1"],L (Var "T") (Var "T1"),
  Atom "happens" [Const "a2", Var "Ts"],Equals (Var "Ts") (Plus (Var "T1") (Num 1)),
  Atom "time" [Var "T1"], Atom "time" [Var "T"], Atom "time" [Var "Ts"]],

  [Atom "supported" [Const "a1",Var "Ts"],  Atom "ant" [Num 1, Var "T"],
  L (Var "T") (Var "Ts"), Atom "time" [Var "Ts"], Atom "time" [Var "T"],
  Atom "time" [Plus (Var "Ts") (Num 1)]],

  [Atom "supported" [Const "a2", Var "Ts"], Atom "ant" [Num 1, Var "T"],
  Atom "happens" [Const "a1", Var "T1"], L (Var "T") (Var "T1"),
  Equals (Var "Ts") (Plus (Var "T1") (Num 1)), Atom "time" [Var "T"],
  Atom "time" [Var "T1"], Atom "time" [Var "Ts"]],

  [F, Atom "happens" [Const "a3", Var "Ts"],
  Atom "happens" [Const "a2", Var "Ts"], Atom "time" [Var "Ts"]]]

          {-

          [Atom "holds" [Func "p" [], Num 0]],

          [Atom "happens" [Const "a", 1], Atom "time" [Num 1]],

          [Atom "happens" [Func "b" [], 5], Atom "time" [Num 5]],

          [Atom "terminates" [Const "a1", Func "p" []]],

          -}
