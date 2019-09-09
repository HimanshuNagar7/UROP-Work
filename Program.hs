module Program where

import DataTypes

program :: Program
program =
  [[Atom "ant" [Num 1,Var "Ts"],Atom "happens" [Const "a",Var "Ts"],Atom "time" [Var "Ts"]],[Atom "ant" [Num 1,Num 1]],[Atom "cons" [Num 1,Var "T",Var "Ts"],Atom "ant" [Num 1,Var "T"],Atom "happens" [Const "a1",Var "T1"],L (Var "T") (Var "T1"),Atom "happens" [Const "a2",Var "Ts"],G (Var "Ts") (Var "T1"),L (Var "Ts") (Plus (Var "T1") (Num 3)),Atom "time" [Var "T1"],Atom "time" [Var "T"],Atom "time" [Var "Ts"]],[Atom "cons" [Num 1,Num 1,Var "Ts"],Atom "happens" [Const "a1",Var "T1"],L (Num 1) (Var "T1"),Atom "happens" [Const "a2",Var "Ts"],G (Var "Ts") (Var "T1"),L (Var "Ts") (Plus (Var "T1") (Num 3)),Atom "time" [Var "T1"],Atom "time" [Var "Ts"]],[Atom "cons" [Num 1,Num 1,Var "Ts"],Atom "happens" [Const "a2",Var "Ts"],G (Var "Ts") (Num 2),L (Var "Ts") (Num 5),Atom "time" [Var "Ts"]],[Atom "supported" [Const "a1",Var "Ts"],Atom "ant" [Num 1,Var "T"],L (Var "T") (Var "Ts"),Atom "time" [Var "Ts"],Atom "time" [Var "T"],Atom "time" [Var "T2"],L (Var "Ts") (Var "T2"),L (Var "T2") (Plus (Var "Ts") (Num 3))],[Atom "supported" [Const "a1",Var "Ts"],L (Num 1) (Var "Ts"),Atom "time" [Var "Ts"],Atom "time" [Var "T2"],L (Var "Ts") (Var "T2"),L (Var "T2") (Plus (Var "Ts") (Num 3))],[Atom "supported" [Const "a2",Var "Ts"],Atom "ant" [Num 1,Var "T"],Atom "happens" [Const "a1",Var "T1"],L (Var "T") (Var "T1"),L (Var "Ts") (Plus (Var "T1") (Num 3)),Atom "time" [Var "T"],Atom "time" [Var "T1"],Atom "time" [Var "Ts"],L (Var "T1") (Var "Ts")],[Atom "supported" [Const "a2",Var "Ts"],Atom "happens" [Const "a1",Var "T1"],L (Num 1) (Var "T1"),L (Var "Ts") (Plus (Var "T1") (Num 3)),Atom "time" [Var "T1"],Atom "time" [Var "Ts"],L (Var "T1") (Var "Ts")],[Atom "supported" [Const "a2",Var "Ts"],L (Var "Ts") (Num 5),Atom "time" [Var "Ts"],L (Num 2) (Var "Ts")],[F,Not (Atom "holds" [Const "p",Minus (Var "Ts") (Num 1)]),Atom "happens" [Const "a2",Var "Ts"],Atom "time" [Var "Ts"],Atom "time" [Minus (Var "Ts") (Num 1)]],[Atom "consTrue" [Var "ID",Var "T"],Atom "cons" [Var "ID",Var "T",Var "Ts"],Atom "time" [Var "Ts"],Atom "time" [Var "T"]],[Atom "initiates" [Const "a",Const "p"]],[Atom "terminates" [Const "a3",Const "p"]],[F,Atom "ant" [Var "ID",Var "T"],Not (Atom "consTrue" [Var "ID",Var "T"]),Atom "time" [Var "T"]],[F,Not (Atom "consTrue" [Num 1,Num 1])]]

initialProgram :: Program
initialProgram =
  [[Atom "ant" [Num 1, Var "Ts"], Atom "happens" [Const "a", Var "Ts"],Atom "time" [Var "Ts"]],

  [Atom "cons" [Num 1,Var "T",Var "Ts"], Atom "ant" [Num 1,Var "T"],
  Atom "happens" [Const "a1", Var "T1"],L (Var "T") (Var "T1"),
  Atom "happens" [Const "a2", Var "Ts"],G (Var "Ts") (Var "T1"),
  L (Var "Ts") (Plus (Var "T1") (Num 3)),
  Atom "time" [Var "T1"], Atom "time" [Var "T"], Atom "time" [Var "Ts"]],

  [Atom "supported" [Const "a1",Var "Ts"],  Atom "ant" [Num 1, Var "T"],
  L (Var "T") (Var "Ts"), Atom "time" [Var "Ts"], Atom "time" [Var "T"],
  Atom "time" [Var "T2"], L (Var "Ts") (Var "T2"), L (Var "T2") (Plus (Var "Ts") (Num 3))],

  [Atom "supported" [Const "a2", Var "Ts"], Atom "ant" [Num 1, Var "T"],
  Atom "happens" [Const "a1", Var "T1"], L (Var "T") (Var "T1"),
  L (Var "Ts") (Plus (Var "T1") (Num 3)), Atom "time" [Var "T"],
  Atom "time" [Var "T1"], Atom "time" [Var "Ts"], L (Var "T1") (Var "Ts")],

  [F, Not (Atom "holds" [Const "p", Minus (Var "Ts") (Num 1)]),
  Atom "happens" [Const "a2", Var "Ts"], Atom "time" [Var "Ts"],
  Atom "time" [Minus (Var "Ts") (Num 1)]],

  [Atom "consTrue" [Var "ID", Var "T"], Atom "cons" [Var "ID", Var "T", Var "Ts"], 
  Atom "time" [Var "Ts"], Atom "time" [Var "T"]],

  [Atom "initiates" [Const "a", Const "p"]], [Atom "terminates" [Const "a3", Const "p"]],

  [F, Atom "ant" [Var "ID", Var "T"], Not (Atom "consTrue" [Var "ID", Var "T"]), Atom "time" [Var "T"]]]


