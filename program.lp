0{happens(Action, T)}1 :- 
  supported(Action, T), time(T).

ant(1, Ts) :- happens(a, Ts), time(Ts).
cons(1, T, Ts) :- ant(1, T), happens(a1, T1), T < T1, happens(a2, Ts), Ts = (T1 + 1), time(T1), time(T), time(Ts).
supported(a1, Ts) :- ant(1, T), T < Ts, time(Ts), time(T), time((Ts + 1)).
supported(a2, Ts) :- ant(1, T), happens(a1, T1), T < T1, Ts = (T1 + 1), time(T), time(T1), time(Ts).
:- happens(a3, Ts), happens(a2, Ts), time(Ts).

holds(P,Ts) :- 
  initiates(E,P), happens(E,Ts), time(Ts).

holds(P,Ts) :- 
  holds(P, Ts-1), not broken(P,Ts),time(Ts).

broken(P,Ts) :- terminates(E,P), happens(E,Ts), time(Ts).

consTrue(ID, T) :- cons(ID, T, Ts), time(Ts).

:- ant(ID, X, T), not consTrue(ID, X, T), time(T).


