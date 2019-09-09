time(3..6).

currentTime(3).

0{happens(Action, T)}1 :- 
  supported(Action, T), T > N, currentTime(N).

happens(a, 1).
happens(a,3).


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

:- ant(ID, T), not consTrue(ID,T), time(T).

ant(1, Ts) :- happens(a, Ts), time(Ts).
ant(1, 1).
cons(1, T, Ts) :- ant(1, T), happens(a1, T1), T < T1, happens(a2, Ts), Ts > T1, Ts < (T1 + 3), time(T1), time(T), time(Ts).
cons(1, 1, Ts) :- happens(a1, T1), 1 < T1, happens(a2, Ts), Ts > T1, Ts < (T1 + 3), time(T1), time(Ts).
supported(a1, Ts) :- ant(1, T), T < Ts, time(Ts), time(T), time(T2), Ts < T2, T2 < (Ts + 3).
supported(a1, Ts) :- 1 < Ts, time(Ts), time(T2), Ts < T2, T2 < (Ts + 3).
supported(a2, Ts) :- ant(1, T), happens(a1, T1), T < T1, Ts < (T1 + 3), time(T), time(T1), time(Ts), T1 < Ts.
supported(a2, Ts) :- happens(a1, T1), 1 < T1, Ts < (T1 + 3), time(T1), time(Ts), T1 < Ts.
:- not holds(p, (Ts - 1)), happens(a2, Ts), time(Ts), time((Ts - 1)).
consTrue(ID, T) :- cons(ID, T, Ts), time(Ts), time(T).
initiates(a, p).
terminates(a3, p).
:- ant(ID, T), not consTrue(ID, T), time(T).
:- not consTrue(1, 1).

ant(1, Ts) :- happens(a, Ts), time(Ts).
ant(1, 1).
cons(1, T, Ts) :- ant(1, T), happens(a1, T1), T < T1, happens(a2, Ts), Ts > T1, Ts < (T1 + 3), time(T1), time(T), time(Ts).
cons(1, 1, Ts) :- happens(a1, T1), 1 < T1, happens(a2, Ts), Ts > T1, Ts < (T1 + 3), time(T1), time(Ts).
cons(1, 1, Ts) :- happens(a2, Ts), Ts > 2, Ts < 5, time(Ts).
supported(a1, Ts) :- ant(1, T), T < Ts, time(Ts), time(T), time(T2), Ts < T2, T2 < (Ts + 3).
supported(a1, Ts) :- 1 < Ts, time(Ts), time(T2), Ts < T2, T2 < (Ts + 3).
supported(a2, Ts) :- ant(1, T), happens(a1, T1), T < T1, Ts < (T1 + 3), time(T), time(T1), time(Ts), T1 < Ts.
supported(a2, Ts) :- happens(a1, T1), 1 < T1, Ts < (T1 + 3), time(T1), time(Ts), T1 < Ts.
supported(a2, Ts) :- Ts < 5, time(Ts), 2 < Ts.
:- not holds(p, (Ts - 1)), happens(a2, Ts), time(Ts), time((Ts - 1)).
consTrue(ID, T) :- cons(ID, T, Ts), time(Ts), time(T).
initiates(a, p).
terminates(a3, p).
:- ant(ID, T), not consTrue(ID, T), time(T).
:- not consTrue(1, 1).

ant(1, Ts) :- happens(a, Ts), time(Ts).
ant(1, 3).
ant(1, 1).
cons(1, T, Ts) :- ant(1, T), happens(a1, T1), T < T1, happens(a2, Ts), Ts > T1, Ts < (T1 + 3), time(T1), time(T), time(Ts).
cons(1, 3, Ts) :- happens(a1, T1), 3 < T1, happens(a2, Ts), Ts > T1, Ts < (T1 + 3), time(T1), time(Ts).
cons(1, 1, Ts) :- happens(a1, T1), 1 < T1, happens(a2, Ts), Ts > T1, Ts < (T1 + 3), time(T1), time(Ts).
cons(1, 1, Ts) :- happens(a2, Ts), Ts > 2, Ts < 5, time(Ts).
supported(a1, Ts) :- ant(1, T), T < Ts, time(Ts), time(T), time(T2), Ts < T2, T2 < (Ts + 3).
supported(a1, Ts) :- 3 < Ts, time(Ts), time(T2), Ts < T2, T2 < (Ts + 3).
supported(a1, Ts) :- 1 < Ts, time(Ts), time(T2), Ts < T2, T2 < (Ts + 3).
supported(a2, Ts) :- ant(1, T), happens(a1, T1), T < T1, Ts < (T1 + 3), time(T), time(T1), time(Ts), T1 < Ts.
supported(a2, Ts) :- happens(a1, T1), 3 < T1, Ts < (T1 + 3), time(T1), time(Ts), T1 < Ts.
supported(a2, Ts) :- happens(a1, T1), 1 < T1, Ts < (T1 + 3), time(T1), time(Ts), T1 < Ts.
supported(a2, Ts) :- Ts < 5, time(Ts), 2 < Ts.
:- not holds(p, (Ts - 1)), happens(a2, Ts), time(Ts), time((Ts - 1)).
consTrue(ID, T) :- cons(ID, T, Ts), time(Ts), time(T).
initiates(a, p).
terminates(a3, p).
:- ant(ID, T), not consTrue(ID, T), time(T).
:- not consTrue(1, 3).
:- not consTrue(1, 1).


#show happens/2.
