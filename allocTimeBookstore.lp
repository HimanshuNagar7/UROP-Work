#const maxTime = 10.
#const noCust = 10.
#const noItem = 10.

time(0..maxTime).


#const orderTime=3.

isCust(1..noCust).
isItem(1..noItem).
number(0..10).

holds(available(Item, 10), 0) :- isItem(Item).


0{happens(Action, T)}1 :- 
  supported(Action, T), time(T).

supported(allocate(Cust, Item, N, T1), T) :- 
  ant(1, (T1, Cust, Item), T1), holds(available(Item, N), T-1), 
  T1 < T, T < T1 + 3, time(T + 1), number(N).

supported(apologize(Cust, Item), T) :- 
  ant(1, (T1, Cust, Item), T1), T = T1 + 4, time(T).

supported(process(Cust, Item, T1), T) :- 
  ant(1, (T1, Cust, Item), T1), T1 < T - 1, T < T1 + 4, time(T), happens(allocate(Cust, Item, N, T1), T - 1).

supported(order(Item), T) :- ant(2, (T - 1, Item), T - 1), time(T + orderTime).

supported(arrive(Item, N), T) :- 
  happens(order(Item), T1), holds(available(Item, N), T-1), T = T1 + orderTime. 

ant(1, (Ts, Cust, Item), Ts) :- 
  happens(request(Cust, Item), Ts), time(Ts).

cons(1, (T, Cust, Item), T, T1 + 1) :- 
  ant(1, (T, Cust, Item), T), holds(available(Item, N), T1-1),
  happens(allocate(Cust, Item, N, T), T1), happens(process(Cust, Item, T), T1 + 1), 
  T < T1, T1 < T + 3, time(T1 + 1).
  
cons(1, (T, Cust, Item), T, T3) :- 
  ant(1, (T, Cust, Item), T), happens(apologize(Cust, Item), T3), 
  T3 = T + 4, time(T3). 

ant(2, (T, Item), T) :- 
  isItem(Item), happens(allocate(Cust, Item, N, T1), T), not holds(onOrder(Item), T),  
  N <= 2, time(T).

cons(2, (T, Item), T, T1) :- 
  ant(2, (T, Item), T), happens(order(Item), T + 1), happens(arrive(Item, N), T1), 
  holds(available(Item, N), T1-1), T1 = T + 1 + orderTime, time(T1).

initiates(allocate(Cust, Item, N, T), available(Item, N - 1)) :- 
  isCust(Cust), isItem(Item), number(N), time(T).
  
terminates(allocate(Cust, Item, N, T), available(Item, N)) :- 
  isCust(Cust), isItem(Item), number(N), time(T).

initiates(order(Item), onOrder(Item)) :- 
  isItem(Item).

initiates(arrive(Item, N), available(Item, N + 2)) :- 
  isItem(Item), number(N + 2).

terminates(arrive(Item, N), onOrder(Item)) :- 
  isItem(Item), number(N).

terminates(arrive(Item, N), available(Item, N)) :- 
  isItem(Item), number(N).

holds(P,Ts) :- 
  initiates(E,P), happens(E,Ts), time(Ts).

holds(P,Ts) :- 
  holds(P, Ts-1), not broken(P,Ts),time(Ts).

broken(P,Ts) :- terminates(E,P), happens(E,Ts), time(Ts).

consTrue(ID, X, T) :- cons(ID, X, T, Ts), time(Ts).

:- ant(ID, X, T), not consTrue(ID, X, T), time(T).

:- happens(allocate(Cust, Item, N, T1), T + 1), 
   holds(available(Item, N), T), N = 0.

:- holds(available(Item, N1), T), holds(available(Item, N2), T), time(T), N1 < N2.

:- happens(allocate(Cust1, Item, N1, T1), T), 
   happens(allocate(Cust2, Item, N2, T2), T), Cust1 != Cust2.

:- happens(allocate(C,I,N1,T),T1),happens(allocate(C,I,N2,T),T2), T1<T2.

:- happens(allocate(Cust1, Item, N1, T1), T), 
   happens(allocate(Cust2, Item, N2, T2), T), T1 < T2.

:- happens(process(C,I,T),T1),happens(process(C,I,T),T2), T1<T2.

%:~ happens(order(Item), T).[1@1, Item, T]
:~ happens(apologize(Cust, Item), T).[1@2, Cust, Item, T]



#show happens/2.
%#show holds/2.
%#show isItem/1.
%#show supported/2.
%#show order/3.
%#show ant/3.
%#show available/3.
