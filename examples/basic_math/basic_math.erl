-module(basic_math).
-import(lists,[append/2]). 
-export([start/0]).

start() ->
    W = 0,
    X = 1,
    Y = 2,
    Z = 3,
    N = [multiplication(W, X)],
    N1 = append(N, [divison(Z+Z+Y, Y)]),
    N2 = append(N1, [addition(X, Y)]),
    N3 = append(N2, [subtraction(X, Y)]),
    N4 = append(N3, [pow(Y, Z)]),
    N5 = append(N4, [log(Y, Z)]),
    io:fwrite("~w~n", [N5]).


multiplication(A, B) -> 
    A * B.

divison(A, B) -> 
    A / B.

addition(A, B) -> 
    A + B.

subtraction(A, B) -> 
    A + B.

pow(A, Exp) ->
    math:pow(A, Exp).

log(A, Base) ->
    math:log(A)/math:log(Base).


