-module(basic_math).
-import(lists,[append/2]). 
-export([start/0, start/1, multiplication/2, divison/2, addition/2, subtraction/2, pow/2, log/2, log/1]).

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

start(X) ->
    N = [multiplication(X, X)],
    N1 = append(N, [divison(X+X+X, X)]),
    N2 = append(N1, [addition(X, X)]),
    N3 = append(N2, [subtraction(X, X)]),
    N4 = append(N3, [pow(X, X)]),
    N5 = append(N4, [log(X, X)]),
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

log(A) ->
    math:log(A).
