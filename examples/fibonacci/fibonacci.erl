-module(fibonacci).

-export([fibonacci/1]).

fibonacci(0) -> 1;
fibonacci(1) -> 1;
fibonacci(N) when N > 1 -> fibonacci(N-1) + fibonacci(N-2);
fibonacci(N) when N < 0 -> 0.