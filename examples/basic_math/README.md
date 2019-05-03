# Basic Math Operations
## Running basic_math.erl
```
erlc basic_math.erl
erl -noshell -s basic_math start -s init stop
```
Or for an interactive mode.
```
erlc basic_math.erl
elr
> c(basic_math).
> basic_math:start().
> basic_math:start(2).
> basic_math:multiplication(2, 3).
> basic_math:divison(6, 2).
> basic_math:addition(1, 1).
> basic_math:subtraction(1, 1).
> basic_math:pow(2, 3).
> basic_math:log(4, 2).
> basic_math:log(2.718281828459).
```