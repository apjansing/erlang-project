# Erlang Web Server
 
## Running this code.
To run this code, first you need to compile the module.
```
erlc poly_app.erl
```
Then you need to start an erlang shell with `erl`, and perform the following commands:
```
1> c(poly_app).
2> inets:start().
3> poly_app:start().
```
Now you can visit [localhost:8081](http://localhost:8081) to see my portfolio webpage. The more interesting portion of this example will be when you visit [localhost:8081/erl/poly_app:service](http://localhost:8081/erl/poly_app:service).

# Acknowledgements
I would like to acknowledge the following for inspiring and providing me with a starting point for this erlang web server.

Tutorialspoint.com, “Erlang Web Programming,” www.tutorialspoint.com. [Online]. Available: https://www.tutorialspoint.com/erlang/erlang_web_programming.htm. [Accessed: 03-May-2019].

G. Smith, YouTube - Building a web app in Erlang - yes you heard me right I said Erlang not Elixir, 25-Mar-2017. [Online]. Available: https://www.youtube.com/watch?v=BO-8Hx8kPtA. [Accessed: 03-May-2019].
