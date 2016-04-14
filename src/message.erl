-module(message).
-export([pong/0]).

pong()->
   receive
	banan->
	   io:format("abnsakd"),
	   bananaisgood;
	potatoes->
	   io:format(dontlike),
  	   dontlike;
	A->
  	   io:format(A+1),
	   A+1
   end.