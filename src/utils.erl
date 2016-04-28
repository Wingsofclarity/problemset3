%% @author Karl Marklund <karl.marklund@it.uu.se>

%% @doc A small collection of utility functions.


-module(utils).

-export([seqs/1, filter/2, split/2, padNumbers/2, sum/4, random_sleep/0]).

%% To use EUnit we must include this.
-include_lib("eunit/include/eunit.hrl").

%%-compile(export_all).


%% @doc Generates a list of lists of increasing sequences of integers
%% starting with the empty list and ending with [1,2, ..., N].
%% === Example ===
%% <div class="example">```
%% > utils:seqs(5).
%% [[],[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]'''
%% </div>
-spec seqs(N::integer()) -> [[integer()]].

seqs(N) ->
    %% NOTE: Simply using a list comprehension such as [[]] ++
    %% [lists:seq(1,M) || M <- lists:seq(1,N)] will be quite slow
    %% since each sequence is generated from scratch. Hence, lets
    %% re-use the last sequnece and add a new element when
    %% constructing the next sequence.

    F = fun(X,[H|T]) -> [[X|H],H|T] end,
    lists:foldl(F, [[]], lists:seq(1,N)),
    lists:reverse([lists:reverse(L) || L <- lists:foldl(F, [[]], lists:seq(1,N))]).


%% @doc Each list in List2 contains the elements Elem in List1 for
%% which one of the Pred(Elem) returns true. The order of the lists in
%% List2 is the same as the order of the predicates. In each list in
%% List2, the relative order of the elements are the same as in the
%% original List1.
%%
%% === Example ===
%% <div class="example">```
%% 1> L = [1,2,3,4,5,6,7,8,9,10].
%% [1,2,3,4,5,6,7,8,9,10]
%% 2> P1 = fun(X) -> X rem 2 == 1 end.
%% #Fun<erl_eval.6.111823515>
%% 3> P2 = fun(X) -> not P1(X) end.
%% #Fun<erl_eval.6.111823515>
%% 4> P3 = fun(X) -> X > 3 andalso X < 7 end.
%% #Fun<erl_eval.6.111823515>
%% 5> utils:filter([P1,P2,P3], L).
%% [[1,3,5,7,9],[2,4,6,8,10],[4,5,6]]'''
%% </div>
-spec filter(Preds, List1) -> List2 when
      Preds :: [Pred],
      Pred :: fun((Elem :: T) -> boolean()),
      List1 :: [T],
      List2 :: [[T]],
      T :: term().

filter(Predicates, List) ->
    Collect = self(),
    [spawn(fun() -> Collect!{I,lists:filter(P,List)} end) ||
	{I, P} <- lists:zip(lists:seq(1, length(Predicates)), Predicates)],

    filter_collect(length(Predicates), []).

filter_collect(0,R) ->
    [L || {_,L} <- lists:sort(R)];
filter_collect(N,R) ->
    receive
	{I, L} -> filter_collect(N-1, [{I,L}|R])
    end.



lqr(L, N) ->
    Len = length(L),

    %% Quotient
    Q = Len div N,

    %% Reminder
    R = Len rem N,

    {Len, Q, R}.

%% @doc Split List into N Lists such that all Lists have approximately the same number of elements.
%%
%% Let Len = length(List), Q = Len div N and R = Len rem N.
%%
%% If R = 0, then all of the lists in Lists will be of length Q.
%%
%% If R =/= 0, then R of the lists in Lists will have
%% lenght Q + 1.
%%
%% === Example ===
%%
%% <div class="example">```
%% 1> L = [1,2,3,4,5,6,7,8,9,10].
%% [1,2,3,4,5,6,7,8,9,10]
%% 2> utils:split(L, 4).
%% [[1,2,3],[4,5,6],[7,8],[9,10]]
%% 3> lists:concat(utils:split(L,3)).
%% [1,2,3,4,5,6,7,8,9,10]'''
%% </div>
-spec split(List, N) -> Lists when
      List :: [T],
      Lists :: [List],
      T :: term(),
      N :: integer().

split([], _) ->
    [];

split(L, N) ->
    Len = length(L),
    if
        Len rem N > 0 ->
            SplitLen = (Len div N) + 1;

        true ->
            SplitLen = Len div N
    end,
    {L1, L2} = lists:split(SplitLen, L),
    [L1 | split(L2, N-1)].

%% @doc Converts an ASCII-value to an int of its value.
%%
%%
%% === Example ===
%%
%% <div class="example">```
%%1> toInt($2).
%%   2
%%2> toInt($a).
%%   10'''
%% </div>
toInt(A) when A>=$0, A=<$9 ->
    A-$0;
toInt(A) when A>=$a, A=<$z ->
    A-$a+10;
toInt(A) when A>=$A, A=<$Z ->
    A-$A+10.

%% @doc Converts an int to its ASCII value. For A>=10 it returns 
%%      ASCII for anon-capital alphabetic characters.
%%
%%
%% === Example ===
%%
%% <div class="example">```
%%1> toInt($2).
%%   2
%%2> toInt($a).
%%   10'''
%% </div>
toChar(A) when A>=0, A=<9 ->
    A+$0;
toChar(A) when A>=10->
    A+$a-10.    

%% @doc Adds Xs and Ys in arbitrary base.
%% 
%% Sum will return in as many numbers as the 
%% largest list of Xs and Ys. If the sum should
%% become one more digit than that Sum will not 
%% display the last digit and CarryOut will be 1.
%%
%% In output of a base>10 digist worth more than 9
%% will be repsented by the alphabet in chronological
%% order in lower case.
%%
%% === Example ===
%%
%% <div class="example">```
%%1>sum("1","2",0,10).
%%  {"3",0}
%%2>sum("19","1",0,10).
%%  {"00",1}
%%3>sum("10","1",0,2).
%%  {"11",0}
%%4>sum("a","a",0,16).
%%  {"4",1}
%%'''
%% </div>
-spec sum(Xs, Ys, CarryIn, Base) -> {Sum,CarryOut} when
        Xs::string(),
        Ys::string(),
        CarryIn::integer(),
        Base::integer(),
        Sum::string(),
        CarryOut::integer().


sum(Xs,Ys,_,_) when length(Xs)=/=length(Ys)->
    error;
sum(Xs,Ys,CarryIn,Base)->
    sum(lists:reverse(Xs),lists:reverse(Ys),CarryIn,Base,[]).

sum([],[],CarryIn,_,Sum)->
    {Sum,CarryIn};
sum([X|Xs],[Y|Ys],CarryIn,Base,Sum)->
    {Single, CarryOut} = sum_aux(toInt(X),toInt(Y),CarryIn,Base),
    sum(Xs,Ys,CarryOut,Base,[toChar(Single)|Sum]).

sum_aux(X,Y,CarryIn,Base)when X+Y+CarryIn>=Base ->
    random_sleep(),
    {X+Y+CarryIn-Base, 1};
sum_aux(X,Y,CarryIn,Base)when X+Y+CarryIn<Base ->
    random_sleep(),
    {X+Y+CarryIn, 0}.

random_sleep()->
    rand:seed(exs1024),
    A = rand:uniform()*1000,
    io:format("I randomed ~w~n", [A]),
    timer:sleep(round(A)).
    

%% @doc Adds zeros (char) to the start of the list until it is of length Count
%%
%% length(Xs)>=Count
%%
%%
%% === Example ===
%%
%% <div class="example">```
%%1>Xs= [$1,$0,$2].
%%2>padNumber(Xs,5).
%%  "00102"
%%1>Ys= [$3,$2,$8].
%%4>padNumber(Xs,3).
%%  "328"'''
%% </div>
padNumber(Xs,Count)when length(Xs)==Count->
    Xs;
padNumber(Xs,Count)->
    padNumber([$0|Xs],Count).


%% @doc Adds zeros (char) to the start of the list that is
%% the shortest until ithey are of equal length
%%
%%
%%
%% === Example ===
%%
%% <div class="example">```
%%1>Xs= [$1,$0,$2],
%%  Ys = [$1,$1,$1,$1].
%%2>padNumbers(Xs,Ys).
%%  {"0102","1111"}'''
%% </div>
-spec padNumbers(Xs,Ys)->Zs when
    T::any(),
    Xs::[T],
    Ys::[T],
    Zs::[T].

padNumbers(Xs, Ys) when length(Xs)>=length(Ys) ->
    K = length(Xs),
    {padNumber(Xs,K),padNumber(Ys,K)};
padNumbers(Xs, Ys) when length(Xs)<length(Ys) ->
    K = length(Ys),
    {padNumber(Xs,K),padNumber(Ys,K)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                          %%
%%			   EUnit Test Cases                                 %%
%%                                                                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seqs_length_test_() ->
    %% The list [[], [1], [1,2], ..., [1,2, ..., N]] will allways have
    %% length N+1.

    [?_assertEqual(N+1, length(seqs(N))) || N <- lists:seq(1, 55)].

seqs_test_() ->
    %% A small collection of expected results {N, seqs(N)}.

    Data = [{0, [[]]}, {1, [[], [1]]}, {2, [[], [1], [1,2]]},
	    {7, [[],
		 [1],
		 [1,2],
		 [1,2,3],
		 [1,2,3,4],
		 [1,2,3,4,5],
		 [1,2,3,4,5,6],
		 [1,2,3,4,5,6,7]]}
	   ],

    [?_assertEqual(L, seqs(N)) || {N, L} <- Data].

filter_test_() ->
    [?_assertEqual([], filter([], L)) || L <- seqs(10)].

filter_true_false_test_() ->
    P1 = fun(_) -> false end,
    P2 = fun(_) -> true end,
    P3 = fun(X) -> X rem 2 == 0 end,

    Expected = fun(L) -> [lists:filter(P,L) || P <- [P1,P2,P3]] end,

    [?_assertEqual(Expected(L), filter([P1,P2,P3], L) ) || L <- seqs(10) ].

filter_test() ->
    L = lists:seq(1,10),

    P1 = fun(X) -> X rem 2 == 0 end,
    P2 = fun(X) -> X rem 2 == 1 end,
    P3 = fun(X) -> X > 3 end,

    %%E = [[2,4,6,8,10],[1,3,5,7,9],[4,5,6,7,8,9,10]],
    E = [lists:filter(P,L) || P <- [P1,P2,P3]],

    ?assertEqual(E, filter([P1,P2,P3], L)).

split_concat_test_() ->
    %% Make sure the result of concatenating the sublists equals the
    %% original list.

    L = lists:seq(1,99),
    [?_assertEqual(L, lists:concat(split(L,N))) || N <- lists:seq(1,133)].

split_n_test_() ->
    %% Make sure the correct number of sublists are generated.

    M = 99,
    L = lists:seq(1,M),
    Num_of_lists = fun(List, N) when N =< length(List) ->
			   N;
		      (List, _) ->
			   length(List)
		   end,
    [?_assertEqual(Num_of_lists(L,N), length(split(L,N))) || N <- L].


expected_stat(L, N) when N =< length(L) ->
    %% When spliting a list L into N sublists, we know there will only by two possible
    %% lengths of the sublists.


    %% Quotient and reminder when dividing length of L with N.
    {_, Q, R} = lqr(L, N),

    %% There will allways be R sublists of length Q+1 and N-R sublists
    %% of length Q.

    {{R, Q+1}, {N-R, Q}};

expected_stat(L, _N) ->
    %% N greater than the length of L, hence all sublists will have
    %% length 1.

    {{length(L), 1}, {0,0}}.

stat(N, M, LL) ->
    %% Return a tuple {{Num_N, N}, {Num_M, M}} where Num_N is the
    %% number of lists of length N in LL and Num_M is the number of
    %% lists of length M in LL.

    S = filter([fun(X) -> X == N end, fun(X) -> X == M end], [length(L) || L <- LL]),

    [Num_N, Num_M] = [length(L) || L <- S],

    {{Num_N, N}, {Num_M, M}}.

split_stat_test_() ->
    %% Assure the list of sublists contains the correct number of
    %% lists of the two expected lengths.

    Assert = fun(L,N) ->
		     {_, Q, _} = lqr(L,N),
		     ?_assertEqual(expected_stat(L,N), stat(Q+1, Q, split(L,N)))
	     end,

    %% Generators can depend on other generator expressions, here N
    %% depends on the length of L.

    [Assert(L,N) ||  L <- seqs(33), N <- lists:seq(1,length(L)+5)].

toInt_test_() ->
    [?_assertMatch(0,toInt($0)),
     ?_assertMatch(10,toInt($a))].

sum_test_() ->
    [?_assertEqual({[$0], 0 } ,sum([$0],[$0],0,10)),
     ?_assertEqual({[$0], 0 } ,sum([$0],[$0],0,35)),
     ?_assertEqual({[$1], 0 } ,sum([$0],[$1],0,10)),
     ?_assertEqual({[$3], 0 } ,sum([$2],[$1],0,10)),
     ?_assertEqual({[$1,$4,$9], 0 } ,sum([$0,$2,$1],[$1,$2,$8],0,10)),
     ?_assertEqual({[$3], 0 } ,sum([$2],[$1],0,14)),
     ?_assertEqual({[$0], 1 } ,sum([$1],[$1],0,2)),
     ?_assertEqual({[$0,$1], 1 } ,sum([$1,$1],[$1,$0],0,2)),
     ?_assertEqual({[$d,$9,$a], 1 } ,sum([$e,$e,$e],[$e,$a,$c],0,16))].

padNumbers_test()->
    Xs= [$1,$0,$2],
    Ys = [$1,$1,$1,$1],
    [?_assertMatch({[$0|Xs],Ys}, padNumbers(Xs,Ys)),
     ?_assertMatch({Ys,[$0|Xs]}, padNumbers(Ys,Xs))].


