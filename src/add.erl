%% @doc Erlang mini project.
-module(add).
-export([start/3, start/4]).

%% @doc TODO: add documentation
-spec start(A,B,Base) -> ok when
      A::integer(),
      B::integer(),
      Base::integer().

start(A,B, Base) ->
    start(A, B, Base, []).

%% @doc TODO: add documentation
-spec start(A, B, Base, Options) -> ok when
      A::integer(),
      B::integer(),
      Base::integer(),
      Option::atom() | tuple(),
      Options::[Option].

start(A, B, Base, Options) ->
    {APadded, BPadded} = utils:padNumbers(
        integer_to_list(A),
        integer_to_list(B)
    ),

    N = proplists:get_value('N', Options, 3),
    As = utils:split(APadded, N),
    Bs = utils:split(BPadded, N),

    StartChild = create_children(
        As, Bs, Base, self()
    ),
    StartChild ! {carry, 0},
    {Result, CarryOut} = receive_results([]),
    SumStr = lists:append([integer_to_list(CarryOut) | Result]),
    {Sum, _} = string:to_integer(SumStr),
    Sum.

%% @doc TODO: add documentation
-spec start_child(A,B,NextPid,ParentPid,Base) -> pid() when
      A :: [char()],
      B :: [char()],
      NextPid :: pid(),
      ParentPid :: pid(),
      Base::integer().

start_child(A, B, NextPid, ParentPid, Base) ->
    spawn(fun() ->
        receive
            {carry, CarryIn} ->
                % io:format("~w; A: ~s; B: ~s; Carry in: ~w~n", [self(), A, B, CarryIn]),

                {Result, CarryOut} = utils:sum(A, B, CarryIn, Base),

                % io:format("~w; RESULT: ~s; CARRY OUT: ~w~n", [self(), Result, CarryOut]),
                ParentPid ! {result, Result},
                NextPid ! {carry, CarryOut}
        end
    end).

create_children([], [], _, NextChild) -> NextChild;

create_children([A|As], [B|Bs], Base, NextChild) ->
    ChildPid = start_child(A, B, NextChild, self(), Base),
    create_children(As, Bs, Base, ChildPid).

receive_results(Results) ->
    receive
        {result, Result} ->
            receive_results([Result|Results]);
        {carry, CarryIn} ->
            % io:format("Received final carry, we are DONE!! ~s~n", [CarryIn]),
            {Results, CarryIn}
    end.
