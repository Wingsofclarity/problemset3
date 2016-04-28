%% @doc Erlang mini project.
-module(add).
-export([start/3, start/4]).

%% @doc TODO: add documentation
-spec start(A,B,Base) -> ok when
      A::string(),
      B::string(),
      Base::integer().

start(A,B, Base) ->
    start(A, B, Base, []).

%% @doc TODO: add documentation
-spec start(A, B, Base, Options) -> ok when
      A::string(),
      B::string(),
      Base::integer(),
      Option::atom() | tuple(),
      Options::[Option].

start(A, B, Base, Options) ->
    {APadded, BPadded} = utils:padNumbers(A, B),

    N = proplists:get_value('N', Options, 3),
    As = utils:split(APadded, N),
    Bs = utils:split(BPadded, N),

    StartChild = create_children(
        As, Bs, Base, self()
    ),
    StartChild ! {carry, 0},
    {Result, CarryOut} = receive_results([]),
    if
        CarryOut =:= 1 ->
            lists:append([integer_to_list(CarryOut) | Result]);

        CarryOut =:= 0 ->
            lists:append(Result)
    end.

%% @doc TODO: add documentation
-spec start_child(A,B,NextPid,ParentPid,Base) -> pid() when
      A :: [char()],
      B :: [char()],
      NextPid :: pid(),
      ParentPid :: pid(),
      Base::integer().

start_child(A, B, NextPid, ParentPid, Base) ->
    spawn(fun() ->
        Children = [
            start_speculative_child(A, B, Base, self(), 0),
            start_speculative_child(A, B, Base, self(), 1)
        ],

        receive
            {carry, CarryIn} ->
                CorrectCarryPid = lists:nth(CarryIn+1, Children),
                WrongCarryPid = lists:nth(2-CarryIn, Children),
                % io:format("~w; A: ~s; B: ~s; Carry in: ~w~n", [self(), A, B, CarryIn]),

                exit(WrongCarryPid, kill),
                CorrectCarryPid ! life,

                % io:format("~w; RESULT: ~s; CARRY OUT: ~w~n", [self(), Result, CarryOut]),
                receive
                    {result, {Result, CarryOut}} ->
                        ParentPid ! {result, Result},
                        NextPid ! {carry, CarryOut}
                end
        end
    end).

start_speculative_child(A, B, Base, ParentPid, CarryInEst) ->
    spawn(fun() ->
        Result = utils:sum(A, B, CarryInEst, Base),

        receive
            life ->
                ParentPid ! {result, Result}
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
