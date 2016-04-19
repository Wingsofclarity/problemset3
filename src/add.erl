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
    As = utils:split(integer_to_list(A), 3), % TODO padding
    Bs = utils:split(integer_to_list(B), 3), % TODO padding

    StartChild = create_children(
        As, Bs, Base, self()
    ),
    StartChild ! {carry, "0"},
    {Result, CarryOut} = receive_results([]),
    io:format("RESULT: ~w~n", [{Result, CarryOut}]),
    lists:append([CarryOut | Result]).

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
                % io:format("~w; A: ~s; B: ~s; Carry in: ~s~n", [self(), A, B, CarryIn]),

                % TODO: Replace with actual calculation
                random:seed(now()),
                {Result, CarryOut} = {
                    integer_to_list(random:uniform(Base*Base*Base - 1 - Base*Base) + Base*Base),
                    integer_to_list(random:uniform(1))
                },

                % io:format("~w; RESULT: ~s~n", [self(), Result]),
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
