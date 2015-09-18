%%% @doc Syntactic sugar for something like a pipe operator
%%%
%%% Calls a sequence of functions to modify state avoiding using
%%% temporary variables or funs. The order of functions is also more
%%% intuitive and readable.
%%% This parse transform converts a call to pipe/2 like below
%%%
%%% pipe(InitState,
%%%      [a(Config, _),
%%%       b(Config, _),
%%%       m:c(_, Config)]).
%%%
%%% which is equivalent with
%%%
%%% State1 = a(Config, InitState),
%%% State2 = b(Config, State1),
%%% State3 = m:c(State2, Config),
%%% State3.
%%%
%%% into a chain of function calls
%%%
%%% m:c(
%%%   b(Config,
%%%     a(Config, InitState)),
%%%   Config)
%%%
%%% The first argument of pipe is the initial state which is
%%% passed on to the first function.
%%% The second argument is a list of such functions which have _ as
%%% exactly one of its arguments, the return value of the previous function
%%% will be passed on in this position.
%%% (The underscore syntax is borrowed from erlando's cuts)
%%%
%%% @end
-module(pipe).

-export([parse_transform/2, format_error/1]).

-include_lib("syntax_tools/include/merl.hrl").

parse_transform(Forms, _Options) ->
    Converted =
        [try erl_syntax_lib:map(fun convert/1, Form)
         catch Error -> Error
         end
         || Form <- Forms],
    erl_syntax:revert_forms(Converted).

convert(Tree) ->
    case Tree of
        ?Q("pipe(_@Init, [_@@Funs])") ->
            chain_funs(Funs, Init);
        ?Q("pipe(_@_, _@WrongArg)") ->
            report_error(erl_syntax:get_pos(WrongArg),
                         "second arg of pipe must be a list of functions");
        _ ->
            Tree
    end.

chain_funs([Fun|Funs], Input) ->
    case Fun of
        ?Q("_@FName(_@@Args)") ->
            check_args(Args, erl_syntax:get_pos(Fun)),
            NewArgs = replace_args(Args, Input),
            NewFun = ?Q("_@FName(_@NewArgs)"),
            chain_funs(Funs, NewFun);
        _ ->
            report_error(erl_syntax:get_pos(Fun),
                         "second arg of pipe must be a list of functions")
    end;
chain_funs([], Input) ->
    Input.

replace_args(Args, Input) ->
    [case erl_syntax:type(A) of
         underscore -> Input;
         _ -> A
     end
     || A <- Args].

check_args(Args, Pos) ->
    Us = [A || A <- Args, erl_syntax:type(A) =:= underscore],
    1 =:= length(Us) orelse
        report_error(Pos, "function must have exactly one underscore").

report_error(Pos, Msg) ->
    throw({error, {Pos, ?MODULE, Msg}}).

format_error(Msg) ->
    Msg.
