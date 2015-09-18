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
    FormList = erl_syntax:form_list(Forms),
    {Converted, _} =
        erl_syntax_lib:mapfold(
          fun convert/2, undefined, FormList),
    erl_syntax:revert_forms(Converted).
    

convert(Tree, File) ->
    case Tree of
        ?Q("-file(\"'@NewFile\", 9090).") ->
            {Tree, erl_syntax:string_value(NewFile)};
        ?Q("pipe(_@Init, [_@@Funs])") ->
            NewTree = chain_funs(Funs, Init, File),
            {NewTree, File};
        ?Q("pipe(_@_, _@WrongArg)") ->
            report_error(File, erl_syntax:get_pos(WrongArg),
                         "second arg of pipe must be a list of functions");
        _ ->
            {Tree, File}
    end.

chain_funs([Fun|Funs], Input, File) ->
    case Fun of
        ?Q("_@FName(_@@Args)") ->
            check_args(Args, File, erl_syntax:get_pos(Fun)),
            NewArgs = replace_args(Args, Input),
            NewFun = ?Q("_@FName(_@NewArgs)"),
            chain_funs(Funs, NewFun, File);
        _ ->
            report_error(File, erl_syntax:get_pos(Fun),
                         "second arg of pipe must be a list of functions")
    end;
chain_funs([], Input, _) ->
    Input.

replace_args(Args, Input) ->
    [case erl_syntax:type(A) of
         underscore -> Input;
         _ -> A
     end
     || A <- Args].

check_args(Args, File, Pos) ->
    Us = [A||A <- Args, erl_syntax:type(A) =:= underscore],
    1 =:= length(Us) orelse
        report_error(File, Pos, "function must have exactly one underscore").

report_error(File, Pos, Msg) ->
    throw({error, _Es = [{File, [{Pos, ?MODULE, Msg}]} ], _Ws = []}).

format_error(Msg) ->
    Msg.
