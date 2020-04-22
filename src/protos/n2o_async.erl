-module(n2o_async).
-compile(export_all).
-description('N2O Async Protocol').
-include("n2o.hrl").
-export([info/3,proc/2]).

-define(NEXT, 2*1024). % 256K chunks for best 25MB/s speed
-define(STOP, 0).


% Async Protocol

info(#async{name = Name, data = Data}, Req, State) ->
    % io:format("async state: ~p~n", [State]),
    Table = async,
%    ?LOG_INFO("Async INFO: ~p",[ Async ]),
    % case Stop of
    %     true -> catch n2o_pi:stop(Table, Name);
    %     false -> skip
    % end,
    catch n2o_pi:stop(Table, Name),
    Pi = #pi{module=?MODULE, table=Table, sup=n2o, name=Name, state = State},
    ClientPid = n2o_cowboy2:pid(State),
    {Pid, _} = n2o_pi:start(Pi),
    n2o_pi:send(Pid, {ClientPid, Data}),
    {reply, {bert, <<>>}, Req, State};

info(Message, Req, State) -> {unknown, Message, Req, State}.

% n2o Handlers

proc(init, #pi{state=State}=Pi) ->
    % io:format("state: ~p~n", [State]),
    % put(context, State),
    % S = get(context),
    % io:format("get state: ~p~n", [S]),
    {ok, Pi};

proc({ClientPid, Data}, #pi{state=#cx{module=Module}=State}=Pi) ->
    % io:format("get state before spawn: (~p) ~n", [get(context)]),
    spawn(fun() -> 
            try 
                % Qty0 = dict(nitro:actions()),
                % io:format("Async pid (~p) ~n", [ClientPid]),
                % io:format("Spawn state: (~p) ~n", [State]),
                % io:format("get state: (~p) ~n", [get(context)]),
                % n2o_ws:to_client(ClientPid, {log, "before async"}),
                put(context, State),
                Module:async(Data)
                % Qty = dict(nitro:actions()),
                % io:format("before flush (~p) actions~n", [Qty]),
                % io:format("state: ~p~n", [State]),
                % Actions = nitro:actions(),
                % n2o_ws:to_client(ClientPid, {flush, Actions})
                % n2o_ws:to_client(ClientPid, {log, "after flush"})

            catch E:R:S -> ?LOG_EXCEPTION(E,R,S) end
          end),
    {reply, [], Pi}
.

dict(undefined) -> 0;
dict(Value) -> length(Value).

