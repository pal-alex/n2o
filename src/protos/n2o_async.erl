-module(n2o_async).
-compile(export_all).
-description('N2O Async Protocol').
-include("n2o.hrl").
-export([info/3,proc/2]).

-define(NEXT, 2*1024). % 256K chunks for best 25MB/s speed
-define(STOP, 0).


% Async Protocol

info(#async{name = Name, data = Data}, Req, State) ->
    Table = async,
%    ?LOG_INFO("Async INFO: ~p",[ Async ]),
    % catch n2o_pi:stop(Table, Name),
    Pi = #pi{module=?MODULE, table=Table, sup=n2o, name=Name, state = State},
    {Pid, _} = n2o_pi:start(Pi),
    n2o_pi:send(Pid, Data),
    {reply, {bert, <<>>}, Req, State};

info(Message, Req, State) -> {unknown, Message, Req, State}.

% n2o Handlers

proc(init, #pi{}=Pi) ->
    {ok, Pi};

proc(Data, #pi{state=#cx{module=Module}=State}=Pi) ->
    spawn(fun() -> 
            try 
                Module:async(Data), 
                % X = Module:async(Data), 
                % Return = term_to_binary({io, n2o_nitro:render_actions(nitro:actions()), X}),
                % Reply = {reply, {bert, Return}},
                Pid = n2o_cowboy2:pid(State), 
                n2o_ws:to_client(Pid, flash, [])
            catch E:R:S -> ?LOG_EXCEPTION(E,R,S) end
          end),
    {reply, [], Pi}
.

