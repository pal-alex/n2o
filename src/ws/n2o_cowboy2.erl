-module(n2o_cowboy2).
-description('N2O Cowboy2 WebSocket Backend').
-include("n2o.hrl").
-compile(export_all).

init(Req,_Opts) -> {cowboy_websocket, Req, Req}.

ws({ok,_,S})                 -> {ok,S};
ws({shutdown,_,S})           -> {shutdown,S};
ws({reply,{binary,Rep},_,S}) -> {reply,{binary,Rep},S};
ws({reply,{json,Rep},_,S})   -> {reply,{binary,n2o_json:encode(Rep)},S};
ws({reply,{bert,Rep},_,S})   -> {reply,{binary,n2o_bert:encode(Rep)},S};
ws({reply,{text,Rep},_,S})   -> {reply,{text,Rep},S};
ws({reply,{default,Rep},_,S})-> {reply,{binary,n2o:encode(Rep)},S};
ws({reply,{Encoder,Rep},_,S})-> {reply,{binary,Encoder:encode(Rep)},S};
ws(X) -> ?LOG_ERROR(#{unknown=>X}), {shutdown,[]}.



websocket_init(S)            -> Res = {ok, Ctx} = ws(n2o_proto:init([],S,[],ws)),
								save_context(Ctx),
								Res.
websocket_handle(D,S)        -> 
	% io:format("ws_handle: ~p~n state: ~p~n", [D, S]),
	ws(n2o_proto:stream(D,[],S)).

websocket_info({log, Text}, State) ->
	% io:format("ws_log_info: ~p~n state: ~p~n", [Text, State]),
	% io:format("ws_log_info: ~p~n", [Text]),
	{reply, {text, Text}, State};

websocket_info({binary, Data}, State) ->
	{reply, {binary, Data}, State};
	
websocket_info(D,S)          -> 
	% io:format("ws_info: ~p~n state: ~p~n", [D, S]),
	% io:format("ws_info: ~p~n", [D]),
	ws(n2o_proto:info(D,[],S)).

terminate(M,R,S)             -> ws(n2o_proto:info(#direct{data={exit,M}},R,S)).

points() -> cowboy_router:compile([{'_', [
		{"/ws/[...]", n2o_cowboy2, []},
		% {"/n2o/[...]", cowboy_static, {dir, n2o_cowboy:fix2(code:priv_dir(n2o)), []}},
	    % {"/app/[...]", cowboy_static, {dir, static(), []}},
		{"/[...]", cowboy_static, {dir, static(), []}}
					 ]}]).

static() -> n2o_cowboy:fix1(code:priv_dir(application:get_env(n2o,app,review)))++"/static".


pid(Ctx) when is_tuple(Ctx) -> maps:get(pid, element(4, Ctx)).
save_context(Ctx) ->
	case ets:whereis(web_context) of
		undefined -> ets:new(web_context, [public, named_table]); % key = sid
		_ -> skip
	end,
	Pid = pid(Ctx),
	io:format("saved web context for a pid ~p~n", [Pid]),
	ets:insert(web_context, {Pid, Ctx}).