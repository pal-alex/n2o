-module(n2o_ws).
-include("n2o.hrl").
-compile(export_all).

send(C,M) -> 
    io:format("send pid ~p~n", [C]),
    C ! M.

proc(init,#pi{name=Name}=Pi) -> n2o:reg(Name), {ok,Pi#pi{state=application:get_env(n2o,proto,n2o_proto)}};

proc({ring, _Topic, Publish}, State) -> proc(Publish, State);

proc({publish, C, Token, Request}, State = #pi{name=Server,state=Module}) ->
    Ctx = #cx { session= n2o:to_binary(Token), node=Server,
                client_pid=C, state=application:get_env(kvs,dba,[]) },
    put(context, Ctx),
    Reply = n2o_proto:try_info(Module,Request,[],Ctx),
    Return = handle(C, Reply),
    {reply, Return, State};

proc(Unknown,#pi{name=Name}=Pi) ->
    io:format("UNKNOWN ~p: ~p~n",[Name,Unknown]),
    {reply,{uknown,Unknown,0},Pi}.

handle(C, {reply,{_,      _} = Reply}) -> handle(C, {reply, Reply, [], []});
handle(_, {reply,{_,      <<>>},_,_}) -> skip;
handle(C, {reply,{text,   Text},_,_}) -> {ok, send(C, {flush,Text})};
handle(C, {reply,{bert,   Term},_,_}) -> {ok, send(C, n2o_bert:encode(Term))};
handle(C, {reply,{json,   Term},_,_}) -> {ok, send(C, n2o_json:encode(Term))};
handle(C, {reply,{binary, Term},_,_}) -> {ok, send(C, Term)};
handle(C, {reply,{default,Term},_,_}) -> {ok, send(C, n2o:encode(Term))};
handle(C, {reply,{Encoder,Term},_,_}) -> {ok, send(C, Encoder:encode(Term))};
handle(_, Reply) -> {error, {"Invalid Return",Reply}}.


% send message to client
flush(Message) -> to_client(flush, Message).
log(Message) ->  to_client(log, Message).

to_client(Type, Message) ->
    {Pid, Ctx} = hd(ets:tab2list(web_context)),
    to_client(Pid, Type, Message)
.
to_client(Ctx, Type, Message) when is_tuple(Ctx) ->
    Pid = n2o_cowboy2:pid(Ctx),
    to_client(Pid, Type, Message);

to_client(Pid, Type, Message) when is_pid(Pid) ->
    n2o_ws:send(Pid, {Type, Message})
.
