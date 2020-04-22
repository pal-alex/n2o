-module(n2o_nitro).
-description('N2O Nitrogen Web Framework Protocol').
-include("n2o.hrl").
-compile(export_all).
% -export([info/3,render_actions/1,io/1,io/2]).

% Nitrogen pickle handler

info({text,<<"N2O,",Auth/binary>>}, Req, State) ->
    info(#init{token=Auth},Req,State);

info(#init{token=Auth}, Req, State) ->
    Token0 = token0(Auth),
    Sid = token(Token0),
    New = State#cx{session = Sid, token = Auth},
    put(context,New),
    io(init, State),
    {reply,{bert, {io,[],{'Token',Token0}}},
            Req,New};

info(#client{data=Message}, Req, State) ->
    % Qty = dict(nitro:actions()),
    % io:format("pickle (~p) ~n", [Qty]),
    % nitro:actions([]),
    {reply,{bert,io(#client{data=Message},State)},Req,State};

info(#pickle{}=Event, Req, State) ->
    % Qty = dict(nitro:actions()),
    % io:format("pickle (~p) ~n", [Qty]),
    % nitro:actions([]),
    {reply,{bert,html_events(Event,State)},Req,State};

info(#flush{data=[]}, Req, State) ->
    % Qty = dict(nitro:actions()),
    % io:format("flush before io (~p)~n", [Qty]),
    Reply = io(<<>>),
    % io:format("flush (~p): ~p~n", [Qty, Reply]),
    {reply,{bert, Reply},Req,State};

info(#flush{data=Actions}, Req, State) ->
    % io:format("flush with data ~n", []),
    {reply, {bert, io(Actions)}, Req, State};
        

% info(#flush{data=Actions}, Req, State) ->
%     % Qty = dict(nitro:actions()),
%     % io:format("flush with data (~p/~p) ~n", [Qty, length(Actions)]),
%     % nitro:actions(Actions),
%     {reply,{bert,io(Actions)},Req,State};

info(#direct{data=Message}, Req, State) ->
    % Qty = dict(nitro:actions()),
    % io:format("direct (~p) ~n", [Qty]),
    % nitro:actions([]),
    % {reply,{bert,case io(Message, State) of
    %                   {io,_,{stack,_}} = Io -> Io;
    %                   {io,Code,Res} -> {io,Code,{direct,Res}} end},
    %         Req,State};

    io(Message, State),

    {reply,{bert,<<>>},Req,State};

info(Message,Req,State) -> {unknown,Message,Req,State}.

% double render: actions could generate actions

render_actions(Actions) ->
    {E, A}  = nitro:render(Actions),
    % io:format("rendered data: ~p~n", [{E, A}]),
    case E == [] of
        true -> nitro:to_binary(A);
        _ -> nitro:to_binary(E)
    end.

% render_actions(Actions) ->
%     % io:format("render_actions start~n", []),
%     nitro:actions([]),
%     First  = nitro:render(Actions),
%     Second = nitro:render(nitro:actions()),
%     nitro:actions([]),
%     nitro:to_binary([First,Second]).

% n2o events

html_events(#pickle{source=Source,pickled=Pickled,args=Linked}, State=#cx{token = Token}) ->
    Ev  = n2o:depickle(Pickled),
    L   = n2o_session:prolongate(),
    Res = case Ev of
          #ev{} when L =:= false -> render_ev(Ev,Source,Linked,State), <<>>;
          #ev{} -> render_ev(Ev,Source,Linked,State), n2o_session:authenticate([], Token);
          _CustomEnvelop -> %?LOG_ERROR("EV expected: ~p~n",[CustomEnvelop]),
                           {error,"EV expected"} end,
    io(Res).

token0(Auth) -> {'Token', Token0} = n2o_session:authenticate([], Auth), 
                Token0. 
token(Token0) -> case n2o:depickle(Token0) of {{S,_},_} -> S; X -> X end.
sid(Auth) -> Token0 = token0(Auth),
             token(Token0).

% calling user code in exception-safe manner

% -ifdef(OTP_RELEASE).

render_ev(#ev{name=F,msg=P,trigger=T},_Source,Linked,State=#cx{module=M}) ->
    try case F of
         api_event -> M:F(P,Linked,State);
            %  event -> [erlang:put(K, nitro:to_binary([V])) || {K,V} <- Linked],
            event -> [nitro:put(K, V) || {K,V} <- Linked],
                    case length(Linked) of
                        L when L =< 1 -> M:F(P);
                        _V ->  % the first element is an event, the second is a data
                              [_E|D] = Linked,
                              M:F({P, hd(D)})
                    end;
                 _ -> M:F(P,T,State) end
    catch E:R:S -> ?LOG_EXCEPTION(E,R,S), {stack,S} end.

io(Event, #cx{module=Module}) ->
    try X = Module:event(Event)
    % {io,render_actions(nitro:actions()),X}
    % {io,[],{stack,[]}}
    catch E:R:S -> ?LOG_EXCEPTION(E,R,S)
        %, {io,[],{stack,S}} 
        end;
io(Data, _State) -> io(Data).

io(Data) ->
    try {io, render_actions(Data), nitro:to_binary(Data)}
    catch E:R:S -> ?LOG_EXCEPTION(E,R,S), {io,[],{stack,S}} end.

% io(Data) ->
%     try {io, render_actions(nitro:actions()), Data}
%     catch E:R:S -> ?LOG_EXCEPTION(E,R,S), {io,[],{stack,S}} end.

% io(Data0) ->
%     Data = lists:flatten(val(nitro:actions()) ++ val(Data0)),
%     try {io, render_actions(Data), <<"data io">>}
%     catch E:R:S -> ?LOG_EXCEPTION(E,R,S), {io,[],{stack,S}} end.

% -else.

% render_ev(#ev{name=F,msg=P,trigger=T},_Source,Linked,State=#cx{module=M}) ->
%     try case F of
%          api_event -> M:F(P,Linked,State);
%             %  event -> [erlang:put(K, nitro:to_binary([V])) || {K,V} <- Linked], M:F(P);
%             event -> [erlang:put(K, V) || {K,V} <- Linked], M:F(P);
%                  _ -> M:F(P,T,State) end
%     catch E:R -> S = erlang:get_stacktrace(), ?LOG_EXCEPTION(E,R,S), {stack,S} end.

% io(Event, #cx{module=Module}) ->
%     try X = Module:event(Event), {io,render_actions(nitro:actions()),X}
%     catch E:R -> S = erlang:get_stacktrace(), ?LOG_EXCEPTION(E,R,S), {io,<<>>,{stack,S}} end.

% io(Data) ->
%     try {io,render_actions(Data), "data io"}
%     catch E:R -> S = erlang:get_stacktrace(), ?LOG_EXCEPTION(E,R,S), {io,<<>>,{stack,S}} end.

% -endif.

val(undefined) -> [];
val(<<>>) -> [];
val(Value) when is_list(Value) -> Value;
val(Value) -> [Value].


dict(undefined) -> 0;
dict(Value) -> length(Value).

