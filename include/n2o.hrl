-ifndef(N2O_HRL).
-define(N2O_HRL, true).

-define(FORMAT(F), case F of F when is_binary(F) -> binary_to_list(F);
                             F when is_atom(F) -> atom_to_list(F);
                             F when is_list(F) -> F end).

-ifdef(OTP_RELEASE).
-include_lib("kernel/include/logger.hrl").
-else.
-define(LOG_INFO(F), io:format(?FORMAT(F)) end).
-define(LOG_INFO(F,X), io:format(?FORMAT(F),X)).
-define(LOG_ERROR(F), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,F])).
-define(LOG_ERROR(F,X), io:format(?FORMAT(F),X)).
-endif.

-define(LOG_EXCEPTION(E,R,S), ?LOG_ERROR(#{exception => E, reason => R, stack => S})).

-record(pi, { name     :: term(),
              table    :: atom(),
              sup      :: atom(),
              module   :: atom(),
              state    :: term()  }).

-record(cx, { handlers  = [] :: list({atom(),atom()}),
              actions   = [] :: list(tuple()),
              req       = [] :: [] | term(),
              module    = [] :: [] | atom(),
              lang      = [] :: [] | atom(),
              path      = [] :: [] | binary(),
              session   = [] :: [] | binary(),
              token     = [] :: [] | binary(),
              formatter = bert :: bert | json | atom(),
              params    = [] :: [] | list(tuple()) | binary(),
              node      = [] :: [] | atom(),
              client_pid= [] :: [] | term(),
              state     = [] :: [] | term(),
              from      = [] :: [] | binary(),
              vsn       = [] :: [] | binary() }).

-define(CTX(ClientId), n2o:cache(ClientId)).
-define(REQ(ClientId), (n2o:cache(ClientId))#cx.req).

% API

-define(QUERING_API, [init/2, finish/2]).
-define(SESSION_API, [init/2, finish/2, get_value/2, set_value/2, clear/0]).
-define(MESSAGE_API, [send/2, reg/1, reg/2, unreg/1, init/0]).

-define(N2O_JSON, (application:get_env(n2o,json,jsone))).

% IO protocol

-record(bin,     { data=[] }).
-record(client,  { data=[] }).
-record(server,  { data=[] }).

% Nitrogen Protocol

-record(init,    { token=[] }).
-record(pickle,  { source=[], pickled=[], args=[] }).
-record(flush,   { data=[] }).
-record(direct,  { data=[] }).
-record(ev,      { module=[], msg=[], trigger=[], name=[] }).

% File Transfer Protocol

-record(ftp,     { id=[], sid=[], filename=[], meta=[], size=[], offset=[], block=[], data=[], status=[] }).
-record(ftpack,  { id=[], sid=[], filename=[], meta=[], size=[], offset=[], block=[], data=[], status=[] }).

% Async Protocol
% -record(async, { name=[], data=[], stop=false }).
-record(async, { name=[], data=[] }).
-endif.
