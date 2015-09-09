-module(erl_cowboy).
-behaviour(gen_server).

-include("erl_cowboy_logger.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         routing/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

routing(Key, Routing) ->
    gen_server:call(?MODULE, {routing, Key, Routing}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    Port = application:get_env(erl_cowboy, port, 80),
    Listeners = application:get_env(erl_cowboy, listeners, 100),
    TLSEnabled = application:get_env(erl_cowboy, tls_enabled, false),
    TLSOpts = application:get_env(erl_cowboy, tls_opts, undefined),
    gen_server:cast(?MODULE, start),
    {ok, #{port => Port, listeners => Listeners,
           routes => #{},
           tls_enabled => TLSEnabled,
           tls_opts => TLSOpts}}.

handle_call({routing, Key, NewRoute}, _From, State = #{routes := Routes}) ->
    NewRoutes = maps:put(Key, NewRoute, Routes),
    ok = update_cowboy(NewRoutes),
    {reply, ok, State#{routes := NewRoutes}};
handle_call(Request, _From, State) ->
    {stop, {notimplemented, Request}, State}.

handle_cast(start, State = #{port := Port,
                             listeners := Listeners,
                             tls_enabled := false}) ->
    {ok, Pid} = start_cowboy(Port, Listeners),
    {noreply, State#{pid => Pid}};
handle_cast(start, State = #{port := Port,
                             listeners := Listeners,
                             tls_enabled := true,
                             tls_opts := TLSOpts}) ->
    {ok, Pid} = start_cowboy_with_tls(Port, Listeners, TLSOpts),
    {noreply, State#{pid => Pid}};
handle_cast(Msg, State) ->
    {stop, {notimplemented, Msg}, State}.

handle_info(Info, State) ->
    {stop, {notimplemented, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start_cowboy(Port, Listeners) ->
    ?INFO("~p listening on port ~p~n", [?MODULE, Port]),
    cowboy:start_http(?MODULE, Listeners, [{port, Port}], [{env, []}]).

start_cowboy_with_tls(Port, Listeners, TLSOpts) ->
    TLSCert = proplists:get_value(certfile, TLSOpts, undefined),
    TLSKey = proplists:get_value(keyfile, TLSOpts, undefined),
    TLSKeyPass = proplists:get_value(password, TLSOpts, undefined),
    ?INFO("~p listening on port ~p with TLS enabled~n\tcert: ~s~n\tkey: ~s)",
          [?MODULE, Port, TLSCert, TLSKey]),
    TransportOpts = [{port, Port}, {certfile, TLSCert}, {keyfile, TLSKey},
                     {password, TLSKeyPass}],
    cowboy:start_https(?MODULE, Listeners, TransportOpts, [{env, []}]).

update_cowboy(Routes) ->
    RouteList = lists:flatten(maps:fold(
        fun(_, Route, Acc) ->
            [Route | Acc]
        end, [], Routes)),
    cowboy:set_env(?MODULE, dispatch, cowboy_router:compile([{'_', RouteList}])).
