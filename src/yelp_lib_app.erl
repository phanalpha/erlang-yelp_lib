%%%-------------------------------------------------------------------
%% @doc yelp_lib public API
%% @end
%%%-------------------------------------------------------------------

-module(yelp_lib_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("../include/yelp.hrl").

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, ConsumerKey} = application:get_env(consumer_key),
    {ok, ConsumerSecret} = application:get_env(consumer_secret),
    {ok, Token} = application:get_env(token),
    {ok, TokenSecret} = application:get_env(token_secret),

    yelp_lib_sup:start_link(
      #oauth_consumer{consumer_key = ConsumerKey
		     ,consumer_secret = ConsumerSecret
		     ,token = Token
		     ,token_secret = TokenSecret
		     } ).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
