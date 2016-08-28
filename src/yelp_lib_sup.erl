%%%-------------------------------------------------------------------
%% @doc yelp_lib top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(yelp_lib_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-include("../include/yelp.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(#oauth_consumer{} = Consumer) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Consumer]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([#oauth_consumer{} = Consumer]) ->
    {ok, { {one_for_all, 0, 1}
	 , [#{ id => yelp_lib
	     , start => {yelp_lib, start_link, [Consumer]}
	     }]
	 } }.

%%====================================================================
%% Internal functions
%%====================================================================
