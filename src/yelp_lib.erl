-module(yelp_lib).

-behaviour(gen_server).

%% API
-export([start/0, start_link/1, search/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2
	,terminate/2, code_change/3]).

-include("../include/yelp.hrl").

-record(state, { consumer :: #oauth_consumer{}
	       }).

-define(SERVER, ?MODULE).

-define(YELP_SEARCH, "https://api.yelp.com/v2/search").
-define(NR_RAND_BYTES, 21).

%% ============================================================
%% API
%% ============================================================

start() ->
    hackney:start(),
    application:start(yelp_lib).

start_link(#oauth_consumer{} = Consumer) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Consumer], []).

search(Parameters) ->
    gen_server:call(?SERVER, {search, Parameters}).

%% ============================================================
%% gen_server
%% ============================================================

init([#oauth_consumer{} = Consumer]) ->
    { ok, #state{consumer = Consumer} }.

handle_call({search, Parameters}, _From, State) ->
    { reply, search(Parameters, State#state.consumer), State };
handle_call(_Request, _From, State) ->
    { reply, unimplemented, State }.

handle_cast(_Message, State) ->
    { noreply, State }.

handle_info(_Info, State) ->
    { noreply, State }.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    { ok, State }.

%% ============================================================
%% internal
%% ============================================================

search(Parameters, OAuthConsumer) ->
    Parameters2 = sign("GET", ?YELP_SEARCH, Parameters, OAuthConsumer),
    case hackney:get( hackney_url:make_url(?YELP_SEARCH, [], Parameters2) ) of
	{ok, StatusCode, Headers, ClientRef} ->
	    {ok, Body2} = hackney:body(ClientRef),
	    if StatusCode =:= 200 ->
		    {ok, cook(jsx:decode(Body2, [return_maps]))};
	       true ->
		    {error, {StatusCode, Headers, Body2}}
	    end;
	{error, _Reason} = Error ->
	    Error
    end.

cook(#{<<"latitude">> := Latitude
      ,<<"longitude">> := Longitude}) ->
    #coordinate{ latitude = Latitude, longitude = Longitude };
cook(#{<<"latitude_delta">> := Latitude
      ,<<"longitude_delta">> := Longitude}) ->
    #coordinate{ latitude = Latitude, longitude = Longitude };
cook(#{<<"span">> := Span
      ,<<"center">> := Center}) ->
    #region{ span = cook(Span), center = cook(Center) };
cook(#{<<"address">> := Address
      ,<<"display_address">> := DisplayAddress
      ,<<"city">> := City
      ,<<"state_code">> := StateCode
      ,<<"postal_code">> := PostalCode
      ,<<"country_code">> := CountryCode
      ,<<"cross_streets">> := CrossStreets
      ,<<"neighborhoods">> := Neighborhoods
      ,<<"coordinate">> := Coordinate}) ->
    #location{ address = Address
	     , display_address = DisplayAddress
	     , city = City
	     , state_code = StateCode
	     , postal_code = PostalCode
	     , country_code = CountryCode
	     , cross_streets = CrossStreets
	     , neighborhoods = Neighborhoods
	     , coordinate = cook(Coordinate) };
cook(#{<<"id">> := Id
      ,<<"is_claimed">> := IsClaimed
      ,<<"is_closed">> := IsClosed
      ,<<"name">> := Name
      ,<<"image_url">> := ImageURL
      ,<<"url">> := URL
      ,<<"mobile_url">> := MobileURL
      ,<<"phone">> := Phone
      ,<<"display_phone">> := DisplayPhone
      ,<<"review_count">> := ReviewCount
      ,<<"categories">> := Categories
      ,<<"rating">> := Rating
      ,<<"rating_img_url">> := RatingImgURL
      ,<<"rating_img_url_small">> := RatingImgURLSmall
      ,<<"rating_img_url_large">> := RatingImgURLLarge
      ,<<"snippet_text">> := SnippetText
      ,<<"snippet_image_url">> := SnippetImageURL
      ,<<"location">> := Location}) ->
    #business{ id = Id
	     , is_claimed = IsClaimed
	     , is_closed = IsClosed
	     , name = Name
	     , image_url = ImageURL
	     , url = URL
	     , mobile_url = MobileURL
	     , phone = Phone
	     , display_phone = DisplayPhone
	     , review_count = ReviewCount
	     , categories = [#category{ name = Name2, alias = Alias } ||
				[ Name2, Alias ] <- Categories]
	     , rating = Rating
	     , rating_img_url = RatingImgURL
	     , rating_img_url_small = RatingImgURLSmall
	     , rating_img_url_large = RatingImgURLLarge
	     , snippet_text = SnippetText
	     , snippet_image_url = SnippetImageURL
	     , location = cook( maps:merge(#{<<"cross_streets">> => undefined
					    ,<<"neighborhoods">> => undefined
					    }, Location) )
	     };
cook(#{<<"region">> := Region
      ,<<"total">> := Total
      ,<<"businesses">> := Businesses}) ->
    #search_response{ region = cook(Region)
		    , total = Total
		    , businesses = [ cook( maps:merge(#{<<"phone">> => undefined,
							<<"display_phone">> => undefined
						       }, Business) ) ||
				       Business <- Businesses ]
		    }.

-spec sign(string(), string(), [{string(), string()}], #oauth_consumer{}) -> [{string(), string()}].
sign(Method, Url, Parameters,
     #oauth_consumer{ consumer_key = ConsumerKey
		    , consumer_secret = ConsumerSecret
		    , token = Token
		    , token_secret = TokenSecret }) ->

    Nonce = base64:encode_to_string( crypto:strong_rand_bytes(?NR_RAND_BYTES) ),
    Parameters2 = [{"oauth_consumer_key", ConsumerKey}
		  ,{"oauth_token", Token}
		  ,{"oauth_signature_method", "HMAC-SHA1"}
		  ,{"oauth_nonce", Nonce}
		  ,{"oauth_timestamp", integer_to_list(unix_timestamp())} |
		   Parameters],
    Text = lists:join("&", [http_uri:encode(S) ||
			       S <- [Method, Url, lists:flatten(normalize(Parameters2))]]),
    Key = lists:join("&", [ConsumerSecret, TokenSecret]),
    Digest = crypto:hmac(sha, Key, Text),

    [{"oauth_signature", base64:encode_to_string(Digest)} | Parameters2].

-spec normalize([{string(), string()}]) -> iodata().
normalize(Parameters) ->
    lists:join( "&", [[http_uri:encode(K), "=", http_uri:encode(V)] ||
			 {K, V} <- lists:sort(Parameters)] ).

unix_timestamp() ->
    {MegaSecs, Secs, _MicroSecs} = erlang:timestamp(),
    1000000 * MegaSecs + Secs.
