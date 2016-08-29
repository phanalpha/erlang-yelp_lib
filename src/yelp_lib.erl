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

-type sort() :: best_matched			% 0
	      | distance			% 1
	      | highest_rated.			% 2

-type location() :: { location, string() }
		  | #coordinate{}
		  | #bounds{}
		  | #geographic_coordinate{}.

-type iso_3166_1_alpha_2() :: string().		% country code
-type iso_639() :: string().			% language code
-type search_term() :: string().
-type search_param() :: search_term()
		      | { limit, integer() }
		      | { offset, integer() }
		      | sort()
		      | { category, [string()] }
		      | location()
		      | { radius, integer() }
		      | deals
		      | { country, iso_3166_1_alpha_2() }
		      | { language, iso_639() }
		      | actionlinks.

%% ============================================================
%% API
%% ============================================================

start() ->
    hackney:start(),
    application:start(yelp_lib).

start_link(#oauth_consumer{} = Consumer) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Consumer], []).

-spec search([search_param()]) -> {ok, #search_response{}} | {error, term()}.
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
    Parameters2 = sign("GET", ?YELP_SEARCH, lists:map(fun regularize/1, Parameters), OAuthConsumer),
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

-spec regularize(search_param()) -> {string(), string()}.
regularize({limit, Limit}) ->
    {"limit", integer_to_list(Limit)};
regularize({offset, Offset}) ->
    {"offset", integer_to_list(Offset)};
regularize(best_matched) ->
    {"sort", "0"};
regularize(distance) ->
    {"sort", "1"};
regularize(highest_rated) ->
    {"sort", "2"};
regularize({category, Categories}) ->
    {"category_filter", lists:flatten( lists:join(",", Categories) )};
regularize({location, Location}) ->
    {"location", Location};
regularize(#coordinate{latitude = Latitude, longitude = Longitude}) ->
    {"cll", lists:flatten( io_lib:format("~f,~f", [Latitude, Longitude]) )};
regularize(#bounds{sw = #coordinate{ latitude = Lat0, longitude = Lng0 }
		  ,ne = #coordinate{ latitude = Lat1, longitude = Lng1 }}) ->
    {"bounds", lists:flatten( io_lib:format("~f,~f|~f,~f", [Lat0, Lng0, Lat1, Lng1]) )};
regularize(#geographic_coordinate{latitude = Latitude
				 ,longitude = Longitude
				 ,accuracy = Accuracy
				 ,altitude = Altitude
				 ,altitude_accuracy = AltitudeAccuracy
				 }) ->
    Es = [ if E =/= undefined ->
		   io_lib:format("~f", [E]);
	      true ->
		   ""
	   end ||
	     E <- [Latitude, Longitude, Accuracy, Altitude, AltitudeAccuracy] ],
    {"ll", lists:flatten( lists:join(",", Es) )};
regularize({radius, Radius}) ->
    {"radius_filter", integer_to_list(Radius)};
regularize(deals) ->
    {"deals_filter", "true"};
regularize({ country, Country }) ->
    {"cc", Country};
regularize({ language, Language }) ->
    {"lang", Language};
regularize(actionlinks) ->
    {"actionlinks", "true"};
regularize(Term) ->
    {"term", Term}.

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
	     , location = cook( maps:merge(#{<<"postal_code">> => undefined
					    ,<<"cross_streets">> => undefined
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
