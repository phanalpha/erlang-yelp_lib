-record(oauth_consumer, { consumer_key	  :: string()
			, consumer_secret :: string()
			, token		  :: string()
			, token_secret	  :: string()
			}).

-record(category, { name	:: string()
		  , alias	:: string()
		  }).

-record(coordinate, { latitude	:: float()
		    , longitude	:: float()
		    }).

-record(bounds, { sw	:: #coordinate{}
		, ne	:: #coordinate{}
		}).

-record(geographic_coordinate, { latitude   :: float()
			       , longitude  :: float()
			       , accuracy   :: float() | undefined
			       , altitude   :: float() | undefined
			       , altitude_accuracy  :: float() | undefined
			       }).

-record(region, { span		:: #coordinate{}
		, center	:: #coordinate{}
		}).

-record(location, { address	    :: [string()]
		  , display_address :: [string()]
		  , city	    :: string()
		  , state_code	    :: string()
		  , postal_code	    :: string()
		  , country_code    :: string()
		  , cross_streets   :: string()
		  , neighborhoods   :: [string()]
		  , coordinate	    :: #coordinate{}
		  }).

-record(business, { id			  :: string()
		  , is_claimed		  :: boolean()
		  , is_closed		  :: boolean()
		  , name		  :: string()
		  , image_url		  :: string()
		  , url			  :: string()
		  , mobile_url		  :: string()
		  , phone		  :: string()
		  , display_phone	  :: string()
		  , review_count	  :: integer()
		  , categories		  :: [#category{}]
		  , rating		  :: float()
		  , rating_img_url	  :: string()
		  , rating_img_url_small  :: string()
		  , rating_img_url_large  :: string()
		  , snippet_text	  :: string()
		  , snippet_image_url	  :: string()
		  , location		  :: #location{}
		  }).

-record(search_response, { region	:: #region{}
			 , total	:: integer
			 , businesses	:: [#business{}]
			 }).
