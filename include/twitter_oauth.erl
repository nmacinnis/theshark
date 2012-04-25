%%
%% This is an example client for the Twitter API.
%%
%% Example usage:
%%
%%   $ make
%%   ...
%%   $ erl -pa ebin -pa path/to/erlang-oauth/ebin -s crypto -s ssl -s inets
%%   ...
%%   1> Consumer = {"...KEY...", "...SECRET...", hmac_sha1}.
%%   ...
%%   2> {ok, Client} = oauth_twitter:start(Consumer).
%%   ...
%%   3> {ok, Token} = oauth_twitter:get_request_token(Client).
%%   ...
%%   4> AuthorizeURL = oauth_twitter:authorize_url(Token).
%%   ...
%%   5> ok = oauth_twitter:get_access_token(Client, "...VERIFIER (PIN)...").
%%   ...
%%   6> {ok, Headers, XML} = oauth_twitter:get_favorites(Client).
%%   ...
%%
%% Note that before fetching the access token (step 5) you need to have
%% authorized the request token and been given the a verifier PIN at twitter.
%%
-module(oauth_twitter).

-compile(export_all).

start(Consumer) ->
  oauth_client:start(Consumer).

get_request_token(Client) ->
  URL = "https://twitter.com/oauth/request_token",
  oauth_client:get_request_token(Client, URL).

authorize_url(Token) ->
  oauth:uri("https://twitter.com/oauth/authorize", [{"oauth_token", Token}]).

get_access_token(Client, Verifier) ->
  URL = "https://twitter.com/oauth/access_token",
  oauth_client:get_access_token(Client, URL, [{"oauth_verifier", Verifier}]).

get_favorites(Client) ->
  URL = "https://api.twitter.com/1/favorites.xml",
  oauth_client:get(Client, URL, []).

verify_credentials(Client) ->
  URL = "https://api.twitter.com/1/account/verify_credentials.xml",
  oauth_client:get(Client, URL, []).

