-module(theshark).
-export([do/0, quote_text/1, make_status/1, make_status_request/1]).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_Type, _StartArgs) ->
    do().

stop(_State) ->
    ok.



do() ->
    inets:start(),
    Request = make_status_request(make_status("Status!")),
    {code, Result} = httpc:request(post, Request, [], []),
    io:format(Result).

quote_text(Text) ->
    "\"" ++ Text ++ "\"".

make_status(Text) ->
    "{" ++ quote_text("status") ++ ":" ++ quote_text(Text) ++ "}".

make_status_request(Status) ->
    io:format(Status),
    Url = "http://api.twitter.com/1/statuses/update.json",
    Headers = [],
    ContentType = "application/x-www-form-urlencoded",
    Body = Status,
    {Url, Headers, ContentType, Body}.
