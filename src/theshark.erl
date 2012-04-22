-module(theshark).

-export([do/0, quote_text/1, make_status/1, make_status_request/1]).

do() ->
    inets:start(),
    Request = make_status_request(make_status("Status!")),
    {code, Result} = http:request(post, Request, [], []),
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
