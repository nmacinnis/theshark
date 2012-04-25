-module(irc_commands).

-compile(export_all).

%% irc command wrappers

pong(Server) ->
    "PONG " ++ Server.

join(Channel) ->
    "JOIN " ++ Channel.

list_names(Channel) ->
    "NAMES " ++ Channel.

password(Password) ->
    "PASS " ++ Password.

say(Who, What) ->
    "PRIVMSG " ++ Who ++ " :" ++ What.

kick(Channel, Name) ->
    "KICK " ++ Channel ++ " " ++ Name ++ " ::commissar:".

nick(Name) ->
    "NICK " ++ Name.

user(Name) ->
    "USER " ++ Name ++ " 0 * :I'm a bot made of Erlang!".

identify(Name, Password) ->
    say("NICKSERV", "IDENTIFY " ++ Name ++ " " ++ Password).


