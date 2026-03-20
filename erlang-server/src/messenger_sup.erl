-module(messenger_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                intensity => 5,
                period => 10},
    
    ChildSpecs = [
        #{
            id => messenger_server,
            start => {messenger_server, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [messenger_server]
        },
        #{
            id => tcp_server,
            start => {tcp_server, start_link, [8080]},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [tcp_server]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
