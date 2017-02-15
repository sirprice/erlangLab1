-module(genS).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([start_link/0,stop/0,write/2,read/1,match/1,delete/1,print/0]).
-behaivour(gen_server).


start_link()->
	gen_server:start_link({local,genS},genS,[],[]).

stop()->
	gen_server:cast(genS,stop).

write(Key, Value)->
	gen_server:cast(genS, {write,Key, Value}).

delete(Key)->
	gen_server:cast(genS, {delete,Key}).

print()->
	gen_server:call(genS,{print}).

read(Key)->
	gen_server:call(genS, {read,Key}).

match(Value)->
	gen_server:call(genS,{match,Value}).


% = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


init([])->
	io:format('Server running...'),
	{ok, inl1:init()}.



% Cast, async
handle_cast({write,Key,Value}, DB)->
	NewDB = inl1:add(Key,Value,DB),
	{noreply,NewDB};

handle_cast({delete,Key}, DB)->
	NewDB = inl1:delete(Key,DB),
	{noreply,NewDB};

handle_cast(stop, Server) ->
	{stop,normal,Server}.



% Call, sync
handle_call({read,Key},_From, DB)->
	{reply, inl1:find(Key, DB), DB};

handle_call({match,Value},_From, DB)->
	{reply, inl1:match(Value, [], DB), DB};

handle_call({print}, _From, DB)->
	{reply, DB, DB}.

% = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


handle_info(_Message,State)->
	{npreply,State}.

terminate(_Reason, State)->
	{ok,_Reason}.

code_change(_OldVersion, State, _Extra)->
	{ok,State}.
