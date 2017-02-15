-module(insert).
-export([insert_read_all/1]).

%% insert_read_all(DbModule) -> ok.
%%  Insert standard 3 elements and reads them.

insert_read_all(Mod) ->
    io:format("Running insert_read_all", []),
    Do = fun () ->
		 ok = write_db(Mod, [{stureplan,"qwe123"},{vasagatan,"ewq321"},{odenplan,"q1w2e3"}]),
	 end,
    ok = with_db(Mod, Do).

%% with_db(DbModule, Do) -> Return.
%%  Start a database, execute Do() then stop the database.

with_db(Mod, Do) ->
    {ok,_} = Mod:start_link(),
    Ret = (catch Do()),				%Don't crash us
    %ok = Mod:stop(),
    timer:sleep(100),				%Give it time to stop
    Ret.

%% write_db(DbModule, [{Key,Val}]) -> ok.
%%  Writes in all key-val into existing database.

write_db(Mod, Vals) ->
    lists:foldl(fun ({K,V}, ok) -> Mod:insert_carAt(K, V) end, ok, Vals).
