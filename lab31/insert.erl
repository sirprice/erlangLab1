-module(insert).
-export([insert_all/1]).

%% insert_read_all(DbModule) -> ok.
%%  Insert standard 3 elements and reads them.

insert_all(Mod) ->

    io:format("Running insert_read_all", []),
    DoInsertCar = fun () ->
		 ok = write_db(Mod, writeCar, [{stureplan,qwe123},{vasagatan,ewq321},{odenplan,q1w2e3}])
	 end,
      DoInsertLoc = fun () ->
		 ok = write_db(Mod,writeLoc, [{stureplan,[bil1,bil2,bil5]},{vasagatan,[bil4,bil6,bil25]},{odenplan,[bil15,bil52,bil75]}])
	 end,
    ok = with_db(Mod, DoInsertCar,DoInsertLoc).

%% with_db(DbModule, Do) -> Return.
%%  Start a database, execute Do() then stop the database.

with_db(Mod, DoInsertCar,DoInsertLoc) ->
    {ok,_} = Mod:start_link(),
    Ret = (catch DoInsertLoc()),
    Ret2 = (catch DoInsertCar()),				%Don't crash us
    %ok = Mod:stop(),
    timer:sleep(100),				%Give it time to stop
    Ret2.

%% write_db(DbModule, [{Key,Val}]) -> ok.
%%  Writes in all key-val into existing database.

write_db(Mod,writeCar, Vals) ->
    lists:foldl(fun ({K,V}, ok) -> Mod:insert_carAt(K, V) end, ok, Vals);
write_db(Mod,writeLoc, Vals) ->
    lists:foldl(fun ({K,V}, ok) -> Mod:insert_location(K, V) end, ok, Vals).
