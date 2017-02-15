-module(zc_car_register).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([start_link/0,get_cars/2,car_pickup/2,car_return/2,cars_at/1,insert_carAt/2,insert_location/2,print/0,stop/0]).
-behaivour(gen_server).
% Starts the car registry.
start_link() -> 
    gen_server:start_link({local,?MODULE},?MODULE,[],[]).

stop()->
	gen_server:cast(?MODULE,stop).

% Get Count cars from the registry. CarRefs is a list of car references. Note 
% there is no guarantee that there are sufficient cars so the list may not 
% contain all the cars requested.
get_cars(LocRef, Count) -> 
    gen_server:call(?MODULE, {get_cars, LocRef, Count}).
    %{ok, CarRefs}.
    

% Notify that the car specified by CarRef has been picked up from the 
% location LocRef and is now in use.
car_pickup(LocRef, CarRef) -> 
    gen_server:cast(?MODULE, {car_pickup,LocRef, CarRef}),
    ok.

% Return the car CarRef to the pickup location LocRef.
car_return(LocRef, CarRef) -> 
    gen_server:cast(?MODULE, {car_return, LocRef, CarRef}),
    ok.


% Return a list CarRefs of all the cars known to be at LocRef.
cars_at(LocRef) -> gen_server:call(?MODULE, {cars_at, LocRef}).
%  CarRefs.
print()->
	gen_server:call(?MODULE,{print}).

%%%%%%%%%%%%%%%%
init([])->
	io:format('Server running...'),
	{ok, db:new()}.

insert_location(LocRef,Cars) -> gen_server:call(?MODULE, {insert_location, {LocRef,Cars}}).
insert_carAt(LocRef,Car) -> gen_server:call(?MODULE, {insert_carAt, {LocRef,Car}}).


% helper functions
server_insert_location({LocRef,Cars},Db) -> db:write(LocRef,Cars,Db).
server_insert_carAt({LocRef,Car},Db) ->
    Result = db:read(LocRef,Car,Db),
    case Result of
        {error,instance} ->db:write(LocRef,[Car],Db);
        {ok,[]} -> db:write(LocRef,Car,Db);
        {ok,[Value]} -> db:write(LocRef,[Car | Value],Db)
    end.



% = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


% Cast, async
handle_cast({insert_location,Value}, DB)-> 
    NewDB = server_insert_location(Value,DB),
	{noreply,NewDB};
handle_cast({insert_carAt,Value}, DB)-> 
    NewDB = server_insert_carAt(Value,DB),
	{noreply,NewDB};

handle_cast({delete,Key}, DB)->
	NewDB = db:delete(Key,DB),
	{noreply,NewDB};

handle_cast(stop, Server) ->
	{stop,normal,Server}.


% Call, sync
handle_call({print},_From, DB)->
	{reply, DB, DB}.



% = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


handle_info(_Message,State)->
	{npreply,State}.

terminate(_Reason, _State)->
	{ok,_Reason}.

code_change(_OldVersion, State, _Extra)->
	{ok,State}.
