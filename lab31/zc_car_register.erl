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
    Db = db:new(),
    Db2 = db:write(carsOnTheRoad,[],Db),
	{ok, Db2}.

insert_location(LocRef,Cars) -> gen_server:cast(?MODULE, {insert_location, {LocRef,Cars}}).
insert_carAt(LocRef,Car) -> gen_server:cast(?MODULE, {insert_carAt, {LocRef,Car}}).


% helper functions
server_insert_location({LocRef,Cars},Db) -> db:write(LocRef,Cars,Db).
server_insert_carAt({LocRef,Car},Db) ->
    Result = db:read(LocRef,Db),
    io:format("handle_call server_insert_carAt = ~w~n  \n", [Result]),
    case Result of
        {error,instance} ->db:write(LocRef,[Car],Db);
        {ok,[]} -> db:write(LocRef,[Car],Db);
        {ok,Value} -> NewV = Value ++  [Car],
            db:write(LocRef,NewV,Db)
    end.





server_get_cars(LocRef, Count,Db) -> 
    Values = db:read(LocRef,Db),
    io:format("server_get_cars = ~w~n  \n", [Values]),
    case Values of 
        {error,instance} -> [];
        {ok,Cars} -> gather(Cars,Count,[])
    end.


 server_car_pickup(carsOnTheRoad,_CarRef,_Db) -> 
    io:format("server_car_return  carsOnTheRoad  \n"),
    error;
 server_car_pickup(LocRef,CarRef,Db) -> 
    Entry = db:read(LocRef,Db),
    io:format("server_car_pickup = ~w~n  \n", [Entry]),
    case Entry of 
        {error,instance} -> Db;
        {ok,Cars} -> 
            NewValue = splice(CarRef,Cars),
            io:format("server_car_pickup after splice= ~w~n  \n", [NewValue]),
            NewDb1 = db:write(LocRef,NewValue,Db),
            io:format("server_car_pickup  after write= ~w~n  \n", [NewDb1]),

            server_insert_carAt({carsOnTheRoad,CarRef},NewDb1)
    end.


cleanRead({error,_Data}) -> [];
cleanRead({ok,Data}) -> Data.

server_car_return(carsOnTheRoad,_CarRef,_Db) ->
     io:format("server_car_return  carsOnTheRoad  \n"),
     error;
server_car_return(LocRef, CarRef,Db) -> 
    {ok,OnRoad} = db:read(carsOnTheRoad,Db),
    case OnRoad of
        [] -> Db;
        _Tmp ->
            OnRoad1 = splice(CarRef,OnRoad),
            NewDb1 = db:write(carsOnTheRoad,OnRoad1,Db),
            server_insert_carAt({LocRef,CarRef},NewDb1)
    end.


server_cars_at(LocRef, Db) -> cleanRead(db:read(LocRef,Db)).
% = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


% Cast, async
handle_cast({insert_location,Value}, Db)-> 
    io:format("handle_cast insert_location = ~w~n  \n", [Value]),
    NewDb = server_insert_location(Value,Db),
	{noreply,NewDb};
handle_cast({insert_carAt,Value}, Db)-> 
io:format("handle_cast insert_carAt = ~w~n  \n", [Value]),
    NewDb = server_insert_carAt(Value,Db),
    
	{noreply,NewDb};

% {car_pickup,LocRef, CarRef}
handle_cast({car_pickup,LocRef, CarRef}, Db)-> 
% io:format("handle_call cmd with lock = ~w~n  \n", [Value]),
    NewDb = server_car_pickup(LocRef,CarRef,Db),    
	{noreply,NewDb};
handle_cast({car_return,LocRef, CarRef}, Db)-> 
% io:format("handle_call cmd with lock = ~w~n  \n", [Value]),
    NewDb = server_car_return(LocRef,CarRef,Db),    
	{noreply,NewDb};

handle_cast({delete,Key}, Db)->
	NewDb = db:delete(Key,Db),
	{noreply,NewDb};

handle_cast({stop}, Server) ->
	{stop,normal,Server}.


% Call, sync
handle_call({get_cars, LocRef, Count},_From, Db)->
    Result = server_get_cars(LocRef,Count,Db),
    io:format("handle_cast get_cars  = ~w~n  \n", [Result]),
	{reply, {ok,Result}, Db};

handle_call({cars_at, LocRef},_From, Db)->
    Result = server_cars_at(LocRef,Db),
    io:format("handle_cast cars_at  = ~w~n  \n", [Result]),
	{reply, Result, Db};

handle_call({print},_From, Db)->
	{reply, Db, Db}.

% 


% = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


handle_info(_Message,State)->
	{npreply,State}.

terminate(_Reason, _State)->
	{ok,_Reason}.

code_change(_OldVersion, State, _Extra)->
	{ok,State}.
%%%%%%%%%%%%%%% Helper functions %%%%%%


gather([],_Count, Result) -> 
    % io:format("gather Result = ~w~n  \n", [Result]),
    Result;
gather(_CarLot,0, Result)  -> 
    % io:format("gather  Result count = 0= ~w~n  \n", [Result]),
     Result;
gather([H|[]],Count, Result) when Count > 0 ->
     Res = Result ++ [H],
    % io:format("gather = ~w~n  \n", [Res]),
    Res;
gather([H|T],Count, Result) when Count > 0 -> Res = Result ++ [H],
    % io:format("gather [H,T],Count, = ~w~n  \n", [Res]),
    gather(T,Count - 1,Res ).


% splice removes the tuple with Key and returns a list without it.
splice(Value,[ Value | Right]) -> Right; % Found the key , return the right part of it
splice(Value,[Entry | Right]) -> [Entry | splice(Value, Right)]; % Key not here walk to next and add Element back
splice(_Value,[]) -> []. % no Elements left, return