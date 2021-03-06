-module(zc_car_register).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([start_link/0,get_cars/2,cars_at/1,insert_car/2,car_pickup/2,car_return/2,print/0,stop/0,car_update/2,generate_cars/2]).
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

% Notify that the car specified by CarRef has been picked up from the 
% location LocRef and is now in use.
car_update(CarRef,LocRef ) -> 
    gen_server:call(?MODULE, {car_update, CarRef, LocRef}).

% Return a list CarRefs of all the cars known to be at LocRef.
cars_at(LocRef) -> 
    gen_server:call(?MODULE, {cars_at, LocRef}).
%  CarRefs.
print()->
	gen_server:call(?MODULE,{print}).

%%%%%%%%%%%%%%%%
init([])->
	io:format('Server running...'),
    Db = db:new(),
    Db2 = db:write(numberOfCars,0,Db),
	{ok, Db2}.

generate_cars(Count, LocRef) ->
    gen_server:cast(?MODULE, {generate_cars, Count,LocRef}).


insert_car(Car,LocRef) -> gen_server:cast(?MODULE, {insert_car, {Car,LocRef}}).
% insert_carAt(LocRef,Car) -> gen_server:cast(?MODULE, {insert_carAt, {LocRef,Car}}).

% helper functions
server_insert_car({Car,LocRef},Db) -> db:write(Car,LocRef,Db).
% server_insert_carAt({LocRef,Car},Db) ->
%     Result = db:read(LocRef,Db),
%     io:format("handle_call server_insert_carAt = ~w~n  \n", [Result]),
%     case Result of
%         {error,instance} ->db:write(LocRef,[Car],Db);
%         {ok,[]} -> db:write(LocRef,[Car],Db);
%         {ok,Value} -> NewV = Value ++  [Car],
%             db:write(LocRef,NewV,Db)
%     end.

server_generate_cars(0,_LocRef,Db) -> Db;
server_generate_cars(Count,LocRef,Db) -> 
    {ok,Number} = db:read(numberOfCars,Db),
    io:format("server_generate_cars min saut = ~w~n  \n", [Number]),
    
    NewDb = db:write(numberOfCars,Number + 1 ,Db),
    io:format("server_generate_cars, efter write\n"),
    NewDb2 = server_insert_car({Number, LocRef},NewDb),
    server_generate_cars(Count -1, LocRef,NewDb2).

server_get_cars(LocRef, Count,Db) -> 
    Values = db:match(LocRef,Db),
    io:format("server_get_cars = ~w~n  \n", [Values]),
    gather(Values,Count,[]).

server_cars_at(LocRef, Db) -> db:match(LocRef,Db).

 server_car_update(CarRef,LocRef,Db) -> 
    io:format("server_car_update, CarRef to be found = ~w~n  \n", [{CarRef,LocRef}]),
    Entry = db:read(CarRef,Db),
    io:format("server_car_pickup = ~w~n  \n", [Entry]),
    case Entry of 
        {error,instance} -> {error,Db};
        {ok,_Location} -> 
            NewDb = db:write(CarRef,LocRef,Db),
            io:format("server_car_pickup  after write= ~w~n  \n", [NewDb]),
            {ok,NewDb}
    end.


% cleanRead({error,_Data}) -> [];
% cleanRead({ok,Data}) -> Data.

server_car_pickup(_LocRef,CarRef,Db) ->
    case db:read(CarRef,Db) of
        {error,instance} -> Db;
        {ok,_OldLoc} -> db:write(CarRef,carsOnTheRoad,Db)
    end.
server_car_return(LocRef, CarRef,Db) -> 
    case db:read(CarRef,Db) of
        {error,instance} -> Db;
        {ok,Car} -> db:write(CarRef,LocRef,Db)
    end.



% = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


% Cast, async
handle_cast({insert_car,Value}, Db)-> 
    io:format("handle_cast insert_car = ~w~n  \n", [Value]),
    NewDb = server_insert_car(Value,Db),
	{noreply,NewDb};

handle_cast({generate_cars,Count,LocRef}, Db)-> 
    NewDb = server_generate_cars(Count,LocRef,Db),
	{noreply,NewDb};

% handle_cast({insert_carAt,Value}, Db)-> 
% io:format("handle_cast insert_carAt = ~w~n  \n", [Value]),
%     NewDb = server_insert_carAt(Value,Db),    
% 	{noreply,NewDb};

% % {car_pickup,LocRef, CarRef}
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

handle_call({car_update, CarRef,LocRef },_From, Db)->
    Result = server_car_update(CarRef,LocRef,Db),
    io:format("handle_call car_update  = ~w~n  \n", [Result]),
	{reply, Result, Db};

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


% % splice removes the tuple with Key and returns a list without it.
% splice(Value,[ Value | Right]) -> Right; % Found the key , return the right part of it
% splice(Value,[Entry | Right]) -> [Entry | splice(Value, Right)]; % Key not here walk to next and add Element back
% splice(_Value,[]) -> []. % no Elements left, return