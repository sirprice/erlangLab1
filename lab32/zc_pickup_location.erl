-module(zc_pickup_location).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([start_link/2,insert_location/2,get_info/1,return_car/2,pickup_car/1,print/0,stop/0]).
-behaivour(gen_server).


% Start a new pickup location containing Spaces number of parking spaces of 
% which Occupied contain cars. LocRef is a unique reference for that rental location.
start_link(Spaces, Occupied) ->  
    gen_server:start_link({local,?MODULE},?MODULE,[{Spaces, Occupied}],[]).
print()->
	gen_server:call(?MODULE,{print}).
stop()->
	gen_server:cast(?MODULE,stop).



% Pick up a car from the rental location LocRef.  
% It will return {error, empty} if there are no free cars.
pickup_car(LocRef) ->
    gen_server:call(?MODULE,{pickup_car,LocRef}).

% Try to return a car to a pickup location. 
% It will return {error, full} if there are no free parking spaces.
return_car(LocRef, CarRef) -> 
    gen_server:call(?MODULE,{return_car,LocRef,CarRef}).


% Get information about the status in the rental location. Note that 
% the return value is a property list and the order of the elements is not specified.
get_info(LocRef) -> 
    gen_server:call(?MODULE,{get_info,LocRef}).
% = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

init([{Spaces, Occupied}])->
	io:format('Location running...'),
    io:format("Location init  = ~w~n  \n", [self()]),

    zc_car_register:generate_cars(Occupied,self()),
    {ok,{Spaces, Occupied}}.

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


handle_cast({delete,Key}, Db)->
	NewDb = db:delete(Key,Db),
	{noreply,NewDb};

handle_cast({stop}, Server) ->
	{stop,normal,Server}.


% Call, sync



handle_call({pickup_car, LocRef},_From, State)->
    {Result,NewState} = server_pickup_car(LocRef,State),
    io:format("handle_call pickup_car  = ~w~n  \n", [Result]),
	{reply, Result, NewState};

handle_call({return_car, LocRef, CarRef},_From, State)->
    {Result,NewState} = server_return_car(LocRef,CarRef,State),
    io:format("handle_call return_car  = ~w~n  \n", [Result]),
	{reply, Result, NewState};
handle_call({get_info, LocRef},_From, State)->
    {Result,NewState} = server_get_info(LocRef,State),
    io:format("handle_call get_info  = ~w~n  \n", [Result]),
	{reply, Result, NewState};



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


% = = = = = = = = = = logic = = = = = = = = = = = = = = = = = = = =

server_get_info(Pid,{Spaces, Occupied}) when Pid == self()->
    {{ok, [{spaces, Spaces + Occupied}, {occupied,Occupied}, {free, Spaces}]},{Spaces, Occupied}};

server_get_info(LocRef,{Spaces, Occupied}) ->
    io:format("server_get_info error ver = ~w~n  \n", [{self(),LocRef}]),
    {{error, [{spaces, Spaces + Occupied}, {occupied,Occupied}, {free, Spaces}]},{Spaces, Occupied}}.

% server_get_cars(LocRef, Count,Db) -> 
%     Values = db:read(LocRef,Db),
%     io:format("server_get_cars = ~w~n  \n", [Values]),
%     case Values of 
%         {error,instance} -> [];
%         {ok,Cars} -> gather(Cars,Count,[])
%     end.



% {Spaces, Occupied}
% {ok, CarRef} | {error, empty}
server_pickup_car(Pid,{Spaces, 0}) when Pid == self()-> {{error, full},{Spaces, 0}};
server_pickup_car(Pid,{Spaces, Occupied}) when Pid == self()->
    Result = zc_car_register:get_cars(self(),1),
    io:format("server_pickup_car result = ~w~n  \n", [Result]),
    case Result of
        {_Err,[]} -> {{error, empty},{Spaces, Occupied}};
        {ok,[CarRef]} -> zc_car_register:car_pickup(self(),CarRef),
            {{ok, CarRef},{Spaces + 1, Occupied - 1}}
    end;
server_pickup_car(_LocRef,{Spaces, Occupied}) -> {{error, wrongLoc},{Spaces, Occupied}}.


%%
server_return_car(Pid, _CarRef,{0, Occupied}) when Pid == self()-> {{error, full},{0, Occupied}};
server_return_car(Pid, CarRef,{Spaces, Occupied}) when Pid == self()->
    zc_car_register:car_return(self(),CarRef),
    {ok,{Spaces - 1, Occupied + 1}};
server_return_car(_LocRef, _CarRef,{Spaces, Occupied}) -> {{error, wrongLoc},{Spaces, Occupied}}.

% server_cars_at(LocRef, Db) -> cleanRead(db:read(LocRef,Db)).
%%%%%%%%%%%%%%% Helper functions %%%%%%


% cleanRead({error,_Data}) -> [];
% cleanRead({ok,Data}) -> Data.

insert_location(LocRef,Cars) -> gen_server:cast(?MODULE, {insert_location, {LocRef,Cars}}).
% insert_carAt(LocRef,Car) -> gen_server:cast(?MODULE, {insert_carAt, {LocRef,Car}}).

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

% gather([],_Count, Result) -> 
%     % io:format("gather Result = ~w~n  \n", [Result]),
%     Result;
% gather(_CarLot,0, Result)  -> 
%     % io:format("gather  Result count = 0= ~w~n  \n", [Result]),
%      Result;
% gather([H|[]],Count, Result) when Count > 0 ->
%      Res = Result ++ [H],
%     % io:format("gather = ~w~n  \n", [Res]),
%     Res;
% gather([H|T],Count, Result) when Count > 0 -> Res = Result ++ [H],
%     % io:format("gather [H,T],Count, = ~w~n  \n", [Res]),
%     gather(T,Count - 1,Res ).


% % splice removes the tuple with Key and returns a list without it.
% splice(Value,[ Value | Right]) -> Right; % Found the key , return the right part of it
% splice(Value,[Entry | Right]) -> [Entry | splice(Value, Right)]; % Key not here walk to next and add Element back
% splice(_Value,[]) -> []. % no Elements left, return

