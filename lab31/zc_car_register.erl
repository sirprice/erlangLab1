% dsa


% Starts the car registry.
start_link() -> {ok,'RefPid'}.

% Get Count cars from the registry. CarRefs is a list of car references. Note 
% there is no guarantee that there are sufficient cars so the list may not 
% contain all the cars requested.
get_cars(LocRef, Count) -> {ok, CarRefs}.

% Notify that the car specified by CarRef has been picked up from the 
% location LocRef and is now in use.
car_pickup(LocRef, CarRef) -> ok.

% Return the car CarRef to the pickup location LocRef.
car_return(LocRef, CarRef) -> ok.


% Return a list CarRefs of all the cars known to be at LocRef.
cars_at(LocRef) -> CarRefs.

