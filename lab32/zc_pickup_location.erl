
% Start a new pickup location containing Spaces number of parking spaces of 
% which Occupied contain cars. LocRef is a unique reference for that rental location.
start_link(Spaces, Occupied) -> {ok, LocRef}

% Pick up a car from the rental location LocRef.  
% It will return {error, empty} if there are no free cars.
pickup_car(LocRef) -> {ok, CarRef} | {error, empty}

% Try to return a car to a pickup location. 
% It will return {error, full} if there are no free parking spaces.
return_car(LocRef, CarRef) -> ok | {error, full}

% Get information about the status in the rental location. Note that 
% the return value is a property list and the order of the elements is not specified.
get_info(LocRef) -> {ok, [{spaces, Spaces}, {occupied,Occupied}, {free, Free}]}

