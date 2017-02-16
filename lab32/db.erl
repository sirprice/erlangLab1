-module (db).
-export ([new/0]).
-export ([destroy/1]).
-export ([write/3]).
-export ([read/2]).
-export ([match/2]).
-export ([delete/2]).
-export ([visitAll/1]).
-export ([makeFlat/1]).
-export ([moveValue/3]).

%%% @author Magnus Lundmark, malundm@kth.se

% this will walk the tree until it matches Key or reach empty leaf
% the last node will be passed into Func ( a fun) , and a tree is returned
walkTree(_Key, empty, Func ) -> Func(empty);
walkTree(Key, {Key, V , Left, Right}, Func) -> Func({Key, V , Left, Right});
walkTree(Key, {NodeKey, V , Left, Right}, Func) when Key < NodeKey ->
    {NodeKey, V, walkTree(Key, Left, Func),Right};
walkTree(Key, {NodeKey, V , Left, Right},Func) when Key > NodeKey ->
    {NodeKey, V, Left, walkTree(Key, Right, Func)}.


% Write a new key to the db , and remove the tuple with Key if its already there
write(Key,Data,Db) -> 
    % a function to do the insertion when the key is found or empty
    Insert = fun (Node) ->
                case Node of
                    empty -> {Key,Data,empty,empty};
                    {_, _ , Left, Right} -> {Key,Data,Left,Right}
                end
            end,
    % walk the tree until Key or empty spot, and insert new data, returning a new treee
    walkTree(Key,Db,Insert).


% skipNode skipes this node and returns a tuple {Branch,SkipedBranch}
% It assumes that we want to go right always, the skiped branch is empty or LeftBranch
skipNode({_, _ , Left, empty}) ->{Left,empty};
skipNode({_, _ , empty, Right}) -> {Right,empty};
skipNode({_, _ , Left, Right}) -> {Left,Right}.

% removeNode Removes node with Key and return the new tree
removeNode(Key,Db) ->
    % The logic to remove a Node and repair the hole it left behined
    Remove = fun (Node) ->
                case Node of
                    empty -> empty;
                    {_, _ , _Left, _Right} -> 
                    	{Branch,SkipedBranch} = skipNode(Node),
                    	walkTree(Key,Branch, fun ( _ ) -> SkipedBranch end)
                end
            end,
    walkTree(Key,Db,Remove).

% delete this key if it exist in db, 
% returns the new Db
delete(Key, Db) -> removeNode(Key,Db). 

% delete this key if it exist in db, 
% returns the new Db
moveValue(FromKey, ToKey, Db) -> Db = 
    Values = db:read(FromKey,Db),
    % io:format("server_get_cars = ~w~n  \n", [Values]),
    case Values of 
        {error,instance} -> Db;
        {ok,Data} -> 
            removeNode(FromKey,Db),
            write(ToKey,Data,Db)
    end.
% creates a new tree, root node
new() -> {root, empty, empty, empty}.
destroy(_) -> ok.

% finds the key, and returns tuple of {ok,Value}, else {error,instance}
find(Key,{Key, Value , _, _}) -> {ok, Value};
find(Key,{NodeKey, _ , LeftBranch, _ }) when Key < NodeKey -> 
    find(Key,LeftBranch);
find(Key,{NodeKey, _ , _ , RightBranch}) when Key > NodeKey -> 
    find(Key,RightBranch);
find(_Key,empty) -> {error,instance}.

read(Key,Db) -> find(Key,Db).

% visitAll print the entire tree as an array
visitAll(empty) -> empty;
visitAll({_Key, Value , empty, empty}) -> Value;
visitAll({_Key, Value , Left, empty}) -> [Value | visitAll(Left)];
visitAll({_Key, Value , empty, Right}) -> [Value | visitAll(Right)];
visitAll({_Key, Value , Left, Right}) -> [Value, visitAll(Left),visitAll(Right)] .


% this takes a nested array and turnes it into a flat array
makeFlat([ H | []]) ->  makeFlat(H);
makeFlat([ [H] | [T]]) ->  makeFlat(H) ++ makeFlat(T);
makeFlat([ H | [T]]) ->  makeFlat(H) ++ makeFlat(T);
makeFlat([ [H] | T]) ->  makeFlat(H) ++ makeFlat(T);
makeFlat([ H | T]) ->  makeFlat(H) ++ makeFlat(T);
makeFlat(V) ->  [V].

% match Search the entire tree for A value/Element, 
% returns an array of all keys with this Value/Element
match(Element,  {Key, Element , Left, Right}) -> 
    L = match(Element,Left),
    R = match(Element,Right),
    T = [Key] ++ L,
    T ++ R;
match(Element,  {_K, _V , Left, Right}) -> 
    L = match(Element,Left),    % Left branch
    R = match(Element,Right),   % right Branch
    L ++ R; % merge them
match(_Element, empty) -> []. % empty case
