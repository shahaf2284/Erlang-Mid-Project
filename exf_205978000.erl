%%%-----------------------------------------------------------------------------------------------------------------------
%%% Exm
%%% @The creator of the project: Shahaf Zohar
%%%                              205978000
%%% Created : 15. Apr 2023 10:00 PM
%%%-----------------------------------------------------------------------------------------------------------------------
-module(exf_205978000).
-record(node,{elem,left,right}).
-record(recstruct,{val,sublist}).
-export([exp_to_bdd/3,solve_bdd/2,listOfLeaves/1,reverseIteration/1]).
-import(timer,[now_diff/2]).
-import(lists,[min/1,nth/2]).

%--------------------------------------------------------------------------------------------------------------------------
exp_to_bdd(BoolFunc, Ordering, DataStructureType)->
  %• This function first arranges the information in a 
  %• list and then according to the selection prints the tree.
  Tstart = erlang:now(),                            % Start the timer
  Lists = permotations(listmaker(BoolFunc)),        % Arreng the to list
  ListT = case DataStructureType of
                    record -> [reducingtRec(recStruct(L,[],BoolFunc)) || L <-Lists ];  %case by coohes record
                    map -> [reducingMap(mapStruct(L,[],BoolFunc)) || L <-Lists ]      %case by coohes map
                end,

  %Creating a tree according to the user's request
  Result = case Ordering of
                   tree_height ->  [tree_high(Tree) || Tree <- ListT];
                   num_of_nodes -> [number_of_nodes(Tree)|| Tree <- ListT];
                   num_of_leafs -> [number_of_leafs(Tree)|| Tree <- ListT]
                 end,
    Tend=erlang:now(),      % Stop the timer
  io:format("--------------------------------------------------------------- ~n"),  
  case Ordering of
                 tree_height ->  io:format("the height of the tree is ~p.~n", [lists:nth(1,Result)]);
                 num_of_nodes -> io:format("The number of nodes is ~p.~n", [lists:nth(1,Result)]);
                 num_of_leafs -> io:format("The number of leaves in the tree is ~p.~n", [lists:nth(1,Result)])
               end,
  io:format("runtime = ~p  microseconds~n", [timer:now_diff(Tend,Tstart)]),      %Print and calculate the time
  lists:nth(find_index(lists:min(Result),Result),ListT).                         %The function takes two arguments: the index of the element  

%--------------------------------------------------------------------------------------------------------------------------
%Function that return all permotations of List.
permotations([])-> [[]];  
permotations(L)->[[H|T] || H<-L,T<-permotations(L--[H])].

%--------------------------------------------------------------------------------------------------------------------------
recStruct([H|T],List,BoolFunc)->  %A function that create a record type tree
  if
    T =:= [] -> #node{elem = H,
                        left = rearrang(BoolFunc,List ++ [{H,false}]),
                        right = rearrang(BoolFunc,List ++ [{H,true}])};
    true -> #node{elem = H,
                        left = recStruct(T,List ++ [{H,false}],BoolFunc),
                        right = recStruct(T,List ++ [{H,true}],BoolFunc)}
  end.

mapStruct([H|T],List,BoolFunc)->  %A function that create a record type tree
  if
    T =:= [] -> #{elem => H,
                        left => rearrang(BoolFunc,List ++ [{H,false}]),
                        right => rearrang(BoolFunc,List ++ [{H,true}])};

    true -> #{elem => H,
                        left => mapStruct(T,List ++ [{H,false}],BoolFunc),
                        right => mapStruct(T,List ++ [{H,true}],BoolFunc)}
  end.

%--------------------------------------------------------------------------------------------------------------------------
%• Reducing the tree for record choosen
reducingtRec(Tree)->                                    
  #node{elem = Elem, left = LT, right = RT} = Tree,       %Tuple maching
  if
    (is_record(LT,node) =:= false) and (is_record(RT,node) =:= false) ->
                if                      
                    LT =:= RT -> RT;
                    true -> Tree        %Stoping the recursive 
                end;
    true -> {LeftT,RightT} = {reducingtRec(LT),reducingtRec(RT)},
                if
                    (is_record(LeftT,node) =:= false) and (is_record(RightT,node) =:= false) and (LeftT =:= RightT) -> RightT;
                    true -> #node{elem = Elem,left = reducingtRec(LT),right = reducingtRec(RT)}
                end
  end.

%--------------------------------------------------------------------------------------------------------------------------
%• Reducing the tree for map choosen
reducingMap(Tree)->                                         
  #{elem := Elem,left := LT,right := RT} = Tree,            %Tuple maching
  if
    (is_map(LT) =:= false) and (is_map(RT) =:= false) ->
      if                              
        LT =:= RT -> RT;
        true -> Tree            %Stoping the recursive 
      end;
    true -> {LeftT,RightT} = {reducingMap(LT), reducingMap(RT)},            %Tuple maching
      if
        (is_map(LeftT) =:= false) and (is_map(RightT) =:= false) and (LeftT =:= RightT) -> RightT;
        true -> #{elem => Elem,left => reducingMap(LT),right => reducingMap(RT)}
      end

  end.

%--------------------------------------------------------------------------------------------------------------------------
%• Here we count the number of nodes in the tree
%• Counting and every time we split bitween right and left untill we got no mach at all 
number_of_nodes(Tree) ->
  if
    is_record(Tree,node) -> #node{elem = _elem,left = LT,right = RT} = Tree,    %Calculation of all the nodes 
                            1 + number_of_nodes(LT) + number_of_nodes(RT);      %in the graph each level add 1
    is_map(Tree) -> #{left := LT,right := RT} = Tree,
                            1 + number_of_nodes(LT) + number_of_nodes(RT);
    true -> 1               %stop recursive and return 1
  end.

%--------------------------------------------------------------------------------------------------------------------------
%• Count the number of leafs Tree
%• Counting and every time we split bitween right and left untill we got no mach at all
number_of_leafs(Tree) ->  
  if
    is_record(Tree,node) -> #node{elem = _Elem,left = LT,right = RT} = Tree,
                                number_of_leafs(LT) + number_of_leafs(RT);
    is_map(Tree) -> #{left := LT,right := RT} = Tree,
                                number_of_leafs(LT) + number_of_leafs(RT);
    true -> 1               %stop recursive and return 1
  end.

%--------------------------------------------------------------------------------------------------------------------------
%• Function that count the number of leafs in Tree
tree_high(Tree) ->  
  if
    is_record(Tree,node) -> #node{elem = _Elem,left = LT,right = RT} = Tree,
                                erlang:max(1 + tree_high(LT),1 + tree_high(RT));
    is_map(Tree) -> #{left := LT,right := RT} = Tree,
                                erlang:max(1 + tree_high(LT),1 + tree_high(RT));
    true -> 1
  end.

%--------------------------------------------------------------------------------------------------------------------------
%• The function receives a BDD tree and a list of values for every Boolean variable that’s used in the
%• Boolean function and returns the result of that function, according to the given BDD tree.
%• A function that, depending on the type of tree, returns the result
solve_bdd(T, Arg) ->  
  Tstart=erlang:now(),
  Res = if
          is_map(T) -> solve_Map(T, Arg);
          is_record(T,node) -> solve_bddRec(T,Arg);
          true -> T
        end,
  Tend=erlang:now(),
  io:format("--------------------------------------------------------- ~n"),  
  io:format("Run time: ~p  microseconds.~n", [timer:now_diff(Tend,Tstart)]),
 	if
    		Res =:= true ->
              		 io:format("The Result: true.~n");
    		Res =:= false ->
    			 io:format("The Result: false.~n")
   	end.
  
%--------------------------------------------------------------------------------------------------------------------------
%• Function that get map Tree and reaches a leaf in the required tree depending on the input and returns it
solve_Map(Tree, Arg) -> 
  #{elem := Elem,left := LT ,right := RT} = Tree,
  ListBool = convertNumToBool(Arg),
  {Element,Value} = find(ListBool,Elem),
  NewList = ListBool -- [{Element,Value}],
  if
    Value =:= true ->
      if
        is_map(RT) -> solve_Map(RT,NewList);
        true -> RT
      end;
    true ->
      if
        is_map(LT) -> solve_Map(LT,NewList);
        true -> LT
      end
  end.

%--------------------------------------------------------------------------------------------------------------------------
%• Function that get record Tree and reaches a leaf in the required tree depending on the input and returns it
solve_bddRec(Tree, Arg) -> 
    ListBool = convertNumToBool(Arg),
    #node{elem = Elem,left = LT,right = RT} = Tree,
    {Element,Value} = find(ListBool,Elem),
    NewList = ListBool -- [{Element,Value}],
    if
        Value =:= true ->
        if
            is_record(RT,node) -> solve_bddRec(RT,NewList);
            true -> RT
        end;
        true ->
        if
            is_record(LT,node) ->solve_bddRec(LT,NewList);
            true -> LT
        end
    end.
%--------------------------------------------------------------------------------------------------------------------------
%Translation to true and false
%Input of the function [{x1,0},{x2,1}....] and convert it to [[x1,false},{x2,true}....]
convertNumToBool([]) -> [];           %stop the recursive
convertNumToBool([H|T]) -> 
    {Element,Number} = H,
    if
        (Number =:= 0) -> 
                [{Element,false}|convertNumToBool(T)];
        (Number =:= 1) -> 
                [{Element,true}|convertNumToBool(T)];
        true -> 
                [{Element,Number}|convertNumToBool(T)]
    end.

%--------------------------------------------------------------------------------------------------------------------------
%• Returns list of pointers to leaves.
listOfLeaves(BddTree) ->  %A function that, depending on the type of tree, returns the list of leaves
  if
      is_record(BddTree,node)-> leavesRecord([],BddTree) ;
      true -> leavesMap([],BddTree)
  end.
%--------------------------------------------------------------------------------------------------------------------------
%• Get map Tree and return list of leaves
leavesMap(Acc,BddTree) ->  
  if
    is_map(BddTree) ->
          #{elem := Elem,left := LT,right := RT} = BddTree,
          leavesMap([Elem] ++ Acc,LT) ++ leavesMap([Elem] ++ Acc,RT);
    true ->  [#{val => BddTree,list => Acc}]
  end.

%--------------------------------------------------------------------------------------------------------------------------
%• Get record BddTree and return list of leaves
leavesRecord(Acc,BddTree) ->  
  if
    is_record(BddTree,node) ->
            #node{elem = Elem,left = LT,right = RT} = BddTree,
            leavesRecord([Elem] ++ Acc,LT) ++ leavesRecord([Elem] ++ Acc,RT);

    true ->  [#recstruct{val = BddTree,sublist = Acc}]
  end.

%--------------------------------------------------------------------------------------------------------------------------
%• Input is pointer to leaf (one of leaves given in result list of step 3).
%• Returns list of nodes on shortest path to the root.
%• In my word:"The function get pointer to leaf and return list of variable that represnt the rout to the route."
reverseIteration(LeafPtr) ->  
  if                % Checking the the type 
    is_map(LeafPtr) ->
              #{list := List} = LeafPtr,            %Creating map to do mach between the input and return the list node like 
              List;
    is_record(LeafPtr,recstruct) ->                % Checking if the stract is "recpointer"
              #recstruct{val = _Val,sublist = List} = LeafPtr,   %The same thing like map case, but now return from record "recstruct"
              List;
    true -> true    
  end.

%--------------------------------------------------------------------------------------------------------------------------
rearrang({Op,Num},List)->
  case Op of
    'not' -> if       
               is_tuple(Num) -> not rearrang(Num,List) ;      % case 'not' in the work explanation, the template will be as follows: 
               true ->  not element_Val(List,Num)             %{'not',{}} or like this {'not', a1}
             end;
    'or'->{Arg1,Arg2} = Num,
          if
            (is_tuple(Arg1) =:= false) and (is_tuple(Arg2) =:= false) ->     % If the two argument is value and the operator is 'or'
                                        element_Val(List,Arg1) or element_Val(List,Arg2);
            (is_tuple(Arg1) =:= false) and (is_tuple(Arg2) =:= true) ->      % If the two Tuple so we need to keep call the rearrang
                                        element_Val(List,Arg1) or rearrang(Arg2,List);
            (is_tuple(Arg1) =:= true) and (is_tuple(Arg2) =:= false) ->      % If one is Tuple and one value 
                                        rearrang(Arg1,List) or element_Val(List,Arg2);
            (is_tuple(Arg1) =:= true) and (is_tuple(Arg2) =:= true) ->        % If one is Tuple and one value 
                                        rearrang(Arg1,List) or rearrang(Arg2,List)
          end;

    'and' -> {Arg1,Arg2} = Num,       % the same explain but about operator 'and'
          if
            (is_tuple(Arg1) =:= false) and (is_tuple(Arg2) =:= false) -> 
                                        element_Val(List,Arg1) and element_Val(List,Arg2);
            (is_tuple(Arg1) =:= false) and (is_tuple(Arg2) =:= true) ->  
                                        element_Val(List,Arg1) and rearrang(Arg2,List);
            (is_tuple(Arg1) =:= true) and (is_tuple(Arg2) =:= false) ->  
                                        rearrang(Arg1,List) and element_Val(List,Arg2);
            (is_tuple(Arg1) =:= true) and (is_tuple(Arg2) =:= true) -> 
                                        rearrang(Arg1,List) and rearrang(Arg2,List)
          end
  end.

%-------------------------------------------------------------------------------------------------------------------------------
%  input Boolian and return list of all variables in the list
listmaker(BoolFunc) -> remove_duplicates(converted(BoolFunc)).  % example: [a1,a2,a3,a4,a5,....]
converted({_Op,Arg})->                                  % the output dosnt metter we want just the argument     
  if
    is_tuple(Arg) -> 
        % We have four options to decompose the array into a list of arguments 
        {Arg1,Arg2} =  Arg,                       
        if
          ((is_tuple(Arg1) =:= false) and (is_tuple(Arg2) =:= true)) ->  [Arg1] ++ converted(Arg2);
          ((is_tuple(Arg1) =:= false) and (is_tuple(Arg2) =:= false)) -> [Arg1,Arg2];
          ((is_tuple(Arg1) =:= true) and (is_tuple(Arg2) =:= false)) -> converted(Arg1) ++ [Arg2];
          ((is_tuple(Arg1) =:= true) and (is_tuple(Arg2) =:= true))  ->  converted(Arg1) ++ converted(Arg2)
      end;
      true -> [Arg]       
  end.
%-------------------------------------------------------------------------------------------------------------------------------
% Function that separates the value from the element and return the value of the element
% the input :[{Elem1,val1},{Elem2,val2},{Elem3,val3},.....]
element_Val([],_Value) -> io:fwrite("Arument key that not in list");
element_Val([{_Element,Value}|_T],_Element) -> Value;                
element_Val([{__Elem,_Value}|T],Element) -> element_Val(T,Element).

%-------------------------------------------------------------------------------------------------------------------------------
%remove all the duplicate element
remove_duplicates(List) -> 
            remove_duplicates(List,[]).

remove_duplicates([],Acc) -> 
            lists:reverse(Acc);
remove_duplicates([H|T],Acc) -> 
            case lists:member(H,Acc) of
                true -> remove_duplicates(T,Acc);
                false -> remove_duplicates(T,[H|Acc])
            end.

%another way to solve this: i found this in chatGPT and it's working also 
%removeDup([]) -> [];    
%removeDup([H|T]) -> [H |[X || X <- removeDup(T),X =/= H]].

%--------------------------------------------------------------------------------------------------------------------------
% Function that find element in list ans return his index.
%find_index(Elem,List)->
%    case lists:member(Elem,List) of
%        true -> lists:nth(1+length(lists:sublist(List,1, lists:member(Elem,List)-1)),List);
%        false -> notfound
%    end.

find_index(_, [], _) ->
    not_found;
find_index(Element, [Element|_], Index) ->
    Index;
find_index(Element, [_|T], Index) ->
    find_index(Element, T, Index+1).

find_index(Element, List) ->
    find_index(Element, List, 1).
%another two way to solve this: i found this in chatGPT and it's working also
% Function that find element in list ans return his index.
%find_index(Elem,List)->
%    case lists:member(Elem,List) of
%        true -> lists:nth(1+length(lists:sublist(List,1, lists:member(Elem,List)-1)),List);
%        false -> notfound
%    end.

%find_index(Item, List) -> find_index(Item, List, 1). 
%find_index(_, [], _)  -> notfound;
%find_index(Item, [Item|_], Index) -> Index;
%find_index(Item, [_|Tl], Index) -> find_index(Item, Tl, Index+1).

%--------------------------------------------------------------------------------------------------------------------------
%Finding the pair with the invented element
find([],_Val) -> io:fwrite("Arument key that not in list");     %Function that find element in list of tuples of couples
find([{X,Y}|_T],X)->{X,Y};                                      %Return all tuple that exist in the input 
find([_H|T],Elem) ->find(T,Elem).
