%%% @doc Generic Zipper Implementation.
%%%      Zippers let you traverse immutable data structures with ease and
%%%      flexibility.
%%% @end
-module(zipper).
-compile({no_auto_import, [node/1]}).

-export([
         %% Creation
         new/4,
         %% Traverse
         up/1,
         down/1,
         left/1,
         leftmost/1,
         right/1,
         rightmost/1,
         next/1,
         is_end/1,
         prev/1,
         root/1,
         traverse/2,
         %% Editing
         insert_left/2,
         insert_right/2,
         replace/2,
         edit/3,
         insert_child/2,
         append_child/2,
         remove/1,
         %% Iteration
         map/2,
         fmap/3,
         filter/2,
         fold/3,
         size/1,
         %% Info
         node/1,
         children/1,
         is_branch/1
        ]).

-type is_branch_fun(T) :: fun((T) -> boolean()).
-type make_node_fun(T) :: fun((T, [T]) -> T).
-type children_fun(T)  :: fun((T) -> [T]).
-type info(T) :: #{ lefts       => [T]
                  , rights      => [T]
                  , parent_node => undefined | T
                  , parent_info => undefined | info(T)
                  , is_modified => boolean()
                  }.

-opaque zipper(T) ::
        #{spec => #{is_branch => is_branch_fun(T),
                    make_node => make_node_fun(T),
                    children  => children_fun(T)},
          node      => T,
          info      => 'end' | info(T)
         }.

-type operation() ::
    next | prev | up | down | left | right | root | node | rightmost | leftmost.

-export_type([zipper/1]).
-export_type([info/1, is_branch_fun/1, make_node_fun/1, children_fun/1]).
-export_type([operation/0]).

%% @doc Builds a new zipper with nodes of type T.
-spec new(is_branch_fun(T), children_fun(T), make_node_fun(T), T) -> zipper(T).
new(IsBranch, Children, MakeNode, Root) ->
    #{spec => #{is_branch => IsBranch,
                children => Children,
                make_node => MakeNode
               },
      node => Root,
      info => #{lefts => [],
                rights => [],
                parent_node => undefined,
                parent_info => undefined
               }
     }.

%% @doc Returns the zipper in the parent node, if possible.
-spec up(zipper(T)) -> zipper(T) | undefined.
up(#{info := #{parent_node := undefined}}) ->
    undefined;
up(Zipper = #{spec := #{make_node := MakeNode},
              node := Node,
              info := #{lefts := Lefts,
                        rights := Rights,
                        parent_node := ParentNode,
                        parent_info := ParentInfo,
                        is_modified := true}}) ->
    Children = lists:reverse(Lefts) ++ [Node | Rights],
    NewParentNode = MakeNode(ParentNode, Children),
    Zipper#{node => NewParentNode,
            info => ParentInfo};
up(Zipper = #{info := #{parent_node := Parent,
                        parent_info := ParentInfo}}) ->
    Zipper#{node => Parent,
            info => ParentInfo}.

%% @doc Returns the zipper in the first child, if any.
-spec down(zipper(T)) -> zipper(T) | undefined.
down(Zipper = #{node := Node,
                info := Info,
                spec := #{children := Children}}) ->
    case is_branch(Zipper) of
        true ->
            [NewNode | Rights] = Children(Node),
            Zipper#{node => NewNode,
                    info => #{lefts => [],
                              rights => Rights,
                              parent_node => Node,
                              parent_info => Info}
                   };
        false ->
            undefined
    end.

%% @doc Returns the zipper on the left, if any.
-spec left(zipper(T)) -> zipper(T) | undefined.
left(#{info := #{lefts := []}}) ->
    undefined;
left(Zipper = #{info := Info = #{lefts := [NewNode | Lefts],
                                 rights := Rights},
                node := Node}) ->
    Zipper#{info => Info#{lefts => Lefts,
                          rights => [Node | Rights]},
            node => NewNode
           }.

%% @doc Returns the leftmost zipper in the current zipper.
-spec leftmost(zipper(T)) -> zipper(T).
leftmost(Zipper = #{info := #{lefts := []}}) ->
    Zipper;
leftmost(Zipper = #{info := Info = #{lefts := Lefts,
                                     rights := Rights},
                    node := Node}) ->
    Fun = fun(Item, Acc) -> [Item | Acc] end,
    [NewNode | NewRights] = lists:foldl(Fun,  [Node | Rights], Lefts),
    Zipper#{info => Info#{lefts => [],
                          rights => NewRights},
            node => NewNode}.

%% @doc Returns the zipper on the right, if any.
-spec right(zipper(T)) -> zipper(T) | undefined.
right(#{info := #{rights := []}}) ->
    undefined;
right(Zipper = #{info := Info = #{rights := [NewNode | Rights],
                                  lefts := Lefts},
                 node := Node}) ->
    Zipper#{info => Info#{rights := Rights,
                          lefts := [Node | Lefts]},
            node := NewNode}.

%% @doc Returns the rightmost zipper in the current zipper.
-spec rightmost(zipper(T)) -> zipper(T).
rightmost(Zipper = #{info := Info = #{rights := Rights,
                                      lefts := Lefts},
                     node := Node}) ->
    Fun = fun(Item, Acc) -> [Item | Acc] end,
    [NewNode | NewLefts] = lists:foldl(Fun,  [Node | Lefts], Rights),
    Zipper#{info => Info#{lefts => NewLefts,
                          rights => []},
            node => NewNode}.

%% @doc Returns the next zipper.
-spec next(zipper(T)) -> zipper(T).
next(Zipper = #{info := 'end'}) ->
    Zipper;
next(Zipper) ->
    case {is_branch(Zipper), right(Zipper)} of
        {true, _} -> down(Zipper);
        {false, undefined} -> next_recur(Zipper);
        {false, Right} -> Right
    end.

-spec next_recur(zipper(T)) -> zipper(T).
next_recur(Zipper) ->
    case up(Zipper) of
        undefined -> Zipper#{info => 'end'};
        UpZipper ->
            case right(UpZipper) of
                undefined -> next_recur(UpZipper);
                Next -> Next
            end
    end.

%% @doc Is it the end of the zipper traversal.
-spec is_end(zipper(_)) -> boolean().
is_end(#{info := 'end'}) ->
    true;
is_end(_Zipper) ->
    false.

%% @doc Returns the previous zipper.
-spec prev(zipper(T)) -> zipper(T) | undefined.
prev(Zipper = #{info := #{lefts := []}}) ->
    up(Zipper);
prev(Zipper) ->
    prev_recur(left(Zipper)).

-spec prev_recur(zipper(T)) -> zipper(T).
prev_recur(Zipper) ->
    case down(Zipper) of
        undefined -> Zipper;
        DownZipper ->
            RightMost = rightmost(DownZipper),
            prev_recur(RightMost)
    end.

%% @doc Returns the node on the root of the zipper.
-spec root(zipper(T)) -> T.
root(Zipper) ->
    case up(Zipper) of
        undefined -> node(Zipper);
        Parent -> root(Parent)
    end.

%% @doc Traverses the zipper following the given list of operations.
%%      If, at some point, an operation is invalid, it will crash.
-spec traverse([operation()], zipper(T)) -> zipper(T) | T | undefined.
traverse([], Zipper) ->
    Zipper;
traverse([Op | Rest], Zipper) ->
    traverse(Rest, zipper:Op(Zipper)).

%% @doc Inserts a node to the left of the current one.
-spec insert_left(T, zipper(T)) -> zipper(T).
insert_left(_, #{info := #{parent_node := undefined}}) ->
    throw(insert_at_top);
insert_left(Node, Zipper = #{info := Info = #{lefts := Lefts}}) ->
    NewInfo = Info#{lefts => [Node | Lefts],
                    is_modified => true},
    Zipper#{info => NewInfo}.

%% @doc Inserts a node to the right of the current one.
-spec insert_right(T, zipper(T)) -> zipper(T).
insert_right(_, #{info := #{parent_node := undefined}}) ->
    throw(insert_at_top);
insert_right(Node, Zipper = #{info := Info = #{rights := Rights}}) ->
    NewInfo = Info#{rights => [Node | Rights],
                    is_modified => true},
    Zipper#{info => NewInfo}.

%% @doc Replaces the current node.
-spec replace(T, zipper(T)) -> zipper(T).
replace(Node, Zipper = #{info := Info}) ->
    Zipper#{node => Node,
            info => Info#{is_modified => true}}.

%% @doc Edits the current node by applying the given function.
%%      The parameters of said function will be [Node | Args].
-spec edit(fun((...) -> T), [term()], zipper(T)) -> zipper(T).
edit(Fun, Args, Zipper = #{node := Node, info := Info}) ->
    NewNode = erlang:apply(Fun, [Node | Args]),
    Zipper#{node => NewNode,
            info => Info#{is_modified => true}}.

%% @doc Adds a node as the leftmost child of the current one.
-spec insert_child(T, zipper(T)) -> zipper(T).
insert_child(Child, Zipper = #{spec := #{make_node := MakeNode}}) ->
    Node = node(Zipper),
    Children  = children(Zipper),
    NewNode = MakeNode(Node, [Child | Children]),
    replace(NewNode, Zipper).

%% @doc Adds a node as the rightmost child of the current one.
-spec append_child(T, zipper(T)) -> zipper(T).
append_child(Child, Zipper = #{spec := #{make_node := MakeNode}}) ->
    Node = node(Zipper),
    Children  = children(Zipper),
    NewNode = MakeNode(Node, Children ++ [Child]),
    replace(NewNode, Zipper).

%% @doc Removes current node from zipper.
%%      Moves down, if possible. If not, it moves to the rightmost node.
-spec remove(zipper(T)) -> zipper(T).
remove(#{info := #{parent_node := undefined}}) ->
    throw(remove_at_top);
remove(#{spec := Spec = #{make_node := MakeNode},
         info := #{lefts := [],
                   rights := Rights,
                   parent_node := ParentNode,
                   parent_info := ParentInfo}}) ->
    NewParentNode = MakeNode(ParentNode, Rights),
    #{spec => Spec,
      node => NewParentNode,
      info => ParentInfo#{is_modified => true}};
remove(#{spec := Spec,
         info := Info  = #{lefts := [Node | Lefts]}}) ->
    NewZipper = #{spec => Spec,
                  node => Node,
                  info => Info#{lefts => Lefts,
                                is_modified => true}},
    prev_removed(NewZipper).

prev_removed(Zipper) ->
    case down(Zipper) of
        undefined -> Zipper;
        Child -> prev_removed(rightmost(Child))
    end.

%% Iteration

%% @doc Applies a function to all nodes of the zipper.
%%      Returns a list with the results.
-spec map(fun((T) -> U), zipper(T)) -> [U].
map(Fun, Zipper) ->
    ApplyAddFun = fun(X, Acc) -> [Fun(X) | Acc] end,
    Result = fold(ApplyAddFun, [], Zipper),
    lists:reverse(Result).

%% @doc Returns the root of the tree, where the value of each node
%%      (after the current location of Zipper) is replaced with the
%%      result from appling Fun to the node as the first argument
%%      and Args as additional arguments.
%% @end
-spec fmap(fun((...) -> T), [term()], zipper(T)) -> T.
fmap(Fun, Args, Zipper) ->
    NewZipper = edit(Fun, Args, Zipper),
    NextZipper = next(NewZipper),
    case is_end(NextZipper) of
        true ->
            root(NewZipper);
        false ->
            fmap(Fun, Args, NextZipper)
    end.

%% @doc Applies Fun recursively on the zipper.
%%      The arguments of Fun will be (Node, Acc) where Acc is the result of the
%%      previous call or the initial value provided.
-spec fold(fun((T, A) -> A), A, zipper(T)) -> A.
fold(Fun, Acc, Zipper) ->
    case is_end(Zipper) of
        true ->
            Acc;
        false ->
            Node = node(Zipper),
            NewAcc = Fun(Node, Acc),
            fold(Fun, NewAcc, next(Zipper))
    end.

%% @doc Returns a list of all the nodes in the zipper that match a predicate.
-spec filter(fun((T) -> boolean()), zipper(T)) -> [T].
filter(Pred, Zipper) ->
    FilterFun = fun(X, Acc) ->
                        case Pred(X) of
                            true -> [X | Acc];
                            false -> Acc
                        end
                end,
    fold(FilterFun, [], Zipper).

%% @doc Returns the size of the zipper.
-spec size(zipper(_)) -> non_neg_integer().
size(Zipper) ->
    IncFun = fun(_, Acc) -> Acc + 1 end,
    fold(IncFun, 0, Zipper).

%% Info

%% @doc Returns the value of the current node in the zipper.
-spec node(zipper(T)) -> T.
node(#{node := Node}) ->
    Node.

%% @doc Returns the list of children zippers.
-spec children(zipper(T)) -> [zipper(T)].
children(Zipper = #{spec := #{children := Children}, node := Node}) ->
    case is_branch(Zipper) of
        true -> Children(Node);
        false -> throw(children_on_leaf)
    end.

%% @doc Is this node a branch?
-spec is_branch(zipper(_)) -> boolean().
is_branch(#{spec := #{is_branch := IsBranch}, node := Node}) ->
    IsBranch(Node).
