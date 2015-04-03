-module(zipper).

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
         %% Info
         node/1,
         children/1,
         is_branch/1
        ]).

-type zipper() ::
        #{is_branch => fun(),
          make_node => fun(),
          children => fun()}.

-type operation() :: next | prev | up | down | left | right | root.

-spec new(fun(), fun(), fun(), term()) -> zipper().
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

-spec up(zipper()) -> zipper() | undefined.
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

-spec down(zipper()) -> zipper() | undefined.
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

-spec left(zipper()) -> zipper().
left(#{info := #{lefts := []}}) ->
    undefined;
left(Zipper = #{info := Info = #{lefts := [NewNode | Lefts],
                                 rights := Rights},
                node := Node}) ->
    Zipper#{info => Info#{lefts => Lefts,
                          rights => [Node | Rights]},
            node => NewNode
           }.

-spec leftmost(zipper()) -> zipper().
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

-spec right(zipper()) -> zipper().
right(#{info := #{rights := []}}) ->
    undefined;
right(Zipper = #{info := Info = #{rights := [NewNode | Rights],
                                  lefts := Lefts},
                 node := Node}) ->
    Zipper#{info => Info#{rights := Rights,
                          lefts := [Node | Lefts]},
            node := NewNode}.

-spec rightmost(zipper()) -> zipper().
rightmost(Zipper = #{info := Info = #{rights := Rights,
                                      lefts := Lefts},
                     node := Node}) ->
    Fun = fun(Item, Acc) -> [Item | Acc] end,
    [NewNode | NewLefts] = lists:foldl(Fun,  [Node | Lefts], Rights),
    Zipper#{info => Info#{lefts => NewLefts,
                          rights => []},
            node => NewNode}.

-spec next(zipper()) -> zipper().
next(Zipper = #{info := 'end'}) ->
    Zipper;
next(Zipper) ->
    case {is_branch(Zipper), right(Zipper)} of
        {true, _} -> down(Zipper);
        {false, undefined} -> next_recur(Zipper);
        {false, Right} -> Right
    end.

-spec next_recur(zipper()) -> zipper().
next_recur(Zipper) ->
    case up(Zipper) of
        undefined -> Zipper#{info => 'end'};
        UpZipper ->
            case right(UpZipper) of
                undefined -> next_recur(UpZipper);
                Next -> Next
            end
    end.

-spec is_end(zipper()) -> boolean().
is_end(#{info := 'end'}) ->
    true;
is_end(_Zipper) ->
    false.

-spec prev(zipper()) -> zipper().
prev(Zipper = #{info := #{lefts := []}}) ->
    up(Zipper);
prev(Zipper) ->
    prev_recur(left(Zipper)).

prev_recur(Zipper) ->
    case down(Zipper) of
        undefined -> Zipper;
        DownZipper ->
            RightMost = rightmost(DownZipper),
            prev_recur(RightMost)
    end.

-spec root(zipper()) -> zipper().
root(Zipper) ->
    case up(Zipper) of
        undefined -> zipper:node(Zipper);
        Parent -> root(Parent)
    end.

-spec traverse([operation()], zipper()) -> zipper().
traverse([], Zipper) ->
    Zipper;
traverse([Op | Rest], Zipper) ->
    traverse(Rest, zipper:Op(Zipper)).

-spec insert_left(term(), zipper()) -> zipper().
insert_left(_, #{info := #{parent_node := undefined}}) ->
    throw(insert_at_top);
insert_left(Node, Zipper = #{info := Info = #{lefts := Lefts}}) ->
    NewInfo = Info#{lefts => [Node | Lefts],
                    is_modified => true},
    Zipper#{info => NewInfo}.

-spec insert_right(term(), zipper()) -> zipper().
insert_right(_, #{info := #{parent_node := undefined}}) ->
    throw(insert_at_top);
insert_right(Node, Zipper = #{info := Info = #{rights := Rights}}) ->
    NewInfo = Info#{rights => [Node | Rights],
                    is_modified => true},
    Zipper#{info => NewInfo}.

-spec replace(term(), zipper()) -> zipper().
replace(Node, Zipper = #{info := Info}) ->
    Zipper#{node => Node,
            info => Info#{is_modified => true}}.

-spec edit(fun(), list(), zipper()) -> zipper().
edit(Fun, Args, Zipper = #{node := Node, info := Info}) ->
    NewNode = erlang:apply(Fun, [Node | Args]),
    Zipper#{node => NewNode,
            info => Info#{is_modified => true}}.

-spec insert_child(term(), zipper()) -> zipper().
insert_child(Child, Zipper = #{spec := #{make_node := MakeNode}}) ->
    Node = zipper:node(Zipper),
    Children  = zipper:children(Zipper),
    NewNode = MakeNode(Node, [Child | Children]),
    replace(NewNode, Zipper).

-spec append_child(term(), zipper()) -> zipper().
append_child(Child, Zipper = #{spec := #{make_node := MakeNode}}) ->
    Node = zipper:node(Zipper),
    Children  = zipper:children(Zipper),
    NewNode = MakeNode(Node, Children ++ [Child]),
    replace(NewNode, Zipper).

-spec remove(zipper()) -> zipper().
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

-spec node(zipper()) -> zipper().
node(#{node := Node}) ->
    Node.

-spec children(zipper()) -> zipper().
children(Zipper = #{spec := #{children := Children}, node := Node}) ->
    case is_branch(Zipper) of
        true -> Children(Node);
        false -> throw(children_on_leaf)
    end.

-spec is_branch(zipper()) -> boolean().
is_branch(#{spec := #{is_branch := IsBranch}, node := Node}) ->
    IsBranch(Node).
