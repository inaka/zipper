-module(zipper).

-export([
         new/4,
         %% Traverse
         up/1,
         down/1,
         left/1,
         right/1,
         next/1,
         is_end/1,
         prev/1,
         root/1,
         traverse/2,
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
                              parent_info => Info
                             }
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

-spec right(zipper()) -> zipper().
right(#{info := #{rights := []}}) ->
    undefined;
right(Zipper = #{info := Info = #{rights := [NewNode | Rights],
                                  lefts := Lefts},
                 node := Node}) ->
    Zipper#{info => Info#{rights := Rights,
                          lefts := [Node | Lefts]},
            node := NewNode
           }.

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
prev(Zipper = #{lefts := []}) ->
    up(Zipper);
prev(Zipper) ->
    case left(Zipper) of
        undefined ->
            up(Zipper);
        Left ->
            prev_recur(Left)
    end.

prev_recur(Zipper) ->
    case down(Zipper) of
        undefined -> Zipper;
        DownZipper ->
            RightMost = rightmost(DownZipper),
            prev_recur(RightMost)
    end.

rightmost(Zipper = #{info := Info = #{rights := Rights,
                                      lefts := Lefts},
                     node := Node}) ->
    Fun = fun(Item, Acc) -> [Item | Acc] end,
    [NewNode | NewLefts] = lists:foldl(Fun,  [Node | Lefts], Rights),
    Zipper#{info => Info#{lefts => NewLefts,
                          rights => []},
            node => NewNode}.

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
