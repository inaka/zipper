-module(zipper_default).

-export([list/1, bin_tree/1, map_tree/1]).

list(Root) ->
    IsBranchFun = fun is_list/1,
    ChildrenFun = fun(List) -> List end,
    MakeNodeFun = fun(_Node, Children) -> Children end,
    zipper:new(IsBranchFun, ChildrenFun, MakeNodeFun, Root).

bin_tree(Root) ->
    IsBranchFun = fun(Node) -> is_tuple(Node) and (tuple_size(Node) == 3) end,
    ChildrenFun = fun({_, Left, Right}) -> [Left, Right] end,
    MakeNodeFun = fun(Node, [Left, Right]) -> {Node, Left, Right} end,
    zipper:new(IsBranchFun, ChildrenFun, MakeNodeFun, Root).

map_tree(Root) ->
    IsBranchFun = fun
                      (#{children := [_ | _]}) -> true;
                      (_) -> false
                      end,
    ChildrenFun = fun(Node) -> maps:get(children, Node) end,
    MakeNodeFun = fun(Node, Children) -> Node#{children => Children} end,
    zipper:new(IsBranchFun, ChildrenFun, MakeNodeFun, Root).
