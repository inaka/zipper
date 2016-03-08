-module(zipper_default).

-export([list/1, bin_tree/1, map_tree/2]).

%% @doc Generates a zipper for lists.
-spec list(list()) -> zipper:zipper(list()).
list(Root) ->
    IsBranchFun = fun is_list/1,
    ChildrenFun = fun(List) -> List end,
    MakeNodeFun = fun(_Node, Children) -> Children end,
    zipper:new(IsBranchFun, ChildrenFun, MakeNodeFun, Root).

%% @doc Generates a zipper for binary trees.
-type bin_tree_node(T) :: nil | {T, bin_tree_node(T), bin_tree_node(T)}.
-spec bin_tree(bin_tree_node(T)) -> zipper:zipper(bin_tree_node(T)).
bin_tree(Root) ->
    IsBranchFun = fun(Node) ->
                          is_tuple(Node)
                              andalso (tuple_size(Node) == 3)
                  end,
    ChildrenFun = fun({_, Left, Right}) -> [Left, Right] end,
    MakeNodeFun = fun(Node, [Left, Right]) -> {Node, Left, Right} end,
    zipper:new(IsBranchFun, ChildrenFun, MakeNodeFun, Root).

%% @doc Generates a zipper for maps.
-spec map_tree(M, CK) -> zipper:zipper(M) when M :: #{K => _}, CK :: K.
map_tree(Root, ChildrenKey) ->
    IsBranchFun = fun (M) ->
                      is_map(M)
                          andalso maps:is_key(ChildrenKey, M)
                          andalso maps:get(ChildrenKey, M) /= []
                  end,
    ChildrenFun = fun(Node) -> maps:get(ChildrenKey, Node) end,
    MakeNodeFun = fun(Node, Children) ->
                        maps:put(ChildrenKey, Children, Node)
                  end,
    zipper:new(IsBranchFun, ChildrenFun, MakeNodeFun, Root).
