-module(zipper_SUITE).

-export([
         all/0
        ]).

-export([
         zipper_new/1,
         zipper_node/1,
         zipper_next/1
        ]).

-define(EXCLUDED_FUNS,
        [
         module_info,
         all,
         test,
         init_per_suite,
         end_per_suite
        ]).

-define(ROOT, #{type => planet,
                attrs => #{name => "Earth"},
                children => [
                             #{type => continent,
                               attrs => #{name => "America"},
                               children => [
                                            #{type => country,
                                              attrs => #{name => "Argentina"},
                                              children => []},
                                            #{type => country,
                                              attrs => #{name => "Brasil"},
                                              children => []}
                                           ]
                              },
                             #{type => continent,
                               attrs => #{name => "Europe"},
                               children => [
                                            #{type => country,
                                              attrs => #{name => "Sweden"},
                                              children => []},
                                            #{type => country,
                                              attrs => #{name => "England"},
                                              children => []}
                                           ]
                              }
                            ]
               }).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
    Exports = ?MODULE:module_info(exports),
    [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec zipper_new(config()) -> ok.
zipper_new(_Config) ->
    Zipper = map_tree_zipper(?ROOT),

    maps:get(is_branch, Zipper),
    maps:get(children, Zipper),
    maps:get(make_node, Zipper).

-spec zipper_node(config()) -> ok.
zipper_node(_Config) ->
    Zipper = map_tree_zipper(?ROOT),
    Root = zipper:node(Zipper),
    Root = ?ROOT.

-spec zipper_next(config()) -> ok.
zipper_next(_Config) ->
    Zipper = map_tree_zipper(?ROOT),
    Zipper1 = zipper:next(Zipper),
    Zipper2 = zipper:next(Zipper1),

    Argentina = zipper:node(Zipper2),
    Argentina =  #{type => country,
                   attrs => #{name => "Argentina"},
                   children => []},

    Zipper3 = zipper:next(Zipper2),
    Brasil = zipper:node(Zipper3),
    Brasil = #{type => country,
               attrs => #{name => "Brasil"},
               children => []},

    Zipper4 = zipper:next(Zipper3),
    Europe = zipper:node(Zipper4),
    Europe = #{type => continent,
               attrs => #{name => "Europe"},
               children => [
                            #{type => country,
                              attrs => #{name => "Sweden"},
                              children => []},
                            #{type => country,
                                              attrs => #{name => "England"},
                              children => []}
                           ]
              }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_tree_zipper(Root) ->
    IsBranchFun = fun is_map/1,
    ChildrenFun = fun(Node) -> maps:get(children, Node) end,
    MakeNodeFun = fun(Node, Children) -> Node#{children => Children} end,
    zipper:new(IsBranchFun, ChildrenFun, MakeNodeFun, Root).
