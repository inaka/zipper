-module(zipper_SUITE).

-export([
         all/0
        ]).

-export([
         zipper_new/1,
         zipper_node/1,
         zipper_children/1,
         zipper_root/1,
         zipper_next/1,
         zipper_prev/1,
         zipper_up/1,
         zipper_down/1,
         zipper_left/1,
         zipper_right/1
        ]).

-define(EXCLUDED_FUNS,
        [
         module_info,
         all,
         test,
         init_per_suite,
         end_per_suite
        ]).

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
    Zipper = map_tree_zipper(root()),

    maps:get(is_branch, Zipper),
    maps:get(children, Zipper),
    maps:get(make_node, Zipper).

-spec zipper_node(config()) -> ok.
zipper_node(_Config) ->
    Zipper = map_tree_zipper(root()),
    Root = zipper:node(Zipper),
    Root = root().

-spec zipper_children(config()) -> ok.
zipper_children(_Config) ->
    Root = root(),
    Zipper = map_tree_zipper(Root),
    Children = zipper:children(Zipper),
    Children = maps:get(children, Root).

-spec zipper_root(config()) -> ok.
zipper_root(_Config) ->
    Root = root(),
    Zipper = map_tree_zipper(Root),
    Zipper1 = zipper:traverse([next, next, next, next, next, next], Zipper),
    Root = zipper:root(Zipper1).

-spec zipper_down(config()) -> ok.
zipper_down(_Config) ->
    Zipper = map_tree_zipper(root()),
    Zipper1 = zipper:down(Zipper),
    America = zipper:node(Zipper1),
    America = #{type => continent,
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

    Zipper2 = zipper:down(Zipper1),
    Argentina = zipper:node(Zipper2),
    Argentina =  #{type => country,
                   attrs => #{name => "Argentina"},
                   children => []},

    undefined = zipper:down(Zipper2).

-spec zipper_up(config()) -> ok.
zipper_up(_Config) ->
    Zipper = map_tree_zipper(root()),
    at_root = zipper:up(Zipper),

    Zipper1 = zipper:traverse([down, down], Zipper),

    Argentina = zipper:node(Zipper1),
    Argentina = #{type => country,
                attrs => #{name => "Argentina"},
                children => []},

    Zipper2 = zipper:up(Zipper1),
    America = zipper:node(Zipper2),
    America = #{type => continent,
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
    Zipper3 = zipper:up(Zipper2),
    Earth = zipper:node(Zipper3),
    Earth = root().

-spec zipper_right(config()) -> ok.
zipper_right(_Config) ->
    Zipper = map_tree_zipper(root()),

    Zipper1 = zipper:traverse([down, down], Zipper),
    Argentina = zipper:node(Zipper1),
    Argentina = #{type => country,
                  attrs => #{name => "Argentina"},
                  children => []},

    Zipper2 = zipper:right(Zipper1),
    Brasil = zipper:node(Zipper2),
    Brasil = #{type => country,
               attrs => #{name => "Brasil"},
               children => []}.

-spec zipper_left(config()) -> ok.
zipper_left(_Config) ->
    Zipper = map_tree_zipper(root()),

    Zipper1 = zipper:traverse([down, down, right], Zipper),
    Brasil = zipper:node(Zipper1),
    Brasil = #{type => country,
               attrs => #{name => "Brasil"},
               children => []},

    Zipper2 = zipper:left(Zipper1),
    Argentina = zipper:node(Zipper2),
    Argentina = #{type => country,
                  attrs => #{name => "Argentina"},
                  children => []}.

-spec zipper_next(config()) -> ok.
zipper_next(_Config) ->
    Zipper = map_tree_zipper(root()),
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
              },

    'end' = zipper:traverse([next, next, next, next, next, next, next], Zipper).

-spec zipper_prev(config()) -> ok.
zipper_prev(_Config) ->
    Zipper = map_tree_zipper(root()),
    at_root = zipper:prev(Zipper),

    Zipper = zipper:traverse([next, prev], Zipper),
    Zipper1 = zipper:traverse([next, next, next], Zipper),

    Brasil = zipper:node(Zipper1),
    Brasil = #{type => country,
               attrs => #{name => "Brasil"},
               children => []},

    Zipper2 = zipper:prev(Zipper1),
    Argentina = zipper:node(Zipper2),
    Argentina =  #{type => country,
                   attrs => #{name => "Argentina"},
                   children => []}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_tree_zipper(Root) ->
    IsBranchFun = fun
                      (#{children := []}) -> false;
                      (#{children := _Children}) -> true
                     end,
    ChildrenFun = fun(Node) -> maps:get(children, Node) end,
    MakeNodeFun = fun(Node, Children) -> Node#{children => Children} end,
    zipper:new(IsBranchFun, ChildrenFun, MakeNodeFun, Root).

root() ->
    #{type => planet,
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
     }.
