-module(zipper_SUITE).

-export([
         all/0
        ]).

-export([
         %% Info
         zipper_node/1,
         zipper_children/1,
         %% Traverse
         zipper_root/1,
         zipper_next/1,
         zipper_prev/1,
         zipper_up/1,
         zipper_down/1,
         zipper_left/1,
         zipper_right/1,
         zipper_leftmost/1,
         zipper_rightmost/1,
         %% Editing
         zipper_insert_left/1,
         zipper_insert_right/1,
         zipper_replace/1,
         zipper_edit/1,
         zipper_edit_all/1,
         zipper_insert_child/1,
         zipper_append_child/1,
         zipper_remove/1,
         %% Iteration
         zipper_map/1,
         zipper_filter/1,
         zipper_size/1
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

-spec zipper_node(config()) -> ok.
zipper_node(_Config) ->
    Root = root(),
    Zipper = zipper_default:map_tree(Root, children),
    Root = zipper:node(Zipper).

-spec zipper_children(config()) -> ok.
zipper_children(_Config) ->
    Root = root(),
    Zipper = zipper_default:map_tree(Root, children),
    Children = zipper:children(Zipper),
    Children = maps:get(children, Root),

    ArgZipper = zipper:traverse([next, next], Zipper),
    ok = try
             zipper:children(ArgZipper)
         catch
             throw:children_on_leaf -> ok
         end.

-spec zipper_root(config()) -> ok.
zipper_root(_Config) ->
    Root = root(),
    Zipper = zipper_default:map_tree(Root, children),
    Zipper1 = zipper:traverse([next, next, next, next, next, next], Zipper),
    Root = zipper:root(Zipper1).

-spec zipper_down(config()) -> ok.
zipper_down(_Config) ->
    Zipper = zipper_default:map_tree(root(), children),
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
    Zipper = zipper_default:map_tree(root(), children),
    undefined = zipper:up(Zipper),

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
    Zipper = zipper_default:map_tree(root(), children),

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
    Zipper = zipper_default:map_tree(root(), children),

    Zipper1 = zipper:traverse([down, down, right], Zipper),
    Brasil = zipper:node(Zipper1),
    Brasil = #{type => country,
               attrs => #{name => "Brasil"},
               children => []},

    Zipper2 = zipper:left(Zipper1),
    Argentina = zipper:node(Zipper2),
    Argentina = #{type => country,
                  attrs => #{name => "Argentina"},
                  children => []},

    undefined = zipper:left(Zipper2).

-spec zipper_next(config()) -> ok.
zipper_next(_Config) ->
    Zipper = zipper_default:map_tree(root(), children),
    Zipper1 = zipper:next(Zipper),
    false = zipper:is_end(Zipper),

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


    Zipper5 = zipper:next(Zipper4),
    Sweden = zipper:node(Zipper5),
    Sweden =  #{type => country,
                   attrs => #{name => "Sweden"},
                   children => []},

    Zipper6 = zipper:next(Zipper5),
    England = zipper:node(Zipper6),
    England = #{type => country,
               attrs => #{name => "England"},
               children => []},

    ZipperEnd = zipper:next(Zipper6),
    true = zipper:is_end(ZipperEnd),
    ZipperEnd = zipper:next(ZipperEnd),
    true = zipper:is_end(ZipperEnd).

-spec zipper_prev(config()) -> ok.
zipper_prev(_Config) ->
    Zipper = zipper_default:map_tree(root(), children),
    undefined = zipper:prev(Zipper),

    Zipper = zipper:traverse([next, prev], Zipper),
    Zipper1 = zipper:traverse([next, next, next, next, next, next], Zipper),

    England = zipper:node(Zipper1),
    England = #{type => country,
               attrs => #{name => "England"},
               children => []},

    Zipper2 = zipper:prev(Zipper1),
    Sweden = zipper:node(Zipper2),
    Sweden = #{type => country,
               attrs => #{name => "Sweden"},
               children => []},

    Zipper3 = zipper:prev(Zipper2),
    Europe = zipper:node(Zipper3),
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

    Zipper4 = zipper:prev(Zipper3),
    Brasil = zipper:node(Zipper4),
    Brasil = #{type => country,
               attrs => #{name => "Brasil"},
               children => []},

    Zipper5 = zipper:prev(Zipper4),
    Argentina = zipper:node(Zipper5),
    Argentina =  #{type => country,
                   attrs => #{name => "Argentina"},
                   children => []},

    Zipper6 = zipper:prev(Zipper5),
    America = zipper:node(Zipper6),
    America =  #{type => continent,
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

    Zipper7 = zipper:prev(Zipper6),
    Earth = zipper:node(Zipper7),
    Earth = root().

-spec zipper_leftmost(config()) -> ok.
zipper_leftmost(_Config) ->
    Zipper = zipper_default:map_tree(root(), children),
    Zipper = zipper:leftmost(Zipper),

    Argentina = zipper:traverse([down, down], Zipper),
    Argentina = zipper:traverse([down, down, right, leftmost], Zipper).

-spec zipper_rightmost(config()) -> ok.
zipper_rightmost(_Config) ->
    Zipper = zipper_default:map_tree(root(), children),
    Zipper = zipper:rightmost(Zipper),

    Europe = zipper:traverse([down, right], Zipper),
    Europe = zipper:traverse([down, rightmost], Zipper).

-spec zipper_insert_left(config()) -> ok.
zipper_insert_left(_Config) ->
    Zipper = zipper_default:map_tree(root(), children),
    ok = try
             zipper:insert_left(#{}, Zipper)
         catch
             _:insert_at_top -> ok
         end,

    AfricaNode = #{type => continent,
                   attrs => #{name => "Africa"},
                   children => [#{type => country,
                                  attrs => #{name => "Kenya"},
                                  children => []},
                                #{type => country,
                                  attrs => #{name => "Egypt"},
                                  children => []},
                                #{type => country,
                                  attrs => #{name => "South Africa"},
                                  children => []}
                               ]
                  },
    America = zipper:traverse([down], Zipper),
    AmericaInserted = zipper:insert_left(AfricaNode, America),

    EarthNode = zipper:root(AmericaInserted),
    Earth = zipper_default:map_tree(EarthNode, children),
    Continents = zipper:children(Earth),
    3 = length(Continents),

    Africa = zipper:down(Earth),
    #{attrs := #{name := "Africa"}} = zipper:node(Africa).

-spec zipper_insert_right(config()) -> ok.
zipper_insert_right(_Config) ->
    Zipper = zipper_default:map_tree(root(), children),
    ok = try
             zipper:insert_right(#{}, Zipper)
         catch
             _:insert_at_top -> ok
         end,

    AsiaNode = #{type => continent,
                 attrs => #{name => "Asia"},
                 children => [#{type => country,
                                attrs => #{name => "China"},
                                children => []},
                              #{type => country,
                                attrs => #{name => "India"},
                                children => []},
                              #{type => country,
                                attrs => #{name => "Israel"},
                                children => []}
                             ]
                },
    America = zipper:traverse([down], Zipper),
    AmericaInserted = zipper:insert_right(AsiaNode, America),

    EarthNode = zipper:root(AmericaInserted),
    Earth = zipper_default:map_tree(EarthNode, children),
    Continents = zipper:children(Earth),
    3 = length(Continents),

    Asia = zipper:traverse([down, right], Earth),
    #{attrs := #{name := "Asia"}} = zipper:node(Asia).

-spec zipper_replace(config()) -> ok.
zipper_replace(_Config) ->
    Zipper = zipper_default:map_tree(root(), children),
    MarsNode = #{type => planet,
                 attrs => #{name => "Mars"},
                 children => []},

    ReplacedEarth = zipper:replace(MarsNode, Zipper),
    MarsNode = zipper:root(ReplacedEarth),

    AsiaNode = #{type => continent,
                 attrs => #{name => "Asia"},
                 children => [#{type => country,
                                attrs => #{name => "China"},
                                children => []},
                              #{type => country,
                                attrs => #{name => "India"},
                                children => []},
                              #{type => country,
                                attrs => #{name => "Israel"},
                                children => []}
                             ]
                },
    Europe = zipper:traverse([down, right], Zipper),
    ReplacedEurope = zipper:replace(AsiaNode, Europe),
    NewEarthNode = zipper:root(ReplacedEurope),
    NewEarth = zipper_default:map_tree(NewEarthNode, children),
    Asia = zipper:traverse([down, right], NewEarth),
    AsiaNode = zipper:node(Asia).

-spec zipper_edit(config()) -> ok.
zipper_edit(_Config) ->
    Zipper = zipper_default:map_tree(root(), children),
    EditNameFun = fun (Node = #{attrs := Attrs}, Name) ->
                          Node#{attrs => Attrs#{name => Name}}
                  end,

    RenamedEarth = zipper:edit(EditNameFun, ["Big Blue Ball"], Zipper),
    #{attrs := #{name := "Big Blue Ball"}} = zipper:root(RenamedEarth),

    Europe = zipper:traverse([down, right], Zipper),
    RenamedEurope = zipper:edit(EditNameFun, ["The Old Continent"], Europe),
    NewEarthNode = zipper:root(RenamedEurope),
    NewEarth = zipper_default:map_tree(NewEarthNode, children),
    OldContinent = zipper:traverse([down, right], NewEarth),
    #{attrs := #{name := "The Old Continent"}} = zipper:node(OldContinent).

-spec zipper_edit_all(config()) -> ok.
zipper_edit_all(_Config) ->
    Zipper = zipper_default:map_tree(root(), children),
    EditNameFun = fun (Node = #{attrs := Attrs}, Name) ->
                          Node#{attrs => Attrs#{name => Name}}
                  end,

    RenamedEarth = zipper:edit_all(EditNameFun, ["X"], Zipper),
    #{attrs := #{name := "X"},
      children := [
                   #{attrs := #{name := "X"},
                     children := [
                                  #{attrs := #{name := "X"},
                                    children := []},
                                  #{attrs := #{name := "X"},
                                    children := []}
                                 ]
                    },
                   #{attrs := #{name := "X"},
                     children := [
                                  #{attrs := #{name := "X"},
                                    children := []},
                                  #{attrs := #{name := "X"},
                                    children := []}
                                 ]
                    }
                  ]
     } = RenamedEarth.


-spec zipper_insert_child(config()) -> ok.
zipper_insert_child(_Config) ->
    Zipper = zipper_default:map_tree(root(), children),

    AsiaNode = #{type => continent,
                 attrs => #{name => "Asia"},
                 children => [#{type => country,
                                attrs => #{name => "China"},
                                children => []},
                              #{type => country,
                                attrs => #{name => "India"},
                                children => []},
                              #{type => country,
                                attrs => #{name => "Israel"},
                                children => []}
                             ]
                },

    AsiaInserted = zipper:insert_child(AsiaNode, Zipper),
    EarthWithAsiaNode = zipper:root(AsiaInserted),
    EarthWithAsia = zipper_default:map_tree(EarthWithAsiaNode, children),
    Asia = zipper:down(EarthWithAsia),
    #{attrs := #{name := "Asia"}} = zipper:node(Asia),

    UruguayNode = #{type => country,
                    attrs => #{name => "Uruguay"},
                    children => []},

    America = zipper:traverse([down, right], EarthWithAsia),
    #{attrs := #{name := "America"}} = zipper:node(America),
    UruguayInserted = zipper:insert_child(UruguayNode, America),
    EarthWithUruguayNode = zipper:root(UruguayInserted),
    EarthWithUruguay = zipper_default:map_tree(EarthWithUruguayNode, children),
    Uruguay = zipper:traverse([down, right, down], EarthWithUruguay),
    #{attrs := #{name := "Uruguay"}} = zipper:node(Uruguay).

-spec zipper_append_child(config()) -> ok.
zipper_append_child(_Config) ->
    Zipper = zipper_default:map_tree(root(), children),

    AsiaNode = #{type => continent,
                 attrs => #{name => "Asia"},
                 children => [#{type => country,
                                attrs => #{name => "China"},
                                children => []},
                              #{type => country,
                                attrs => #{name => "India"},
                                children => []},
                              #{type => country,
                                attrs => #{name => "Israel"},
                                children => []}
                             ]
                },

    AsiaInserted = zipper:append_child(AsiaNode, Zipper),
    EarthWithAsiaNode = zipper:root(AsiaInserted),
    EarthWithAsia = zipper_default:map_tree(EarthWithAsiaNode, children),
    Asia = zipper:traverse([down, rightmost], EarthWithAsia),
    #{attrs := #{name := "Asia"}} = zipper:node(Asia),

    UruguayNode = #{type => country,
                    attrs => #{name => "Uruguay"},
                    children => []},

    America = zipper:traverse([down], EarthWithAsia),
    #{attrs := #{name := "America"}} = zipper:node(America),
    UruguayInserted = zipper:append_child(UruguayNode, America),
    EarthWithUruguayNode = zipper:root(UruguayInserted),
    EarthWithUruguay = zipper_default:map_tree(EarthWithUruguayNode, children),
    Uruguay = zipper:traverse([down, down, rightmost], EarthWithUruguay),
    #{attrs := #{name := "Uruguay"}} = zipper:node(Uruguay).

-spec zipper_remove(config()) -> ok.
zipper_remove(_Config) ->
    Zipper = zipper_default:map_tree(root(), children),
    ok = try
             zipper:remove(Zipper)
         catch
             _:remove_at_top -> ok
         end,

    America = zipper:down(Zipper),
    AmericaRemoved = zipper:remove(America),
    #{attrs := #{name := "Earth"}} = zipper:node(AmericaRemoved),
    EarthNoAmericaNode = zipper:root(AmericaRemoved),
    EarthNoAmerica = zipper_default:map_tree(EarthNoAmericaNode, children),
    1 = length(zipper:children(EarthNoAmerica)),

    Europe = zipper:traverse([down, right], Zipper),
    EuropeRemoved = zipper:remove(Europe),
    #{attrs := #{name := "Brasil"}} = zipper:node(EuropeRemoved),
    EarthNoEuropeNode = zipper:root(EuropeRemoved),
    EarthNoEurope = zipper_default:map_tree(EarthNoEuropeNode, children),
    1 = length(zipper:children(EarthNoEurope)).

-spec zipper_map(config()) -> ok.
zipper_map(_Config) ->
    Zipper = zipper_default:map_tree(root(), children),

    GetNameFun = fun(#{attrs := #{name := Name}}) -> Name end,
    ["Earth",
     "America", "Argentina", "Brasil",
     "Europe", "Sweden", "England"] = zipper:map(GetNameFun, Zipper).

-spec zipper_filter(config()) -> ok.
zipper_filter(_Config) ->
    Zipper = zipper_default:map_tree(root(), children),

    FilterFun = fun(#{attrs := #{name := Name}}) -> Name == "Brasil" end,
    [#{type := country,
       attrs := #{name := "Brasil"},
       children := []}] = zipper:filter(FilterFun, Zipper).

-spec zipper_size(config()) -> ok.
zipper_size(_Config) ->
    Zipper = zipper_default:map_tree(root(), children),
    7 = zipper:size(Zipper).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
