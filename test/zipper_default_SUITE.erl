-module(zipper_default_SUITE).

-export([
         all/0
        ]).

-export([
         zipper_list/1,
         zipper_bin_tree/1,
         zipper_map_tree/1
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

-spec zipper_list(config()) -> ok.
zipper_list(_Config) ->
    Root = [1, 2, 3, 4],
    Zipper = zipper_default:list(Root),
    Root = zipper:node(Zipper),
    1 = zipper:traverse([down, node], Zipper),
    2 = zipper:traverse([down, right, node], Zipper),
    3 = zipper:traverse([down, right, right, node], Zipper),
    4 = zipper:traverse([down, right, right, right, node], Zipper),

    undefined = zipper:traverse([down, left], Zipper),
    undefined = zipper:traverse([down, down], Zipper).

-spec zipper_bin_tree(config()) -> ok.
zipper_bin_tree(_Config) ->
    Root = {1,
            {2, 3, 4},
            {5,
             {6, 7, 8},
             {9, 10, 11}}},
    Zipper = zipper_default:bin_tree(Root).

-spec zipper_map_tree(config()) -> ok.
zipper_map_tree(_Config) ->
    Root = {1,
            {2, undefined, undefined},
            {3,
             {4, undefined, undefined},
             {5, undefined, undefined}}},
    zipper_default:map_tree(Root).
