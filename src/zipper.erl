-module(zipper).

-export([
         new/4,
         %% Traverse
         up/1,
         down/1,
         left/1,
         right/1,
         next/1,
         prev/1,
         root/1,
         %% Info
         node/1,
         children/1
        ]).

-type zipper() ::
        #{is_branch => fun(),
          make_node => fun(),
          children => fun()}.

-spec new(fun(), fun(), fun(), term()) -> zipper().
new(_IsBranch, _Children, _MakeNode, _Root) ->
    #{}.

-spec up(zipper()) -> zipper().
up(_Zipper) ->
    #{}.

-spec down(zipper()) -> zipper().
down(_Zipper) ->
    #{}.

-spec left(zipper()) -> zipper().
left(_Zipper) ->
    #{}.

-spec right(zipper()) -> zipper().
right(_Zipper) ->
    #{}.

-spec next(zipper()) -> zipper().
next(_Zipper) ->
    #{}.

-spec prev(zipper()) -> zipper().
prev(_Zipper) ->
    #{}.

-spec root(zipper()) -> zipper().
root(_Zipper) ->
    #{}.

-spec node(zipper()) -> zipper().
node(_Zipper) ->
    #{}.

-spec children(zipper()) -> zipper().
children(_Zipper) ->
    #{}.
