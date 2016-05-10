Generic zipper implementation in Erlang.

## Zippers: what are they good for?

Zippers let you traverse immutable data structures with ease and flexibility.

### Contact Us
For **questions** or **general comments** regarding the use of this library, please use our public
[hipchat room](http://inaka.net/hipchat).

If you find any **bugs** or have a **problem** while using this library, please [open an issue](https://github.com/inaka/galgo/issues/new) in this repo (or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io)

## Usage

For a map tree structure like the following:

```erlang
Root = #{type => planet,
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
        },
```

You can build a zipper by providing three simple functions:
- `IsBranchFun`: takes a node and returns `true` if it is a branch node or
  `false` otherwise.
- `ChildrenFun`: takes a node and returns a list of its children.
- `MakeNodeFun`: takes a node and a list of children and returns a new node
  containg the supplied list as children.

This is an example of how you would define a zipper and then use it to traverse
the map tree structure above:

```erlang
%% Create the zipper
IsBranchFun = fun
                  (#{children := [_ | _]}) -> true;
                  (_) -> false
              end,
ChildrenFun = fun(Node) -> maps:get(children, Node) end,
MakeNodeFun = fun(Node, Children) -> Node#{children => Children} end,
Zipper = zipper:new(fun is_map/1, ChildrenFun, MakeNodefun, Root),

%% Traverse the zipper with next
Zipper1 = zipper:next(Zipper),
Zipper2 = zipper:next(Zipper),

%% Get the current zipper node
Argentina = zipper:node(Zipper2).
io:format("~p", [Argentina]),
%%= #{type => country,
%%=   attrs => #{name => "Argentina"},
%%=   children => []}

%% Go up and get the node
Zipper3 = zipper:up(Zipper2).
America = zipper:node(Zipper2).
io:format("~p", [America]),
%%= #{type => country,
%%=   attrs => #{name => "America"},
%%=   children => [#{...}, #{...}]}
```

## Tests

Circular dependency in test environment ([Katana Test](https://github.com/inaka/katana-test) -> [Elvis Core](https://github.com/inaka/elvis_core) -> [Zipper](https://github.com/inaka/zipper)) is fixed by including Zipper as a dep in the test profile in `rebar.config`
```erlang
...
{profiles, [
  {test, [
    {deps, [
      %% The tag wil be replaced by the rebar.config.script
      {zipper,      {git, "https://github.com/inaka/zipper.git", {tag, "irrelevant"}}},
      ...
    ]}
  ]}
]}.
...
```
but then, we still replace the tag with the current branch. This is done in `rebar.config.script`.
Therefore, it's really important to have the branch updated and pushed to github before running the tests with `rebar3 ct`.

## References

- [The Zipper, GERARD HUET](https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf)
- [clojure.zip](http://clojure.github.io/clojure/clojure.zip-api.html#clojure.zip/zipper)
