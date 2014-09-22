Generic zipper implementation in Erlang.

## Zippers: what are they good for?

Zippers let you traverse immutable data structures with ease and flexibility.

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

## References

- [The Zipper, GERARD HUET](https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf)
- [clojure.zip](http://clojure.github.io/clojure/clojure.zip-api.html#clojure.zip/zipper)