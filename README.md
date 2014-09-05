# zipper

Generic zipper implementation in Erlang.

## Zippers: what are they good for?

Zippers let you traverse immutable data structures

## Usage

To create a  a zipper for a nested map tree structure where tree node
has the following form:

```erlang
#{type => city,
  attrs => #{name => "Buenos Aires"},
  children => []}
```

```erlang
Root = #{type => world,
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
                 children => []},
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
                 children => []},
             ]
            }
         ]
        },

%% Create the zipper
IsBranchFun = fun is_map/1,
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