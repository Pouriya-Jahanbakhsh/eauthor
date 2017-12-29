# eauthor
A library for adding information of code authors to compiled Erlang modules.

# Example
Suppose we have following module named `test.erl`:
```erlang
-module(test).
-export([help/0, sum/2, sum/3]).


help() ->
    io:format("Use sum/2 for two integers\n"
              "Use sum/3 for three integers\n").

sum(Arg1, Arg2) ->
    Arg1 + Arg2.


sum(Arg1, Arg2, Arg3) ->
    Arg1 + Arg2 + Arg3.
```

Before compile, define attribute
```erlang
-compile({parse_transform, eauthor}).
```
in your module or include `eauthor`'s header file `eauthor.hrl` in your module
```erlang
-include("PATH/TO/eauthor.hrl").
%% or if author is part of your project
-include_lib("eauthor/include/eauthor.hrl").
```
or define flag `{parse_transform, eauthor}` for compiler.
After compile, our module has new function named `authors/0`
```erlang
%% For doing this, compiler should access to compiled eauthor module.
%% So i add path to file eauthor.beam. (i used rebar3 for compiling eauthor)
1> code:add_patha("_build/default/lib/eauthor/ebin").
true

2> compile:file("test.erl", {parse_transform, eauthor}).
{ok,test}

3> test:authors().
#{{help,0} => undefined,{sum,2} => undefined,{sum,3} => undefined}
```
`{help,0}` means function `help/0`, `{sum,2}` means function `sum/2` and so on. Because we did not define anything about authors, we have `undefined` value in above. Edite the code to:
```erlang
-module(test).
-export([help/0, sum/2, sum/3]).
%% Add 'author' attribute. This is author of module.
-author("foo").
-include("_build/default/lib/eauthor/include/eauthor.hrl").

%% This is author of next function (help/0)
-function_author("bar").
help() ->
    io:format("Use sum/2 for two integers\n"
    "Use sum/3 for three integers\n").

%% Its author will be module's author ("foo")
sum(Arg1, Arg2) ->
    Arg1 + Arg2.


-function_author("baz").
sum(Arg1, Arg2, Arg3) ->
    Arg1 + Arg2 + Arg3.
```
Save file and back to shell:
```erlang
%% I included eauthor's header file, so i don't need define {parse_transform, eauthor} again.
4> c(test).                                            
{ok,test}

5> test:authors().
#{{help,0} => "bar",{sum,2} => "foo",{sum,3} => "baz"}
```

#### `Author`
**`pouriya.jahanbakhsh@gmail.com`**

#### `License`
**`BSD 3-Clause`**

#### `Hex version`
[**`17.12.30`**](https://hex.pm/packages/eauthor)
