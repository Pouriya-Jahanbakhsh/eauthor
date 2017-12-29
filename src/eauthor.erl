%%% ------------------------------------------------------------------------------------------------
%%% "eauthor" is available for use under the following license, commonly known as the 3-clause (or
%%% "modified") BSD license:
%%%
%%% Copyright (c) 2017-2018, Pouriya Jahanbakhsh
%%% (pouriya.jahanbakhsh@gmail.com)
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification, are permitted
%%% provided that the following conditions are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright notice, this list of
%%%    conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright notice, this list of
%%%    conditions and the following disclaimer in the documentation and/or other materials provided
%%%    with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its contributors may be used to
%%%    endorse or promote products derived from this software without specific prior written
%%%    permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
%%% FITNESS FOR A  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%% ------------------------------------------------------------------------------------------------
%% @author   Pouriya Jahanbakhsh <pouriya.jahanbakhsh@gmail.com>
%% @version  17.12.30
%% @doc
%%           A library for adding function authors information in compiled modules.
%% @end
%% -------------------------------------------------------------------------------------------------
-module(eauthor).
-author("pouriya.jahanbakhsh@gmail.com").
%% -------------------------------------------------------------------------------------------------
%% Exports:

%% for compiler:
-export([parse_transform/2]).

%% -------------------------------------------------------------------------------------------------
%% Record & Macros & Includes:

-define(DEF_MOD_AUTHOR, 'undefined').

%% -------------------------------------------------------------------------------------------------
%% Compiler API:

parse_transform(AST, _Opts) ->
    ModAuthor = module_author(AST),
    Authors = function_authors(AST, ModAuthor, ModAuthor, []),
    {_, {_, ModAttrLine, _, _}=Mod, AST2} = lists:keytake(module, 3, AST),
    {_, {_, LastLine}, AST3} = lists:keytake(eof, 1, AST2),
    {Export, Func} = authors_func(Authors, ModAttrLine, LastLine),
    [Mod, Export|lists:reverse([{eof, LastLine+2}, Func|clean(AST3, [])])].

%% -------------------------------------------------------------------------------------------------
%% Internal functions:

module_author([{attribute, _, author, Author}|_]) when erlang:is_list(Author) orelse
                                                       erlang:is_atom(Author) ->
    Author;
module_author([{attribute, _, author, Author}|_]) ->
    io_lib:print(Author);
module_author([_|AST]) ->
    module_author(AST);
module_author([]) ->
    ?DEF_MOD_AUTHOR.


function_authors([Node|AST], ModAuthor, FuncAuthor, FuncAuthors) ->
    case erl_syntax:type(Node) of
        attribute ->
            function_authors(AST, ModAuthor, function_author(Node, FuncAuthor), FuncAuthors);
        function ->
            function_authors(AST
                            ,ModAuthor
                            ,ModAuthor
                            ,[{erl_syntax_lib:analyze_function(Node), FuncAuthor}|FuncAuthors]);
        _ ->
            function_authors(AST, ModAuthor, FuncAuthor, FuncAuthors)
    end;
function_authors(_, _, _, FuncAuthors) ->
    lists:reverse(FuncAuthors).


function_author({attribute, _, function_author, Author}, _) when erlang:is_list(Author) orelse
                                                                 erlang:is_atom(Author) ->
    Author;
function_author({attribute, _, function_author, Author}, _) ->
    io_lib:print(Author);
function_author(_, ModAuthor) ->
    ModAuthor.


authors_func(Authors, ModAttrLine, FuncLine) ->
    ClauseLine = FuncLine+1,
    Fold =
        fun({{Name, Arity}, Author}, Authors2) ->
            Key = {tuple, ClauseLine, [{atom, ClauseLine, Name}, {integer, ClauseLine, Arity}]},
            Val =
                if
                    erlang:is_list(Author) ->
                        {string, ClauseLine, Author};
                    true -> % atom
                        {atom, ClauseLine, Author}
                end,
            [{map_field_assoc, ClauseLine, Key, Val}|Authors2]
        end,
    Export = {attribute, ModAttrLine+1, export, [{authors, 0}]},
    FuncClause = {clause, FuncLine, [], [], [{map, ClauseLine, lists:foldl(Fold, [], Authors)}]},
    Func = {function, FuncLine, authors, 0, [FuncClause]},
    {Export, Func}.


clean([{attribute, _, function_author, _}|AST], Ret) ->
    clean(AST, Ret);
clean([Node|AST], Ret) ->
    clean(AST, [Node|Ret]);
clean([], Ret) ->
    Ret.