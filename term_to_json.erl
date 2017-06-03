%%%-------------------------------------------------------------------
%%% @doc
%%%     文件名称：term_to_json.erl
%%%     内容摘要：基本函数库
%%%     当前版本：1.0.0
%%%     作者 auhiewuil@gmail.com
%%%     创建日期：10/30/2015
%%%
%%%     修改记录1：
%%%     修改日期:
%%%     修改内容：
%%% @end
%%%-------------------------------------------------------------------

-module(term_to_json).

-export([term_to_json/1]).

% listobj特定区分列表数据与非列表数据的XML Tag
-define(LISTTAG, entity).

%% @doc
%%	term列表转为json格式字符串
%% @end
term_to_json([]) -> "";
term_to_json(Term) ->
	"{" ++ term_to_json_sub(Term) ++ "}".

%%------------------------------------------------
%%               internal function
%%------------------------------------------------
term_to_json_sub([]) -> "";
% 列表
term_to_json_sub([{A,_}, {A,_}|_T] = L) ->
	"[" ++ list_parse(L) ++ "]";
term_to_json_sub([H|T]) ->
	%H is atom()
	case is_tuple(H) of
		true ->
			{K, V} = H,
			term_to_json_key(term_to_json_convert(K), term_to_json_convert(V));
		false ->
			case erlang:is_list(H) of
			    true ->
			        term_to_json_sub(H);
			    false ->
			        ""
			end
	end
	%% 找到列表最后一个元素，不添加逗号
	++ case T of [] -> ""; _ -> "," end ++ term_to_json_sub(T).

term_to_json_sub_1([{A, _}, {A, _} | _] = V1) ->
	"[" ++ list_parse(V1) ++ "]";
term_to_json_sub_1([{?LISTTAG, _} | _] = V1) ->
	"[" ++ term_to_json_sub(V1) ++ "]";
% 处理XML中CDATA
term_to_json_sub_1([{cdata, [91,123|_] = CDATA}] = _V1) ->
    case is_json(CDATA) of
        {true, Json} ->
            Json;
        {false, _} ->
            "\"" ++ term_to_json_convert(CDATA) ++ "\""
    end;
term_to_json_sub_1([{cdata, [123|_] = CDATA}] = _V1) ->
    case is_json(CDATA) of
        {true, Json} ->
            Json;
        {false, _} ->
            "\"" ++ term_to_json_convert(CDATA) ++ "\""
    end;
term_to_json_sub_1([{cdata, CDATA}] = _V1) ->
    "\"" ++ term_to_json_convert(CDATA) ++ "\"";
term_to_json_sub_1([{text,_},{cdata, CDATA}] = _V1) ->
    %"\"" ++ term_to_json_convert(CDATA) ++ "\"";
    term_to_json_sub_1([{cdata, CDATA}]);
term_to_json_sub_1([{text,_},{cdata, CDATA},{text,_}] = _V1) ->
    %"\"" ++ term_to_json_convert(CDATA) ++ "\"";
    term_to_json_sub_1([{cdata, CDATA}]);
term_to_json_sub_1([{_, _} | _] = V1) ->
	"{" ++ term_to_json_sub(V1) ++ "}";
% 判断V值是否是标准JSON格式字符串
term_to_json_sub_1([91,123,125,93] = _V1) ->
    "\"\"";
term_to_json_sub_1([91,123|_] = V1) ->
    case is_json(V1) of
        {true, _} ->
            %io:format("term_to_json_sub_1 3, ~p~n", [V1]),
            V1;
        {false, _} ->
            %io:format("term_to_json_sub_1 3_1, ~p~n", [V1]),
            "\"" ++ V1 ++ "\""
    end;
term_to_json_sub_1([123,125] = _V1) ->
    "\"\"";
% 判断V值是否是标准JSON格式字符串
term_to_json_sub_1([123|_] = V1) ->
    case is_json(V1) of
        {true, _} ->
            %io:format("term_to_json_sub_1 4, ~p~n", [V1]),
            V1;
        {false, _} ->
            "\"" ++ V1 ++ "\""
    end;
term_to_json_sub_1(V1) ->
	"\"" ++ V1 ++ "\"".

term_to_json_key(?LISTTAG,Value) ->
	"{" ++ term_to_json_sub(Value) ++ "}";
term_to_json_key(Key,Value) when erlang:is_atom(Key) ->
	"\"" ++ atom_to_list(Key) ++ "\":" ++ term_to_json_sub_1(Value);
term_to_json_key(Key,Value) when erlang:is_integer(Key) ->
	"\"" ++ integer_to_list(Key) ++ "\":" ++ term_to_json_sub_1(Value);
term_to_json_key(Key,Value) ->
	"\"" ++ Key ++ "\":" ++ term_to_json_sub_1(Value).

term_to_json_convert(?LISTTAG) ->
	?LISTTAG;
term_to_json_convert(L) when erlang:is_atom(L) ->
	term_to_json_convert(atom_to_list(L));
term_to_json_convert(L) when erlang:is_integer(L) ->
	term_to_json_convert(integer_to_list(L));
term_to_json_convert(L) when erlang:is_binary(L) ->
	term_to_json_convert(binary_to_list(L));
% 判断字符串是否是[或者{开头和]或者}结尾
% 如果是则通过json库解析字符串，成功解析则追加在V上，否则按照后续逻辑处理
term_to_json_convert([91,123|_] = L) ->
    case is_json(L) of
        {true, Json} ->
            %io:format("term_to_json_convert 4, ~p~n", [Json]),
            Json;
        {false, _} ->
            %io:format("term_to_json_convert 4_1, ~p~n", [L]),
            json_character_convert(L)
    end;
term_to_json_convert([123|_] = L) ->
    %io:format("term_to_json_convert 5, ~p~n",[L]),
    case is_json(L) of
        {true, Json} ->
            Json;
        {false, _} ->
            json_character_convert(L)
    end;
term_to_json_convert([H] = _L) when erlang:is_binary(H) ->
    term_to_json_convert(binary_to_list(H));
term_to_json_convert(L) ->
	json_character_convert(L).

json_character_convert(L) ->
    Fun = fun(O, Acc) when is_list(O) -> 
			[term_to_json_convert(O)|Acc];
		(O, Acc) ->json_ascii(O,Acc) 
	end,
    lists:reverse(lists:foldl(Fun, [], L)).

json_ascii(8, Acc) ->
	[8|[92|Acc]];
json_ascii(9, Acc) ->
%	[9|[92|Acc]];
    Acc;
json_ascii(10, Acc) ->
%	[10|[92|Acc]];
    Acc;
json_ascii(12, Acc) ->
	[12|[92|Acc]];
json_ascii(13, Acc) ->
	[13|[92|Acc]];
json_ascii(34, Acc) ->
	%[39|Acc];
	[34|[92|Acc]];
json_ascii(47, Acc) ->
	[47|[92|Acc]];
json_ascii(92, Acc) ->
	[92|[92|Acc]];
json_ascii(L, Acc) ->
	[L|Acc].

is_json(L) ->
    try
        {ok, Bin, _} = ewp_json:decode_to_term(L),
        %io:format("isjson true~n"),
        %?printlog("RJSON Bin:~p~n",[Bin]),
        RJson = "{" ++ term_to_json_sub(Bin) ++ "}",
        %?printlog("RJSON:~p~n",[RJson]),
        {true, RJson}
    catch
        _:_ ->
            {false, []}
    end.

% 列表数据分离
list_parse([]) ->
	"";
list_parse([{_,V}= _H|T]) ->
	term_to_json_sub([{?LISTTAG,V}])
	++ case T of [] -> ""; _ -> "," end ++ list_parse(T).

