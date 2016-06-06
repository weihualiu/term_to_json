%%%-------------------------------------------------------------------
%%% @doc
%%%     文件名称：term_to_json.erl
%%%     内容摘要：基本函数库
%%%     当前版本：1.0.0
%%%     作者：liu.weihua@rytong.com
%%%     创建日期：10/30/2015
%%%
%%%     修改记录1：
%%%     修改日期:
%%%     修改内容：
%%% @end
%%%-------------------------------------------------------------------

-module(term_to_json).

-export([term_to_json/1, now_milliseconds/0, get_value/3]).

%% @doc
%%	term列表转为json格式字符串
%% @end
term_to_json([]) -> "";
term_to_json(Term) ->
	"{" ++ term_to_json_sub(Term) ++ "}".

%% @doc
%% 获取当前毫秒值，the elapsed time since 00:00 GMT, January 1, 1970 (zero hour)
%% @end
now_milliseconds() ->
	% MegaSecs 10^6秒  Secs 秒  MicroSecs 微秒
    {MegaSecs, Secs, MicroSesc} = erlang:now(),
    MegaSecs * 1000000000 + Secs * 1000 + MicroSesc div 1000.

%% @doc
%%	从tuple列表中找出对应Key的Value
%% @end
get_value([], Find, Default) ->
	Default;
get_value(Data, [], Default) when is_list(Data) ->
	Default;
get_value(Data, Find, Default) when is_list(Data) ->
	case proplists:get_value(Find, Data) of
		undefined ->
			Default;
		_D ->
			_D
	end;
get_value(Data, Find, Default) ->
	Default.

%%------------------------------------------------
%%               internal function
%%------------------------------------------------

%% listobj特定区分列表数据与非列表数据的XML Tag
term_to_json_sub([]) -> "";

term_to_json_sub([H|T]) ->
	%H is atom()
	case is_tuple(H) of
		true ->
			{K, V} = H,
			term_to_json_key(term_to_json_convert(K), term_to_json_convert(V));
		false ->
			""
	end
	%% 找到列表最后一个元素，不添加逗号
	++ case T of [] -> ""; _ -> "," end ++ term_to_json_sub(T).

term_to_json_sub_1([{listobj, _} | _] = V1) ->
	"[" ++ term_to_json_sub(V1) ++ "]";
term_to_json_sub_1([{_, _} | _] = V1) ->
	"{" ++ term_to_json_sub(V1) ++ "}";
term_to_json_sub_1(V1) ->
	"\"" ++ V1 ++ "\"".

term_to_json_key(listobj,Value) ->
	"{" ++ term_to_json_sub(Value) ++ "}";
term_to_json_key(Key,Value) when erlang:is_atom(Key) ->
	"\"" ++ atom_to_list(Key) ++ "\":" ++ term_to_json_sub_1(Value);
term_to_json_key(Key,Value) when erlang:is_integer(Key) ->
	"\"" ++ integer_to_list(Key) ++ "\":" ++ term_to_json_sub_1(Value);
term_to_json_key(Key,Value) ->
	"\"" ++ Key ++ "\":" ++ term_to_json_sub_1(Value).

term_to_json_convert(L) when erlang:is_atom(L) ->
	term_to_json_convert(atom_to_list(L));
term_to_json_convert(L) when erlang:is_integer(L) ->
	term_to_json_convert(integer_to_list(L));
term_to_json_convert(L) ->
	lists:reverse(lists:foldl(fun(O, Acc) -> json_ascii(O,Acc) end, [], L)).

json_ascii(8, Acc) ->
	[8|[92|Acc]];
json_ascii(9, Acc) ->
	[9|[92|Acc]];
json_ascii(10, Acc) ->
	[10|[92|Acc]];
json_ascii(12, Acc) ->
	[12|[92|Acc]];
json_ascii(13, Acc) ->
	[13|[92|Acc]];
json_ascii(34, Acc) ->
	[39|Acc];
json_ascii(47, Acc) ->
	[47|[92|Acc]];
json_ascii(92, Acc) ->
	[92|[92|Acc]];
json_ascii(L, Acc) ->
  [L|Acc].
