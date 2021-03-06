%%%-----------------------------------------------------------------------------
%%% @author Bruno Santidrian
%%% @doc An adapter interface to comunicate with a database (Mnesia in this case).
%%% @end
%%%-----------------------------------------------------------------------------

-module(jim_database).

-include_lib("stdlib/include/qlc.hrl").

-export([init_database/0]).

-export([add/1, delete/3, modify/3, search/2, primary_search/2]).

%% @type entry() = {tablename(), {Attributes}}
%% @type record() = {tablename(), Attributes}
%% @type tablename() = atom()
%% @type password() = string()
%% @type field() = atom()
%% @type result() = {Attributes}
%% @type results() = [result]


%% @spec init_database() -> ok | {error, Error}
init_database() ->
	try
		mnesia:create_schema([node()]),
		mnesia:start(),
		Tables = jim_database_data:tables(),
		init_tables(Tables),
		init_password_table(),
		ok
	catch
		_C:E -> {error, E}
	end.

init_tables([]) -> ok;
init_tables([Table | T]) ->
	init_table(Table),
	init_tables(T).

init_table(Table) ->
	{TableName, Fields} = Table,
	LFields = tuple_to_list(Fields),
	mnesia:create_table(TableName, [{attributes, LFields}]).

init_password_table() ->
	mnesia:create_table(password, [{attributes, [objective, password]}]).

%% @spec add(additions()) -> ok | {error, Reason}
%% @type additions() = [addition()]
%% @type addition() = {tablename(), {Attributes}, password()}
add([]) -> ok;
add([Addition|T]) ->
	add(Addition),
	add(T);
add({TableName, Attributes, Password}) ->
	Entry = {TableName, Attributes},
	case jim_database_checks:correct_addition(Entry, Password) of
		true -> Add = fun() -> 
						   add_entry(Entry),
						   link_password(Entry, Password)
					  end,
				mnesia:transaction(Add);
		{false, Reason} -> {error, ["bad entry", Reason]}
	end.

add_entry(Entry) ->
	Record = jim_utils:entry_to_record(Entry),
	mnesia:write(Record).

link_password(Entry, Password) ->
	{TableName, Attributes} = Entry,
	PrimaryKeyValue = jim_utils:primarykey_value(TableName, Attributes),
	Record = jim_utils:entry_to_record({password, {{TableName, PrimaryKeyValue}, Password}}),
	mnesia:write(Record).

%% @spec delete(tablename(), PrimaryKeyValue::any(), password()) -> ok | {error, Reason}
delete(TableName, PrimaryKeyValue, Password) ->
	case jim_database_checks:correct_delete(TableName, PrimaryKeyValue) of
		true -> delete_(TableName, PrimaryKeyValue, Password);
		{false, Reason} -> {error, Reason}
	end.

delete_(TableName, PrimaryKeyValue, Password) ->
	RealPassword = password(TableName, PrimaryKeyValue),
	case jim_utils:match_password(Password, RealPassword) of
		true -> delete_(TableName, PrimaryKeyValue);
		false -> {error, "bad_password"}
	end.

delete_(TableName, PrimaryKeyValue) ->
	Delete = fun() ->
				  mnesia:delete({TableName, PrimaryKeyValue})
			 end,
	case mnesia:transaction(Delete) of
		{atomic, ok} -> ok;
		Any -> {error, Any}
	end.

%% @spec modify(objective(), modifications(), password()) -> ok | {error, Reason}
%% @type objective() = {tablename(), PrimaryKeyValue}
%% @type modifications() = [modification()]
%% @type modification() = {field(), NewValue}
modify(Objective, Modifications, Password) ->
	case jim_database_checks:correct_modify(Objective, Modifications) of
		true -> modify_(Objective, Modifications, Password);
		{false, Reason} -> {error, Reason}
	end.

modify_({TableName, PrimaryKeyValue}, Modifications, Password) ->
	RealPassword = password(TableName, PrimaryKeyValue),
	case jim_utils:match_password(Password, RealPassword) of
		true -> modify_({TableName, PrimaryKeyValue}, Modifications);
		false -> {error, "incorrect password"}
	end.

modify_({TableName, PrimaryKeyValue}, [Modifications]) ->
	OriginalEntry = primary_search(TableName, PrimaryKeyValue),
	ModificatedEntry = modify_entry(TableName, OriginalEntry, [Modifications]),
	ModificatedRecord = jim_utils:entry_to_record({TableName, ModificatedEntry}),
	Replace = fun () ->
				mnesia:delete({TableName, PrimaryKeyValue}),
				mnesia:write(ModificatedRecord)
			 end,
	case mnesia:transaction(Replace) of
		{atomic, ok} -> ok;
		Any -> {error, Any}
	end.

modify_entry(_TableName, OriginalEntry, []) -> OriginalEntry;
modify_entry(TableName, OriginalEntry, [Modification | T]) ->
	{Field, NewValue} = Modification,
	ModificationPosition = jim_database_data:field_position(TableName, Field),
	ModificatedEntry = setelement(ModificationPosition, OriginalEntry, NewValue),
	modify_entry(TableName, ModificatedEntry, T).

%% @spec primary_search(tablename(), PrimaryKeyValue) -> results() | {error, Reason}
primary_search(TableName, PrimaryKeyValue) ->
	case search(TableName, {jim_database_data:primary_key(TableName), PrimaryKeyValue}) of
		[Entry] -> Entry;
		[] -> []
	end.

%% @spec search(tablename(), {field(), Value}) -> results() | {error, Reason}
search(TableName, {Field, Value}) ->
	case jim_database_checks:correct_search(TableName, {Field, Value}) of
		true -> search_(TableName, {Field, Value});
		{false, Reason} -> {error, ["bad search", Reason]}
	end.

search_(TableName, {Field, Value}) ->
	FieldPosition = jim_database_data:field_position(TableName, Field) + 1,
	Query = fun() -> Q = qlc:q([jim_utils:record_to_entry(Record) || Record <- mnesia:table(TableName),
						        element(FieldPosition, Record) == Value]),
					 qlc:e(Q)
			end,
	case mnesia:transaction(Query) of
		{atomic, Results} -> Results;
		Any -> {error, Any}
	end.

%% @spec password(record()) -> password()
password(Record) ->
	{TableName, Attributes} = Record,
	PrimaryKeyValue = jim_utils:primarykey_value(TableName, Attributes),
	password(TableName, PrimaryKeyValue).

%% @spec password(tablename(), PrimaryKeyValue) -> password()
password(TableName, PrimaryKeyValue) ->
	Objective = {TableName, PrimaryKeyValue},
	Query = fun() -> Q = qlc:q([jim_utils:record_to_entry(Record) || Record <- mnesia:table(password),
						        element(2, Record) == Objective]),
					 qlc:e(Q)
			end,
	case mnesia:transaction(Query) of
		{atomic, [{_Objective, Password}]} -> Password;
		Any -> {error, Any}
	end.