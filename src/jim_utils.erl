-module(jim_utils).

-export([type_of/1, entry_to_record/1, record_to_entry/1, 
	     password/1, password/2, match_password/2, primarykey_value/2,
		 to_string/1]).

%% @type record() = {tablename(), Attributes}
%% @type entry() = {tablename(), {Attributes}}
%% @type tablename() = atom()
%% @type password() = string()


%% @spec type_of(any()) -> Type::atom()
type_of(X) when is_integer(X)   -> integer;
type_of(X) when is_float(X)     -> float;
type_of(X) when is_list(X)      -> list;
type_of(X) when is_tuple(X)     -> tuple;
type_of(X) when is_bitstring(X) -> bitstring;
type_of(X) when is_binary(X)    -> binary;
type_of(X) when is_boolean(X)   -> boolean;
type_of(X) when is_function(X)  -> function;
type_of(X) when is_pid(X)       -> pid;
type_of(X) when is_port(X)      -> port;
type_of(X) when is_reference(X) -> reference;
type_of(X) when is_atom(X)      -> atom;
type_of(_X)                     -> unknown.


%% @spec entry_to_record(entry()) -> record()
entry_to_record({TableName, Attributes}) ->
	AttributesList = tuple_to_list(Attributes),
	RecordList = [TableName | AttributesList],
	list_to_tuple(RecordList).

%% @spec record_to_entry(record()) -> entry()
record_to_entry(Record) ->
	LRecord = tuple_to_list(Record),
	[_TableName | LEntry] = LRecord,
	list_to_tuple(LEntry).

%% @spec password(tablename(), PrimaryKeyValue) -> password()
password(TableName, PrimaryKeyValue) ->
	Entry = jim_database:primary_search(TableName, PrimaryKeyValue),
	password(Entry).

%% @spec password(entry()) -> password()
password(Entry) ->
	LEntry = tuple_to_list(Entry),
	Password = lists:last(LEntry),
	Password.

%% @spec match_password(password(), password()) -> true | false
match_password(Password, RealPassword) ->
	Password == RealPassword.

%% @spec primarykey_value(tablename(), entry()) -> PrimaryKeyValue
primarykey_value(TableName, Entry) ->
	PrimaryKey = jim_database_data:primary_key(TableName),
	ValuePosition = jim_database_data:field_position(TableName, PrimaryKey),
	element(ValuePosition, Entry).

%% @spec to_string(any()) -> string()
to_string(Term) ->
	R = io_lib:format("~p",[Term]),
	lists:flatten(R).