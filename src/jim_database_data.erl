-module(jim_database_data).

-export([tables/0, table_names/0, fields/1, field_position/2,
		 field_type/2, get_table_by_name/1, primary_key/1]).
%% This module is thougth to be automatic generated from jim_database_info.erl,
%% the content is only for testing prouposes

tables() -> [
			 {object, {direction, location, metadata, password}},
			 {user, {id, name, sex, password}}
	  	    ].

table_names() -> [object, user].

fields(object) -> [direction, location, metadata, password];
fields(user) -> [id, name, sex, password].

field_position(object, direction) -> 1;
field_position(object, location) -> 2;
field_position(object, metadata) -> 3;
field_position(object, password) -> 4;
field_position(user, id) -> 1;
field_position(user, name) -> 2;
field_position(user, sex) -> 3;
field_position(user, password) -> 4.

field_type(object, direction) -> list;
field_type(object, location) -> tuple;
field_type(object, password) -> list;
field_type(object, metadata) -> list;
field_type(user, id) -> integer;
field_type(user, name) -> list;
field_type(user, sex) -> atom;
field_type(user, password) -> list;
field_type(_, _) -> {error, "bad arg"}.

primary_key(object) -> direction;
primary_key(user) -> id.

get_table_by_name(TableName) ->
	Tables = tables(),
	get_table_by_name(TableName, Tables).

get_table_by_name(TableName, []) -> 
	{error, "doesn't exist table", TableName};
get_table_by_name(TableName, Tables) ->
	[Table | T] = Tables,
	Name = element(1, Table),
	case TableName of
		Name -> Table;
		_ -> get_table_by_name(TableName, T)
	end.