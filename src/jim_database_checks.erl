-module(jim_database_checks).

-export([correct_entry/1, correct_delete/2, correct_search/2, correct_modify/2]).



correct_entry({TableName, Attributes}) ->
	case exist_table(TableName) of
    	true -> correct_attributes(TableName, Attributes);
		{false, _Reason} -> {false, "doesn't exist the table"}
	end.

correct_search(TableName, {Field, Value}) ->
	case exist_table(TableName) of
		true -> correct_pair_field_value(TableName, {Field, Value});
		{false, _Reason} -> {false, "doesn't exist the table"}
	end.

correct_delete(TableName, PrimaryKeyValue) ->
	case jim_database_checks:exist_table(TableName) of
		true -> case exist_entry(TableName, PrimaryKeyValue) of
					true -> true;
					{false, _Reason} -> {error, "doesn't exist the entry"}
				end;
		{false, _Reason} -> {error, "doesn't exist the table"}
	end.

correct_modify(_Objective, []) -> true;
correct_modify(Objective, [Modification | T]) ->
	case correct_modification(Objective, Modification) of
		{false, Reason} -> {false, Reason};
		true -> correct_modify(Objective, T)
	end.
	

%% Support functions ---------------------------------------------------

exist_table(TableName) ->
	case jim_database_data:get_table_by_name(TableName) of
		{TableName, _Attributes} -> true;
		_ -> {false, "not found"}
	end.

exist_entry(TableName, PrimaryKeyValue) ->
	PrimaryKey = jim_database_data:primary_key(TableName),
	case jim_database:search(TableName, {PrimaryKey, PrimaryKeyValue}) of
		[] -> {false, "entry not found"};
		_ -> true
	end.

correct_attributes(TableName, Attributes) ->
	case correct_attributes_number(TableName, Attributes) of
		true -> correct_attributes_type(TableName, Attributes);
		{false, _Reason} -> {false, "incorrect number of attributes"}
	end.

correct_attributes_number(TableName, Attributes) ->
	Table = jim_database_data:get_table_by_name(TableName),
	{TableName, TableAttributes} = Table,
	case tuple_size(Attributes) == tuple_size(TableAttributes) of
		true -> true;
		false -> {false, "different tuple size"}
	end.

correct_attributes_type(TableName, Attributes) ->
	AttributesList = tuple_to_list(Attributes),
	AttributeNamesList = jim_database_data:fields(TableName),
	correct_attributes_type(TableName, AttributesList, AttributeNamesList, true).

correct_attributes_type(_TableName, [], [], PreviousCorrect) ->
	case PreviousCorrect of
		true -> true;
		false -> {false, "incorrect type of attributes"}
	end;
correct_attributes_type(TableName, AttributesList, AttributeNamesList, PreviousCorrect) ->
	[Attribute | TA] = AttributesList,
	[AttributeName | TAN] = AttributeNamesList,
	case PreviousCorrect of
		false -> {false, "incorrect type of attributes"};
		true -> CorrectType = jim_utils:type_of(Attribute) ==
							  jim_database_data:field_type(TableName, AttributeName),
				correct_attributes_type(TableName, TA, TAN, CorrectType)
	end.

correct_pair_field_value(TableName, {Field, Value}) ->
	case field_in_table(TableName, Field) of
		true -> match_field_value_types(TableName, {Field, Value});
		{false, _Reason} -> {false, "donesn't exist the field"}
	end.

field_in_table(TableName, Field) ->
	TableFields = jim_database_data:fields(TableName),
	case lists:member(Field, TableFields) of
		true -> true;
		false -> {false, "There is not the field in the table"}
	end.

match_field_value_types(TableName, {Field, Value}) ->
	FieldType = jim_database_data:field_type(TableName, Field),
	ValueType = jim_utils:type_of(Value),
	case FieldType == ValueType of
		true -> true;
		false -> {false, "different types in field and value"}
	end.


correct_modification({TableName, PrimaryKeyValue}, {Field, NewValue}) ->
	case correct_objective({TableName, PrimaryKeyValue}) of
		true -> correct_new_pair_field_value(TableName, {Field, NewValue});
		{false, Reason} -> {false, Reason}
	end.

correct_objective({TableName, PrimaryKeyValue}) ->
	case exist_table(TableName) of
		true -> case exist_entry(TableName, PrimaryKeyValue) of
					true -> true;
					{false, _Reason} -> {false, "doesn't exist the entry"}
				end;
		{false, _Reason} -> {false, "doesn't exist the table"}
	end.

correct_new_pair_field_value(TableName, {Field, NewValue}) ->
	case Field =/= jim_database_data:primary_key(TableName) of
		true -> case correct_pair_field_value(TableName, {Field, NewValue})  of
					true -> true;
					{false, Reason} -> {false, Reason}
				end;
		false -> {false, "can't modify the primary key"}
	end.