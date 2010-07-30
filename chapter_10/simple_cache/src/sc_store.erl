-module(sc_store).

-export([
         init/0,
         insert/2,
         delete/1,
         lookup/1
        ]).

-define(TABLE_ID, ?MODULE).
-define(WAIT_FOR_TABLES, 5000).

-record(key_to_pid, {key, pid}).

init() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),
    {ok, CacheNodes} = resource_discovery:fetch_resources(simple_cache),
    dynamic_db_init(lists:delete(node(), CacheNodes)).

insert(Key, Pid) ->
    mnesia:dirty_write(#key_to_pid{key = Key, pid = Pid}).

lookup(Key) ->
    case mnesia:dirty_read(key_to_pid, Key) of
        [{key_to_pid, Key, Pid}] ->
	    case is_pid_alive(Pid) of
		true -> {ok, Pid};
		false -> {error, not_found}
	    end;
        [] ->
	    {error, not_found}
    end.

delete(Pid) ->
    case mnesia:dirty_index_read(key_to_pid, Pid, #key_to_pid.pid) of
        [#key_to_pid{} = Record] ->
            mnesia:dirty_delete_object(Record);
        _ ->
            ok
    end.


%% Internal Functions

dynamic_db_init([]) ->
    mnesia:create_table(key_to_pid,
                        [{index, [pid]},
                         {attributes, record_info(fields, key_to_pid)}
                        ]);
dynamic_db_init(CacheNodes) ->
    add_extra_nodes(CacheNodes).

add_extra_nodes([Node|T]) ->
    case mnesia:change_config(extra_db_nodes, [Node]) of
        {ok, [Node]} ->
            mnesia:add_table_copy(schema, node(), ram_copies),

            mnesia:add_table_copy(key_to_pid, node(), ram_copies),

            Tables = mnesia:system_info(tables),
            mnesia:wait_for_tables(Tables, ?WAIT_FOR_TABLES);
        _ ->
            add_extra_nodes(T)
    end.

is_pid_alive(Pid) when node(Pid) =:= node() ->
    is_process_alive(Pid);
is_pid_alive(Pid) ->
    case lists:member(node(Pid), nodes()) of
	false ->
	    false;
	true ->
	    case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
		true ->
		    true;
		false ->
		    false;
		{badrpc, _Reason} ->
		    false
	    end
    end.
    
