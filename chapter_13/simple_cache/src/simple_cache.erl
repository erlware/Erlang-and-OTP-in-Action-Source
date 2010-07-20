-module(simple_cache).

-export([insert/2, lookup/1, delete/1]).

-define(HBASE_NODE, 'hbase@localhost').

insert(Key, Value) ->
    case sc_store:lookup(Key) of
        {ok, Pid} ->
            sc_event:replace(Key, Value),
            sc_element:replace(Pid, Value);
        {error, _} ->
            {ok, Pid} = sc_element:create(Value),
            sc_store:insert(Key, Pid),
            sc_event:create(Key, Value)
    end,
    sc_hbase:put(?HBASE_NODE, Key, Value).

lookup(Key) ->
    sc_event:lookup(Key),
    try
        case sc_store:lookup(Key) of
            {ok, Pid} ->
                {ok, Value} = sc_element:fetch(Pid),
                {ok, Value};
            {error, _} ->
                {ok, Value} = sc_hbase:get(?HBASE_NODE, Key),
                insert(Key, Value),
                {ok, Value}
        end
    catch
        _Class:_Exception ->
            {error, not_found}
    end.

delete(Key) ->
    sc_event:delete(Key),
    case sc_store:lookup(Key) of
        {ok, Pid} ->
	    sc_hbase:delete(?HBASE_NODE, Key),
            sc_element:delete(Pid);
        {error, _Reason} ->
            ok
    end.
