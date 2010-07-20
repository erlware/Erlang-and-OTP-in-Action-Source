%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh-2.local>
%%% @copyright (C) 2009, Martin Logan
%%% @doc
%%%  Handle a socket connection for incomming http packets. 
%%% @end
%%% Created : 10 Sep 2009 by Martin Logan <martinjlogan@Macintosh-2.local>
%%%-------------------------------------------------------------------
-module(gws_server).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {lsock, socket, request_line, headers = [], body = <<>>,
		content_remaining = 0, callback, user_state, parent}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Callback, LSock, UserArgs) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Callback, LSock, UserArgs) ->
    gen_server:start_link(?MODULE, [Callback, LSock, UserArgs, self()], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Callback, LSock, UserArgs, Parent]) ->
    error_logger:info_msg("in init calling to ~p~n", [Callback]),
    {ok, UserState} = Callback:init(UserArgs),
    {ok, #state{lsock = LSock, callback = Callback, user_state = UserState, parent = Parent}, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({http, _Socket, {http_header, _Length, <<"Expect">>, _, <<"100-continue">>}}, #state{headers = Headers} = State) ->
    gen_tcp:send(State#state.socket, gen_web_server:http_reply(100)),
    inet:setopts(State#state.socket, [{active,once}]),
    {noreply, State#state{headers = [{'Expect', <<"100-continue">>}|Headers]}};
handle_info({http, _Socket, {http_header, _Length, 'Content-Length', _, Value}}, #state{headers = Headers} = State) ->
    ContentRemaining = list_to_integer(binary_to_list(Value)),
    inet:setopts(State#state.socket, [{active,once}]),
    {noreply, State#state{headers = [{'Content-Length', Value}|Headers], content_remaining = ContentRemaining}};
handle_info({http, _Socket, {http_header, _Length, Key, _, Value}}, #state{headers = Headers} = State) ->
    inet:setopts(State#state.socket, [{active,once}]),
    {noreply, State#state{headers = [{Key, Value}|Headers]}};
handle_info({http, _Socket, {http_request, _Method, _Path, _HTTPVersion} = RequestLine}, State) ->
    inet:setopts(State#state.socket, [{active,once}]),
    {noreply, State#state{request_line = RequestLine}};
handle_info({http, _Socket, http_eoh}, #state{content_remaining = 0} = State) ->
    Reply = callback(State),
    gen_tcp:send(State#state.socket, Reply),
    {stop, normal, State};
handle_info({http, _Socket, http_eoh}, State) ->
    inet:setopts(State#state.socket, [{active,once}, {packet, raw}]),
    {noreply, State};
handle_info({tcp, _Socket, Packet}, State) ->
    PacketSize       = byte_size(Packet),
    ContentRemaining = State#state.content_remaining - PacketSize,
    Body             = list_to_binary([State#state.body, Packet]),
    NewState = State#state{body = Body, content_remaining = ContentRemaining},
    case ContentRemaining of
	0 ->
	    Reply = callback(NewState),
	    gen_tcp:send(State#state.socket, Reply),
	    {stop, normal, State};
	ContentLeftOver when ContentLeftOver > 0 ->
	    inet:setopts(State#state.socket, [{active,once}]),
	    {noreply, NewState}
    end;
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info(timeout, #state{lsock = LSock, parent = Parent} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    gws_connection_sup:start_child(Parent),
    inet:setopts(Socket,[{active,once}]),
    {noreply, State#state{socket = Socket}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
callback(State) -> 
    #state{callback     = Callback,
	   request_line = RequestLine,
	   headers      = Headers,
	   body         = Body,
	   user_state   = UserState} = State,
    handle_message(RequestLine, Headers, Body, Callback, UserState).

handle_message({http_request, 'GET', _, _} = RequestLine, Headers, _Body, CallBack, UserState) ->
    CallBack:get(RequestLine, Headers, UserState);
handle_message({http_request, 'DELETE', _, _} = RequestLine, Headers, _Body, CallBack, UserState) ->
    CallBack:delete(RequestLine, Headers, UserState);
handle_message({http_request, 'HEAD', _, _} = RequestLine, Headers, _Body, CallBack, UserState) ->
    CallBack:head(RequestLine, Headers, UserState);

handle_message({http_request, 'POST', _, _} = RequestLine, Headers, Body, CallBack, UserState) ->
    CallBack:post(RequestLine, Headers, Body, UserState);
handle_message({http_request,'PUT',_,_} = RequestLine, Headers, Body, CallBack, UserState) ->
    CallBack:put(RequestLine, Headers, Body, UserState);
handle_message({http_request, 'TRACE', _, _} = RequestLine, Headers, Body, CallBack, UserState) ->
    CallBack:head(RequestLine, Headers, Body, UserState);
handle_message({http_request, 'OPTIONS', _, _} = RequestLine, Headers, Body, CallBack, UserState) ->
    CallBack:options(RequestLine, Headers, Body, UserState);
handle_message(RequestLine, Headers, Body, CallBack, UserState) ->
    CallBack:other_methods(RequestLine, Headers, Body, UserState).

