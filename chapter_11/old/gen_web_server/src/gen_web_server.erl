%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2010, Martin Logan
%%% @doc
%%%  The main interface for the gen_web_server application
%%% @end
%%% Created : 10 Feb 2010 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(gen_web_server).

%% API
-export([start_link/4, start_link/3, http_reply/1, http_reply/3]).

-export([behaviour_info/1]).

-include("eunit.hrl").

behaviour_info(callbacks) ->
    [{init,1},
     {head, 3},
     {get, 3},
     {delete, 3},
     {options, 4},
     {post, 4},
     {put, 4},
     {trace, 4},
     {other_methods, 4}];
behaviour_info(_Other) ->
    undefined.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Start a new gen_web_server behaviour container.
%% @spec (Callback, IP, Port, UserArgs) -> {ok, Pid}
%% @end
%%--------------------------------------------------------------------
start_link(Callback, IP, Port, UserArgs) ->
    gws_connection_sup:start_link(Callback, IP, Port, UserArgs).

%% @spec start_link(Callback, Port, UserArgs) -> {ok, Pid} | ignore | {error, Error}
%% @equiv start_link(Callback, DefaultIP, Port, UserArgs)
start_link(Callback, Port, UserArgs) ->
    start_link(Callback, default_ip, Port, UserArgs).
    
%%--------------------------------------------------------------------
%% @doc helper function for creating a very minimally specified
%%      http message
%% @spec (Code, Headers, Body) -> ok
%% @end
%%--------------------------------------------------------------------
http_reply(Code, Headers, Body) when is_list(Body) ->
    http_reply(Code, Headers, list_to_binary(Body));
http_reply(Code, Headers, Body) ->
    list_to_binary(["HTTP/1.1 ", code_to_code_and_string(Code), "\r\n",
		    format_headers(Headers),
		    "Content-Length: ", integer_to_list(size(Body)), 
		    "\r\n\r\n", Body]).

%% @spec (Code) -> ok
%% @equiv http_reply(Code, [{"Content-Type", "text/html"}], "") 
http_reply(Code) ->
    http_reply(Code, [{"Content-Type", "text/html"}], <<>>).

format_headers([{Header, Value}|T]) ->
    [tos(Header), ": ", Value, "\r\n"|format_headers(T)];
format_headers([]) ->
    [].

tos(Val) when is_atom(Val) -> atom_to_list(Val);
tos(Val)                   -> Val.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Given a number of a standard HTTP response code, return
%% a binary (string) of the number and name.
%%
%% Example:
%% ```code_to_code_and_string(404) => "404 Not Found"
%% '''
%%
%% The supported status codes are taken from:
%%   ["http://en.wikipedia.org/wiki/List_of_HTTP_status_codes"]
%%
%% @spec (integer()) -> binary()
code_to_code_and_string(100) -> "100 Continue";
code_to_code_and_string(101) -> "101 Switching Protocols";
code_to_code_and_string(102) -> "102 Processing";
code_to_code_and_string(200) -> "200 OK";
code_to_code_and_string(201) -> "201 Created";
code_to_code_and_string(202) -> "202 Accepted";
code_to_code_and_string(203) -> "203 Non-Authoritative Information";
code_to_code_and_string(204) -> "204 No Content";
code_to_code_and_string(205) -> "205 Reset Content";
code_to_code_and_string(206) -> "206 Partial Content";
code_to_code_and_string(207) -> "207 Multi-Status";
code_to_code_and_string(300) -> "300 Multiple Choices";
code_to_code_and_string(301) -> "301 Moved Permanently";
code_to_code_and_string(302) -> "302 Found";
code_to_code_and_string(303) -> "303 See Other";
code_to_code_and_string(304) -> "304 Not Modified";
code_to_code_and_string(305) -> "305 Use Proxy";
code_to_code_and_string(307) -> "307 Temporary Redirect";
code_to_code_and_string(400) -> "400 Bad Request";
code_to_code_and_string(401) -> "401 Unauthorized";
code_to_code_and_string(402) -> "402 Payment Required";
code_to_code_and_string(403) -> "403 Forbidden";
code_to_code_and_string(404) -> "404 Not Found";
code_to_code_and_string(405) -> "405 Method Not Allowed";
code_to_code_and_string(406) -> "406 Not Acceptable";
code_to_code_and_string(407) -> "407 Proxy Authentication Required";
code_to_code_and_string(408) -> "408 Request Time-out";
code_to_code_and_string(409) -> "409 Conflict";
code_to_code_and_string(410) -> "410 Gone";
code_to_code_and_string(411) -> "411 Length Required";
code_to_code_and_string(412) -> "412 Precondition Failed";
code_to_code_and_string(413) -> "413 Request Entity Too Large";
code_to_code_and_string(414) -> "414 Request-URI Too Large";
code_to_code_and_string(415) -> "415 Unsupported Media Type";
code_to_code_and_string(416) -> "416 Requested range not satisfiable";
code_to_code_and_string(417) -> "417 Expectation Failed";
code_to_code_and_string(421) ->
    "421 There are too many connections from your internet address";
code_to_code_and_string(422) -> "422 Unprocessable Entity";
code_to_code_and_string(423) -> "423 Locked";
code_to_code_and_string(424) -> "424 Failed Dependency";
code_to_code_and_string(425) -> "425 Unordered Collection";
code_to_code_and_string(426) -> "426 Upgrade Required";
code_to_code_and_string(449) -> "449 Retry With";
code_to_code_and_string(500) -> "500 Internal Server Error";
code_to_code_and_string(501) -> "501 Not Implemented";
code_to_code_and_string(502) -> "502 Bad Gateway";
code_to_code_and_string(503) -> "503 Service Unavailable";
code_to_code_and_string(504) -> "504 Gateway Time-out";
code_to_code_and_string(505) -> "505 HTTP Version not supported";
code_to_code_and_string(506) -> "506 Variant Also Negotiates";
code_to_code_and_string(507) -> "507 Insufficient Storage";
code_to_code_and_string(509) -> "509 Bandwidth Limit Exceeded";
code_to_code_and_string(510) -> "510 Not Extended";
code_to_code_and_string(Code) -> Code.

http_reply_test() ->
    Reply = <<"HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: 0\r\n\r\n">>,
    ?assertMatch(Reply, http_reply(200)),
    Reply2 = <<"HTTP/1.1 200 OK\r\nheader: value\r\nContent-Length: 8\r\n\r\nall good">>,
    ?assertMatch(Reply2, http_reply(200, [{"header", "value"}], "all good")),
    ?assertMatch(Reply2, http_reply(200, [{"header", "value"}], <<"all good">>)),
    ?assertMatch(Reply2, http_reply(200, [{"header", "value"}], ["all"," good"])).
