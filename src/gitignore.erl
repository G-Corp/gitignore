-module(gitignore).
-behaviour(gen_server).

%% API.
-export([start_link/1
         , open/1
         , close/1
         , accepts/2
         , denies/2
         , filter/3]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          file
         }).

open(File) ->
  start_link(File).
start_link(File) ->
	gen_server:start_link(?MODULE, [File], []).

close(Pid) ->
  gen_server:call(Pid, close, infinity).

accepts(_Pid, _File) ->
  ok.

denies(_Pid, _File) ->
  ok.

filter(_Pid, _Files, Type) when Type =:= accepts; 
                                Type =:= denies ->
  ok.

% @hidden
init([File]) ->
	{ok, #state{file = File}}.

% @hidden
handle_call(close, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

% @hidden
handle_cast(_Msg, State) ->
	{noreply, State}.

% @hidden
handle_info(_Info, State) ->
	{noreply, State}.

% @hidden
terminate(_Reason, _State) ->
	ok.

% @hidden
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
