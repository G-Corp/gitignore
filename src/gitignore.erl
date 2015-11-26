-module(gitignore).
-behaviour(gen_server).

%% API.
-export([start_link/2
         , open/1
         , compile/1
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
          data
         }).

-spec open(File::iodata()) -> {ok, pid()} | {error, term()}.
open(File) ->
  start_link(File, file).
-spec compile(Data::iodata()) -> {ok, pid()} | {error, term()}.
compile(Data) ->
  start_link(Data, data).
-spec start_link(FileOrData::iodata(), Type :: file | data) -> {ok, pid()} | {error, term()}.
start_link(File, file) ->
  case file:read_file(File) of
    {ok, Data} ->
      gen_server:start_link(?MODULE, [Data], []);
    E -> E
  end;
start_link(Data, data) ->
	gen_server:start_link(?MODULE, [eutils:to_binary(Data)], []).

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
init([Data]) ->
	{ok, #state{data = Data}}.

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
