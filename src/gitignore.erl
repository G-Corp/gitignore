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
          data,
          negatives = [],
          positives = []
         }).

% @doc
% Open en compile the given gitignore file 
% @end
-spec open(File::iodata()) -> {ok, pid()} | {error, term()}.
open(File) ->
  start_link(File, file).

% @doc
% Compile the given list of gitignore patterns
% @end
-spec compile(Data::list()) -> {ok, pid()} | {error, term()}.
compile(Data) when is_list(Data)->
  start_link(Data, data).

-spec start_link(FileOrData::list() | iodata(), Type :: file | data) -> {ok, pid()} | {error, term()}.
start_link(File, file) ->
  case file:read_file(File) of
    {ok, Data} ->
      Data1 = string:tokens(eutils:to_string(Data), "\n\r"),
      start_link(Data1, data);
    E -> E
  end;
start_link(Data, data) when is_list(Data) ->
	gen_server:start_link(?MODULE, Data, []).

% @doc
% Terminate the gitignore gen_server for the  given <tt>Pid</tt>
% @end
-spec close(Pid::pid()) -> ok.
close(Pid) ->
  gen_server:call(Pid, close, infinity).

% @doc
% Return true if the given <tt>File</tt> is accepted, false otherwise
% @end
-spec accepts(Pid::pid(), File::string()) -> true | false.
accepts(Pid, File) ->
  gen_server:call(Pid, {accepts, File}).

% @doc
% Return true if the given <tt>File</tt> is denied, false otherwise
% @end
-spec denies(Pid::pid(), File::string()) -> true | false.
denies(Pid, File) ->
  gen_server:call(Pid, {denies, File}).

% @doc
% Filter the list of file 
% @end
-spec filter(Pid::pid(), Files::list(), Status::accepts | denies) -> list().
filter(Pid, Files, Status)  when Status =:= accepts;
                                 Status =:= denies ->
  lists:foldl(fun(File, Acc) ->
                  case gen_server:call(Pid, {Status, File}) of
                    true ->
                      [File|Acc];
                    false ->
                      Acc
                  end
              end, [], Files).

% @hidden
init(Data) ->
  {ok, lists:foldl(fun(Line, Acc) ->
                       Line1 = eutils:to_string(Line),
                       Line2 = string:strip(Line1, both, 32),
                       Line3 = string:strip(Line2, both, 9),
                       case Line3 of
                         [$#|_] -> Acc;
                         "" -> Acc;
                         [$\\|Entry] ->
                           add_entry(Entry, Acc, positives);
                         [$!|Entry] ->
                           add_entry(Entry, Acc, negatives);
                         Entry -> 
                           add_entry(Entry, Acc, positives)
                       end
                   end, #state{data = Data}, Data)}.

add_entry(Entry, #state{negatives = Regexs} = State, negatives) ->
  State#state{negatives = [prepare_entry(Entry)|Regexs]};
add_entry(Entry, #state{positives = Regexs} = State, positives) ->
  State#state{positives = [prepare_entry(Entry)|Regexs]}.

prepare_entry(Entry) ->
  Entry1 = case Entry of
             [$/|E] -> [$^|E];
             _ -> Entry
           end,
  Entry2 = re:replace(Entry1, "[-\/\.\+\?\)\(\}\{\\]\\[]", "\\\\&", [global, {return,list}]),
  Entry3 = re:replace(Entry2, "\\\\\\.\\\\\\[([^\\]\\\\]*)\\\\\\]$", "\\\\\\.[\\1]$", [global, {return,list}]),
  Entry4 = re:replace(Entry3, "\\*\\*", "(.+)", [global, {return,list}]),
  re:replace(Entry4, "\\*", "([^\\/]+)", [global, {return,list}]).

% @hidden
handle_call(close, _From, State) ->
  {stop, normal, ok, State};
handle_call(state, _From, State) ->
  {reply, State, State};
handle_call({accepts, File}, _From, State) ->
  {reply, do_accepts(File, State), State};
handle_call({denies, File}, _From, State) ->
  {reply, not do_accepts(File, State), State};
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

do_accepts(File, #state{negatives = Negatives,
                        positives = Positives}) ->
  MatchNeg = lists:any(fun(RE) ->
                           case re:run(File, RE) of
                             match -> true;
                             {match, _} -> true;
                             _ -> false
                           end
                       end, Negatives),
  if
    MatchNeg -> true;
    true ->
      false == lists:any(fun(RE) ->
                             case re:run(File, RE) of
                               match -> true;
                               {match, _} -> true;
                               _ -> false
                             end
                         end, Positives)
  end.
