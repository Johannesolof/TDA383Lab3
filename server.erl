-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
  #server_st{ name = ServerName, clients = [], channels = [] }.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.
connect(St, Pid, Nick) when St#server_st.clients == [] ->
  NewSt = #server_st{ name = St#server_st.name,
                      clients = [{Pid, Nick} | St#server_st.clients],
                      channels = St#server_st.channels },
  {connected, NewSt};

connect(St, Pid, Nick) ->
  [H|T] = St#server_st.clients,
  case H of
    {Pid,_} -> {user_already_connected, St};
    {_,Nick} -> {nick_taken, St};
    {_,_} -> connect(T, St, Nick)
  end.

disconnect(St, Pid) ->
  NewClients = lists:keydelete(Pid, 1, St#server_st.clients),
  NewSt = #server_st{ name = St#server_st.name,
                      clients = NewClients, 
                      channels = St#server_st.channels },
  {reply, disconnected, NewSt}.

join(St, Pid, ChanId, ChanMembers, ChanList) ->
  NewChanMembers = [Pid | ChanMembers],
  NewChanList = [{ChanId, NewChanMembers} | ChanList],
  NewSt = {
    name = St#server_st.name,
    clients = St#server_st.clients,
    channels = NewChanList
  },
  {reply, joined, NewSt}.


getChannel(St, ChanId) ->
  io:fwrite("get~n"),
  Result = lists:keyTake(ChanId, 1, St#server_st.channels),
  case Result of
    false ->
      io:fwrite("false~n"),
      {{ChanId, []}, St#server_st.channels};
    {value, Channel, ChanList} ->
      io:fwrite("value~n"),
      {Channel, ChanList}
  end.

handle(St, {connect, Pid, Nick}) ->
  {Response, NewSt} = connect(St, Pid, Nick),
  io:fwrite("Server: ~p is connected~n", [Pid]), %TODO: needs to represent the state better
  {reply, Response, NewSt};

handle(St, {disconnect, Pid}) ->
  disconnect(St, Pid);

handle(St, {join, Pid, ChanId}) ->
  {{ChanId, ChanMembers}, ChanList} = getChannel(St, ChanId),
  case lists:member(Pid, ChanList) of
    true ->
      {reply, user_already_joined, St};
    false ->
      join(St, Pid, ChanId, ChanMembers, ChanList)
  end;

% DEPRECATED
handle(St, {isConnected, Pid}) ->
  {reply, lists:keymember(Pid, 1, St#server_st.clients), St};

handle(St, Request) ->
  io:fwrite("Server: recived request ~p~n", [Request]),
  Response = "unkown",
  io:fwrite("Server: unkown request~n"),
  {reply, Response, St}.

%% -----------Utility functions------------

%consoleMsg(Msg, Arg) ->
%  io:fwrite("Server: " ++ Msg, Arg).
