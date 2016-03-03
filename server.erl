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

connect(St, Pid, Nick) ->
  Conflict = checkConflict(St#server_st.clients, Pid, Nick),
  case Conflict of
    {false, _} ->
      NewSt = updateState(St, clients, [{Pid, Nick} | St#server_st.clients]),
      {connected, NewSt};
    {true, Reason} ->
      {Reason, St}
  end.

updateState(St, clients, Clients) ->
  #server_st{ name = St#server_st.name,
              clients = Clients,
              channels = St#server_st.channels };

updateState(St, channels, Channels) ->
  #server_st{ name = St#server_st.name,
              clients = St#server_st.clients,
              channels = Channels }.

% checks if a client is already connected or if the desired nick is occupied
checkConflict([], _, _) ->
  {false, ok};

checkConflict(Clients, Pid, Nick) ->
  [H|T] = Clients,
  case H of
    {Pid,_} -> {true, user_already_connected};
    {_,Nick} -> {true, nick_taken};
    {_,_} -> checkConflict(T, Pid, Nick)
  end.

disconnect(St, Pid) ->
  NewClients = lists:keydelete(Pid, 1, St#server_st.clients),
  NewSt = updateState(St, clients, NewClients),
  {reply, disconnected, NewSt}.

% returns the pid for a channel
% if the channel does not exist it is created and added to the server state
getChannelPid(St, ChanId) ->
  case lists:keyfind(ChanId, 1, St#server_st.channels) of
    false ->
      ChanPid = genserver:start(list_to_atom(ChanId), channel:initial_state(ChanId), fun channel:handle/2),
      NewSt = updateState(St, channels, [{ChanId, ChanPid} | St#server_st.channels]),
      {NewSt, ChanPid};
    {_ChanId, ChanPid} ->
      {St, ChanPid}
  end.

handle(St, {connect, Pid, Nick}) ->
  {Response, NewSt} = connect(St, Pid, Nick),
  {reply, Response, NewSt};

handle(St, {disconnect, Pid}) ->
  disconnect(St, Pid);

handle(St, {join, ChanId}) ->
  {NewSt, ChanPid} = getChannelPid(St, ChanId),
  {reply, {channel_pid, ChanPid}, NewSt}; 

handle(St, _Request) ->
  Response = "Unknown",
  {reply, Response, St}.
