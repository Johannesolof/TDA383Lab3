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

checkConflict(Clients, _, _) when (Clients =:= []) ->
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

join(St, Pid, ChanId, Members, Channels) ->
  NewMembers = [Pid | Members],
  NewChannels = [{ChanId, NewMembers} | Channels],
  NewSt = updateState(St, channels, NewChannels),
  {reply, joined, NewSt}.

leave(St, Pid, ChanId, Members, Channels) ->
  NewMembers = lists:delete(Pid, Members),
  NewChannels = [{ChanId, NewMembers} | Channels],
  NewSt = updateState(St, channels, NewChannels),
  {reply, left, NewSt}.

getChannel(St, ChanId) ->
  case lists:keytake(ChanId, 1, St#server_st.channels) of
    false ->
      {{ChanId, []}, St#server_st.channels};
    {value, Channel, ChanList} ->
      {Channel, ChanList}
  end.

memberOfAnyChannel(_, []) -> false;

memberOfAnyChannel(Pid, Channels) ->
  [{_, Members}|T] = Channels,
  Result = lists:any(fun(CurrentPid) ->
                  Pid =:= CurrentPid
            end, Members),

  case Result of
    true ->
      true;
    false ->
      memberOfAnyChannel(Pid, T)
  end.

getNick(St, Pid) ->
  lists:keyfind(Pid, 1, St#server_st.clients).

send(ChanId, Members, Nick, Msg) ->
  RequestAtom = {incoming_msg, ChanId, Nick, Msg},
  lists:foreach(fun(Recipient) ->
                  genserver:request(Recipient, RequestAtom)
                end, Members).


handle(St, {connect, Pid, Nick}) ->
  {Response, NewSt} = connect(St, Pid, Nick),
  {reply, Response, NewSt};

handle(St, {disconnect, Pid}) ->
  case memberOfAnyChannel(Pid, St#server_st.channels) of
    true ->
      {reply, leave_channels_first, St};
    false ->
      disconnect(St, Pid)
  end;

handle(St, {join, Pid, ChanId}) ->
  {{ChanId, Members}, Channels} = getChannel(St, ChanId),
  case lists:member(Pid, Members) of
    true ->
      {reply, user_already_joined, St};
    false ->
      join(St, Pid, ChanId, Members, Channels)
  end;

handle(St, {leave, Pid, ChanId}) ->
  {{ChanId, Members}, Channels} = getChannel(St, ChanId),
  case lists:member(Pid, Members) of
    true ->
      leave(St, Pid, ChanId, Members, Channels);
    false ->
      {reply, user_not_joined, St}
  end;

handle(St, {send, Pid, ChanId, Msg}) ->
  {{ChanId, Members}, _} = getChannel(St, ChanId),
  case lists:member(Pid, Members) of
    true ->
      case getNick(St, Pid) of
        false ->
          {reply, internal_server_error, St};   % this should not be possible
        {Pid, Nick} ->
          send(ChanId, lists:delete(Pid, Members), Nick, Msg),
          {reply, sent, St}
      end;   
    false ->
      {reply, user_not_joined, St}
  end;

handle(St, Request) ->
  Response = "Unknown",
  {reply, Response, St}.

%% -----------Utility functions------------

%consoleMsg(Msg, Arg) ->
%  io:fwrite("Server: " ++ Msg, Arg).
