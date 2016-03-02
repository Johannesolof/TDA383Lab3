-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.


%% Produce initial state
initial_state(Nick, GUIName) ->
  #client_st { gui = GUIName , nick = Nick, connected = false, channels = [] }.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

updateState(St, {connect, Server}) ->
  #client_st{ gui = St#client_st.gui,
              nick = St#client_st.nick,
              server = Server,
              connected = true,
              channels = St#client_st.channels };

updateState(St, disconnect) ->
  #client_st{ gui = St#client_st.gui,
              nick = St#client_st.nick,
              server = undef,
              connected = false,
              channels = St#client_st.channels };

updateState(St, {channels, Channels}) ->
  #client_st{ gui = St#client_st.gui,
              nick = St#client_st.nick,
              server = St#client_st.server,
              connected = St#client_st.connected,
              channels = Channels }.


disconnect(St) ->
  case request(St, St#client_st.server, {disconnect, self()}) of
      disconnected ->
        NewSt = updateState(St, disconnect),
        {reply, ok, NewSt} ;
      leave_channels_first ->
        {reply, {error, leave_channels_first, "Leave all channels before disconnecting!"}, St} ;
      {request_error, Error, Msg} ->
        {reply, {error, Error, Msg}, St};
      Unknown ->
        {reply, {error, failed, "Unkown response: "++Unknown}, St}
  end.

connect(St, Server) ->
  case St#client_st.connected of
    false ->
      ServerAtom = list_to_atom(Server),
      try genserver:request(ServerAtom, {connect, self(), St#client_st.nick}) of
        connected ->
          NewSt = updateState(St, {connect, ServerAtom}),
          {reply, ok, NewSt} ;
        user_already_connected ->
          {reply, {error, user_already_connected, "Already connected!"}, St} ;
        nick_taken ->
          {reply, {error, nick_taken, "Nick already taken!"}, St} ;
        Unknown ->
          {reply, {error, failed, "Unknown response: "++Unknown}, St}
      catch
        _Exception:_Reason ->
          {reply, {error, server_not_reached, "Could not connect to server!"}, St}
      end;
    true ->
      {reply, {error, user_already_connected, "Already connected to "++Server}, St}
  end.

join(St, Channel) ->
  % ask server for channel pid
  case request(St, St#client_st.server, {join, Channel}) of
    {channel_pid, ChanPid} ->
      % ask channel process for permission to join
      case request(St, ChanPid, {join, self()}) of
        joined ->
          ChannelAtom = list_to_atom(Channel),
          NewSt = updateState(St, {channels, [ChannelAtom | St#client_st.channels]}),
          {reply, ok, NewSt};
        user_already_joined ->
          {reply, {error, user_already_joined, "Already in channel!"}, St};
        {request_error, Error, Msg} ->
          {reply, {error, Error, Msg}, St};
        Unknown ->
          {reply, {error, failed, "Unkown response: "++Unknown}, St}
      end;
    {request_error, Error, Msg} ->
        {reply, {error, Error, Msg}, St};
    Unknown ->
        {reply, {error, failed, "Unkown response: "++Unknown}, St}
  end.

% request is used for all requests to server once connected
% before making the request the method checks if the client is connected
% if an exception occurs during the genserver request this is also catched by the method
request(St, Server, RequestAtom) ->
  case St#client_st.connected of
    true ->
      try genserver:request(Server, RequestAtom) of
        Response ->
          Response
      catch
        _Exception:_Reason ->
          {request_error, server_not_reached, "Exception occured while making request to server!"}
      end;
    false ->
      {request_error, user_not_connected, "You are not connected to a server!"}
  end.

leave(St, Channel) ->
  ChannelAtom = list_to_atom(Channel),
  case request(St, ChannelAtom, {leave, self()}) of
    left ->
      NewSt = updateState(St, {channels, lists:delete(ChannelAtom, St#client_st.channels)}),
      {reply, ok, NewSt};
    user_not_joined ->
      {reply, {error, user_not_joined, "You are not in that channel!"}, St};
    {request_error, Error, Msg} ->
      {reply, {error, Error, Msg}, St};
    Unknown ->
      {reply, {error, failed, "Unkown response: "++Unknown}, St}
  end.

send(St, Channel, Msg) ->
  ChannelAtom = list_to_atom(Channel),
  case request(St, ChannelAtom, {send, self(), St#client_st.nick, Msg}) of
    sent ->
      {reply, ok, St};
    user_not_joined ->
      {reply, {error, user_not_joined, "You are not in that channel!"}, St};
    {request_error, Error, Msg} ->
      {reply, {error, Error, Msg}, St};
    Unknown ->
      {reply, {error, failed, "Unkown response: "++Unknown}, St}
  end.  

%% Connect to server
handle(St, {connect, Server}) ->
  connect(St, Server);

%% Disconnect from server
handle(St, disconnect) ->
  case (St#client_st.channels =:= []) of 
    true ->
      disconnect(St);
    false ->
      {reply, {error, leave_channels_first, "Leave all channels before disconnecting!"}, St}
  end;

% Join channel
handle(St, {join, Channel}) ->
  join(St, Channel);

%% Leave channel
handle(St, {leave, Channel}) ->
  leave(St, Channel);

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
  send(St, Channel, Msg);

%% Get current nick
handle(St, whoami) ->
  {reply, St#client_st.nick, St};

%% Change nick
handle(St, {nick, Nick}) ->
  Result = case St#client_st.connected of
    true ->
      {reply, {error, user_already_connected, "Changing name while connected to a server is prohibited!"}, St};
    false ->
      NewSt = #client_st{ gui = St#client_st.gui, nick = Nick, connected = St#client_st.connected },
      {reply, ok, NewSt}
  end,
  Result;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
  gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
  {reply, ok, St}.
