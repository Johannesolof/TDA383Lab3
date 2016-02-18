-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.


%% Produce initial state
initial_state(Nick, GUIName) ->
  #client_st { gui = GUIName , nick = Nick, connected = false }.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

disconnect(St) ->
  case genserver:request(St#client_st.server, {disconnect, self()}) of
      disconnected ->
        NewSt = #client_st{ gui = St#client_st.gui,
                            nick = St#client_st.nick,
                            connected = false },
        {reply, ok, NewSt} ;
      user_not_connected ->
        {reply, {error, user_not_connected, "User not connected!"}, St} ;
      Unknown ->
        {reply, {error, failed, "Unkown response: "++Unknown}, St}
  end.

connect(St, Server) ->
  ServerAtom = list_to_atom(Server),
  try genserver:request(ServerAtom, {connect, self(), St#client_st.nick}) of
      connected ->
        NewSt = #client_st{ gui = St#client_st.gui,
                            nick = St#client_st.nick,
                            server = ServerAtom,
                            connected = true },
        {reply, ok, NewSt} ;
      user_already_connected ->
        {reply, {error, user_already_connected, "Already connected!"}, St} ;
      nick_taken ->
        {reply, {error, nick_taken, "Nick already taken!"}, St} ;
      Unknown ->
        {reply, {error, failed, "Unknown response: "++Unknown}, St}
  catch
    Exception:Reason ->
      {reply, {error, failed, "Could not connect to server!"}, St}
  end.

%% Connect to server
handle(St, {connect, Server}) ->
  case St#client_st.connected of
    true ->
      {reply, {error, user_already_connected, "Already connected to...?"}, St} ; %TODO: Print server name
    false ->
      connect(St, Server)
  end;

%% Disconnect from server
handle(St, disconnect) ->
  case St#client_st.connected of
    true ->
      disconnect(St);
    false ->
      {reply, {error, failed, "Not connected!"}, St}
  end;

% Join channel
handle(St, {join, Channel}) ->

  % {reply, ok, St} ;
  {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Leave channel
handle(St, {leave, Channel}) ->
  % {reply, ok, St} ;
  {reply, {error, not_implemented, "Not implemented"}, St} ;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
  % {reply, ok, St} ;
  {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Get current nick
handle(St, whoami) ->
  {reply, St#client_st.nick, St} ;

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
