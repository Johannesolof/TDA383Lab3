-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
  #server_st{ name = ServerName}.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

handle(St, {connect, Nick}) ->
  lists:append(St#server_st.clients, Nick),
  Response = connected,
  consoleMsg("~p is connected~n", [St#server_st.clients]),
  {reply, Response, St};

handle(St, Request) ->
  consoleMsg("recived request ~p~n", [Request]),
  Response = "unkown",
  io:fwrite("Server: unkown request~n"),
  {reply, Response, St}.

consoleMsg(Msg, Arg) ->
  io:fwrite("Server: " ++ Msg, Arg).
