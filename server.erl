-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
  #server_st{ name = ServerName, clients = []}.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

handle(St, {connect, Pid}) ->
  NewSt = #server_st{ name = St#server_st.name, clients = [Pid | St#server_st.clients]},
  
  Response = connected,
  io:fwrite("Server: ~p is connected~n", Pid),
  {reply, Response, NewSt};

handle(St, Request) ->
  io:fwrite("Server: recived request ~p~n", [Request]),
  Response = "unkown",
  io:fwrite("Server: unkown request~n"),
  {reply, Response, St}.

%% -----------Utility functions------------

%consoleMsg(Msg, Arg) ->
%  io:fwrite("Server: " ++ Msg, Arg).
