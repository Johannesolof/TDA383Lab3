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
findClient(St, Pid, Nick) when St#server_st.clients == [] ->
  {connected, #server_st{ name = St#server_st.name,
            clients = [{Pid, Nick} | St#server_st.clients]}};

findClient(St, Pid, Nick) ->
  [H|T] = St#server_st.clients,
  case H of
    {Pid,_} -> {user_already_connected, St};
    {_,Nick} -> {nick_taken, St};
    {_,_} -> findClient(T, St, Nick)
  end.


handle(St, {connect, Pid, Nick}) ->
  {Response, NewSt} = findClient(St, Pid, Nick),

  io:fwrite("Server: ~p is connected~n", [Pid]),
  {reply, Response, NewSt};

handle(St, Request) ->
  io:fwrite("Server: recived request ~p~n", [Request]),
  Response = "unkown",
  io:fwrite("Server: unkown request~n"),
  {reply, Response, St}.

%% -----------Utility functions------------

%consoleMsg(Msg, Arg) ->
%  io:fwrite("Server: " ++ Msg, Arg).
