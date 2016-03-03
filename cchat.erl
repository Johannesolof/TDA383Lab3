% Top level module
-module(cchat).
-export([server/0,client/0,start/0,start2/0,send_job/3]).
-include_lib("./defs.hrl").

%% Start a server
server() ->
    Server = "shire",
    genserver:start(list_to_atom(Server), server:initial_state(Server), fun server:handle/2).

%% Start a client GUI
client() ->
    gui:start().

%% Start local server and one client
start() ->
    server(),
    client().

%% Start local server and two clients
start2() ->
    server(),
    client(),
    client().

send_job(Server, F, Tasks) ->
    try genserver:request(list_to_atom(Server), get_clients) of
        {ok, Clients} -> 
            Assignments = assign_tasks(Clients, Tasks),
            pmap(fun( {Client, X}) -> 
                                  genserver:request(Client, {eval, F, X}) end, 
                          Assignments);
        _Unknown -> io:fwrite("Unknown")
    catch _Exception:_Reason ->
        server_not_reached
    end.        

assign_tasks([], _) -> [] ;
assign_tasks(Clients, Tasks) ->        
    [{lists:nth(((N-1) rem length(Clients)) + 1, Clients), Task}
    || {N,Task} <- lists:zip(lists:seq(1,length(Tasks)), Tasks)].

pmap(F, Xs) ->
    S = self(),
    Ref = make_ref(),
    Pids = lists:map(fun(X) -> spawn( fun() -> S!{self(), Ref, F(X)} end)
                     end, Xs),
    gather(Pids, Ref).

gather([Pid|T], Ref) ->
   receive {Pid, Ref, Ret} -> [Ret|gather(T,Ref)]
   end; 
gather([], _) -> [].
