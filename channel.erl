-module(channel).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

initial_state(ChannelName) ->
  #channel_st{ name = ChannelName, clients = [] }.

updateState(St, clients, Clients) ->
  #channel_st {
    name = St#channel_st.name,
    clients = Clients
  }.

send(St, Pid, Nick, Msg) ->
    Request = {incoming_msg, St#channel_st.name, Nick, Msg},
    lists:foreach(fun(Client) -> genserver:request(Client, Request)
                  end, lists:delete(Pid, St#channel_st.clients)).

handle(St, {join, Pid}) ->
    case lists:member(Pid, St#channel_st.clients) of 
        true -> {reply, user_already_joined, St};
        false ->  
            {reply, joined, updateState(St, clients, [Pid | St#channel_st.clients]) }
    end;

handle(St, {leave, Pid}) ->
    case lists:member(Pid, St#channel_st.clients) of
       false -> {reply, user_not_joined, St};
       true -> {replay, left, updateState(St, clients, lists:delete(Pid, St#channel_st.clients))}
    end;

handle(St, {send, Pid, Nick, Msg}) ->
    case lists:member(Pid, St#channel_st.clients) of
        false -> {reply, user_not_joined, St};
        true -> 
            spawn(fun() -> send(St, Pid, Nick, Msg) end),
            {reply, sent, St}
    end;

handle(St, _Request) ->
    Response = "Unknown",
    {reply, Response, St}. 
