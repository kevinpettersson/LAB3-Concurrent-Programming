-module(server).
-export([start/1,stop/1]).

-record(server_st,{
    channels = [] %List of channels that the server has created.
    }).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    genserver:start(ServerAtom, #server_st{}, fun handle/2).
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    %not_implemented.


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:stop(ServerAtom).
    % TODO Implement function
    % Return ok

% Handle join requests from clients
handle(State, {join, Client, Channel}) ->
    case lists:member(Channel, State#server_st.channels) of
        true -> 
            % If the channel exists, request the channel process to join the client
            Result = genserver:request(list_to_atom(Channel), {join, Client}),
            case Result of
                joined -> 
                    {reply, joined, State};  % Client successfully joined the channel
                failed ->
                    {reply, failed, State}    % Handle the case where joining the channel fails
            end;
        false ->
            % if the channel does not exist, create a new channel and start a new process.
            genserver:start(list_to_atom(Channel), [Client], fun channel_handle/2),
            UpdatedChannels = [Channel | State#server_st.channels],
            {reply, joined, State#server_st{channels = UpdatedChannels}}  % Add the new channel to the state
    end.

% Channel handler function when a client wants to join
channel_handle(Clients, {join, Client}) ->
    case lists:member(Client, Clients) of
        true ->
            {reply, failed, Clients};  % Client is already in the channel
        false ->
            {reply, joined, [Client | Clients]}  % Add the client to the channel
    end;

% Channel handler when a client want to leave.
channel_handle(Clients, {leave, Client}) ->
    case lists:member(Client, Clients) of
        true -> 
            UpdatedClients = lists:delete(Client, Clients),
            {reply, left_channel_sucesfull, UpdatedClients};
        false ->
            {reply, {error, user_not_joined, "Client not in channel"}, Clients}
    end;

channel_handle(Clients, {message_send, Nick, Msg, Client}) ->
    case lists:member(Client, Clients) of
        true ->
            lists:foreach(handle(self(), {message_send, Client, Msg}), Clients),
            {reply, message_sent, Clients};
        false -> 
            {reply, {error, user_not_joined, "Cant write msg in a channel not joined"}, Clients}
    
    end.



