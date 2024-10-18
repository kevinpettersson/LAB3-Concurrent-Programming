-module(server).
-export([start/1,stop/1]).

% Start a new server process with the given name
start(ServerAtom) ->
    % pass in the name an empty list as the initial state and the handle/2 function for message handling.
    genserver:start(ServerAtom, [], fun handle/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % send a message to stop all channel processes.
    genserver:request(ServerAtom, stop_channels),
    % call stop to kill the server process itself
    genserver:stop(ServerAtom).

% stops all channels 
handle(State, stop_channels) ->
    % iterate trough each channel in the server, applying the genserver:stop function on each element in the state.
    lists:foreach(fun(Channel) -> genserver:stop(list_to_atom(Channel)) end, State),
    % return with ok and a empty list signaling all channels were removed.
    {reply, ok, []};


% Handle join requests from clients
handle(State, {join, Client, Channel}) ->
    % if the channel exist in servers state
    case lists:member(Channel, State) of
        true -> 
            % if true, send join request to the channel
            case genserver:request(list_to_atom(Channel), {join, Client}) of
                % client successfully joined the channel
                joined -> 
                    {reply, joined, State}; 
                % client was already in the channel
                failed ->
                    {reply, failed, State};  
                _ ->
                    {reply, {error, server_not_reached, "adad"}, State}
            end;
        false ->
            % if the channel does not exist, start a new channel process by calling genserver:start/3
            % placing client in the channels initial state and giving it the channel_handle/2 for message handling.
            genserver:start(list_to_atom(Channel), [Client], fun channel_handle/2),
            % concatenate newly created channel to the servers State.
            UpdatedChannels = [Channel | State],
            {reply, joined, UpdatedChannels} 
    end.

% channel handler function when a client wants to join
channel_handle(Clients, {join, Client}) ->
    % if the client is in the channel
    case lists:member(Client, Clients) of
        % if true reply with failed
        true ->
            {reply, failed, Clients};
        % if false reply with joined and concatenate the client to the channels state.
        false ->
            {reply, joined, [Client | Clients]}
    end;

% channel handler when a client want to leave.
channel_handle(Clients, {leave, Client}) ->
    % if the client is in the channel
    case lists:member(Client, Clients) of
        % if true then remove client from the channels state.
        true -> 
            UpdatedClients = lists:delete(Client, Clients),
            {reply, left_channel_sucesfull, UpdatedClients};
        false ->
            {reply, {error, user_not_joined, "client not in channel"}, Clients}
    end;

% channel handler for sending a message to a channel
channel_handle(Clients, {message_send, Channel, Client, Nick, Msg}) ->
    % if the client is in the channel
    case lists:member(Client, Clients) of
        %if true spawn new process to deal with sending the message to each client and reply ok.
        true ->
            spawn(fun() -> lists:foreach( fun(Pid) ->
                    % if Pid is the client 
                    case Pid == Client of
                        % do nothing just reply ok.
                        true -> 
                            ok;
                        % else send a message_recive request to client.
                        false -> 
                            genserver:request(Pid, {message_receive, Channel, Nick, Msg})
                    end
                end,
                Clients) end),
            {reply, ok, Clients};
        false ->
            {reply, failed, Clients}
    end.



    



