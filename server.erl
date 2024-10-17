-module(server).
-export([start/1,stop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    genserver:start(ServerAtom, [], fun handle/2).
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    %not_implemented.


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:request(ServerAtom, stop_channels),
    genserver:stop(ServerAtom).
    
    % TODO Implement function
    % Return ok

handle(State, stop_channels) ->
     lists:foreach(fun(Ch) -> genserver:stop(list_to_atom(Ch)) end, State),
    {reply, ok, []};


% Handle join requests from clients
handle(State, {join, Client, Channel}) ->
    case lists:member(Channel, State) of
        true -> 
            % If the channel exists, request the channel process to join the client
            case genserver:request(list_to_atom(Channel), {join, Client}) of
                joined -> 
                    {reply, joined, State};  % Client successfully joined the channel
                failed ->
                    {reply, failed, State};    % Handle the case where joining the channel fails
                _ ->
                    {reply, {error, server_not_reached, "adad"}, State}
            end;
        false ->
            % if the channel does not exist, create a new channel and start a new process.
            genserver:start(list_to_atom(Channel), [Client], fun channel_handle/2),
            UpdatedChannels = [Channel | State],
            {reply, joined, UpdatedChannels}  % Add the new channel to the state
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

% Server-side handler for sending a message to a channel
channel_handle(Clients, {message_send, Channel, Client, Nick, Msg}) ->
        case lists:member(Client, Clients) of
            true ->
                spawn(fun() -> lists:foreach( fun(Pid) ->
                     case Pid == Client of
                            true -> 
                                ok;
                            false -> 
                                genserver:request(Pid, {message_receive, Channel, Nick, Msg})
                        end
                    end,
                    Clients) end),
                {reply, ok, Clients};
            false ->
                {reply, failed, Clients}
        end.

    



