-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    channels % atom for keeping track of which channels a client has joined.
}).


% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom,
        channels = []   % initiate a empty list to keep track of which channels a client are in.
    }.

% Join channel
handle(St, {join, Channel}) ->
    % check so the server is a registrered process.
    case lists:member(St#client_st.server, registered()) of
        true ->
            % send a join-request to server,
            % catch in-case there is a unexpected error in the server we still want to send a message to the GUI.
            case catch genserver:request(St#client_st.server, {join, self(), Channel}) of
                % if server returns joined, update clients channel-list and return ok to GUI.
                joined -> 
                    UpdatedChannels = [Channel | St#client_st.channels],           
                    {reply, ok, St#client_st{channels = UpdatedChannels}};
                failed ->
                    {reply, {error, user_already_joined, "already in channel"}, St};
                _ -> 
                    {reply, {error, server_not_reached, "server unreachable in client:handle_join"}, St}
            end;
        false ->
            {reply, {error, server_not_reached, "server unreachable"}, St}
    end;

% Leave channel
handle(St, {leave, Channel}) ->
    % check if the channel is alredy in clients channel-list
    case lists:member(Channel, St#client_st.channels) of 
        true -> 
            % send a leave-request to server
            case genserver:request(list_to_atom(Channel), {leave, self()}) of 
                % if leaving was sucessfull then delete channel from clients channel-list.
                left_channel_sucesfull -> 
                    UpdatedChannels = lists:delete(Channel, St#client_st.channels),
                    {reply, ok, St#client_st{channels = UpdatedChannels}};
                % if the server responds that the client was not joined.
                not_in_channel -> 
                    {reply, {error, user_not_joined, "not in channel"}, St}; 
                _ -> 
                    {reply, {error, server_not_reached, "server unreachable in client:handle_leave"}}
            end;
        false -> 
            {reply, {error, user_not_joined, "not in channel"}, St}
    end;


% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    ChannelAtom = list_to_atom(Channel),
    % if the channel is a registrered process
    case whereis(ChannelAtom) of
        % if the channel was not a registrered process, reply with an error to the gui.
        undefined ->
            {reply, {error, server_not_reached, "channel does not exist"}, St};
        % if the channel was a registrered process
        _ ->
            % send a message_send request to the server
            case genserver:request(ChannelAtom, {message_send, Channel, self(), St#client_st.nick, Msg}) of
                ok ->
                    {reply, ok, St};
                failed ->
                    {reply, {error, user_not_joined, "not in channel"}, St};
                _ ->
                    {reply, {error, server_not_reached, "server unreachable in client:handl_message_send"}, St}
            end
    end;

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}};

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
