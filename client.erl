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

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
    case genserver:request(St#client_st.server, {join, self(), Channel}) of
        joined -> 
            {reply, ok, St#client_st{channels = [Channel | St#client_st.channels]}};
        failed ->
            {reply, {error, already_joined, "already in channel"}, St};
        _ -> 
            {reply, {error, error_in_client_handle_join, "unexpected error client_handle_join"}, St}
    end;
                
    % TODO: Implement this function
    % {reply, ok, St} ;
    %{reply, {error, not_implemented, "join not implemented"}, St} ;

% Leave channel
handle(St, {leave, Channel}) ->
    case lists:member(Channel, St#client_st.channels) of
        true ->
            case genserver:request(list_to_atom(Channel), {leave, self()}) of
                left_channel_sucesfull -> 
                    UpdatedChannels = lists:delete(Channel, St#client_st.channels),
                    {reply, ok, St#client_st{channels = UpdatedChannels}};
                not_in_channel ->
                    {reply, {error, user_not_joined, "not in channel"}, St};
                _ -> 
                    {reply, {error, user_not_joined, "unexpected error in client:handle_leave"}}
            end;
        false ->
            {reply, {error, user_not_joined, "not in channel"}, St}
    end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    case genserver:request(#client_st.server, {message_send, Channel, Msg, self()}) of
        message_sent -> 
            {reply, ok, St};
        user_not_joined ->
            {reply, {error, user_not_joined, "cant send message here"}, St}
    end;


    % TODO: Implement this function
    % {reply, ok, St} ;

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}} ;

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
