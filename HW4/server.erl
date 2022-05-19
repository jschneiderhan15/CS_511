-module(server).

-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
    catch(unregister(server)),
    register(server, self()),
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{server_up, self()}
    end,
    loop(
      #serv_st{
	 nicks = maps:new(), %% nickname map. client_pid => "nickname"
	 registrations = maps:new(), %% registration map. "chat_name" => [client_pids]
	 chatrooms = maps:new() %% chatroom map. "chat_name" => chat_pid
	}
     ).

loop(State) ->
    receive 
	%% initial connection
	{ClientPID, connect, ClientNick} ->
	    NewState =
		#serv_st{
		   nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
		   registrations = State#serv_st.registrations,
		   chatrooms = State#serv_st.chatrooms
		  },
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, join, ChatName} ->
	    NewState = do_join(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, leave, ChatName} ->
	    NewState = do_leave(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to register a new nickname
	{ClientPID, Ref, nick, NewNick} ->
	    NewState = do_new_nick(State, Ref, ClientPID, NewNick),
	    loop(NewState);
	%% client requests to quit
	{ClientPID, Ref, quit} ->
	    NewState = do_client_quit(State, Ref, ClientPID),
	    loop(NewState);
	{TEST_PID, get_state} ->
	    TEST_PID!{get_state, State},
	    loop(State)
    end.

%% executes join protocol from server perspective
do_join(ChatName, ClientPID, Ref, State) ->
	% Check if the chatname exists in the group of chatrooms already
	case maps:is_key(ChatName, State#serv_st.chatrooms) of
		% If exists, pull the current data from the already existing room and update the list of registrations
		true ->
			ChatRoomPID = maps:get(ChatName, State#serv_st.chatrooms),
			NewList = lists:append([ClientPID], maps:get(ChatName, State#serv_st.registrations)),
			NewReg = maps:update(ChatName, NewList, State#serv_st.registrations),
			NewRooms = State#serv_st.chatrooms;
		% If not, the server must spawn the chatroom.
		false ->
			ChatRoomPID = spawn(chatroom, start_chatroom, [ChatName]),
			NewReg = maps:put(ChatName, [ClientPID], State#serv_st.registrations),
			NewRooms = maps:put(ChatName, ChatRoomPID, State#serv_st.chatrooms)
	end,

	% Retrieving client nickname from serv_st nicknames
	ClientNick = maps:get(ClientPID, State#serv_st.nicks),

	% Sending join request to the chat room
	ChatRoomPID!{self(), Ref, register, ClientPID, ClientNick},

	% Return the state to the client
	#serv_st{
		nicks = State#serv_st.nicks,
		registrations = NewReg,
		chatrooms = NewRooms}.

%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
	% Looking up the chatroom's PID from the serv_st
    ChatPID = maps:get(ChatName, State#serv_st.chatrooms),
	% Getting client PIDs of the room
	ClientPIDS = maps:get(ChatName, State#serv_st.registrations),
	% Removing the client PID from the existing list
	NewReg = maps:update(ChatName, ClientPIDS -- [ClientPID], State#serv_st.registrations),
	% Sending unregister message to chatroom
	ChatPID!{self(), Ref, unregister, ClientPID},
	% Sending leave message to client
	ClientPID!{self(), Ref, ack_leave},
	% Returning the state
	#serv_st{
		nicks = State#serv_st.nicks,
		registrations = NewReg,
		chatrooms = State#serv_st.chatrooms
	}.

%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
	% Check if the nickname is already in use
    case lists:member(NewNick, maps:values(State#serv_st.nicks)) of
		% If in use, server sends back error to client
		true ->
			ClientPID!{self(), Ref, err_nick_used},
			NewNicks = State#serv_st.nicks;
		% If not, server updates chatrooms with new nickname
		false ->
			% Updating client PID with new nicknames
			NewNicks = maps:update(ClientPID, NewNick, State#serv_st.nicks),
			% Filtering out registrations without the given client PID
			FilteredRooms = maps:filter(fun(Name, PIDs) -> not lists:member(ClientPID, PIDs) end, State#serv_st.registrations),
			ChatNames = maps:keys(FilteredRooms),
			% Filtering out chatrooms without the given clientPID
			ChatRoomPIDS = maps:filter(fun(Name, PID) -> not lists:member(Name, ChatNames) end, State#serv_st.chatrooms),
			% Mapping the function to send each relavant chatroom the message 
			maps:map(fun(Name, PID) ->
				PID!{self(), Ref, update_nick, ClientPID, NewNick} end, ChatRoomPIDS),
			% Sending message to client
			ClientPID!{self(), Ref, ok_nick}
		end,

		% Updating state of the server
		#serv_st{
			nicks = NewNicks,
			registrations = State#serv_st.registrations,
			chatrooms = State#serv_st.chatrooms
		}.
			

%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
	% Same process as in leaving, filtering out client PIDs
   FilteredPIDs = maps:filter(fun(Name, PIDs) -> not lists:member(ClientPID, PIDs) end, State#serv_st.registrations),
   ChatNames = map:keys(FilteredPIDs),
   % Filtering names from the filtered PIDs
   FilteredNames = maps:filter(fun(Name, PID) -> not lists:member(Name, ChatNames) end, State#serv_st.chatrooms),
   % Telling each chatroom where the client is registered that the client is leaving
   maps:map(fun(Name, PID) -> PID!{self(), Ref, unregister, ClientPID} end, FilteredNames),
   NewReg = maps:map(fun(Name, PIDs) -> lists:delete(ClientPID, PIDs) end, State#serv_st.registrations),
   % Server sending message to client to quit
   ClientPID!{self(), Ref, ack_quit},

	% Returning the state 
	#serv_st{
		nicks = maps:remove(ClientPID, State#serv_st.nicks),
		registrations = NewReg,
		chatrooms = State#serv_st.chatrooms
	}.

