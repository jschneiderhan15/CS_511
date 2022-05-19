%
%   Homework 3 
%   Name(s) : Cindy Zhang & Jack Schneiderhan
%   Date    : 11 / 09 / 2021
%   Pledge  : I pledge my honor that I have abided by the Stevens Honor System.
%

-module(shipping).
-compile(export_all).
-include_lib("./shipping.hrl").

get_ship(Shipping_State, Ship_ID) ->
    % Returns False if Ship_ID is not found
    % Returns Tuple if Ship_ID is found
    case lists:keyfind(Ship_ID, #ship.id, Shipping_State#shipping_state.ships) of
        false ->
            error;
        _ ->
            lists:keyfind(Ship_ID, #ship.id, Shipping_State#shipping_state.ships)
    end.

get_container(Shipping_State, Container_ID) ->
    % Returns False if Container_ID is not found
    % Returns Tuple if Container_ID is found
    case lists:keyfind(Container_ID, #container.id, Shipping_State#shipping_state.containers) of
        false ->
            error;
        _ ->
            lists:keyfind(Container_ID, #container.id, Shipping_State#shipping_state.containers)
    end.

get_port(Shipping_State, Port_ID) ->
    % Return False if Port_ID is not found
    % Return Tuple if Port_ID is found
    case lists:keyfind(Port_ID, #port.id, Shipping_State#shipping_state.ports) of
        false ->
            error;
        _ ->
            lists:keyfind(Port_ID, #port.id, Shipping_State#shipping_state.ports)
    end.

get_occupied_docks(Shipping_State, Port_ID) ->
    % Ship_Loc retrieves a list of all ship locations
    % Returns D_ID (Dock_ID) when P_ID and Port_ID matches
    Ship_loc = Shipping_State#shipping_state.ship_locations,
    [D_ID || {P_ID, D_ID, _S_ID} <- Ship_loc, P_ID == Port_ID].

get_ship_location(Shipping_State, Ship_ID) ->
    % Ship_Loc retrieves a list of all ship locations
    % Output retrieves a list of all tuples P_ID (Port_ID) and
    %        D_ID (Dock_ID) when S_ID and Ship_ID matches
    % Returns error when the list is empty, otherwise 
    % Return the head of the list.
    Ship_loc = Shipping_State#shipping_state.ship_locations,
    Output = [{P_ID, D_ID} || {P_ID, D_ID, S_ID} <- Ship_loc, S_ID == Ship_ID],
    if Output == [] ->
        error;
    true ->
        lists:nth(1,Output)
    end.

get_container_weight(Shipping_State, Container_IDs) ->
    % All_Containers retrieves a list of containers with the matching Container_IDs
    % Returns error when a member of the container is an error
    % Returns the sum of All_Containers' weights
    All_Containers = [get_container(Shipping_State, Container_ID) || Container_ID <- Container_IDs],
    case lists:member(error, All_Containers) of 
      true -> error;
      false ->
        lists:sum([Container#container.weight || Container <- All_Containers])
    end.  

get_ship_weight(Shipping_State, Ship_ID) ->
    % Ship_Containers collects the ship_inventory
    % if error then error
    % otherwise gets the weight with the CIDs
    Ship_containers = Shipping_State#shipping_state.ship_inventory,
    case maps:find(Ship_ID, Ship_containers) of
      {ok, C_IDS} -> get_container_weight(Shipping_State, C_IDS);
      error -> error
    end.


load_ship(Shipping_State, Ship_ID, Container_IDs) ->
    % Retrieves the locatoions of the ships at given port, their inventories, and containers
    {Port_ID, _} = get_ship_location(Shipping_State, Ship_ID),
    Port_inventory = Shipping_State#shipping_state.port_inventory,
    Port_containers = maps:get(Port_ID, Port_inventory),
    % Checks if the container IDs are a sub list of the container ports (function made for us)
    case is_sublist(Port_containers, Container_IDs) of
        false -> error;
        true ->
          Ship = get_ship(Shipping_State, Ship_ID),
          Ship_invs = Shipping_State#shipping_state.ship_inventory,
          Containers_count = length(Container_IDs),
          Ship_current_containers = maps:get(Ship_ID, Ship_invs),
          Ship_current_count = length(Ship_current_containers),
          %checks if the ship's containers added with new containers would precede its load capacity
          if
            Ship_current_count + Containers_count > Ship#ship.container_cap ->
              error;
            true -> 
              New_port_inventory = maps:put(Port_ID, lists:subtract(Port_containers, Container_IDs), Port_inventory),
              New_ship_inventory = maps:put(Ship_ID, lists:append(Ship_current_containers, Container_IDs), Ship_invs),
              New_shipping_state = #shipping_state{
                ships=Shipping_State#shipping_state.ships,
                containers=Shipping_State#shipping_state.containers,
                ports=Shipping_State#shipping_state.ports,
                ship_locations=Shipping_State#shipping_state.ship_locations,
                ship_inventory=New_ship_inventory,
                port_inventory=New_port_inventory
              },
              {ok, New_shipping_state}
          end
    end.

unload_ship_all(Shipping_State, Ship_ID) ->
    % Creates new inventory for ports and ships and creates a capacity based off their lengths
    {Port_ID, _} = get_ship_location(Shipping_State, Ship_ID),
    Port = get_port(Shipping_State, Port_ID),
    Port_inventory = Shipping_State#shipping_state.port_inventory,
    Port_containers = maps:get(Port_ID, Port_inventory),
    Ship_inventory = Shipping_State#shipping_state.ship_inventory,
    Ship_containers = maps:get(Ship_ID, Ship_inventory),
    New_capacity = length(Port_containers) + length(Ship_containers),
    % if the capacity is larger than the ports cap, throw an error
    % otherwise return shipping state of all the containers are offloaded to the port.
    if 
      New_capacity > Port#port.container_cap -> error;
      true ->
        New_port_inventory = maps:put(Port_ID, lists:append(Port_containers, Ship_containers), Port_inventory),
        New_ship_inventory = maps:put(Ship_ID, [], Ship_inventory),
        New_shipping_state = #shipping_state{
          ships=Shipping_State#shipping_state.ships,
          containers=Shipping_State#shipping_state.containers,
          ports=Shipping_State#shipping_state.ports,
          ship_locations=Shipping_State#shipping_state.ship_locations,
          ship_inventory=New_ship_inventory,
          port_inventory=New_port_inventory
        },
        {ok, New_shipping_state}
    end.

unload_ship(Shipping_State, Ship_ID, Container_IDs) ->
    %Takes the ship inventory and makes a list of containers with the given IDs
    Ship_inventory = Shipping_State#shipping_state.ship_inventory,
    Ship_containers = maps:get(Ship_ID, Ship_inventory),
    %If the container list is a sublist of the ID list,
    %Return error if the list isn't a sublist
    % Offload the ships to each respective port, and return shipping state to console.
    case is_sublist(Ship_containers, Container_IDs) of
        false -> 
          io:format("The given containers are not all on the same ship..."),
          error;
        true ->
            {Port_ID, _} = get_ship_location(Shipping_State, Ship_ID),
            Port = get_port(Shipping_State, Port_ID),
            Port_inventory = Shipping_State#shipping_state.port_inventory,
            Port_containers = maps:get(Port_ID, Port_inventory),
            Container_count = length(Container_IDs) + length(Port_containers),
            if 
              Container_count > Port#port.container_cap -> error;
              true ->
                New_port_inventory = maps:put(Port_ID, lists:append(Port_containers, Container_IDs), Port_inventory),
                New_ship_inventory = maps:put(Ship_ID, lists:subtract(Ship_containers, Container_IDs), Ship_inventory),
                {ok, Shipping_State#shipping_state{ship_inventory=New_ship_inventory, port_inventory=New_port_inventory}}
            end
    end.

set_sail(Shipping_State, Ship_ID, {Port_ID, Dock}) ->
    % Retrieves the occupied ships from the dock
    Occupied = get_occupied_docks(Shipping_State, Port_ID),
    % If the dock given is occupied, error. Otherwise, change the ships location to the new location
    case lists:member(Dock, Occupied) of
        true -> error;
        false -> 
            Old_ship_locations = Shipping_State#shipping_state.ship_locations,
            New_ship_locations = lists:keyreplace(Ship_ID, 3, Old_ship_locations, {Port_ID, Dock, Ship_ID}),
            {ok, Shipping_State#shipping_state{ship_locations=New_ship_locations}}
    end.


%% Determines whether all of the elements of Sub_List are also elements of Target_List
%% @returns true is all elements of Sub_List are members of Target_List; false otherwise
is_sublist(Target_List, Sub_List) ->
    lists:all(fun (Elem) -> lists:member(Elem, Target_List) end, Sub_List).




%% Prints out the current shipping state in a more friendly format
print_state(Shipping_State) ->
    io:format("--Ships--~n"),
    _ = print_ships(Shipping_State#shipping_state.ships, Shipping_State#shipping_state.ship_locations, Shipping_State#shipping_state.ship_inventory, Shipping_State#shipping_state.ports),
    io:format("--Ports--~n"),
    _ = print_ports(Shipping_State#shipping_state.ports, Shipping_State#shipping_state.port_inventory).


%% helper function for print_ships
get_port_helper([], _Port_ID) -> error;
get_port_helper([ Port = #port{id = Port_ID} | _ ], Port_ID) -> Port;
get_port_helper( [_ | Other_Ports ], Port_ID) -> get_port_helper(Other_Ports, Port_ID).


print_ships(Ships, Locations, Inventory, Ports) ->
    case Ships of
        [] ->
            ok;
        [Ship | Other_Ships] ->
            {Port_ID, Dock_ID, _} = lists:keyfind(Ship#ship.id, 3, Locations),
            Port = get_port_helper(Ports, Port_ID),
            {ok, Ship_Inventory} = maps:find(Ship#ship.id, Inventory),
            io:format("Name: ~s(#~w)    Location: Port ~s, Dock ~s    Inventory: ~w~n", [Ship#ship.name, Ship#ship.id, Port#port.name, Dock_ID, Ship_Inventory]),
            print_ships(Other_Ships, Locations, Inventory, Ports)
    end.

print_containers(Containers) ->
    io:format("~w~n", [Containers]).

print_ports(Ports, Inventory) ->
    case Ports of
        [] ->
            ok;
        [Port | Other_Ports] ->
            {ok, Port_Inventory} = maps:find(Port#port.id, Inventory),
            io:format("Name: ~s(#~w)    Docks: ~w    Inventory: ~w~n", [Port#port.name, Port#port.id, Port#port.docks, Port_Inventory]),
            print_ports(Other_Ports, Inventory)
    end.
%% This functions sets up an initial state for this shipping simulation. You can add, remove, or modidfy any of this content. This is provided to you to save some time.
%% @returns {ok, shipping_state} where shipping_state is a shipping_state record with all the initial content.
shipco() ->
    Ships = [#ship{id=1,name="Santa Maria",container_cap=20},
              #ship{id=2,name="Nina",container_cap=20},
              #ship{id=3,name="Pinta",container_cap=20},
              #ship{id=4,name="SS Minnow",container_cap=20},
              #ship{id=5,name="Sir Leaks-A-Lot",container_cap=20}
             ],
    Containers = [
                  #container{id=1,weight=200},
                  #container{id=2,weight=215},
                  #container{id=3,weight=131},
                  #container{id=4,weight=62},
                  #container{id=5,weight=112},
                  #container{id=6,weight=217},
                  #container{id=7,weight=61},
                  #container{id=8,weight=99},
                  #container{id=9,weight=82},
                  #container{id=10,weight=185},
                  #container{id=11,weight=282},
                  #container{id=12,weight=312},
                  #container{id=13,weight=283},
                  #container{id=14,weight=331},
                  #container{id=15,weight=136},
                  #container{id=16,weight=200},
                  #container{id=17,weight=215},
                  #container{id=18,weight=131},
                  #container{id=19,weight=62},
                  #container{id=20,weight=112},
                  #container{id=21,weight=217},
                  #container{id=22,weight=61},
                  #container{id=23,weight=99},
                  #container{id=24,weight=82},
                  #container{id=25,weight=185},
                  #container{id=26,weight=282},
                  #container{id=27,weight=312},
                  #container{id=28,weight=283},
                  #container{id=29,weight=331},
                  #container{id=30,weight=136}
                 ],
    Ports = [
             #port{
                id=1,
                name="New York",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=2,
                name="San Francisco",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=3,
                name="Miami",
                docks=['A','B','C','D'],
                container_cap=200
               }
            ],
    %% {port, dock, ship}
    Locations = [
                 {1,'B',1},
                 {1, 'A', 3},
                 {3, 'C', 2},
                 {2, 'D', 4},
                 {2, 'B', 5}
                ],
    Ship_Inventory = #{
      1=>[14,15,9,2,6],
      2=>[1,3,4,13],
      3=>[],
      4=>[2,8,11,7],
      5=>[5,10,12]},
    Port_Inventory = #{
      1=>[16,17,18,19,20],
      2=>[21,22,23,24,25],
      3=>[26,27,28,29,30]
     },
    #shipping_state{ships = Ships, containers = Containers, ports = Ports, ship_locations = Locations, ship_inventory = Ship_Inventory, port_inventory = Port_Inventory}.
