:- module(turn, [parse_turn/3,
                 assert_next_turn/0,
                 next_turnid/2,
               ]).
:- use_module(library(dcg/basics)).
:- use_module(library(uri)).
:- use_module(db).
:- use_module(slack_app).

% Answer request for the turn order with the list of usernames.
parse_turn(TurnDict, Message) :-
    ( phrase(`order`, TurnDict.text, _)
    -> order(Message)
    ; current_turn_player(Message)
    ).

order(Message) :-
    db:usernames_in_order(UserNameList),
    message_turn_order(UserNameList, Message).

message_turn_order(UserNameList, Message) :-
    format(atom(Message),
           'The turn order is:\n ~w',
           [UserNameList]).

% Return the name of the player who's turn it is
current_turn_player(Message) :-
    db:current_turnid(CurrentTurnId),
    db:turn(CurrentTurnId, UserId, Timestamp, _),
    slack_app:userid_username(UserId, UserName),
    message_whos_turn(UserName, Timestamp, Message).

message_whos_turn(UserName, Timestamp, Message) :-
    server_utils:iso_format_datetime(Timestamp, DateTime),
    format(atom(Message),
           'It\'s currently ~w\'s turn, which began at ~w',
           [UserName, DateTime]).

assert_next_turn :-
    db:current_turnid(CurrentTurnId),
    db:turn(CurrentTurnId, CurrentPlayerId, _, _),
    next_player_in_order(CurrentPlayerId, NextPlayerId),
    next_turnid(CurrentTurnId, NextTurnId)
    get_time(Timestamp),
    db:assert_turn(NextTurnId, NextPlayerId, Timestamp, -1),
    post_message_turn_begin(NextPlayerId),
    % Include this goal (remove the '*') to enable timed turns
    *(
        turn_period(TurnPeriodSeconds),
        alarm(TurnPeriodSeconds, assert_next_turn, _)
    ).

turn_period(TurnPeriodSeconds) :-
    TurnPeriodSeconds #= 2 * 24 * 60 * 60.

post_message_turn_begin(NextPlayerId) :-
    slack_app:userid_username(NextPlayerId, NextPlayerName),
    message_advance_turn(NextPlayerName, Message),
    slack_app:broadcast_channel(Channel),
    slack_app:post_message(Channel, Message, _{ok="true"}).

message_advance_turn(UserName, Message) :-
    format(atom(Message),
           'It is now ~w\'s turn!'
           [UserName]).

next_turnid(CurrentTurnId, NextTurnId) :-
    NextTurnId #= CurrentTurnId + 1.

next_player_in_order(PlayerId, FollowingPlayerId) :-
    db:usernames_in_order(UserNamesInOrder),
    nextto(PlayerId, FollowingPlayerId, UserNamesInOrder).
