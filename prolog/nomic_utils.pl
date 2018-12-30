:- module(nomic_utils,
          [ number_of_players/1,
          ]).
use_module(db)

number_of_players(NumberOfPlayers) :-
    db:usernames_in_order(UserNameList),
    length(UserNameList, NumberOfPlayers).
