:- module(nomic_test, [
              doesnt_modify_db/1,
              bad_user_config/1
          ]).
% TODO
% - full gameplay scenario? i.e. wrap all unit tests into a roll-play
%   - test the main components in sequence
%   - how to redirect output?

% TODO: :- set_prolog_flag(double_quotes, chars).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_header)).
:- use_module(library(iostream)).
:- use_module(slack_app).
:- use_module(user_config).
:- use_module(db).

%% Start another autonomic instance on a separate port and with a separate
%% test file. This will need to be run in another swipl instance too as only
%% one database file can be used by the persistency library at a time.
setup_tests :-
    process_create(path(rm), ['test.db'], []),
    process_create(path(touch), ['test.db'], []),
    slack_app:init_autonomic(889989, 'test.db').

test_nomic :-
    check_user_config,
    create_players,
    fail.

% Utility for creating http requests in the same format as is
% produced from the http server.
% There's definitely a neater way to do it though :/
http_post_request(Data, Path, Request, Close) :-
    with_output_to(
        atom(RequestData),
        http_post_data(Data, current_output, [])
    ),
    format(atom(FirstLine),
           'POST ~w HTTP/1.1~nHost: rowanskewes.com~w~n',
           [Path, Path]),
    atom_concat(FirstLine, RequestData, RequestRaw),
    format(user_error, 'RequestRaw: ~n~w~n', [RequestRaw]),
    open_any(string(RequestRaw), read, RequestStream, Close, []),
    http_read_request(RequestStream, Request),
    format(user_error, 'Request:~n~w~n', [Request]).

send_user_config(UserId, UserName, Text) :-
    http_post_request(form([user_id = UserId,
                            user_name = UserName,
                            text = Text]),
                      '/user_id',
                      Request,
                      Close),
    once(slack_app:slash_command_handler(
                       user_config:user_config, Request)),
    close_any(Close).

% TODO: make statements about the state of the database
database_empty :-
    \+ db:userid_timezone_score(_, _, _),
    \+ db:proposal(_, _, _),
    \+ db:rule(_, _, _, _, _),
    \+ db:turn(_, _, _, _),
    \+ db:vote(_, _, _).

/*
  A meta-interpreter that fails if the persistent database
  is modified.

  TODO: will this cause lots of backtracking?
        Should it throw an error instead of failing?
 */
doesnt_modify_db(true).
doesnt_modify_db((A,B)) :- !,
    doesnt_modify_db(A),
    doesnt_modify_db(B).
doesnt_modify_db(db:assert_userid_timezone_score(_,_,_)) :- !,
    fail.
doesnt_modify_db(db:assert_proposal(_,_,_)) :- !,
    fail.
doesnt_modify_db(db:assert_rule(_,_,_,_,_)) :- !,
    fail.
doesnt_modify_db(db:assert_turn(_,_,_,_)) :- !,
    fail.
doesnt_modify_db(db:assert_vote(_,_,_)) :- !,
    fail.
doesnt_modify_db(BuiltIn) :-
    predicate_property(BuiltIn, built_in),
    !,
    call(BuiltIn).
doesnt_modify_db(Goal) :-
    clause(Goal, Body),
    doesnt_modify_db(Body).


%% Check that the database is unchanged after malformed /user_config requests
%% Check that the first player to be added to the db begins with a turn
check_user_config :-
    fail.

bad_user_config(UserId) :-
    send_user_config(UserId, thingoname, 'Not a valid timezone right here').

bad_user_config_timezone(_UserId) :-
    fail.

%% Assert the other players into db
create_players :-
    fail.



% first player submits proposals with a correct one submitted
% https://api.slack.com/methods/chat.postMessage


% other players vote


% turn advances when enough votes cast


% next player submits proposals with a correct amend submitted


% other players vote and turn advances


% next player submits proposals with a correct transmute submitted


% other players vote and turn advances


% next player submits proposals with a correct repeal submitted
