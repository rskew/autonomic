:- module(db,
          [ attach_db_file/1,               % +File
            userid_timezone_score/3,        % ?UserId, ?TimeZone, ?Score
            update_userid_timezone_score/3, % +UserId, +TimeZone, +Score
            proposal/3 ,                    % ?ProposalId, ?UserId, ?Proposal
            assert_proposal/3 ,             % +ProposalId, +UserId, +Proposal
            rule/6,                 % ?Number, ?Category, ?Title, ?Body, ?UserId, ?AmendmentNumber
            assert_rule/5,                  % +Number, +Category, +Title, +Body, +UserId
            assert_amendment/6,             % +OldNumber, +NewNumber, +NewCategory, +NewTitle, +NewBody, +UserId
            highest_rule_number/1,          % -HighestRuleNumber
            turn/4,                         % ?TurnId, ?UserId, ?Timestamp, ?ProposalId
            assert_turn/4,                  % +TurnId, +UserId, +Timestamp, +ProposalId
            usernames_in_order/1,           % ?UserNameList
            assert_token/1,                 % +Token
            token/1,                        % ?Token
            vote/3,                         % ?ProposalId, ?UserId, ?Nomination
            assert_vote/3,                  % +ProposalId, +UserId, +Nomination
            current_turn/4
          ]).

/*
  File backed database for:
  - players
  - rules
  - turns

  Basically just a thin interface to the 'persistency' library which backs up
  specified facts to a file when they are asserted into the dynamic database.

  A players user_id corresponds to their Slack team id.
 */

:- use_module(library(persistency)).
:- use_module(server_utils).

:- persistent
   userid_timezone_score_(
       userid:atom,
       timezone:atom,
       score:integer
   ),
   rulenumber_category_title_body_userid_amendmentnumber(
       number:integer,
       category:atom,
       title:atom,
       body:atom,
       userid:atom,
       amendment_id:integer
   ),
   proposalid_userid_proposal(
       proposal_id:integer,
       user_id:atom,
       proposal:compound
   ),
   turnid_userid_timestamp_proposalid(
       turn_id:integer,
       user_id:atom,
       timestamp:float,
       proposal_id:integer
   ),
   token_(
       token:atom
   ),
   vote_(
       proposal_id:integer,
       user_id:atom,
       nomination:atom
   ).

attach_db_file(File) :-
    db_attach(File, []).

%% current_user_timezone(?UserId, ?TimeZone, Score) is nondet.
userid_timezone_score(UserId, TimeZone, Score) :-
    userid_timezone_score_(UserId, TimeZone, Score).

update_userid_timezone_score(UserId, TimeZoneStr, Score) :-
    atom_string(TimeZone, TimeZoneStr),
    userid_timezone_score(UserId, TimeZone, Score),
    !.
update_userid_timezone_score(UserId, TimeZoneStr, Score) :-
    % Check TimeZone is valid
    format(user_error, '~nTimeZoneStr:~n~w~n', [TimeZoneStr]),
    server_utils:current_timezone_offset(TimeZoneStr, _),
    atom_string(TimeZone, TimeZoneStr),
    maplist(nonvar, [UserId, TimeZone, Score]),
    format(user_error, '~w, ~w, ~w', [UserId, TimeZone, Score]),
    (  retractall_userid_timezone_score_(UserId, _, _),
       assert_userid_timezone_score_(UserId, TimeZone, Score)).

proposal(ProposalId, UserId, Proposal) :-
    proposalid_title_body_userid(ProposalId, UserId, Proposal).

assert_proposal(ProposalId, UserId, Proposal) :-
    proposalid_title_body_userid(ProposalId, UserId, Proposal),
    !.
assert_proposal(ProposalId, UserId, Proposal) :-
    assert_proposalid_title_body_userid(ProposalId, UserId, Proposal).

%% rule(?Number, ?Category, ?Title, ?Body, ?UserId, ?AmendmentNumber) is nondet.
rule(Number, Category, Title, Body, UserId, AmendmentNumber) :-
    rulenumber_category_title_body_userid_amendmentnumber(
        Number, Category, Title, Body, UserId, AmendmentNumber).

%% add_rule(+Number, +Category, +Title, +Body, +UserId) is det.
assert_rule(Number, Category, Title, Body, UserId) :-
    rulenumber_category_title_body_userid_amendmentnumber(
        Number, Category, Title, Body, UserId, -1),
    !.
assert_rule(Number, Category, Title, Body, UserId) :-
    assert_rulenumber_category_title_body_userid_amendmentnumber(
        Number, Category, Title, Body, UserId, -1).

%% add_amendment(+OldNumber, +NewNumber, +NewCategory, +NewTitle, +NewBody, +UserId) is det.
assert_amendment(OldNumber, NewNumber, NewCategory, NewTitle, NewBody, UserId) :-
    rule(OldNumber, OldCategory, OldTitle, OldBody, OldUserId, -1),
    maplist(nonvar, [OldNumber, OldCategory, OldTitle, OldBody, OldUserId,
                     NewNumber, NewCategory, NewTitle, NewBody, UserId]),
    ( retractall_rulenumber_category_title_body_userid_amendmentnumber(
          OldNumber, OldCategory, OldTitle, OldBody, OldUserId, _),
      assert_rulenumber_category_title_body_userid_amendmentnumber(
          OldNumber, OldCategory, OldTitle, OldBody, OldUserId, NewNumber),
      assert_rulenumber_category_title_body_userid_amendmentnumber(
          NewNumber, NewCategory, NewTitle, NewBody, UserId, -1)
    ).

highest_rule_number(HighestRuleNumber) :-
    findall(Number, rule(Number,_,_,_,_,_), Rules),
    max_list(Rules, HighestRuleNumber).

assert_turn(TurnId, UserId, Timestamp, ProposalId) :-
    turnid_userid_timestamp_proposalid(TurnId, UserId,
                                       Timestamp, ProposalId),
    !.
assert_turn(TurnId, UserId, Timestamp, ProposalId) :-
    assert_turnid_userid_timestamp_proposalid(TurnId, UserId,
                                              Timestamp, ProposalId).

current_turn(CurrentTurnId, UserId, Timestamp, ProposalId) :-
    findall(Id, turnid_userid_timestamp_proposalid(Id,_,_,_), Ids),
    max_list(Ids, CurrentTurnId),
    turn(CurrentTurnId, UserId, Timestamp, ProposalId).

update_turn_proposal(TurnId, ProposalId) :-
    turnid_userid_timestamp_proposalid(TurnId, UserId, Timestamp, -1),
    maplist(nonvar, [TurnId, UserId, Timestamp, ProposalId]),
    retractall_turnid_userid_timestamp_proposalid(TurnId, UserId, Timestamp, -1),
    assert_turnid_userid_timestamp_proposalid(TurnId, UserId, Timestamp, ProposalId).

turn(TurnId, UserId, Timestamp, ProposalId) :-
    turnid_userid_timestamp_proposalid(TurnId, UserId, Timestamp, ProposalId).

usernames_in_order(UserNameList) :-
    findall(UserId, userid_timezone_score_(UserId, _, _), UserIdList),
    maplist(userid_username, UserIdList, UserNameList).

assert_token(Token) :-
    token_(Token),
    !.
assert_token(Token) :-
    assert_token_(Token).

token(Token) :-
    token_(Token).

vote(ProposalId, UserId, Nomination) :-
    vote_(ProposalId, UserId, Nomination).

assert_vote(ProposalId, UserId, Nomination) :-
    vote_(ProposalId, UserId, Nomination),
    !.
assert_vote(ProposalId, UserId, Nomination) :-
    assert_vote_(ProposalId, UserId, Nomination).
