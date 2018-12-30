:- module(db,
          [ attach_db_file/1,           % +File
            userid_timezone/2,          % ?UserId, ?TimeZone
            assert_userid_timezone/2,   % +UserId, +TimeZone
            proposal/3 ,                % ?ProposalId, ?UserId, ?Proposal
            assert_proposal/3 ,         % +ProposalId, +UserId, +Proposal
            current_rule/6,             % ?Number, ?Category, ?Title, ?Body, ?UserId, ?AmendmentNumber
            assert_rule/5,                 % +Number, +Category, +Title, +Body, +UserId
            assert_amendment/6,            % +OldNumber, +NewNumber, +NewCategory, +NewTitle, +NewBody, +UserId
            highest_rule_number/1,      % -HighestRuleNumber
            turn/4,                     % ?TurnId, ?UserId, ?Timestamp, ?ProposalId
            assert_turn/4,              % +TurnId, +UserId, +Timestamp, +ProposalId
            usernames_in_order/1,       % ?UserNameList
            assert_token/1,             % +Token
            token/1,                    % ?Token
            vote/3,                     % ?ProposalId, ?UserId, ?Nomination
            assert_vote/3,              % +ProposalId, +UserId, +Nomination
          ]).
:- use_module(library(persistency)).

:- use_module(turn).

:- persistent
   userid_timezone_(
       name:atom,
       timezone:atom
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
       user_id:integer,
       timestamp:integer,
       proposal_id:integer
   ),
   token_(
       token:atom
   ),
   vote_(
       proposal_id:integer,
       user_id:integer,
       nomination:atom
   ).


/*
  File backed database for:
  - players
  - rules
  - turns

  A player is stored as a user_id corresponding to their user id in the Slack
  team and their timezone.

  A rule is stored with:
  - rule number
  - category
  - rule title
  - rule body
  - user id that proposed the rule
  - rule number of a following amendment (or -1 if there is no amendment)
  An amendment is stored as a new rule, with the amended rule updated
  with the number of the amendment. This way a chain of amendments to a rule
  can be followed from the base rule.

  A turn is stored with:
  - turn number
  - player id
  - timestamp
  - proposal id (initially -1)
  A turn goes through the phases:
  - beginning: turn entered into database
  - proposal submitted: update turn with proposal id
  - proposal voted for/against or timeout: turn end,
    enter next turn into database
 */

attach_db_file(File) :-
    db_attach(File, []).

%% current_user_timezone(+UserId, -TimeZone) is nondet.
userid_timezone(UserId, TimeZone) :-
    userid_timezone_(UserId, TimeZone).

assert_userid_timezone(UserId, TimeZoneStr) :-
    atom_string(TimeZone, TimeZoneStr),
    userid_timezone(UserId, TimeZone), !.

assert_userid_timezone(UserId, TimeZoneStr) :-
    % Check TimeZone is valid
    server_utils:current_timezone_offset(TimeZoneStr,_),
    atom_string(TimeZone, TimeZoneStr),
    (  retractall_userid_timezone_(UserId, _),
       assert_userid_timezone_(UserId, TimeZone)).

proposal(ProposalId, UserId, Proposal) :-
    proposalid_title_body_userid(ProposalId, UserId, Proposal).

assert_proposal(ProposalId, UserId, Proposal) :-
    assert_proposalid_title_body_userid(ProposalId, UserId, Proposal).

%% current_rule(?Number, ?Category, ?Title, ?Body, ?UserId, ?AmendmentNumber) is nondet.
current_rule(Number, Category, Title, Body, UserId, AmendmentNumber) :-
    rulenumber_category_title_body_userid_amendmentnumber(
        Number, Category, Title, Body, UserId, AmendmentNumber).

%% add_rule(+Number, +Category, +Title, +Body, +UserId) is det.
assert_rule(Number, Category, Title, Body, UserId) :-
    assert_rulenumber_category_title_body_userid_amendmentnumber(
        Number, Category, Title, Body, UserId, -1).

%% add_amendment(+OldNumber, +NewNumber, +NewCategory, +NewTitle, +NewBody, +UserId) is det.
assert_amendment(OldNumber, NewNumber, NewCategory, NewTitle, NewBody, UserId) :-
    current_rule(OldNumber, OldCategory, OldTitle, OldBody, OldUserId, OldAmendmentNumber),
    OldAmendmentNumber = -1,
    (  retractall_rulenumber_category_title_body_userid_amendmentnumber(
           OldNumber, OldCategory, OldTitle, OldBody, OldUserId, _),
       assert_rulenumber_category_title_body_userid_amendmentnumber(
           OldNumber, OldCategory, OldTitle, OldBody, OldUserId, NewNumber),
       assert_rulenumber_category_title_body_userid_amendmentnumber(
           NewNumber, NewCategory, NewTitle, NewBody, UserId, -1)
    ).

highest_rule_number(HighestRuleNumber) :-
    findall(Number, current_rule(Number,_,_,_,_,_), Rules),
    max_list(Rules, HighestRuleNumber).

assert_turn(TurnId, UserId, Timestamp, ProposalId) :-
    assert_turnid_userid_timestamp_proposalid(TurnId, UserId,
                                              Timestamp, ProposalId).

current_turnid(CurrentTurnId) :-
    findall(Id, turnid_userid_timestamp_proposalid(Id,_,_,_), Ids),
    max_list(Ids, CurrentTurnId).

update_turn_proposal(TurnId, ProposalId) :-
    turnid_userid_timestamp_proposalid(TurnId, UserId, Timestamp, -1),
    retractall_turnid_userid_timestamp_proposalid(TurnId, UserId Timestamp, -1),
    assert_turnid_userid_timestamp_proposalid(TurnId, UserId, Timestamp, ProposalId).

turn(TurnId, UserId, Timestamp, ProposalId) :-
    turnid_userid_timestamp_proposalid(TurnId, UserId, Timestamp, ProposalId).

usernames_in_order(UserNameList) :-
    findall(UserId, userid_timezone_(UserId, _), UserIdList),
    maplist(userid_username, UserIdList, UserNameList).

assert_token(Token) :-
    assert_token_(Token).

token(Token) :-
    token_(Token).

vote(ProposalId, UserId, Nomination) :-
    vote_(ProposalId, UserId, Nomination).

assert_vote(ProposalId, UserId, Nomination) :-
    assert_vote_(ProposalId, UserId, Nomination).
