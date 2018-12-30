:- module(db,
          [ parse_vote/2,
          ]).
:- use_module(library(dcg/basics)).
:- use_module(library(solution_sequences)).
:- use_module(db).
:- use_module(proposal).

% Case: no active proposal to vote for
parse_vote(VoteDict, Message) :-
    \+ proposal:active_proposal(ProposalId),
    message_no_active_proposal(Message).

% Case: invalid nomination
parse_vote(VoteDict, Message) :-
    proposal:active_proposal(ProposalId),
    \+ phrase(nomination(Nomination), VoteDict.text),
    message_invalid_nomination(Message).

% Case: successful vote
parse_vote(VoteDict, Message) :-
    proposal:active_proposal(ProposalId),
    once(phrase(nomination(Nomination), VoteDict.text)),
    db:vote(ProposalId, VoteDict.user_id, Nomination),
    message_vote_submitted(Message),
    ( voting_complete(ProposalId, Outcome)
    -> close_proposal(ProposalId, Outcome),
       turn:assert_next_turn
    ).

close_proposal(ProposalId, Outcome) :-
    ( Outcome = yes
    -> proposal:action_proposal(ProposalId)
    ),
    post_proposal_closed(ProposalId, Outcome).

message_no_active_proposal(Message) :-
    format(atom(Message),
           'There is currently no proposal to vote for :/',
           []).

nomination(yes) -->
    ( `yes`
    ; `affirmative`
    ; `for`
    ; `yea`
    ; `absolutely`
    ),
    exclamations,
    blanks.
nomination(no) -->
    ( `no`
    ; `negative`
    ; `against`
    ; `nay`
    ; `no way`
    ),
    exclamations,
    blanks.

exclamations -->
    [].
exclamations -->
    `!`, exclamations.

message_invalid_nomination(Message) :-
    format(atom(Message),
           'Your nomination could not be understood :/',
          []).

message_vote_submitted(Message) :-
    format(atom(Message),
           'Your vote has been submitted :D',
           []).

/*
  Voting for a proposal is over when either every person has voted or,
  in the case where a rule has been first submitted more then a full round
  of turns ago (a 'legacy' proposal), it can be passed by a simple majority.
  As soon as a simple majority exists voting can stop.
 */
voting_complete(ProposalId, Outcome) :-
    votes(ProposalId, Votes),
    ( proposal:legacy_proposal(ProposalId)
    -> simple_majority(ProposalId, Votes)
    ; all_votes_submitted(ProposalId, Votes)
    ).

simple_majority(ProposalId, Votes) :-
    nomic_utils:number_of_players(NumberOfPlayers),
    NumberForMajority #= div(NumberOfPlayers, 2) + 1,
    list_to_set(Votes, NominationOptions),
    member(NominationOption, NominationOptions),
    call_nth(member(NominationOption, Votes), NumberForMajority).

all_votes_submitted(ProposalId, Votes) :-
    nomic_utils:number_of_players(NumberOfPlayers),
    length(Votes, NumberOfPlayers).


post_proposal_closed(ProposalId, Outcome) :-
    slack_app:broadcast_channel(Channel),
    db:proposal(ProposalId, Title, Body, UserId),
    slack_app:userid_username(UserId, UserName),
    message_voting_complete(UserName, Title, Body, Outcome, Message),
    slack_app:post_message(Channel, Message).

message_voting_complete(UserName, Title, Body, Outcome, Message) :-
    format(atom(Message),
           'Voting has closed for ~w\'s rule:\nTitle: ~w\nBody: ~w\n'
           'The outcome was: ~w',
          [UserName, Title, Body, Outcome]).
