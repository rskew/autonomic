:- module(proposal, [active_proposal/1,
                     next_rule_number/1,
                     rule_category/1,
                     mutable/1,
                     action_proposal/1,
                     legacy_proposal/1
                     ]).

:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(db).
:- use_module(proposal_enact).
:- use_module(proposal_amend).
:- use_module(proposal_transmute).
:- use_module(proposal_repeal).
:- use_module(proposal_messages).

% Functors (data constructors) for facts about rule types
rule_category(mutable).
rule_category(immutable).
rule_category(inactive).

mutable(mutable).
mutable(inactive).

% Case: Not the user's turn
parse_proposal(ProposalDict, Message) :-
    \+ db:current_turn(_TurnId, ProposalDict.user_id, _Timestamp, _ProposalId),
    proposal_messages:not_your_turn(Message).

% Case: User has already submitted a proposal this turn
parse_proposal(ProposalDict, Message) :-
    db:current_turn(_TurnId, ProposalDict.user_id, _Timestamp, ProposalId),
    active_proposal(ProposalId),
    proposal_messages:already_submitted_proposal(Message).

% Case: User can submit proposal
parse_proposal(ProposalDict, Message) :-
    with_output_to(user_error, format('ProposalDict: ~w~n',[ProposalDict])),
    db:current_turn(_TurnId, ProposalDict.user_id, _Timestamp, ProposalId),
    \+ active_proposal(ProposalId),
    ( phrase(`enact `, ProposalDict.text, ProposalDict.text)
        -> proposal_enact:enact(ProposalDict.user_id, ProposalDict.text, Message)
    ; phrase(`amend `, ProposalDict.text, ProposalDict.text)
        -> proposal_amend:amend(ProposalDict.user_id, ProposalDict.text, Message)
    ; phrase(`transmute `, ProposalDict.text, ProposalDict.text)
      -> transmute(ProposalDict.user_id, ProposalDict.text, Message)
    ; phrase(`repeal `, ProposalDict.text, ProposalDict.text)
      -> repeal(ProposalDict.user_id, ProposalDict.text, Message)
    ).

active_proposal(ProposalId) :-
    db:current_turn(_CurrentTurnId, _, _, ProposalId),
    ProposalId #\= -1.

/*
  The initial rules specify a different amount of votes required to pass
  a 'legacy proposal'.
 */
% Case: Same title and body as a previous proposal
legacy_proposal(ProposalId) :-
    db:proposal(ProposalId, Title, Body, _),
    db:proposal(PreviousProposalId, Title, Body, _),
    at_least_a_round_of_turns_ago(ProposalId, PreviousProposalId).

% Case: explicitly enact a previous proposal
legacy_proposal(ProposalId) :-
    db:proposal(ProposalId, _, Body, _),
    phrase(enact_previous_proposal(PreviousProposalId, PreviousTitle), Body),
    db:proposal(PreviousProposalId, PreviousTitle, _, _),
    at_least_a_round_of_turns_ago(ProposalId, PreviousProposalId).

at_least_a_round_of_turns_ago(ProposalId, PreviousProposalId) :-
    nomic_utils:number_of_players(NumberOfPlayers),
    ProposalId - PreviousProposalId > NumberOfPlayers.

enact_previous_proposal(ProposalId, _Title) -->
    `Enact proposal `,
    ProposalId.

enact_previous_proposal(_ProposalId, Title) -->
    `Enact proposal `,
    optional(quote),
    Title,
    optional(quote).

submit_proposal(UserId, Proposal) :-
    next_proposal_id(ProposalId),
    db:assert_proposal(ProposalId, UserId, Proposal).

% Case: enactment
action_proposal(ProposalId) :-
    db:proposal(ProposalId, UserId, enactment(Title, Body)),
    proposal_enact:enact_rule(UserId, Title, Body),
    broadcast_new_rule(UserId, Title, Body).

% Case: amendment
action_proposal(ProposalId) :-
    db:proposal(ProposalId, UserId, amendment(RuleNumber, AmendmentTitle, AmendmentBody)),
    proposal_amend:amend_rule(UserId, RuleNumber, AmendmentTitle, AmendmentBody),
    % TODO: print entire amendment chain for rule
    broadcast_new_rule(UserId, AmendmentTitle, AmendmentBody).

% Case: transmutement
action_proposal(ProposalId) :-
    db:proposal(ProposalId, UserId, transmutement(RuleNumber, NewCategory)),
    proposal_transmute:transmute_rule(UserId, RuleNumber, NewCategory),
    % TODO: broadcast rule category
    broadcast_new_rule(UserId, _Title, _Body).

% Case: repealment
action_proposal(ProposalId) :-
    db:proposal(ProposalId, UserId, repealment(RuleNumber)),
    proposal_repeal:repeal_rule(UserId, RuleNumber),
    % Rule category will now be 'inactive'
    broadcast_new_rule(UserId, _Title, _Body).

% TODO: add category to broadcast
broadcast_new_rule(UserId, Title, Body) :-
    slack_app:userid_username(UserId, UserName),
    message_new_rule(UserName, Title, Body, Message),
    slack_app:broadcast_channel(Channel),
    slack_app:post_message(Channel, Message).

message_new_rule(UserName, Title, Body, Message) :-
    format(atom(Message),
           'New rule submitted by ~w:\nTitle: ~w\nBody: ~w~nDon\'t forget to /roll :-)',
           [UserName, Title, Body]).

% TODO post with voting button
post_proposal_submission(UserId, Proposal) :-
    slack_app:userId_username(UserId, UserName),
    slack_app:broadcast_channel(Channel),
    proposal_messages:proposal_submission(UserName, Proposal, Message),
    slack_app:post_message(Channel, Message).

next_rule_number(NewNumber) :-
    db:highest_rule_number(HighestRuleNumber),
    NewNumber #= HighestRuleNumber + 1.
