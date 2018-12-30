:- module(proposal, [parse_proposal/3,
                     parse_enact//2,
                     active_proposal/1,
                     rule_number//1,
                     repeal/3,
                     action_proposal/1,
                     legacy_proposal/1,
                     ]).
:- use_module(library(dcg/basics)).
:- use_module(db).
:- use_module(proposal_messages).


parse_proposal(ProposalDict, Message) :-
    %with_output_to(user_error, format('FullArgCodes: ~w~n',[FullArgCodes])),
    with_output_to(user_error, format('ProposalDict: ~w~n',[ProposalDict])),
    % Verify user is allowed to propose a rule change
    % ...
    % Specifically react to the understood commands.
    % Since the commands have the same signature, it looks like you could
    % instead grab the first word and 'call' it. This would mean calling
    % arbitrary code from an HTTP request, a bit of a security hole.
    % But could you do:
    %   member(ProposalCommand, [enact, amend, transmute, repeal]),
    %   term_string(ProposalCommand, ProposalCommandString)
    %   phrase(ProposalCommandString, FullArgCodes, CommandArgs),
    %   call(ProposalCommand, UserId, CommandArgs, Message).

    ( phrase(`enact `, ProposalDict.text, CommandText)
        -> enact(ProposalDict.user_id, CommandText, Message)
    ; phrase(`amend `, ProposalDict.text, CommandText)
        -> amend(ProposalDict.user_id, CommandText, Message)
    ; phrase(`transmute `, ProposalDict.text, CommandText)
      -> transmute(ProposalDict.user_id, CommandText, Message)
    ; phrase(`repeal `, ProposalDict.text, CommandText)
      -> repeal(ProposalDict.user_id, CommandText, Message)
    ).


/*
 Example use of enact:

 /proposal enact title: 'most general rule' body: "Do things, but only when time"
*/

% Case: bad usage of enact
enact(_UserId, CommandText, Message) :-
    \+ phrase(parse_enact(_Title, _Body), CommandText),
    proposal_messages:enact_help(Message).

% Case: already submitted a proposal this turn
enact(_UserId, _CommandText, Message) :-
    active_proposal(_ProposalId),
    message_second_proposal(Message).

active_proposal(ProposalId) :-
    db:current_turnid(CurrentTurnId),
    db:turn(CurrentTurnId, _, _, ProposalId),
    ProposalId #\= -1.

message_second_proposal(Message) :-
    format(atom(Message),
           'You have already submitted a proposal this turn!',
           []).

% Case: Title already in use
enact(_UserId, CommandText, Message) :-
    \+ active_proposal(_),
    phrase(parse_enact(Title, _Body), CommandText),
    db:current_rule(_,_,Title,_,_,_),
    proposal_messages:title_exists(Title, Message).

% Case: successful usage of enact
enact(UserId, CommandText, Message) :-
    \+ active_proposal(_),
    phrase(parse_enact(Title, Body), CommandText),
    \+ db:current_rule(_,_,Title,_,_,_),
    % TODO button to submit proposal
    submit_proposal(UserId, enact(Title, Body)),
    proposal_messages:enact_confirmation(Title, Body, Message),
    slack_app:userid_username(UserId, UserName),
    post_proposal_submission(UserName, Title, Body).

broadcast_new_rule(UserId, Title, Body) :-
    slack_app:userid_username(UserId, UserName),
    message_new_rule(UserName, Title, Body, Message),
    slack_app:broadcast_channel(Channel),
    slack_app:post_message(Channel, Message).

message_new_rule(UserName, Title, Body, Message) :-
    format(atom(Message),
           'New rule submitted by ~w:\nTitle: ~w\nBody: ~w',
          [UserName, Title, Body]).

% TODO post proposal submission
% - TODO with voting button
post_proposal_submission(UserName, Title, Body) :-
    fail.

enact_rule(UserId, TitleCodes, BodyCodes) :-
    next_rule_number(NewNumber),
    atom_codes(Title, TitleCodes), atom_codes(Body, BodyCodes),
    db:assert_rule(NewNumber, mutable, Title, Body, UserId).

next_rule_number(NewNumber) :-
    db:highest_rule_number(HighestRuleNumber),
    NewNumber is HighestRuleNumber + 1.

parse_enact(Title, Body) -->
    title(Title),
    body(Body).

parse_enact(Title, Body) -->
    quote, string(Title), `:`, quote, ` `, blanks,
    string(Body), blanks.

parse_enact([], Body) -->
    body(Body).

title(Title) -->
    case_insensitive(`title`), `:`, blanks,
    optional(quote), string(Title), optional(quote), blanks.

body(Body) -->
    case_insensitive(`body`), `:`, blanks,
    optional(quote), string(Body), optional(quote), blanks.

case_insensitive([]) --> [].
case_insensitive([X|Xs]) -->
    alpha_to_lower(X),
    case_insensitive(Xs).

quote --> ( `\'` ; `\`` ; `\"`).

optional(Pattern) --> ( Pattern ; [] ).

/*
 Example use of amend:

 '/proposal amend rule number 48 title: 'The silly rule' amendment title: 'The less silly rule' body: The silly rule is less silly now.'
*/

% Case: Bad usage
amend(_UserId, CommandText, Message) :-
    \+ phrase(parse_amend(_Number, _OldTitle, _NewTitle, _AmendmentBody),
              CommandText),
    proposal_messages:amend_help(Message).

% Case: Number and/or title of rule to amend don't match
amend(_UserId, CommandText, Message) :-
    once(phrase(parse_amend(Number, OldTitle, _NewTitle, _AmendmentBody),
                CommandText)),
    \+ db:current_rule(Number,_,OldTitle,_,_,_),
    proposal_messages:bad_base_rule(Number, OldTitle, Message).

% Case: Target rule is not mutable
amend(_UserId, CommandText, Message) :-
    once(phrase(parse_amend(Number, OldTitle, _NewTitle, _AmendmentBody),
                CommandText)),
    db:current_rule(Number, Category, OldTitle,_,_,_),
    \+ mutable(Category),
    proposal_messages:base_rule_not_mutable(Number, Message).

% Case: Title of amendment already in use
amend(_UserId, CommandText, Message) :-
    once(phrase(parse_amend(Number, OldTitle, NewTitle, _AmendmentBody),
                CommandText)),
    db:current_rule(Number, Category, OldTitle,_,_,_),
    mutable(Category),
    db:current_rule(_,_,NewTitle,_,_,_),
    proposal_messages:title_exists(NewTitle, Message).

% Case: successful usage of amend
amend(UserId, CommandText, Message) :-
    once(phrase(parse_amend(Number, OldTitle, NewTitle, AmendmentBody),
                CommandText)),
    db:current_rule(RuleNumber, Category, OldTitle,_,_,_),
    mutable(Category),
    \+ db:current_rule(_,_,NewTitle,_,_,_),
    submit_proposal(UserId, amend(RuleNumber, NewTitle, AmendmentBody)),
    proposal_messages:amend_confirmation(Number, OldTitle, NewTitle, AmendmentBody, Message).

parse_amend(Number, OldTitle, NewTitle, AmendmentBody) -->
    one_or_more_of_sequence([rule_number(Number), title(OldTitleCodes)]),
    zero_or_more_of([`new `, `with `, `amendment `]),
    ( title(NewTitleCodes) ; { NewTitleCodes = [] } ),
    body(AmendmentBodyCodes),
    { ( \+ var(OldTitleCodes)
      -> atom_codes(OldTitle, OldTitleCodes)
      ; true ),
      db:current_rule(Number,_,OldTitle,_,_,_),
      atom_codes(NewTitle, NewTitleCodes),
      atom_codes(AmendmentBody, AmendmentBodyCodes) }.

one_or_more_of_sequence(Patterns) -->
    sequence_of_optionals(Patterns, Matches),
    { length(Matches, N), N #> 0 }.

sequence_of_optionals([], []) --> [].

sequence_of_optionals([Pattern | Patterns], [Pattern | Matches]) -->
    Pattern,
    sequence_of_optionals(Patterns, Matches).

sequence_of_optionals([_ | Patterns], [Matches]) -->
    sequence_of_optionals(Patterns, Matches).

one_of([Pattern | Patterns]) -->
    ( Pattern ; one_of(Patterns) ).

zero_or_more_of(Patterns) -->
    ( one_of(Patterns)
    -> zero_or_more_of(Patterns)
    ; []
    ).

rule_number(Number) -->
    zero_or_more_of([`rule `, `number `]),
    integer(Number), blanks.

mutable(mutable).
mutable(inactive).

amend_rule(UserId, Number, NewTitle, AmendmentBody) :-
    next_rule_number(NewNumber),
    db:assert_amendment(Number, NewNumber, mutable, NewTitle, AmendmentBody, UserId).


/*
 Example use of transmute:

 '/proposal transmute rule number 3 to mutable'
*/

% Case: Bad usage
transmute(_UserId, CommandText, Message) :-
    \+ phrase(parse_transmute(_Number, _Title, _Category), CommandText),
    proposal_messages:transmute_help(Message).

% Case: Number and/or title of rule to amend don't match
transmute(_UserId, CommandText, Message) :-
    once(phrase(parse_transmute(Number, Title, _Category), CommandText)),
    \+ db:current_rule(Number,_,Title,_,_,_),
    proposal_messages:bad_base_rule(Number, Title, Message).

% Case: Rule to transmute is not the end of a rule chain
transmute(_UserId, CommandText, Message) :-
    once(phrase(parse_transmute(Number, Title, _Category), CommandText)),
    \+ db:current_rule(Number,_,Title,_,_,-1),
    proposal_messages:base_rule_not_transmutable(Number, Message).

% Case: Invalid Target
transmute(_UserId, CommandText, Message) :-
    once(phrase(parse_transmute(Number, Title, Category), CommandText)),
    db:current_rule(Number, CurrentCategory, Title,_,_,-1),
    \+ ( rule_category(Category),
         valid_transmute(CurrentCategory, Category) ),
    proposal_messages:transmute_invalid_target(Number, Category, Message).

% Case: Successful usage of transmute
transmute(UserId, CommandText, Message) :-
    once(phrase(parse_transmute(Number, Title, Category), CommandText)),
    db:current_rule(RuleNumber, CurrentCategory, Title,_,_,-1),
    rule_category(Category),
    valid_transmute(CurrentCategory, Category),
    submit_proposal(UserId, transmute(Number, Category)),
    proposal_messages:transmute_confirmation(Number, Category, Message).

parse_transmute(Number, Title, Category) -->
    one_or_more_of_sequence([rule_number(Number), title(TitleCodes)]),
    zero_or_more_of([`transmute `, `to `, `be `, `into `]),
    nonempty_string(CategoryCodes), blanks,
    { ( \+ var(TitleCodes)
      -> atom_codes(Title, TitleCodes)
      ; true ),
      db:current_rule(Number,_,Title,_,_,_),
      atom_codes(Category, CategoryCodes) }.

nonempty_string([Element | Tail]) --> [Element], string(Tail).

rule_category(mutable).
rule_category(immutable).
rule_category(inactive).

valid_transmute(mutable, immutable).
valid_transmute(immutable, mutable).
valid_transmute(inactive, mutable).

transmute_rule(UserId, Number, Category) :-
    db:current_rule(Number, CurrentCategory,_,_,_,_),
    format(atom(NewTitle),
           'Transmute rule ~w from \'~w\' to \'~w\'',
           [Number, CurrentCategory, Category]),
    next_rule_number(NewNumber),
    db:assert_amendment(Number, NewNumber, Category, NewTitle, '', UserId).


/*
 Example use of repeal:

 /proposal repeal rule number 80 title: the worst of rules everz
*/

% Case: Bad usage
repeal(_UserId, CommandText, Message) :-
    \+ phrase(parse_repeal(_Number, _Title), CommandText),
    proposal_messages:repeal_help(Message).

% Case: Number and/or title of rule to repeal don't match
repeal(_UserId, CommandText, Message) :-
    once(phrase(parse_repeal(Number, Title), CommandText)),
    \+ db:current_rule(Number,_,Title,_,_,_),
    proposal_messages:bad_base_rule(Number, Title, Message).

% Case: Rule to repeal is not the end of a rule chain
repeal(_UserId, CommandText, Message) :-
    once(phrase(parse_repeal(Number, _Title), CommandText)),
    \+ db:current_rule(Number,_,_,_,_,-1),
    proposal_messages:base_rule_not_transmutable(Number, Message).

% Case: Rule is not mutable
repeal(_UserId, CommandText, Message) :-
    once(phrase(parse_repeal(Number, _Title), CommandText)),
    db:current_rule(Number, Category,_,_,_,_),
    \+ mutable(Category),
    proposal_messages:base_rule_not_mutable(Number, Message).

% Case: Rule to repeal is not active
repeal(_UserId, CommandText, Message) :-
    once(phrase(parse_repeal(Number, _Title), CommandText)),
    db:current_rule(Number, Category,_,_,_,-1),
    \+ active(Category),
    proposal_messages:repeal_rule_not_active(Number, Message).

% Case: Successful usage of repeal
repeal(UserId, CommandText, Message) :-
    once(phrase(parse_repeal(Number, Title), CommandText)),
    db:current_rule(RuleNumber, Category, Title,_,_,-1),
    mutable(Category),
    active(Category),
    submit_proposal(UserId, repeal(RuleNumber))
    proposal_messages:repeal_confirmation(RuleNumber, Message).

parse_repeal(RuleNumber, Title) -->
    one_or_more_of_sequence([rule_number(RuleNumber), title(TitleCodes)]),
    % TitleCodes will now be bound if the title was part of the message.
    % In this case, convert the char codes to an atom to allow the
    % call to db:current_rule find the number that corresponds with the given
    % rule or to check that the given Number and Title correspond
    % to the same rule.
    % If TitleCodes is unbound (i.e. the message didn't specify the rule title),
    % then the call to db:current_rule will return
    % the title of the rule with number Number.
    { ( \+ var(TitleCodes)
      -> atom_codes(Title, TitleCodes)
      ; true ),
      db:current_rule(RuleNumber,_,Title,_,_,_)
    }.

repeal_rule(UserId, Number) :-
    format(atom(NewTitle),
           'Repeal rule ~w',
           [Number]),
    next_rule_number(NewNumber),
    db:assert_amendment(Number, NewNumber, inactive, NewTitle, '', UserId).

active(mutable).
active(immutable).

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

enact_previous_proposal(ProposalId, Title) -->
    `Enact proposal `,
    optional(quote),
    Title,
    optional(quote).

submit_proposal(UserId, Proposal) :-
    next_proposal_id(ProposalId),
    db:assert_proposal(ProposalId, UserId, Proposal).

% Case: enact
action_proposal(ProposalId) :-
    db:proposal(ProposalId, UserId, enact(Title, Body)),
    enact_rule(UserId, Title, Body),
    broadcast_new_rule(UserId, Title, Body).

% Case: amend
action_proposal(ProposalId) :-
    db:proposal(ProposalId, UserId, amend(RuleNumber, AmendmentTitle, AmendmentBody)),
    amend_rule(UserId, RuleNumber, AmendmentTitle, AmendmentBody),
    % TODO: print entire amendment chain for rule
    broadcast_new_rule(UserId, AmendmentTitle, AmendmentBody).

% Case: transmute
action_proposal(ProposalId) :-
    db:proposal(ProposalId, UserId, transmute(RuleNumber, NewCategory)),
    transmute_rule(UserId, RuleNumber, NewCategory),
    % TODO: add category to broadcast_new_rule
    broadcast_new_rule(UserId, Title, Body).

% Case: repeal
action_proposal(ProposalId) :-
    db:proposal(ProposalId, UserId, repeal(RuleNumber)),
    repeal_rule(UserId, RuleNumber),
    % Rule category will now be 'inactive'
    broadcast_new_rule(UserId, Title, Body).
