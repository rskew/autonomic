:- module(proposal_repeal, [
              repeal/3,
              repeal_rule/2
          ]).

/*
 Example use of repeal:

 /proposal repeal rule number 80 title: the worst of rules everz
*/

:- use_module(library(dcg/basics)).
:- use_module(db).
:- use_module(proposal).
:- use_module(proposal_messages).
:- use_module(nomic_utils, [rule_title//1,
                            rule_number//1,
                            one_or_more_of_sequence//1,
                            zero_or_more_of//1
                           ]).

% Case: Bad usage
repeal(_UserId, CommandText, Message) :-
    \+ phrase(parse_repeal(_RuleNumber, _Title), CommandText),
    proposal_messages:repeal_help(Message).

% Case: RuleNumber and/or title of rule to repeal don't match
repeal(_UserId, CommandText, Message) :-
    once(phrase(parse_repeal(RuleNumber, Title), CommandText)),
    \+ db:rule(RuleNumber, _, Title, _, _, _),
    proposal_messages:bad_base_rule(RuleNumber, Title, Message).

% Case: Rule to repeal is not the end of a rule chain
repeal(_UserId, CommandText, Message) :-
    once(phrase(parse_repeal(RuleNumber, _Title), CommandText)),
    \+ db:rule(RuleNumber, _, _, _, _, -1),
    proposal_messages:base_rule_not_transmutable(RuleNumber, Message).

% Case: Rule is not mutable
repeal(_UserId, CommandText, Message) :-
    once(phrase(parse_repeal(RuleNumber, _Title), CommandText)),
    db:rule(RuleNumber, Category, _, _, _, _),
    \+ proposal:mutable(Category),
    proposal_messages:base_rule_not_mutable(RuleNumber, Message).

% Case: Rule to repeal is not active
repeal(_UserId, CommandText, Message) :-
    once(phrase(parse_repeal(RuleNumber, _Title), CommandText)),
    db:rule(RuleNumber, inactive, _, _, _,-1),
    proposal_messages:repeal_rule_not_active(RuleNumber, Message).

% Case: Successful usage of repeal
repeal(UserId, CommandText, Message) :-
    once(phrase(parse_repeal(RuleNumber, Title), CommandText)),
    db:rule(RuleNumber, mutable, Title, _, _, -1),
    submit_proposal(UserId, repealment(RuleNumber)),
    proposal:post_proposal_submission(UserId, repealment(RuleNumber)),
    format(atom(Message), 'Repealment submitted :-)', []).

parse_repeal(RuleNumber, Title) -->
    one_or_more_of_sequence([rule_number(RuleNumber), rule_title(TitleCodes)]),
    { ( \+ var(TitleCodes)
      -> atom_codes(Title, TitleCodes)
      ; true ),
      db:rule(RuleNumber, _, Title, _, _, _)
    }.

repeal_rule(UserId, Number) :-
    format(atom(NewTitle),
           'Repeal rule ~w',
           [Number]),
    proposal:next_rule_number(NewNumber),
    db:assert_amendment(Number, NewNumber, inactive, NewTitle, '', UserId).
