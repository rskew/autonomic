:- module(proposal_transmute, [
              transmute/3,
              transmute_rule/3
          ]).


/*
 Example use of transmute:

 '/proposal transmute rule number 3 to mutable'
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
transmute(_UserId, CommandText, Message) :-
    \+ phrase(parse_transmute(_RuleNumber, _Title, _Category), CommandText),
    proposal_messages:transmute_help(Message).

% Case: RuleNumber and/or title of rule to amend don't match
transmute(_UserId, CommandText, Message) :-
    once(phrase(parse_transmute(RuleNumber, Title, _Category), CommandText)),
    \+ db:rule(RuleNumber,_,Title,_,_,_),
    proposal_messages:bad_base_rule(RuleNumber, Title, Message).

% Case: Rule to transmute is not the end of a rule chain
transmute(_UserId, CommandText, Message) :-
    once(phrase(parse_transmute(RuleNumber, Title, _Category), CommandText)),
    \+ db:rule(RuleNumber,_,Title,_,_,-1),
    proposal_messages:base_rule_not_transmutable(RuleNumber, Message).

% Case: Invalid Target
transmute(_UserId, CommandText, Message) :-
    once(phrase(parse_transmute(RuleNumber, Title, Category), CommandText)),
    db:rule(RuleNumber, CurrentCategory, Title,_,_,-1),
    \+ ( proposal:rule_category(Category),
         valid_transmute(CurrentCategory, Category) ),
    proposal_messages:transmute_invalid_target(RuleNumber, Category, Message).

% Case: Successful usage of transmute
transmute(UserId, CommandText, Message) :-
    once(phrase(parse_transmute(RuleNumber, Title, Category), CommandText)),
    db:rule(RuleNumber, CurrentCategory, Title,_,_,-1),
    rule_category(Category),
    valid_transmute(CurrentCategory, Category),
    submit_proposal(UserId, transmutement(RuleNumber, Category)),
    proposal:post_proposal_submission(UserId, transmutement(RuleNumber, Category)),
    format(atom(Message), 'Transmutation submitted :-)', []).

parse_transmute(Number, Title, Category) -->
    one_or_more_of_sequence([rule_number(Number), rule_title(TitleCodes)]),
    zero_or_more_of([`transmute `, `to `, `be `, `into `]),
    nonempty_string(CategoryCodes), blanks,
    { ( \+ var(TitleCodes)
      -> atom_codes(Title, TitleCodes)
      ; true ),
      db:rule(Number,_,Title,_,_,_),
      atom_codes(Category, CategoryCodes) }.

valid_transmute(mutable, immutable).
valid_transmute(immutable, mutable).
valid_transmute(inactive, mutable).

transmute_rule(UserId, Number, Category) :-
    db:rule(Number, CurrentCategory,_,_,_,_),
    format(atom(NewTitle),
           'Transmute rule ~w from \'~w\' to \'~w\'',
           [Number, CurrentCategory, Category]),
    proposal:next_rule_number(NewNumber),
    db:assert_amendment(Number, NewNumber, Category, NewTitle, '', UserId).

