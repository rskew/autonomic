:- module(proposal_amend, [
              amend/3,
              amend_rule/4
          ]).

/*
 Example use of the amend slash command:

 '/proposal amend rule number 48 title: 'The silly rule' amendment title: 'The less silly rule' body: The silly rule is less silly now.'
*/
:- use_module(library(dcg/basics)).
:- use_module(proposal_messages).
:- use_module(nomic_utils, [quote//0,
                            rule_title//1,
                            rule_number//1,
                            one_or_more_of_sequence//1,
                            zero_or_more_of//1
                           ]).

% Case: Bad usage, can't be parsed
amend(_UserId, CommandText, Message) :-
    \+ phrase(parse_amend(_Number, _OldTitle, _NewTitle, _AmendmentBody),
              CommandText),
    proposal_messages:amend_help(Message).

% Case: Number and/or title of rule to amend don't match
amend(_UserId, CommandText, Message) :-
    once(phrase(parse_amend(Number, OldTitle, _NewTitle, _AmendmentBody),
                CommandText)),
    \+ db:rule(Number,_,OldTitle,_,_,_),
    proposal_messages:bad_base_rule(Number, OldTitle, Message).

% Case: Target rule is not mutable
amend(_UserId, CommandText, Message) :-
    once(phrase(parse_amend(Number, OldTitle, _NewTitle, _AmendmentBody),
                CommandText)),
    db:rule(Number, Category, OldTitle,_,_,_),
    \+ mutable(Category),
    proposal_messages:base_rule_not_mutable(Number, Message).

% Case: Title of amendment already in use
amend(_UserId, CommandText, Message) :-
    once(phrase(parse_amend(Number, OldTitle, NewTitle, _AmendmentBody),
                CommandText)),
    db:rule(Number, Category, OldTitle,_,_,_),
    mutable(Category),
    db:rule(_,_,NewTitle,_,_,_),
    proposal_messages:title_exists(NewTitle, Message).

% Case: successful usage of amend
amend(UserId, CommandText, Message) :-
    once(phrase(parse_amend(RuleNumber, OldTitle, NewTitle, AmendmentBody),
                CommandText)),
    db:rule(RuleNumber, Category, OldTitle,_,_,_),
    mutable(Category),
    \+ db:rule(_,_,NewTitle,_,_,_),
    submit_proposal(UserId, amendment(RuleNumber, NewTitle, AmendmentBody)),
    proposal:post_proposal_submission(
                 UserId, amendment(RuleNumber, NewTitle, AmendmentBody)),
    format(atom(Message), 'Amendment submitted :-)', []).

parse_amend(Number, OldTitle, NewTitle, AmendmentBody) -->
    optional(`rule`), blanks,
    optional(`with`), blanks,
    optional(`number`), blanks,
    one_or_more_of_sequence([rule_number(Number), rule_title(OldTitleCodes)]),
    zero_or_more_of([`new`, `with`, `amendment`]), blanks,
    ( rule_title(NewTitleCodes) ; { NewTitleCodes = [] } ),
    body(AmendmentBodyCodes),
    { ( \+ var(OldTitleCodes)
      -> atom_codes(OldTitle, OldTitleCodes)
      ; true ),
      db:rule(Number,_,OldTitle,_,_,_),
      atom_codes(NewTitle, NewTitleCodes),
      atom_codes(AmendmentBody, AmendmentBodyCodes) }.

amend_rule(UserId, Number, NewTitle, AmendmentBody) :-
    next_rule_number(NewNumber),
    db:assert_amendment(Number, NewNumber, mutable, NewTitle, AmendmentBody, UserId).
