:- module(proposal_messages, [enact_confirmation/3,
                              enact_help/1,
                              title_exists/2,
                              amend_confirmation/5,
                              amend_help/1,
                              bad_base_rule/3,
                              base_rule_not_mutable/2,
                              transmute_help/1,
                              base_rule_not_transmutable/2,
                              transmute_invalid_target/3,
                              transmute_confirmation/3,
                              repeal_help/1,
                              repeal_confirmation/2,
                              repeal_rule_not_active/2,
                              already_submitted_proposal/1,
                              proposal_submission/3,
                              not_your_turn/1
                             ]).

:- use_module(db).

/*********************
 * Response Messages *
 *********************

 *** A note about strings ***

 They are a bit weird in Prolog, a word can be:
 - A Prolog term
 - An actual string
 - A list of characters
 - A list of character codes

 The following is a good reference:
 http://www.swi-prolog.org/pldoc/man?section=strings
*/

enact_confirmation(_Title, _Body, Message) :-
    Message = 'enact confirmation placeholder'.

enact_help(
    'enact help placeholder'
    ).

title_exists(_Title, 'title exists placeholder').

bad_base_rule(Number, Title, Message) :-
    format(atom(Message),
           'There is no rule with number ~w and title \"~w\"',
           [Number, Title]).

base_rule_not_mutable(Number, Message) :-
    db:rule(Number,_,Title,_,_,_),
    format(atom(Message),
           'Rule number ~w \"~w\" is not mutable',
           [Number, Title]).

amend_help('amend help placeholder').

amend_confirmation(_,_,_,_,'amend confirmation placeholder').

transmute_help('transmute help placeholder').

transmute_bad_base_rule(_Number, _Title, 'transmute bad base rule placeholder').

base_rule_not_transmutable(Number, Message) :-
    db:rule(Number,_,Title,_,_,_),
    format(atom(Message),
           'Rule ~w \"~w\" is not the last rule in its chain',
           [Number, Title]).

transmute_invalid_target(Number, Category, Message) :-
    db:rule(Number,CurrentCategory,_,_,_,_),
    format(atom(Message),
           'Rule ~w with category \'~w\' cannot be transmuted into \'~w\'',
           [Number, CurrentCategory, Category]).

transmute_confirmation(Number, Category, Message) :-
    db:rule(Number,CurrentCategory,_,_,_,_),
    format(atom(Message),
           'Transmute rule ~w with category \'~w\' into \'~w\' confirmation placeholder',
           [Number, CurrentCategory, Category]).

repeal_help('repeal help placeholder').

repeal_rule_not_active(Number, Message) :-
    format(atom(Message),
           'Rule ~w is not active',
           [Number]).

repeal_confirmation(Number, Message) :-
    db:rule(Number,_,Title,_,_,_),
    format(atom(Message),
           'Rule ~w \"~w\" repeal confirmation placeholder',
           [Number, Title]).

not_your_turn(Message) :-
    format(atom(Message),
           'You may only submit a proposal when it is your turn :-)',
           []).

already_submitted_proposal(Message) :-
    format(atom(Message),
           'You have already submitted a proposal this turn :-)',
           []).

proposal_submission(UserName, enact(Title, Body), Message) :-
    format(atom(Message),
           '~w has just submitted a proposal to enact:~n~w~n~w',
           [UserName, Title, Body]).
