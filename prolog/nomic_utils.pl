:- module(nomic_utils,
          [ number_of_players/1,
            rule_title//1,
            rule_body//1,
            rule_number//1,
            quote//0,
            optional//1,
            case_insensitive//1,
            one_or_more_of_sequence//1,
            one_of//1,
            zero_or_more_of//1
          ]).

:- use_module(library(clpfd)).
:- use_module(db).

number_of_players(NumberOfPlayers) :-
    db:usernames_in_order(UserNameList),
    length(UserNameList, NumberOfPlayers).

rule_title(Title) -->
    case_insensitive(`title`), `:`, blanks,
    optional(quote), string(Title), optional(quote), blanks.

rule_body(Body) -->
    case_insensitive(`body`), `:`, blanks,
    optional(quote), string(Body), optional(quote), blanks.

rule_number(Number) -->
    zero_or_more_of([`rule `, `number `]),
    integer(Number), blanks.

quote --> ( `\'` ; `\`` ; `\"`).

optional(Pattern) --> ( Pattern ; [] ).

case_insensitive([]) --> [].
case_insensitive([X|Xs]) -->
    alpha_to_lower(X),
    case_insensitive(Xs).

one_or_more_of_sequence(Patterns) -->
    sequence_of_optionals(Patterns, Matches),
    { length(Matches, N), N #> 0 }.

sequence_of_optionals([], []) --> [].

sequence_of_optionals([Pattern | Patterns], [Pattern | Matches]) -->
    Pattern,
    sequence_of_optionals(Patterns, Matches).

sequence_of_optionals([_ | Patterns], Matches) -->
    sequence_of_optionals(Patterns, Matches).

one_of([Pattern | Patterns]) -->
    ( Pattern ; one_of(Patterns) ).

zero_or_more_of([]) --> [].
zero_or_more_of([Pattern|Patterns]) -->
    ( Pattern
    -> zero_or_more_of(Patterns)
    ; []
    ).

nonempty_string([Element | Tail]) --> [Element], string(Tail).
