:- module(proposal_enact, [
              parse_enact//2,
              enact_rule/3
          ]).
:- use_module(library(dcg/basics)).
:- use_module(db).
:- use_module(proposal).
:- use_module(proposal_messages).
:- use_module(nomic_utils, [quote//0]).


/*
 Example use of enact:

 /proposal enact title: 'most general rule' body: "Do things, but only when time"
*/

% Case: bad usage of enact, cannot be parsed
enact(_UserId, CommandText, Message) :-
    \+ phrase(parse_enact(_Title, _Body), CommandText),
    proposal_messages:enact_help(Message).

% Case: Title already in use
enact(_UserId, CommandText, Message) :-
    \+ proposal:active_proposal(_),
    phrase(parse_enact(Title, _Body), CommandText),
    db:rule(_,_,Title,_,_,_),
    proposal_messages:title_exists(Title, Message).

% Case: successful usage of enact
enact(UserId, CommandText, Message) :-
    \+ proposal:active_proposal(_),
    phrase(parse_enact(Title, Body), CommandText),
    \+ db:rule(_,_,Title,_,_,_),
    % TODO present proposal for review with button to submit
    proposal:submit_proposal(UserId, enactment(Title, Body)),
    proposal_messages:enact_confirmation(Title, Body, Message),
    proposal:post_proposal_submission(UserId, enactment(Title, Body)).

% Case: Title and Body
parse_enact(Title, Body) -->
    proposal:rule_title(Title),
    proposal:rule_body(Body).

% Case: No title
parse_enact([], Body) -->
    proposal:rule_body(Body).

enact_rule(UserId, TitleCodes, BodyCodes) :-
    proposal:next_rule_number(NewNumber),
    atom_codes(Title, TitleCodes),
    atom_codes(Body, BodyCodes),
    db:assert_rule(NewNumber, mutable, Title, Body, UserId).
