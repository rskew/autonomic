:- module(slack_app,[post_message/3,
                     userid_username/2,
                     broadcast_channel/1,
                    ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(process)).

:- use_module(server_utils).
:- use_module(db).
:- use_module(user_config).
:- use_module(proposal).
:- use_module(turn).

init_autonomic :-
    server_utils:server(8899),
    db:attach_db_file('nomic.db'),
    % Start the first turn
    once(db:userid_timezone(NextPlayerId,_)),
    NextTurnId #= 0
    db:assert_turn(NextTurnId, NextPlayerId, Timestamp, -1),
    % One turn takes two days
    turn_period(TurnPeriodSeconds),
    % Include this goal to enable a time limit on turns
    *turn_loop(TurnPeriodSeconds).

broadcast_channel(general).

% When a message comes in for a certain endpoint, e.g. someone sends a '/turns'
% command from slack, invoke the general slash_command_handler with the appropriate
% predicate that handles the logic for the specific command.
% E.g. the '/turns' command, we pass the slash command handler with its first
% argument populated with the turns:parse_turns predicate, so that when a http
% message comes in to the '/turns' endpoint (registered in the slack app for the
% '/turns' slash command), slash_command_handler is invoked with turns:parse_turns
% predicate as its first argument and the http message as its second argument.
:- http_handler('/autonomic', slash_command_handler(autonomic_help), []).
:- http_handler('/roll', roll, []).
:- http_handler('/interact', interact, []).
:- http_handler('/user_config', slash_command_handler(user_config:parse_user_config), []).
:- http_handler('/turns', slash_command_handler(turn:parse_turns), []).
:- http_handler('/proposal', slash_command_handler(proposal:parse_proposal), []).
:- http_handler('/vote', slash_command_handler(vote:parse_vote), []).
:- http_handler('/', not_found, [prefix]).

call(CommandLogic, UserId, UserName, ArgsCodes, Message),
autonomic_help(_, UserName, _ Message) :-
    format(atom(Message),
           'Helloooo there ~w, I am autonomic :) I\'m here to help with all the '
           'bookkeeping involved in playing nomic over Slack. I respond to the '
           'following commands:\n'
           '`/user_config` prints your user settings. At '
           'the moment only your timezone is stored. Change your timezone with e.g. `/user_config timezone Atlantic/Reykjavic` according to your timezone in https://en.wikipedia.org/wiki/List_of_tz_database_time_zones.\n'
           '`/turns` prints the user who\'s turn it currently is and when that turn started.\n'
           '`/proposal` enables submitting a proposed rule change when it is your turn. '
           'For example:\n'
           '`/proposal enact title: "absolute rule" body: "This rule is absolute."`\n'
           '`/proposal emend title: "absolute rule" new title: "softened rule" body:"This rule is not as absolute as it might seem."`\n'
           '`/proposal repeal title: "softened rule"`',
           [UserName]).

% for initial development, to be removed
roll_response(
    _{ text: 'Freakin hello from Prolog logic land!',
       attachments: [ _{ text: 'json also a thing',
                         callback_id: 'roller',
                         color: '#3AA3E3',
                         fallback: 'You are unable to choose a game',
                         attachment_type: 'default',
                         actions: [ _{ name: 'show_in_channel',
                                       text: 'Show in channel',
                                       type: 'button',
                                       value: 'show_in_channel'
                                     }
                                  ]
                       }
                    ]
     }).


% for initial development, to be removed
interact_response(
    _{ text: 'Ooh, you clicked the button!',
       attachments: [ _{ text: DateTimeStr,
                         callback_id: 'roller',
                         color: '#3AA3E3',
                         fallback: 'You are unable to choose a game',
                         actions: [ _{ name: 'a_button',
                                       text: 'Fresh Button!',
                                       type: 'button',
                                       value: 'a_button'
                                     },
                                    _{ name: 'options',
                                       text: 'try these ;)',
                                       type: 'select',
                                       options: [ _{ text: 'goof',
                                                     value: 'goof'
                                                   },
                                                  _{ text: 'foog',
                                                     value: 'foog'
                                                   }
                                                ]
                                     }
                                  ]
                       }
                    ],
       response_type: 'in_channel'
     }) :-
    get_time(Timestamp),
    server_utils:iso_format_timestamp(Timestamp, DateTimeStr).


/*************************
 * Http request handlers *
 *************************/

not_found(Request) :-
    with_output_to(user_error, print_term(Request,[])),
    format('Content-type: text/plain~n~n', []),
    format('403 error, nothing at this location :/~n~n', []).


% for initial development, to be removed
roll(Request) :-
    with_output_to(user_error, print_term(Request,[])),
    format('Content-type: application/json~n~n', []),
    roll_response(Response),
    json_write(current_output, Response, []).


% for initial development, to be removed
interact(Request) :-
    http_read_data(Request, Body, []),
    with_output_to(user_error, print_term(Request,[])),
    with_output_to(user_error, print_term(Body,[])),
    interact_response(Response),
    format('Content-type: application/json~n~n', []),
    json_write(current_output, Response, []).


slash_command_handler(CommandLogic, Request) :-
    % Get the request body
    http_read_data(Request, Body, []),
    with_output_to(user_error, format('Request: ~w~n',[Request])),
    with_output_to(user_error, format('Body: ~w~n',[Body])),
    % Get the information of interest from the body
    %messagebody_userid_username_args(
    %    Body, UserId, UserName, ArgsString),
    dict_create(RequestDict, _, Body),
    with_output_to(user_error, format('Dict: ~w~n',[RequestDict])),
    % Do stuff with the info
    %string_codes(ArgsString, ArgsCodes),
    %call(CommandLogic, UserId, UserName, ArgsCodes, Message),
    call(CommandLogic, RequestDict, Message),
    % Convert from list-o-lists to slack message
    build_slack_message(Message, Response),
    % Send the response back to Slack.
    % current_output is routed to the http response channel.
    with_output_to(current_output,
                   format('Content-type: application/json~n~n', [])),
    json_write(current_output, Response, []).


% An interaction can produce a bunch of responses, which are presented
% as message 'attachments' to make them visually separate when displayed in slack.
build_slack_message(Components, WellFormedSlackMessage) :-
    flatten(Components, FlatComponents),
    maplist(make_attachment, FlatComponents, Attachments),
    WellFormedSlackMessage = _{ attachments: Attachments }.

make_attachment(attachment(Text), _{ text: Text }).
make_attachment(Text, _{ pretext: Text }).



%/*
% messagebody_userid_username_args(+HttpMessageBody, ?UserId, ?UserName, ?Args)
%
% This predicate relates an HttpMessage with the values of the 'user_id',
% 'user_name' and 'text' fields.
% It can be used to extract those fields from a received message.
%*/
%% Case: Empty message, stop parsing.
%messagebody_userid_username_args([], _, _, _).
%
%% Case: text field is at the head of the list, grab that and keep parsing.
%messagebody_userid_username_args(
%    [text=Args | RestBody], UserId, UserName, Args) :-
%    !,
%    messagebody_userid_username_args(
%        RestBody, UserId, UserName, _).
%
%% Case: user_id field is at the head of the list, grab that and keep parsing.
%messagebody_userid_username_args(
%    [user_id=UserId | RestBody], UserId, UserName, Args) :-
%    !, messagebody_userid_username_args(
%        RestBody, _, UserName, Args).
%
%% Case: user_name field is at the head of the list, grab that and keep parsing.
%messagebody_userid_username_args(
%    [user_name=UserName | RestBody], UserId, UserName, Args) :-
%    !,
%    messagebody_userid_username_args(
%        RestBody, UserId, _, Args).
%
%% Case: some other field is at the head of the list, keep parsing.
%messagebody_userid_username_args(
%    [_=_ | RestBody], UserId, UserName, Args) :-
%    messagebody_userid_username_args(
%        RestBody, UserId, UserName, Args).


/*
  userid_username(+UserId, ?UserName) is det

  Query Slack to find the current username for a particular userid.
  Documentation for the Slack API: https://api.slack.com/methods/users.info
 */
userid_username(UserId, UserName) :-
    db:token(Token),
    http_post([host('https://slack.com'),
               path('/api/users.info')],
              form([token = Token,
                    user  = UserId]),
              Reply),
    dict_create(ReplyDict, _, Reply),
    _{ok:"true", user_name:UserName} :< ReplyDict.

post_message(Channel, Message) :-
    db:token(Token),
    http_post([host('https://slack.com'),
               path('/api/chat.postMessage')],
              form([token   = Token,
                    channel = Channel,
                    text    = Message]),
              Reply),
    dict_create(ReplyDict, _, Reply),
    _{ok:"true"} :< ReplyDict.
