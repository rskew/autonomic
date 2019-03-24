:- module(user_config, [user_config/2,
                       parse_user_config//1]).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(func)).
:- use_module(db).

% Case: couldn't parse user_config
user_config(UserDict, Message) :-
    \+ phrase(parse_user_config(_Timezone), atom_codes $ UserDict.text),
    message_user_config_default(UserDict.user_id, UserDict.user_name, Message).

% Case: successful usage or bad timezone
user_config(UserDict, Message) :-
    ( ( phrase(parse_user_config(Timezone), atom_codes $ UserDict.text),
        db:userid_timezone_score(UserDict.user_id, _Timezone, Score),
        db:update_userid_timezone_score(UserDict.user_id, Timezone, Score)
      )
    -> message_set_timezone(UserDict.user_id, Message)
    ; message_bad_timezone(UserDict.user_id,
                           UserDict.user_name,
                           Message)
    ).

parse_user_config(Timezone) -->
    `timezone`, blanks, string(TimezoneCodes),
    { length $ TimezoneCodes #> 0,
      atom_codes(Timezone, TimezoneCodes) }.

message_user_config_default(
    UserId, UserName,
    [UserDataMessage,
     HelpMessage]) :-
    message_user_data(UserId, UserName, UserDataMessage),
    message_help(HelpMessage).

message_bad_timezone(UserId, UserName,
        [BadTimeZoneMessage,
         DefaultMessage]) :-
    format(atom(BadTimeZoneMessage), 'Invalid timezone.', []),
    message_user_config_default(UserId, UserName, DefaultMessage).

message_set_timezone(UserId, [Message]) :-
    db:userid_timezone_score(UserId, TimeZone, _Score),
    server_utils:current_timezone_offset(TimeZone, Offset),
    format(atom(Message),
           'Time zone successfully set to ~w with an offset of ~w hours',
           [TimeZone, Offset]).

message_help(
    ['To set your timezone use:',
     attachment('/user_config timezone _your_actual_timezone_'),
     'You can find a list of valid timezones at https://en.wikipedia.org/wiki/List_of_tz_database_time_zones']).

message_user_data(UserId, UserName,
                  [Intro, attachment(Text)]) :-
    format(atom(Intro), 'User info for ~w:', [UserName]),
    ( db:userid_timezone_score(UserId, TimeZone, Score)
    -> format(atom(Text), 'UserId: ~w~nTimeZone: ~w~nScore: ~w',
              [UserId, TimeZone, Score])
    ; format(atom(Text), 'Error: user ~w not in database', [UserId])
    ).
