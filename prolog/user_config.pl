:- module(user_config, [parse_user_config/3]).
:- use_module(db).


parse_user_config(UserDict, Message) :-
    %split_string(ArgString, ' ', ' ', ["timezone", TimeZone]),
    get_dict(timezone, UserDict, TimeZone),
    ( db:assert_userid_timezone(UserDict.user_id, Timezone)
    -> message_set_timezone(UserDict.user_id, Message)
    ;  db:userid_username(UserDict.user_id, UserDict.user_name.g),
       message_bad_timezone(UserDict.user_id,
                            UserDict.user_name,
                            Timezone,
                            Message)
    ).

parse_user_config(UserId, _, Message) :-
    message_user_config_default(UserId, UserName, Message).

message_user_config_default(
    UserId, UserName,
    [UserDataMessage,
     HelpMessage]) :-
    message_user_data(UserId, UserName, UserDataMessage),
    message_help(HelpMessage).

message_bad_timezone(UserId, UserName, TimeZone,
        [ BadTimeZoneMessage,
          UserDataMessage,
          HelpMessage
        ]) :-
    format(atom(BadTimeZoneMessage), '~w is invalid as a timezone.', [TimeZone]),
    message_user_data(UserId, UserName, UserDataMessage),
    message_help(HelpMessage).

message_set_timezone(UserId, [Message]) :-
    db:userid_timezone(UserId, TimeZone),
    server_utils:current_timezone_offset(TimeZone, Offset),
    format(atom(Message),
           'Time zone successfully set to ~w with an offset of ~w hours',
           [TimeZone, Offset]).

message_help(
    ['To set your timezone use:',
     attachment('/user_config timezone <your_actual_timezone>'),
     'You can find a list of valid timezones at https://en.wikipedia.org/wiki/List_of_tz_database_time_zones']).

message_user_data(UserId, UserName,
                  [Intro, attachment(Text)]) :-
    format(atom(Intro), 'User info for ~w:', [UserName]),
    ( db:userid_timezone(UserId, TimeZone)
    -> format(atom(Text), 'TimeZone: ~w', [TimeZone])
    ; format(atom(Text), 'Error: user not in database', [])
    ).
