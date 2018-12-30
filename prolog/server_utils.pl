:- module(server_utils,[iso_format_timestamp/2,
                        current_timezone_offset/2
                       ]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(dcg/basics)).

server(Port) :-
    write('starting server, such fun'),
    http_server(http_dispatch, [port(Port)]).

iso_format_timestamp(Timestamp, DateTimeStr) :-
    get_time(Timestamp),
    SecondsWestOfGreenwich = local,
    stamp_date_time(Timestamp, DateTime, SecondsWestOfGreenwich),
    format_time(atom(DateTimeStr),
                '%FT%T%z',
                DateTime, posix).


%% current_timezone_offset(+TimezoneStr,-Offset) is semidet
current_timezone_offset(TimezoneStr, Offset) :-
    % Check TimezoneStr is valid
    format(atom(TimezonePath),'/usr/share/zoneinfo/~w',TimezoneStr),
    process_create(path(zdump), ['-v',TimezonePath], [stdout(pipe(Pipe0))]),
    read_stream_to_codes(Pipe0,ZDumpCodes),
    \+ once(phrase(all_lines_end_in_NULL,ZDumpCodes)),
    close(Pipe0),
    % Get timezone offset the easy way
    process_create(path(date), ['+%z'], [env(['TZ' = TimezoneStr]),
                                         stdout(pipe(Pipe1))]),
    read_line_to_codes(Pipe1,Codes),
    atom_codes(Line,Codes),
    *with_output_to(user_error, format(Line)),
    atom_number(Line, IsoOffset),
    Minutes is mod(IsoOffset,100),
    Hours is floor(IsoOffset/100),
    Offset is Hours + (Minutes / 60),
    close(Pipe1).

all_lines_end_in_NULL --> ``.
all_lines_end_in_NULL -->
    string_without(`NULL\n`,_),
    `NULL\n`,
    all_lines_end_in_NULL.
