-module(erl_ical_SUITE).

-include_lib("stdlib/include/assert.hrl").

%% CT Callbacks
-export([all/0]).

%% Tests
-export([ basic/1
        , with_refresh_interval/1
        , with_color/1
        , with_name/1
        , with_summary/1
        , with_description/1
        , with_url/1
        , with_repeat/1
        , all_day/1
        , free/1
        , with_alarm/1
        ]).

-spec all() -> [atom()].
all() ->
  [ basic
  , with_refresh_interval
  , with_color
  , with_name
  , with_summary
  , with_description
  , with_url
  , with_repeat
  , all_day
  , free
  , with_alarm
  ].

%%==============================================================================================
%% Tests
%%==============================================================================================

-spec basic(any()) -> any().
basic(_) ->
  ?assertEqual( << "BEGIN:VCALENDAR\r\n"
                   "VERSION:2.0\r\n"
                   "PRODID:PRODID\r\n"
                   "BEGIN:VEVENT\r\n"
                   "UID:born\r\n"
                   "DTSTAMP:19900507T000000Z\r\n"
                   "DTSTART:19900507\r\n"
                   "TRANSP:OPAQUE\r\n"
                   "END:VEVENT\r\n"
                   "END:VCALENDAR\r\n"
                >>
              , encoded(calendar(event()))
              ).

-spec with_name(any()) -> any().
with_name(_) ->
  Calendar = erl_ical:with_name("calendar has a name", calendar(event())),
  Data = parsed(Calendar),
  ?assertEqual(<<"calendar has a name">>, proplists:get_value(<<"NAME">>, Data)),
  ?assertEqual(<<"calendar has a name">>, proplists:get_value(<<"X-WR-CALNAME">>, Data)).

-spec with_refresh_interval(any()) -> any().
with_refresh_interval(_) ->
  Calendar = erl_ical:with_refresh_interval("P1D", calendar(event())),
  Data = parsed(Calendar),
  ?assertEqual(<<"P1D">>, proplists:get_value(<<"REFRESH-INTERVAL;VALUE=DURATION">>, Data)),
  ?assertEqual(<<"P1D">>, proplists:get_value(<<"X-PUBLISHED-TTL">>, Data)).

-spec with_color(any()) -> any().
with_color(_) ->
  Calendar = erl_ical:with_color("yellow", calendar(event())),
  Data = parsed(Calendar),
  ?assertEqual(<<"yellow">>, proplists:get_value(<<"COLOR">>, Data)).

-spec with_summary(any()) -> any().
with_summary(_) ->
  Summary = << "this is my summary and it is pretty long, look at this summary! Very long, but"
               " very informative, too. Let's see if it properly splits!"
            >>,
  Event = erl_ical:with_summary(Summary, event()),
  Encoded = encoded(calendar(Event)),
  ?assertEqual( << "BEGIN:VCALENDAR\r\n"
                   "VERSION:2.0\r\n"
                   "PRODID:PRODID\r\n"
                   "BEGIN:VEVENT\r\n"
                   "UID:born\r\n"
                   "DTSTAMP:19900507T000000Z\r\n"
                   "DTSTART:19900507\r\n"
                   "SUMMARY:this is my summary and it is pretty long, look at this summary! Ver\r\n"
                   " y long, but very informative, too. Let's see if it properly splits!\r\n"
                   "TRANSP:OPAQUE\r\n"
                   "END:VEVENT\r\n"
                   "END:VCALENDAR\r\n"
                >>
              , Encoded
              ),
  {ok, [{<<"VCALENDAR">>, Data}]} = erl_ical:parse(Encoded),
  EventData = proplists:get_value(<<"VEVENT">>, Data),
  ?assertEqual(Summary, proplists:get_value(<<"SUMMARY">>, EventData)).

-spec with_description(any()) -> any().
with_description(_) ->
  Event = erl_ical:with_description(<<"I have described.">>, event()),
  Data = parsed(calendar(Event)),
  EventData = proplists:get_value(<<"VEVENT">>, Data),
  ?assertEqual(<<"I have described.">>, proplists:get_value(<<"DESCRIPTION">>, EventData)).

-spec with_url(any()) -> any().
with_url(_) ->
  Event = erl_ical:with_url(<<"https://truqu.com/">>, event()),
  Data = parsed(calendar(Event)),
  EventData = proplists:get_value(<<"VEVENT">>, Data),
  ?assertEqual(<<"https://truqu.com/">>, proplists:get_value(<<"URL;VALUE=URI">>, EventData)).

-spec with_repeat(any()) -> any().
with_repeat(_) ->
  Event1 = erl_ical:with_repeat(yearly, event()),
  Data1 = parsed(calendar(Event1)),
  EventData1 = proplists:get_value(<<"VEVENT">>, Data1),
  ?assertEqual( <<"FREQ=YEARLY;INTERVAL=1;BYMONTH=05;BYMONTHDAY=07">>
              , proplists:get_value(<<"RRULE">>, EventData1)
              ),
  Event2 = erl_ical:with_repeat({monthly, 3}, event()),
  Data2 = parsed(calendar(Event2)),
  EventData2 = proplists:get_value(<<"VEVENT">>, Data2),
  ?assertEqual( <<"FREQ=MONTHLY;INTERVAL=3;BYMONTHDAY=07">>
              , proplists:get_value(<<"RRULE">>, EventData2)
              ).

-spec all_day(any()) -> any().
all_day(_) ->
  Event = erl_ical:all_day(event()),
  Data = parsed(calendar(Event)),
  EventData = proplists:get_value(<<"VEVENT">>, Data),
  ?assertEqual(<<"19900507">>, proplists:get_value(<<"DTSTART">>, EventData)),
  ?assertEqual(<<"19900508">>, proplists:get_value(<<"DTEND">>, EventData)).

-spec free(any()) -> any().
free(_) ->
  Event = erl_ical:free(event()),
  Data = parsed(calendar(Event)),
  EventData = proplists:get_value(<<"VEVENT">>, Data),
  ?assertEqual(<<"FREE">>, proplists:get_value(<<"X-MICROSOFT-CDO-BUSYSTATUS">>, EventData)),
  ?assertEqual(<<"TRANSPARENT">>, proplists:get_value(<<"TRANSP">>, EventData)).

-spec with_alarm(any()) -> any().
with_alarm(_) ->
  Alarm = erl_ical:display_alarm(<<"description">>, <<"trigger">>),
  Event = erl_ical:with_alarm(Alarm, event()),
  Data = parsed(calendar(Event)),
  EventData = proplists:get_value(<<"VEVENT">>, Data),
  AlarmData = proplists:get_value(<<"VALARM">>, EventData),
  ?assertEqual(<<"DISPLAY">>, proplists:get_value(<<"ACTION">>, AlarmData)),
  ?assertEqual(<<"trigger">>, proplists:get_value(<<"TRIGGER">>, AlarmData)),
  ?assertEqual(<<"description">>, proplists:get_value(<<"DESCRIPTION">>, AlarmData)).

%%==============================================================================================
%% Internal functions
%%==============================================================================================

-spec calendar(erl_ical:event()) -> erl_ical:calendar().
calendar(Event) -> erl_ical:calendar("PRODID", [Event]).

-spec encoded(erl_ical:calendar()) -> binary().
encoded(Calendar) -> list_to_binary(erl_ical:encode(Calendar)).

-spec parsed(erl_ical:calendar()) -> [KVPair] when KVPair :: {binary(), binary() | [KVPair]}.
parsed(Calendar) ->
  {ok, [{<<"VCALENDAR">>, Data}]} = erl_ical:parse(erl_ical:encode(Calendar)),
  Data.

-spec event() -> erl_ical:event().
event() -> erl_ical:event({1990, 5, 7}, "born").

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
