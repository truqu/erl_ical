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
                   "DTSTAMP:19900507\r\n"
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
  ?assertEqual( << "BEGIN:VCALENDAR\r\n"
                   "VERSION:2.0\r\n"
                   "PRODID:PRODID\r\n"
                   "NAME:calendar has a name\r\n"
                   "X-WR-CALNAME:calendar has a name\r\n"
                   "BEGIN:VEVENT\r\n"
                   "UID:born\r\n"
                   "DTSTAMP:19900507\r\n"
                   "DTSTART:19900507\r\n"
                   "TRANSP:OPAQUE\r\n"
                   "END:VEVENT\r\n"
                   "END:VCALENDAR\r\n"
                >>
              , encoded(Calendar)
              ).

-spec with_refresh_interval(any()) -> any().
with_refresh_interval(_) ->
  Calendar = erl_ical:with_refresh_interval("P1D", calendar(event())),
  ?assertEqual( << "BEGIN:VCALENDAR\r\n"
                   "VERSION:2.0\r\n"
                   "PRODID:PRODID\r\n"
                   "REFRESH-INTERVAL;VALUE=DURATION:P1D\r\n"
                   "X-PUBLISHED-TTL:P1D\r\n"
                   "BEGIN:VEVENT\r\n"
                   "UID:born\r\n"
                   "DTSTAMP:19900507\r\n"
                   "DTSTART:19900507\r\n"
                   "TRANSP:OPAQUE\r\n"
                   "END:VEVENT\r\n"
                   "END:VCALENDAR\r\n"
                >>
              , encoded(Calendar)
              ).

-spec with_color(any()) -> any().
with_color(_) ->
  Calendar = erl_ical:with_color("yellow", calendar(event())),
  ?assertEqual( << "BEGIN:VCALENDAR\r\n"
                   "VERSION:2.0\r\n"
                   "PRODID:PRODID\r\n"
                   "COLOR:yellow\r\n"
                   "BEGIN:VEVENT\r\n"
                   "UID:born\r\n"
                   "DTSTAMP:19900507\r\n"
                   "DTSTART:19900507\r\n"
                   "TRANSP:OPAQUE\r\n"
                   "END:VEVENT\r\n"
                   "END:VCALENDAR\r\n"
                >>
              , encoded(Calendar)
              ).

-spec with_summary(any()) -> any().
with_summary(_) ->
  Event =
    erl_ical:with_summary( << "this is my summary and it is pretty long, look at this summary!"
                              " Very long, but very informative, too. Let's see if it properly"
                              " splits!"
                           >>
                         , event()
                         ),
  ?assertEqual( << "BEGIN:VCALENDAR\r\n"
                   "VERSION:2.0\r\n"
                   "PRODID:PRODID\r\n"
                   "BEGIN:VEVENT\r\n"
                   "UID:born\r\n"
                   "DTSTAMP:19900507\r\n"
                   "DTSTART:19900507\r\n"
                   "SUMMARY:this is my summary and it is pretty long, look at this summary! Ver\r\n"
                   " y long, but very informative, too. Let's see if it properly splits!\r\n"
                   "TRANSP:OPAQUE\r\n"
                   "END:VEVENT\r\n"
                   "END:VCALENDAR\r\n"
                >>
              , encoded(calendar(Event))
              ).

-spec with_description(any()) -> any().
with_description(_) ->
  Event = erl_ical:with_description(<<"I have described.">>, event()),
  ?assertEqual( << "BEGIN:VCALENDAR\r\n"
                   "VERSION:2.0\r\n"
                   "PRODID:PRODID\r\n"
                   "BEGIN:VEVENT\r\n"
                   "UID:born\r\n"
                   "DTSTAMP:19900507\r\n"
                   "DTSTART:19900507\r\n"
                   "DESCRIPTION:I have described.\r\n"
                   "TRANSP:OPAQUE\r\n"
                   "END:VEVENT\r\n"
                   "END:VCALENDAR\r\n"
                >>
              , encoded(calendar(Event))
              ).

-spec with_url(any()) -> any().
with_url(_) ->
  Event = erl_ical:with_url(<<"https://truqu.com/">>, event()),
  ?assertEqual( << "BEGIN:VCALENDAR\r\n"
                   "VERSION:2.0\r\n"
                   "PRODID:PRODID\r\n"
                   "BEGIN:VEVENT\r\n"
                   "UID:born\r\n"
                   "DTSTAMP:19900507\r\n"
                   "DTSTART:19900507\r\n"
                   "URL;VALUE=URI:https://truqu.com/\r\n"
                   "TRANSP:OPAQUE\r\n"
                   "END:VEVENT\r\n"
                   "END:VCALENDAR\r\n"
                >>
              , encoded(calendar(Event))
              ).

-spec with_repeat(any()) -> any().
with_repeat(_) ->
  Event = erl_ical:with_repeat(yearly, event()),
  ?assertEqual( << "BEGIN:VCALENDAR\r\n"
                   "VERSION:2.0\r\n"
                   "PRODID:PRODID\r\n"
                   "BEGIN:VEVENT\r\n"
                   "UID:born\r\n"
                   "DTSTAMP:19900507\r\n"
                   "DTSTART:19900507\r\n"
                   "RRULE:FREQ=YEARLY;INTERVAL=1;BYMONTH=05;BYMONTHDAY=07\r\n"
                   "TRANSP:OPAQUE\r\n"
                   "END:VEVENT\r\n"
                   "END:VCALENDAR\r\n"
                >>
              , encoded(calendar(Event))
              ).

-spec all_day(any()) -> any().
all_day(_) ->
  Event = erl_ical:all_day(event()),
  ?assertEqual( << "BEGIN:VCALENDAR\r\n"
                   "VERSION:2.0\r\n"
                   "PRODID:PRODID\r\n"
                   "BEGIN:VEVENT\r\n"
                   "UID:born\r\n"
                   "DTSTAMP:19900507\r\n"
                   "DTSTART:19900507\r\n"
                   "DTEND:19900508\r\n"
                   "TRANSP:OPAQUE\r\n"
                   "END:VEVENT\r\n"
                   "END:VCALENDAR\r\n"
                >>
              , encoded(calendar(Event))
              ).

-spec free(any()) -> any().
free(_) ->
  Event = erl_ical:free(event()),
  ?assertEqual( << "BEGIN:VCALENDAR\r\n"
                   "VERSION:2.0\r\n"
                   "PRODID:PRODID\r\n"
                   "BEGIN:VEVENT\r\n"
                   "UID:born\r\n"
                   "DTSTAMP:19900507\r\n"
                   "DTSTART:19900507\r\n"
                   "X-MICROSOFT-CDO-BUSYSTATUS:FREE\r\n"
                   "TRANSP:TRANSPARENT\r\n"
                   "END:VEVENT\r\n"
                   "END:VCALENDAR\r\n"
                >>
              , encoded(calendar(Event))
              ).

%%==============================================================================================
%% Internal functions
%%==============================================================================================

-spec calendar(erl_ical:event()) -> erl_ical:calendar().
calendar(Event) -> erl_ical:calendar("PRODID", [Event]).

-spec encoded(erl_ical:calendar()) -> binary().
encoded(Calendar) -> list_to_binary(erl_ical:encode(Calendar)).

-spec event() -> erl_ical:event().
event() -> erl_ical:event({1990, 5, 7}, "born").

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
