-module(erl_ical).

-record( calendar
       , { prod_id :: binary()
         , components :: [component(), ...]
         , name = null :: null | binary()
         , refresh_interval = null :: null | binary()
         , color = null :: null | binary()
         }
       ).
-record( event
       , { timestamp :: calendar:datetime1970()
         , start :: calendar:date() | calendar:datetime1970()
         , 'end' = null :: null | calendar:date() | calendar:datetime1970()
         , uid :: binary()
         , summary = null :: null | binary()
         , description = null :: null | binary()
         , repeat = null :: null | repeat()
         , url = null :: null | binary()
         , is_free = false :: boolean()
         , alarms = [] :: [alarm()]
         }
       ).
-record(alarm, {action :: binary(), description :: iodata(), trigger :: binary()}).

-type component() :: alarm() | event().
-type repeat() :: yearly | {monthly, pos_integer()}.

-opaque alarm() :: #alarm{}.
-opaque event() :: #event{}.
-opaque calendar() :: #calendar{}.

-export_type([calendar/0, component/0, event/0, alarm/0, repeat/0]).

%% Building: Calendar
-export([calendar/2, with_name/2, with_refresh_interval/2, with_color/2]).

%% Building: Events
-export([ event/2
        , with_summary/2
        , with_description/2
        , with_repeat/2
        , with_url/2
        , with_alarm/2
        , all_day/1
        , free/1
        ]).

%% Building: Alarms
-export([display_alarm/2]).

%% Encoding
-export([encode/1]).

%% Parsing (TENTATIVE)
-export([parse/1]).

-define(CRLF, "\r\n").

%%==============================================================================================
%% Building: Calendar
%%==============================================================================================

-spec calendar(ProdId :: binary(), Components :: [component(), ...]) -> calendar().
calendar(ProdId, Components) -> #calendar{prod_id = ProdId, components = Components}.

-spec with_name(binary(), calendar()) -> calendar().
with_name(Name, Calendar) -> Calendar#calendar{name = Name}.

-spec with_refresh_interval(binary(), calendar()) -> calendar().
with_refresh_interval(Interval, Calendar) -> Calendar#calendar{refresh_interval = Interval}.

-spec with_color(binary(), calendar()) -> calendar().
with_color(Color, Calendar) -> Calendar#calendar{color = Color}.

%%==============================================================================================
%% Building: Events
%%==============================================================================================

-spec event(TimeStamp :: calendar:date() | calendar:datetime1970(), UId :: binary()) -> event().
event(TimeStamp, UId) -> #event{timestamp = TimeStamp, start = TimeStamp, uid = UId}.

-spec with_summary(binary(), event()) -> event().
with_summary(Summary, Event) -> Event#event{summary = Summary}.

-spec with_description(binary(), event()) -> event().
with_description(Description, Event) -> Event#event{description = Description}.

-spec with_repeat(repeat(), event()) -> event().
with_repeat(Repeat, Event) -> Event#event{repeat = Repeat}.

-spec with_url(binary(), event()) -> event().
with_url(Url, Event) -> Event#event{url = Url}.

-spec with_alarm(alarm(), event()) -> event().
with_alarm(Alarm, Event) -> Event#event{alarms = [Alarm | Event#event.alarms]}.

-spec all_day(event()) -> event().
all_day(Event = #event{start = {_, _, _} = Start}) -> Event#event{'end' = add_days(Start, 1)};
all_day(Event = #event{start = {Date, _}}) ->
  Event#event{start = Date, 'end' = add_days(Date, 1)}.

-spec add_days(calendar:date(), integer()) -> calendar:date().
add_days(Date, Days) ->
  calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + Days).

-spec free(event()) -> event().
free(Event) -> Event#event{is_free = true}.

%%==============================================================================================
%% Building: Alarm
%%==============================================================================================

-spec display_alarm(iodata(), binary()) -> alarm().
display_alarm(Description, Trigger) ->
  #alarm{action = <<"DISPLAY">>, description = Description, trigger = Trigger}.

%%==============================================================================================
%% Parse
%%==============================================================================================

-spec parse(iodata()) -> tql:either([KVPairs], unparseable) when
    KVPairs :: {binary(), binary() | [KVPairs]}.
parse(Data) ->
  {ok, RE} = re:compile("([^:]+):(.*?)\r\n(?!\s)", [dotall]),
  case re:run(Data, RE, [global, {capture, all, binary}, {newline, crlf}]) of
    {match, Lines} ->
      case
        lists:foldr( fun ([_, Key, Value], {Acc, Stack}) ->
                           if
                             Key == <<"END">> -> {[], [Acc | Stack]};
                             Key == <<"BEGIN">> ->
                               [H | T] = Stack,
                               {[{Value, Acc} | H], T};
                             true ->
                               { [{Key, binary:replace(Value, <<"\r\n ">>, <<>>, [global])}
                                  | Acc]
                               , Stack
                               }
                           end
                     end
                   , {[], []}
                   , Lines
                   )
      of
        {Res, []} -> {ok, Res};
        _ -> {error, unparseable}
      end;
    _ -> {error, unparseable}
  end.

%%==============================================================================================
%% Encoding
%%==============================================================================================

-spec encode(calendar()) -> iodata().
encode(Calendar) ->
  [ kvPair("BEGIN", "VCALENDAR")
  , kvPair("VERSION", "2.0")
  , kvPair("PRODID", Calendar#calendar.prod_id)
  , kvPair("NAME", Calendar#calendar.name)
  , kvPair("COLOR", Calendar#calendar.color)
  , kvPair("X-WR-CALNAME", Calendar#calendar.name)
  , kvPair("REFRESH-INTERVAL;VALUE=DURATION", Calendar#calendar.refresh_interval)
  , kvPair("X-PUBLISHED-TTL", Calendar#calendar.refresh_interval)
  , lists:map(fun encode_component/1, Calendar#calendar.components)
  , kvPair("END", "VCALENDAR")
  ].

-spec kvPair(iodata(), iodata()) -> iodata().
kvPair(_, null) -> [];
kvPair(Key, Value) ->
  split_lines([ Key
              , $:
              , binary:replace(iolist_to_binary(Value), <<"\n">>, <<"\\n">>, [global])
              , ?CRLF
              ]).

-spec split_lines(iodata()) -> iodata().
split_lines(Line) ->
  case iolist_size(Line) of
    X when X > 77 ->
      BLine = iolist_to_binary(Line),
      [ binary:part(BLine, 0, 75)
      , ?CRLF
      , split_lines([$\s, binary:part(BLine, 75, byte_size(BLine) - 75)])
      ];
    _ -> Line
  end.

-spec encode_component(component()) -> iodata().
encode_component(Event = #event{}) ->
  [ kvPair("BEGIN", "VEVENT")
  , kvPair("UID", Event#event.uid)
  , kvPair("DTSTAMP", encode_timestamp(Event#event.timestamp))
  , kvPair("DTSTART", encode_time_or_date(Event#event.start))
  , kvPair("DTEND", encode_time_or_date(Event#event.'end'))
  , kvPair("RRULE", encode_repeat(Event#event.repeat, Event#event.start))
  , kvPair("SUMMARY", Event#event.summary)
  , kvPair("DESCRIPTION", Event#event.description)
  , kvPair("URL;VALUE=URI", Event#event.url)
  , kvPair("X-MICROSOFT-CDO-BUSYSTATUS", encode_free(Event#event.is_free))
  , kvPair("TRANSP", encode_transparency(Event#event.is_free))
  , lists:map(fun encode_component/1, Event#event.alarms)
  , kvPair("END", "VEVENT")
  ];
encode_component(Alarm = #alarm{}) ->
  [ kvPair("BEGIN", "VALARM")
  , kvPair("ACTION", Alarm#alarm.action)
  , kvPair("TRIGGER", Alarm#alarm.trigger)
  , kvPair("DESCRIPTION", Alarm#alarm.description)
  , kvPair("END", "VALARM")
  ].

-spec encode_repeat(null | repeat(), TimeStamp) -> null | iodata() when
    TimeStamp :: calendar:date() | calendar:datetime1970().
encode_repeat(null, _) -> null;
encode_repeat(yearly, {_, M, D}) ->
  io_lib:format("FREQ=YEARLY;INTERVAL=1;BYMONTH=~2.10.0B;BYMONTHDAY=~2.10.0B", [M, D]);
encode_repeat({monthly, N}, {_, _, D}) ->
  io_lib:format("FREQ=MONTHLY;INTERVAL=~.10B;BYMONTHDAY=~2.10.0B", [N, D]);
encode_repeat(Repeat, {Date, _}) -> encode_repeat(Repeat, Date).

-spec encode_time_or_date(null) -> null;
                         (calendar:date() | calendar:datetime1970()) -> iodata().
encode_time_or_date(null) -> null;
encode_time_or_date({Y, M, D}) -> io_lib:format("~4.10.0B~2.10.0B~2.10.0B", [Y, M, D]);
encode_time_or_date({{Y, M, D}, {H, I, S}}) ->
  io_lib:format( "~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B~2.10.0BZ"
               , [Y, M, D, H, I, round(S)]
               ).

-spec encode_timestamp(null) -> null;
                      (calendar:date() | calendar:datetime1970()) -> iodata().
encode_timestamp(null) -> null;
encode_timestamp({_, _, _} = Date) -> encode_timestamp({Date, {0, 0, 0}});
encode_timestamp({{Y, M, D}, {H, I, S}}) ->
  io_lib:format( "~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B~2.10.0BZ"
               , [Y, M, D, H, I, round(S)]
               ).

-spec encode_free(boolean()) -> null | iodata().
encode_free(false) -> null;
encode_free(true) -> "FREE".

-spec encode_transparency(boolean()) -> iodata().
encode_transparency(false) -> "OPAQUE";
encode_transparency(true) -> "TRANSPARENT".

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
