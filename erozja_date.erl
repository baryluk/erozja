-module(erozja_date).

-export([to_timestamp/1]).

to_timestamp(D=undefined) ->
	D;
to_timestamp(DateString) ->
	DateTime = case from_date_822(DateString) of
		bad_date ->
			from_date_3339(DateString);
		DT -> DT
	end,
	case DateTime of
		bad_date -> DateTime;
		_ ->
			Timestamp = calendar:datetime_to_gregorian_seconds(DateTime),
			Timestamp
	end.

% rss: RFC 822
% "Fri, 20 Aug 2010 11:42:32 +0000"
% "Fri, 20 Aug 2010 11:42:32 PST"
from_date_822(DateString) ->
	DateTime = httpd_util:convert_request_date(DateString),
	DateTime.

% atom: RFC 3339 section 5.6, it is much simplfied profile of ISO 8601 (in. 8601 there is much more optional elements)
% "1990-12-31T23:59:60Z"
% "1985-04-12T23:20:50.52Z"
% "1996-12-19T16:39:57-08:00"
% "1990-12-31T15:59:60-08:00"
% "1990-12-31T15:59:60-08"
% "1937-01-01T12:00:27.87+00:20"
% "1937-01-01T12:00:27,87+00:20"

from_date_3339([Y1, Y2, Y3, Y4, $-, Mo1, Mo2, $-, D1, D2, $T, H1, H2, $:, Min1, Min2, $:, S1, S2 | TimeOffset]) ->
	try
		Y = list_to_integer([Y1, Y2, Y3, Y4]),
		Mo = list_to_integer([Mo1, Mo2]),
		D = list_to_integer([D1, D2]),
		H = list_to_integer([H1, H2]),
		Min = list_to_integer([Min1, Min2]),
		S = list_to_integer([S1, S2]),
		ok = check_date(Y, Mo, D, H, Min, S),
		% todo: fractions in seconds, time zone
		DateTime = {{Y, Mo, D}, {H, Min, S}},
		DateTime
	catch
		error:_ -> bad_date
	end;
from_date_3339(_) ->
	bad_date.


is_leap_year(Year) when Year > 0, Year < 3000 ->
	(Year rem 4 =:= 0) andalso ( (Year rem 100 =/= 0) or (Year rem 400 =:= 0) ).

max_day(_Year, 1) -> 31;
max_day(Year, 2) ->
	Leap = is_leap_year(Year),
	if Leap -> 29;
	   true -> 28
	end;
max_day(_Year, 3) -> 31;
max_day(_Year, 4) -> 30;
max_day(_Year, 5) -> 31;
max_day(_Year, 6) -> 30;
max_day(_Year, 7) -> 31;
max_day(_Year, 8) -> 31;
max_day(_Year, 9) -> 30;
max_day(_Year, 10) -> 31;
max_day(_Year, 11) -> 30;
max_day(_Year, 12) -> 31.

check_date(Y, Mo, D, H, Min, S) when Y >= 0, Y =< 9999, Mo >= 1, Mo =< 13, D >= 1, D =< 31,
									H >= 0, H =< 23, Min >= 0, Min =< 59, S >= 0, S =< 60 ->
	MaxMo = max_day(Y, Mo),
	if
	  D > MaxMo ->
		bad_date;
	  true ->
		if
		  S =:= 60 ->
			if
				(H =:= 23) and (Min =:= 59) ->
				% leap seconds, are only added at end of day, as 23:59:60
					Leap = have_leap_seconds({Y,Mo,D}),
					if
					  Leap -> ok; % ok, this day is in list of comulative seconds
					  true ->
						if Y < 2010 -> bad_date; % we know it is bad, becuase it is before 2010, and not in DB
						   true ->   % be permisive, if date is after {2010,1,1}, as we cannot really check it
							  if (Mo =:= 6) and (D =:= 30) -> ok; % but make it valid, for end of December and June
							     (Mo =:= 12) and (D =:= 31) -> ok;
							     (Mo =:= 3) and (D =:= 31) -> ok; % end of March and September is another possibility. never used yet.
							     (Mo =:= 9) and (D =:= 30) -> ok;
							     true -> bad_date % it is bad_date with high probability, as IERS always change leap seconds at most 2 times a year
							  end
						end
					end;
				true ->
				% leap seconds at other HH:MM is and error
					bad_date
			end;
		  true ->
			ok
		end
	end;
check_date(_, _, _, _, _, _) ->
	bad_date.

% See ftp://maia.usno.navy.mil/ser7/tai-utc.dat
leap_dates() ->
	DB = [
	{{1972,06,30}, 11},
	{{1972,12,31}, 12},
	{{1973,12,31}, 13},
	{{1974,12,31}, 14},
	{{1975,12,31}, 15},
	{{1976,12,31}, 16},
	{{1977,12,31}, 17},
	{{1978,12,31}, 18},
	{{1979,12,31}, 19},
	{{1981,06,30}, 20},
	{{1982,06,30}, 21},
	{{1983,06,30}, 22},
	{{1985,06,30}, 23},
	{{1987,12,31}, 24},
	{{1989,12,31}, 25},
	{{1990,12,31}, 26},
	{{1992,06,30}, 27},
	{{1993,06,30}, 28},
	{{1994,06,30}, 29},
	{{1995,12,31}, 30},
	{{1997,06,30}, 31},
	{{1998,12,31}, 32},
	{{2005,12,31}, 33},
	{{2008,12,31}, 34}
	],
	DB.

cumullative_leap_seconds(YMD) ->
	% list should be reversed, for faster lookup of newer dates
	case lists:dropwhile(fun(E) -> (E =< YMD) end, leap_dates()) of
		[] ->
			32;
		[{_, X}|_] ->
			X
	end.

have_leap_seconds(YMD) ->
	% list should be reversed, for faster lookup of newer dates
	proplists:is_defined(YMD, leap_dates()).
