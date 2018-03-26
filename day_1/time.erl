%%@doc Time-related utility functions
%%@copyright 2018 Csaba Hoch
-module(time).
-export([hms_to_seconds/3,
         seconds_to_hms/1,
         seconds_to_h/1,
         seconds_to_m/1,
         seconds_to_s/1]).

%% @doc Convert H:M:S into seconds.
-spec hms_to_seconds(non_neg_integer(),
                     non_neg_integer(),
                     non_neg_integer()) -> non_neg_integer().
hms_to_seconds(H, M, S) ->
    H * 3600 + M * 60 + S.

%% @doc Get the "hour" component of the given seconds.
-spec seconds_to_h(non_neg_integer()) -> non_neg_integer().
seconds_to_h(Seconds) ->
    Seconds div 3600.

%% @doc Get the "minutes" component of the given seconds.
-spec seconds_to_m(non_neg_integer()) -> non_neg_integer().
seconds_to_m(Seconds) ->
    (Seconds rem 3600) div 60.

%% @doc Get the "seconds" component of the given seconds.
-spec seconds_to_s(non_neg_integer()) -> non_neg_integer().
seconds_to_s(Seconds) ->
    (Seconds rem 60).

%% @doc Convert seconds into H:M:S.
-spec seconds_to_hms(non_neg_integer()) -> {non_neg_integer(),
                                            non_neg_integer(),
                                            non_neg_integer()}.
seconds_to_hms(Seconds) ->
    {Seconds div 3600,
     (Seconds rem 3600) div 60,
     (Seconds rem 60)}.
