%%
%% Log macros
%%
-ifndef(__LOG_HRL__).
-define(__LOG_HRL__, true).

%% Lager logging levels
%%   debug, info, notice, warning, error, critical, alert, emergency, none.

-define(DEBUG(Fmt), lager:log(debug, self(), Fmt)).
-define(DEBUG(Fmt, Args), lager:log(debug, self(), Fmt, Args)).

-define(INFO(Fmt), lager:log(info, self(), Fmt)).
-define(INFO(Fmt, Args), lager:log(info, self(), Fmt, Args)).

-define(WARNING(Fmt), lager:log(warning, self() Fmt)).
-define(WARNING(Fmt, Args), lager:log(warning, self(), Fmt, Args)).


-define(ERROR(Fmt), lager:log(error, self(), Fmt)).
-define(ERROR(Fmt, Args), lager:log(error, self(), Fmt, Args)).

-endif.
