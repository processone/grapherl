%%
%% Log macros
%%
-ifndef(__LOG_HRL__).
-define(__LOG_HRL__, true).

%% Lager logging levels
%%   debug, info, notice, warning, error, critical, alert, emergency, none.

-define(DEBUG(Fmt), lager:debug(Fmt)).
-define(DEBUG(Fmt, Args), lager:debug(Fmt, Args)).
-define(DEBUG(Attrs, Fmt, Args), lager:debug(Attrs, Fmt, Args)).

-define(INFO(Fmt), lager:info(Fmt)).
-define(INFO(Fmt, Args), lager:info(Fmt, Args)).
-define(INFO(Attrs, Fmt, Args), lager:info(Attrs, Fmt, Args)).

-define(notice(Fmt), lager:notice(Fmt)).
-define(notice(Fmt, Args), lager:notice(Fmt, Args)).
-define(notice(Attrs, Fmt, Args), lager:notice(Attrs, Fmt, Args)).

-define(WARNING(Fmt), lager:warning(Fmt)).
-define(WARNING(Fmt, Args), lager:warning(Fmt, Args)).
-define(WARNING(Attrs, Fmt, Args), lager:warning(Attrs, Fmt, Args)).

-define(ERROR(Fmt), lager:error(Fmt)).
-define(ERROR(Fmt, Args), lager:error(Fmt, Args)).
-define(ERROR(Attrs, Fmt, Args), lager:error(Attrs, Fmt, Args)).

-define(CRITICAL(Fmt), lager:critical(Fmt)).
-define(CRITICAL(Fmt, Args), lager:critical(Fmt, Args)).
-define(CRITICAL(Attrs, Fmt, Args), lager:critical(Attrs, Fmt, Args)).

-define(ALERT(Fmt), lager:alert(Fmt)).
-define(ALERT(Fmt, Args), lager:alert(Fmt, Args)).
-define(ALERT(Attrs, Fmt, Args), lager:alert(Attrs, Fmt, Args)).

-define(EMERGENCY(Fmt), lager:emergency(Fmt)).
-define(EMERGENCY(Fmt, Args), lager:emergency(Fmt, Args)).
-define(EMERGENCY(Attrs, Fmt, Args), lager:emergency(Attrs, Fmt, Args)).

-endif.
