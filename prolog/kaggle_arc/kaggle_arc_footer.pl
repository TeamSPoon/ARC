:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.


:- set_prolog_flag(verbose_load,false).
:- set_prolog_flag(verbose_autoload,false).

:- fixup_module_exports_now.
:- fixup_exports.

