:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.


:- enable_arc_expansion.

%:- set_prolog_flag(verbose_load,true).  
%:- set_prolog_flag(verbose_autoload,true).

%:- learn_shapes.
:- ensure_loaded(h_muarc_utils).
:- ensure_loaded(h_muarc_ui_ansi).
:- ensure_loaded(h_muarc_deepening).
:- ensure_loaded(h_muarc_typecheck).
:- ensure_loaded(h_muarc_interpreter).
:- ensure_loaded(h_muarc_test_loader).
:- ensure_loaded(h_muarc_domaintypes).
:- ensure_loaded(h_muarc_test_iface).
:- ensure_loaded(h_muarc_explaination).
:- ensure_loaded(h_muarc_howdiff).
:- ensure_loaded(h_muarc_imageproc).
:- ensure_loaded(h_muarc_physics).
:- ensure_loaded(h_muarc_db).
:- ensure_loaded(h_muarc_heuristics).
:- ensure_loaded(h_muarc_intruder).
:- ensure_loaded(h_muarc_test_cache).
:- ensure_loaded(h_muarc_individuation).


:- ensure_loaded(h_muarc_object).
:- ensure_loaded(h_muarc_boards).
:- ensure_loaded(h_muarc_learning).
:- ensure_loaded(h_muarc_imagens).
:- ensure_loaded(h_muarc_recognise).
:- ensure_loaded(h_muarc_uniqueness).
:- ensure_loaded(h_muarc_ui_html).
:- ensure_loaded(h_muarc_test_easy).
:- ensure_loaded(h_muarc_test_old).
:- set_prolog_flag(verbose_load,false).
:- set_prolog_flag(verbose_autoload,false).


