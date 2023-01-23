/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

% INSTRUCTIONS
% =swipl kaggle_arc.pl=
% =:- start_arc_http_server.=
%
% Then navigate to http://localhost:1766 in your browser

/*
:- module(kaggle_arc_ui_html,
  [ start_arc_http_server/0,
    stop_arc_http_server/0
  ]
).
*/
:- include(kaggle_arc_header).

:- use_module(library(debug)).
%:- use_module(library(thread_pool)).
%:- use_module(library(http/thread_httpd)).
%:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_path)).
:- use_module(library(http/websocket)).

:- multifile
    http:location/3.                % Alias, Expansion, Options
:- dynamic
    http:location/3.                % Alias, Expansion, Options

%http:location(icons, root(icons), [ priority(-100) ]).
%http:location(css,   root(css),   [ priority(-100) ]).
%http:location(js,    root(js),    [ priority(-100) ]).
http:location(swish,    root(swish),    [ priority(-100) ]).

:- multifile
    user:file_search_path/2.
:- dynamic
    user:file_search_path/2.

%user:file_search_path(icons, library('http/web/icons')).
%user:file_search_path(css,   library('http/web/css')).
%user:file_search_path(js,    library('http/web/js')).


:- meta_predicate(noisey_debug(0)).
noisey_debug(Goal):- collapsible_section(debug,Goal).

:- meta_predicate(collapsible_section(0)).
collapsible_section(Goal):- collapsible_section(object,Goal).

:- meta_predicate(collapsible_section(+,0)).
collapsible_section(Type,Goal):-
  invent_header(Goal,Title),!,
  collapsible_section(Type,Title,toplevel,Goal).

avacp(Vars):-  prolog_current_choice(Chp),'$attvars_after_choicepoint'(Chp, Vars).

print_title(Var):- (var(Var);Var==[]),!.
print_title([L|List]):- is_list(List), !, print_title(L),write(' '),print_title(List).
print_title(Title):- trim_newlines(pp(Title)).
%print_title(Title):- trim_newlines(ppt(Title)).

:- meta_predicate(collapsible_section(+,+,0)).
collapsible_section(Type,Title,Goal):-
  collapsible_section(Type,Title,toplevel,Goal).

:- meta_predicate(collapsible_section(+,+,+,0)).
/*

memristive device arrays

when NNs first came out , they were oftenj based on trying to emulate the design of physical memristive device arrays
physical memristive device arrays (Neural Networks) were programmed (like in my highschool electronics class in the 1980s) like eeproms.. 
by shining UV light to reinitialize them.  The training routine was that we submitted a latched vector of electrical voltages.. Pushed STORE.. 
latched a new set of voltages pushed STORE etc (Idealy these vortages and resistences were computer from microphone or timed video output ) hoping 
at the end of training the bottem set (the output) of latched voltages ... (that when inverted (inverting the mask patterns) could recreate the input signals 
would be have the correct readings on the ohm meter (as submitted in training))




like for exmaple when i built the speech encoder/recoder in my teens 

collapsible_section(Type,Title,true,Goal):-
  (nb_current('$collapsible_section',Was);Was=[]),
  length(Was,Depth),
  setup_call_cleanup(format('~N~@!mu~w! ~@ |~n',[dash_chars(Depth,' '), Type, print_title(Title)]),
                     locally(b_setval('$collapsible_section',[Type|Was]),wots(S,Goal)), 
                     format('~N~w~@\u00A1mu~w\u00A1~n',[S,dash_chars(Depth,' '), Type])).
*/


collapsible_section(Tag,Title,Showing,Goal):-
  once(nb_current('$collapsible_section',Was);Was=[]), length(Was,Depth),!,wots(Ident,dash_chars(Depth,' ')),
  setup_call_cleanup(format('~N~w!mu~w! ~@ |~n',[Ident, Tag, print_title(Title)]),
                     locally(b_setval('$collapsible_section',[c(Tag)|Was]),
                                      tabbed_print_im(Depth+2,old_write_expandable(Tag=Title,Goal,Showing))), 
                     format('~N~w\u00A1mu~w\u00A1 ',[Ident, Tag])).

with_tagged(Tag,Goal):- 
  once(nb_current('$collapsible_section',Was);Was=[]), length(Was,Depth),!,wots(Ident,dash_chars(Depth,' ')),
  setup_call_cleanup(
    bfly_html_goal(format('~w<~w> ~N',[Ident,Tag])),
    locally(b_setval('$collapsible_section',[h(Tag)|Was]),tabbed_print_im(Depth+2,(Goal))),
    bfly_html_goal(format('~w</~w> ',[Ident,Tag]))).


title_to_html(Title,HtmlTitle):- with_pp(plain,into_attribute(Title,HtmlTitle)),!.


old_write_expandable(Title,Goal,Showing):- 
   setup_call_cleanup(flag('$old_write_expandable_depth',Depth,Depth+1),
   write_csection(Title,Goal,Showing),
   flag('$old_write_expandable_depth',_,Depth)).

expandable_inlines:- expandable_mode(javascript).
expandable_mode(How):- var(How),!,luser_getval(expansion,How).
expandable_mode(How):- luser_getval(expansion,V),!,How==V.

:- luser_default(expansion,javascript).
:- luser_setval(expansion,bfly).

in_expandable(_Title,Goal,_Showing):- expandable_inlines, !, (call_maybe_det(Goal,Det),((Det==true-> ! ; true))).
in_expandable(Title,Goal,Showing):- Showing==always,!,ignore(ppt(Title)),(call_maybe_det(Goal,Det),((Det==true-> ! ; true))).
in_expandable(Title,Goal,_Showing):- flag('$old_write_expandable_depth',X,X), X>2, in_expandable(Title,Goal,always).
in_expandable(Title,Goal,Showing):- (Showing==toplevel;Showing==maybe), flag('$old_write_expandable_depth',X,X), X==1,!,in_expandable(Title,Goal,true).
in_expandable(Title,Goal,Showing):- (Showing==maybe, flag('$old_write_expandable_depth',X,X), X=<2), !, ignore(ppt(Title)),!,in_expandable(Title,Goal,true).
in_expandable(Title,Goal,Showing):- write_csection(Title,Goal,Showing).

call_maybe_det(Goal,Det):- true,call(Goal),deterministic(Det),true.

old_in_expandable(Title,Goal,Showing):- 
 on_xf_ignore_flush(ensure_colapable_styles), 
 (Showing -> PX='128'; PX='600'),
 (Showing -> Exp=''; Exp='panel_hidden'),
  inline_html_format([
   '<pre><button type="button" class="accordian">',Title,' (click to un/expand)</button>',
   '<div class="',write(Exp),'" style="max-height: ',PX,'px">',invoke_arc_cmd(Goal),'</div></pre>']).


format_s(S):- atomic(S),!,format('~w',[S]).
format_s(S):- format('~s',[S]).

tabbed_print_im(_Tab,Goal):- expandable_inlines, !, call(Goal).
tabbed_print_im(Tab,Goal):- Tab2 is Tab, tabbed_print(Tab2,Goal).

:- meta_predicate(trim_newlines(0)).
trim_newlines(Goal):- wots(S,Goal),trim_leading_trailing_whitespace(S,SS),write(SS).
trim_leading_trailing_whitespace(In,Out):-
  split_string(In, " ", "\s\t\n\r",List), 
  atomics_to_string(List,' ',Out).

:- meta_predicate(invent_header(+,-)).
invent_header(Term,Title):- \+ compound(Term),!, Title = goal(Term).
invent_header(Term,Title):- compound_name_arity(Term,F,_),
  (( \+ dumb_functor(Term)) -> (maybe_ia(Term,E),Title=g(F,E));
     (header_arg(Term,E),invent_header(E,Title))).
header_arg(Term,E):- sub_term(E,Term), E\=@=Term, compound(E), \+ is_list(E).
maybe_ia(Term,DT):- header_arg(Term,E), !, \+ dumb_functor(E), data_type(E,DT),!.
maybe_ia(_Term,args).

dumb_functor(Term):- is_list(Term),!, \+ is_grid(Term).
dumb_functor(Term):- predicate_property(Term,meta_predicate(_)),!.
dumb_functor(Term):- compound_name_arity(Term,F,_),atom(F),upcase_atom(F,UC),!,downcase_atom(F,UC).

test_collapsible_section:- 
  collapsible_section(info,
    forall(nth0(N,[a,b,c,d],E),writeln(N=E))).

%test_collapsible_section:- 
     



:- multifile user:file_search_path/2.

%user:file_search_path(document_root,	'/srv/htdocs').
%user:file_search_path(document_root,  AbsolutePath):- arc_sub_path(arc_apps,AbsolutePath).

% user:file_search_path(arc_apps,  AbsolutePath):- arc_sub_path(arc_apps,AbsolutePath).

%:- http_handler('/ARC/', http_reply_from_files(arc_apps, []), [prefix]).


user:file_search_path(arc,  AbsolutePath):- arc_sub_path('.',AbsolutePath).

%:- http_handler('/swish/muarc/swish_config.json', swish_reply_config_root,[priority(200)]).
:- http_handler('/swish/muarc/arcproc_left', arcproc_left, [prefix,chunked,time_limit(30)]).
:- http_handler('/swish/muarc/arcproc_right', arcproc_right, [prefix,time_limit(3600)]). % one hour
:- http_handler('/swish/muarc/arcproc_main', arcproc_main, [prefix,chunked,time_limit(3600)]).
:- http_handler('/swish/muarc/arcproc_iframe', arcproc_iframe, [prefix,chunked,time_limit(120)]).
:- http_handler('/arcproc_left', arcproc_left, [prefix,chunked,time_limit(30)]).
:- http_handler('/arcproc_right', arcproc_right, [prefix,time_limit(3600)]). % one hour
:- http_handler('/arcproc_main', arcproc_main, [prefix,chunked,time_limit(3600)]).
:- http_handler('/arcproc_iframe', arcproc_iframe, [prefix,chunked,time_limit(120)]).
:- http_handler('/swish/muarc/', swish_arc, [prefix]).
:- http_handler('/', swish_arc_root, [prefix]).


start_arc_http_server :-
    catch_log((default_port(Port),start_arc_http_server(Port))),
    catch_log((start_arc_http_server(3020))).

start_arc_http_server(Port):- atom_concat('http@',Port,IDName),thread_property(ID,status(running)),ID==IDName,!.
start_arc_http_server(Port) :-
    catch_log(http_server(http_dispatch, [port(Port)])).

stop_arc_http_server :-
    default_port(Port),
    stop_arc_http_server(Port).
stop_arc_http_server(Port) :-
    http_stop_server(Port, []).

default_port(1766).

%! web_socket_echo(+WebSocket) is nondet.
% This predicate is used to read in a message via websockets and echo it
% back to the client
web_socket_echo(WebSocket) :-
    ws_receive(WebSocket, Message, [format(json)]),
    ( Message.opcode == close
    -> true
    ; get_response_echo(Message.data, Response),
      write("Response: "), writeln(Response),
      ws_send(WebSocket, json(Response)),
      web_socket_echo(WebSocket)
    ).

%! get_response(+Message, -Response) is det.
% Pull the message content out of the JSON converted to a prolog dict
% then add the current time, then pass it back up to be sent to the
% client
get_response_echo(Message, Response) :-
  get_time(Time),
  Response = _{message:Message.message, time: Time}.


%:- http_handler('/swish/arc/user'/User), user(Method, User),[ method(Method), methods([get,post,put]) ]).


%:- http_handler('/favicon.ico', http_reply_file('favicon.ico', []), []).

%http:location(images,	root(images), []).

webui_tests:-
  test_print_tree,
  bfly_tests.

no_web_dbg:-!.
no_web_dbg:-
 nop(((
  unsetenv('DISPLAY'),
  no_xdbg_flags,
  no_x_flags,
  nop((set_prolog_flag(xpce,true)))))).

%:- no_web_dbg.

intern_arc_request_data(Request):-
  intern_request_data(Request),
  save_in_luser(Request).

save_in_luser(NV):- \+ compound(NV),!.
save_in_luser(NV):- is_list(NV),!,must_maplist(save_in_luser,NV),!.
save_in_luser(media(_,_,_,_)):-!.
save_in_luser(NV):- NV=..[N,V],save_in_luser(N,V),!.
save_in_luser(N=V):- save_in_luser(N,V),!.
save_in_luser(NV):- dmsg(not_save_in_luser(NV)),!.


save_in_luser(_,V):- is_list(V),save_in_luser(V).
%save_in_luser(session,V):- !, save_in_luser(V).
save_in_luser(N,V):- decode_luser(V,VV),save_in_luser2(N,VV).

save_in_luser2(task,V):- !, set_current_test(V),get_current_test(CT),dmsg(current_test(V-->CT)).
save_in_luser2(test_suite_name,V):- !, nop(maybe_set_suite(V)).
%save_in_luser2(cmd,V):-  !, ignore(set_test_cmd(V)),!.
save_in_luser2(N,V):- luser_setval(N,V), luser_default(N,V), 
  ignore((is_list(V),last(V,E),compound(E),save_in_luser(V))).

decode_luser(V,O):- url_decode_term(V,VV,_),VV\==V,decode_luser(VV,O),!.
decode_luser(V,O):- atom_to_term_safe(V,VV,_),VV\==V,decode_luser(VV,O),!.
decode_luser(V,V).

begin_arc_html_request(LR,Request):- var(Request), current_predicate(get_http_current_request/1),call(call,get_http_current_request,Request),
  nonvar(Request),!, begin_arc_html_request(LR,Request).
begin_arc_html_request(LR,Request):- 
 notrace(as_if_webui(begin_arc_html(LR,Request))).
begin_arc_html(LR,Request):-
  ignore((member(search(List),Request),member(task=Task,List),  atom_id(Task,ID), nop((dmsg(Task-> ID))), set_current_test(ID))),  
  %ignore((current_output(Out), set_stream(Out,buffer(false)))), flush_output,
%  format('<!DOCTYPE html>',[]),
  ignore((LR==right,write_begin_html('ARC Solver',true))),
  ignore((LR==left, fail,write_begin_html('ARC Solver Menu',inline_to_bfly_html))),
  %ignore((wants_html, (write_begin_html('ARC Solver')))),
  %ignore((wants_html, (intern_arc_request_data(Request)))),

  /* set_stream(Out,close_on_exec(false)),
  set_stream(Out,close_on_abort(true)),
  set_stream(Out,encoding(octet)),
  set_stream(Out,write_errors(ignore)))),*/
  %ignore(set_test_param),
  
  nop(ensure_readable_html),
  flush_output,
  intern_arc_request_data(Request).

set_test_param:-!.
set_test_param:-
  ignore((as_if_webui((get_param_sess(task,Task), Task\=='',  Task\=="",
  atom_id(Task,ID), dmsg(Task-> ID), set_current_test(ID))))),!.

%:- http_handler('/swish', http_redirect(moved, '/swish/'), []).

swish_arc(Request):- swish_arc_root(Request).
%  muarc_tmp:arc_directory(ARC_DIR),
%  http_reply_from_files(ARC_DIR, [unsafe(true), static_gzip(true)], Request).
wfln(_):- !.
wfln(P):- stream_property(X,file_no(2)),nl(X),writeln(X,P),flush_output(X).
%swish_arc_root(Request):-  arc_sub_path('.',DEMO), arc_reply_from_files(DEMO, Request),!.
swish_arc_root(Request):-  wfln(Request), ipe(attempt_file_reply(Request)),!.
swish_arc_root(Request):-  current_predicate(swish_page:swish_reply2/2),
  Options = [], 
  call(call,swish_page:swish_reply2(Options, Request)),!.
swish_arc_root(Request):- ipe(attempt_file_reply(Request)),!.
swish_arc_root(Request):- attempt_file_reply(Request).

ipe(G):- catch(G,E,(is_good_reply(E),throw(E))).

%is_good_reply(E):- wfln(E),fail.
is_good_reply(existence_error(_,_)):-!,fail.
is_good_reply(not_found(_)):-!,fail.
is_good_reply(http_reply(moved_temporary(_))):-!.
is_good_reply(http_reply(not_found(_))):-!,fail.
is_good_reply(http_reply(_)).
is_good_reply(http_reply(_,_)).
is_good_reply(http_reply(_,_,_)).

extra_files_in('/',DEMO):- arc_sub_path('.',DEMO).
extra_files_in(swish,'/opt/logicmoo_workspace/packs_web/swish/web').
%extra_files_in(swish,'/opt/logicmoo_workspace/packs_web/ClioPatria/web').
extra_files_in(www,'/opt/logicmoo_workspace/packs_web/ClioPatria/web').
extra_files_in(plugin,'/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/swish/config-available/web/plugin').
extra_files_in('/','/opt/logicmoo_workspace/packs_sys/logicmoo_nlu/ext/swish/pack/sCASP/prolog/scasp/web').

attempt_file_reply(Request):- 
  wfln(afr(Request)),
  select(path_info(PathInfo), Request, NewRequestP),
  extra_files_in(Swish,Dir),
  chop_begpath(Swish,PathInfo,Rest),
  append([path_info(Rest)], NewRequestP, NewRequest),  
  ipe(arc_reply_from_files(Dir, NewRequest)),!.

attempt_file_reply(Request):- 
  select(path_info(PathInfo), Request, NewRequestP),
  extra_files_in(Swish,_),
  chop_begpath(Swish,PathInfo,Rest),
  extra_files_in(_,Dir),
  append([path_info(Rest)], NewRequestP, NewRequest),  
  ipe(arc_reply_from_files(Dir, NewRequest)),!.

attempt_file_reply(Request):- 
 select(path_info(PathInfo), Request, NewRequestP),
  extra_files_in(Swish,_),
  chop_begpath(Swish,PathInfo,Rest), 
  append([path_info(Rest)], NewRequestP, NewRequest),
  extra_files_in(_,Dir),
  absolute_file_name(Rest, Path, [relative_to(Dir), access(exist)]),
  arc_reply_file(Path, NewRequest),!.

attempt_file_reply(Request):- 
 select(path_info(PathInfo), Request, NewRequestP),
  chop_begpath('/',PathInfo,Rest), 
  append([path_info(Rest)], NewRequestP, NewRequest),
  extra_files_in(_,Dir),
  absolute_file_name(Rest, Path, [relative_to(Dir), access(exist)]),
  arc_reply_file(Path, NewRequest),!.

attempt_file_reply(Request):- 
  extra_files_in(_,Dir),
  ipe(arc_reply_from_files(Dir,Request)),!.

attempt_file_reply(Request):- 
  member(path_info(PathInfo), Request),
  atom_concat('/',Rest,PathInfo),
  extra_files_in(_,Dir), absolute_file_name(Rest, Path, [relative_to(Dir), access(exist)]),
  mime_ext(Path,Ext),
  throw(http_reply(file(Ext,Path),[])).

attempt_file_reply(Request):- 
  member(path_info(PathInfo), Request),
  absolute_file_name(PathInfo, Path),
  mime_ext(Path,Ext),
  throw(http_reply(file(Ext,Path),[])).

attempt_file_reply(Request):- 
  member(path_info(PathInfo), Request),
  extra_files_in(_,Dir), absolute_file_name(PathInfo, Path, [relative_to(Dir), access(exist)]),
  mime_ext(Path,Ext),
  throw(http_reply(file(Ext,Path),[])).


mime_ext(Path,Ext):- file_name_extension(_,Path0,Path),Path0\=='',!,mime_ext(Path0,Ext).
mime_ext(js,'text/javascript'):-!.
mime_ext(txt,'text/plain'):- !.
mime_ext(X,Mime):- mime_ext(X),!,atom_concat('text/',X,Mime).
mime_ext(_,'text/html'):- !.
mime_ext(html).
mime_ext(css).


arc_reply_from_files(Dir, Request):- ipe(http_reply_from_files(Dir, [unsafe(false), static_gzip(true)], Request)).
arc_reply_from_files(Dir, Request):- ipe(http_reply_from_files(Dir, [unsafe(true), static_gzip(true)], Request)).
arc_reply_from_files(Dir, Request):- ipe(http_reply_from_files(Dir, [], Request)).
arc_reply_file(Path,Request):- ipe(http_reply_file(Path, [unsafe(false)], Request)).
arc_reply_file(Path,Request):- ipe(http_reply_file(Path, [unsafe(true)], Request)).
arc_reply_file(Path,Request):- ipe(http_reply_file(Path, [], Request)).

finish_chop(PathInfo,Right):- atom_concat('/',Right,PathInfo),!.
finish_chop(PathInfo,PathInfo).
chop_begpath(Swish,PathInfo,Rest):- atom_concat(Swish,Rest0,PathInfo),finish_chop(Rest0,Rest).
chop_begpath(Swish,PathInfo,Rest):- atom_concat('/',Right,PathInfo),chop_begpath(Swish,Right,Rest).

arcproc_left(Request):- 
 format('Content-type: text/html~n~n',[]),
  %no_web_dbg,
   with_toplevel_pp(http,
    (as_if_webui(((begin_arc_html_request(left,Request),
      arc_html_format([handler_logicmoo_left,write_end_html])))))),!.
arcproc_left(Request):- xlisting_web:handler_logicmoo_cyclone(Request),!.

%arcproc_right(Request):- swish_arc(Request),!.

arc_html_format(TextAndGoal):- wants_html,!,arc_inline_html_format(TextAndGoal).
arc_html_format(TextAndGoal):- bfly_in_out(call(call,inline_html_format(TextAndGoal))).

arc_inline_html_format(Var):- var(Var),!, arc_inline_html_format(writeln(var(Var))).
arc_inline_html_format(Var):- string(Var),!, write(Var).
arc_inline_html_format([H|T]):- is_codelist([H|T]),!,sformat(S,'~s',[[H|T]]),!,arc_inline_html_format(S).
arc_inline_html_format([H|T]):- is_charlist([H|T]),!,sformat(S,'~s',[[H|T]]),!,arc_inline_html_format(S).
arc_inline_html_format(TextAndGoal):- is_list(TextAndGoal),!,maplist(arc_inline_html_format,TextAndGoal).
%arc_inline_html_format(Msg):- flush_output_safe, wdmsg(call(Msg)),fail.
arc_inline_html_format(format(H,V)):-!, format(H,V).
arc_inline_html_format(TextAndGoal):- inline_html_format(TextAndGoal),flush_output_safe.

% arc_find_tests(menu):- ignore(menu).
arc_find_tests(F):- find_tests(F).

:- dynamic(xlisting_whook:offer_testcase/1).
:- multifile(xlisting_whook:offer_testcase/1).
xlisting_whook:offer_testcase(F):- arc_find_tests(F).

handler_logicmoo_left:- handler_logicmoo_menu,!.
handler_logicmoo_menu:-   
 %set_prolog_flag(gui_tracer,false), 
  %ignore(arc_http_nav_menu),
  write_nav('mySideNavL','L','Suite Menu',ignore(full_test_suite_list)),
  write_nav('mySideNavR','R','WebUI Menu',ignore((test_webui_menu,show_tests))),
  !. 

test_webui_menu :- as_if_webui((write_menu_opts('i'))).

write_nav(ID,LR,_Title,Goal):- 
 format('<div id="~w" class="sidenav~w">
  <a href="javascript:void(0)" class="closebtn~w" onclick="toggleNav~w(\'~w\')">&times;</a>',[ID,LR,LR,LR,ID]),
  call(Goal),write('</div>').



set_http_debug_error(Bool):- 
  %set_prolog_flag(gui_tracer,false),
  set_prolog_flag(debug,false),
  set_prolog_flag(gui_tracer,Bool),
  set_prolog_flag(debug_on_error,Bool),
  set_prolog_flag(debug_on_interrupt,Bool),
  set_prolog_flag(determinism_error,silent),
  set_prolog_flag(report_error,Bool),
  nop((Bool==false->nop(set_prolog_flag(on_error,halt));set_prolog_flag(on_error,status))).

echo_file(File):- read_file_to_string(File,Str,[]),write(Str).

:- volatile(wrote_arc_start/1).
:- thread_local(wrote_arc_start/1).
write_arc_start(Where):- var(Where), current_output(Where), write_arc_start(Where).
write_arc_start(Where):- wrote_arc_start(Where),!.
write_arc_start(Where):- asserta(wrote_arc_start(Where)),
  format(Where,'<script type="text/javascript">~@</script>
<style>~@</style>',[echo_file('kaggle_arc_ui_html.js'),echo_file('kaggle_arc_ui_html.css')]).
write_arc_end(Where):- retractall(wrote_arc_start(Where)).
old_write_arc_start:- get_time(Now),Now10M is floor(Now * 10_000_000),
  update_changes, format('<html><head>
  <script type="text/javascript" href="./kaggle_arc_ui_html.js?time=~|~`0t~d~5+"></script>
  <link rel="stylesheet" type="text/css" href="./kaggle_arc_ui_html.css?time=~|~`0t~d~5+">
</head>
<body>',[Now10M,Now10M]).


%map_html_entities_mono(I,O):- atom_codes(O,I),!.
map_html_entities_mono(I,O):- map_html_entities(I,O).

map_html_entities(Code,S):- Code>160, !, sformat(S, '&#~w;',[Code]).
map_html_entities(Code,S):- Code<33, !, sformat(S, '&#~w;',[Code]).
/*
map_html_entities(Code,S):- Code == 124,!,sformat(S, '&#~w;',[Code]).
map_html_entities(Code,S):- Code>255, !, sformat(S, '&#~w;',[Code]).
map_html_entities(62,'&gt;'). map_html_entities(60,'&lt;'). map_html_entities(38,'&amp;'). map_html_entities(32,'&nbsp;').
*/
map_html_entities(Code,S):- name(S,[Code]),!.

bformatw(G):- g_out(bformat(G)).

with_style(S,G):-
 (wants_html -> 
   sccs(format('<span style="~w">',[S]),once(G),write('</span>')) 
    ; call(G)).

html_echo(G)--> [G].

mforeach(Generator, Rule) -->
    foreach(Generator, Rule, []).

:- use_module(library(dcg/high_order),[foreach // 3]).


print_card_list(List):- print_card_list(1,List).
print_card_list(N,List):- with_tag('p',with_tag_class(div,'grow',print_card_l(N,List))).

print_card_l(_,Nil):- Nil==[],!.
print_card_l(N,[H|T]):- print_card_n(N,H), N2 is N+1, print_card_l(N2,T).

print_card_n(N,Var):- \+ callable(Var),!,print_tb_card(print_wrappable(Var),print_wrappable(N)).
print_card_n(N,card(T,B)):- !, print_tb_card(T,(write(N),call(B))).
print_card_n(N,GF):- grid_footer(GF,G,F), is_gridoid(G),!,
  data_type(G,DT), print_tb_card(print_grid(G),wprl([N,DT,F])).

print_card_n(N,G):- is_gridoid(G),!,
  data_type(G,DT), print_tb_card(print_grid(G),wprl([N,DT])).

print_card_n(_,H):- string(H),!,write(H).
print_card_n(_,H):- atom(H),!,write(H).
print_card_n(N,H):- callable_arity(H,0),print_tb_card(call(H),wprl([N,H])).
print_card_n(N,H):- callable_arity(H,1),call(H,R), data_type(R,DT),print_tb_card(wprl([N,H,DT]),R).
print_card_n(N,H):- data_type(H,DT),print_tb_card(pp(H),wprl([N,DT])).


print_tb_card(Top,Bottem):- \+ wants_html, !, sccs(Top,nl_if_needed,wprl(Bottem)),!.

print_tb_card(Top,Bottem):- 
  wots(S,once(Bottem)),
  replace_in_string(['<br>'='\n','"'=' ','<hr>'='\n','<br/>'='\n','\n'=' ','  '=' ','  '=' '],S,RS),
  %RS=S,
  locally(nb_setval(grid_footer,RS),
    (once(Top),
     ignore((nb_current(grid_footer,S),S\==[],write(S))))).
/*
print_tb_card(Top,Bottem):-
  with_tag_class(div,"column, wrapper",
   with_tag_class(div,"card, first_div",
     (call(Top), with_tag_class(div,"container, second_div",print_wrappable(Bottem))))).
*/

print_wrappable(L):- \+ wants_html, !, wprl(L).
print_wrappable(L):- with_tag_class(pre,wrappable,wprl(L)).
wprl(L):- is_list(L),!,maplist(wprl,L).
wprl(H):-  callable_arity(H,0),call(H),write(' ').
wprl(H):-  ppt(H),write(' ').

% width: fit-content
% html_table(ListOfLists):- setup_call_cleanup(write('<table style="width: fit-content;m width: 100%; border: 0px">'), maplist(html_table_row,ListOfLists), write('</table>')),!.
html_table(ListOfLists):- with_tag_class('table','tblo', maplist(html_table_row,ListOfLists)).
html_table_row(ListOfLists):- with_tag('tr',html_table_col(ListOfLists)).
html_table_col(ListOfLists):- is_list(ListOfLists),\+ is_grid(ListOfLists),!,maplist(html_table_col,ListOfLists).
html_table_col(H):- with_tag('td',print_card_n('',H)).


print_grid_html:- arc_grid(Grid),print_grid_html(_SH,_SV,_EH,_EV,Grid).
%print_grid_html(Grid):-print_grid_html(_SH,_SV,_EH,_EV,Grid),!.
%print_grid_html(Name,Grid):- !,html_table([[print_grid(A),print_grid(B)]])
%print_grid_html(Name,Grid):-print_grid(_OH,_OV,Name,Grid),!.
%print_grid_html(SH,SV,EH,EV,Grid):- print_grid_html_old(SH,SV,EH,EV,Grid),!.
%print_grid_html(SH,SV,EH,EV,Grid):- write('<pre>\n'),print_grid_ansi(SH,SV,EH,EV,Grid),!,write('\n').
print_grid_html(SH,SV,EH,EV,Grid):- 
(plain_var(EH) ->grid_size(Grid,EH,_) ; true),ignore(SH=1),
  (plain_var(EV) ->grid_size(Grid,_,EV) ; true),ignore(SV=1),
    bg_sym_ui(BGC),
     with_luser(alt_grid_dot,'_',print_grid_http_bound(BGC,SH,SV,EH,EV,Grid)),!.

has_content(Header):- nonvar(Header), Header\==[], Header\=='', Header\=="".

print_grid_http_bound(BGC,SH,SV,EH,EV,Grid):- 
  grid_to_task_pair(Grid,TaskIDSubTask),
  Width is EH-SH+1,
  TWidthM is Width*20,
  max_min(TWidthM,150,TWidth,_),
  HGHT is EV-SV+5,
  THGHT is HGHT*10,
  (format('<p><table nwidth="~wpx" nheight="~wpx" id="~w" onclick="clickGrid(`~w`)" class="grid_table">',
                      [TWidth,THGHT,TaskIDSubTask,TaskIDSubTask]),
  Header = TaskIDSubTask,
  ignore((
   (has_content(Header)->true;(nb_current(grid_header,Footer),has_content(Header),nb_setval(grid_header,[]))),
    with_tag('tr',format('<th nwidth="~wpx" colspan="~w" style="all: none;" class="wrappable">~w</td>',[TWidth,Width,Header])))),

  print_grid_http_bound(_,BGC,SH,SV,EH,EV,Grid),
  ignore((
   (has_content(Footer)->true;(nb_current(grid_footer,Footer),has_content(Footer),nb_setval(grid_footer,[]))),
    with_tag('tr',format('<th nwidth="~wpx" colspan="~w" style="all: none;" class="wrappable">~w</td>',[TWidth,Width,Footer])))),
  write('</table></p>')),!.


print_grid_http_bound(ID,BGC,SH,SV,EH,EV,Grid):- 
 ignore((nonvar(ID),format('<p><table onclick="clickGrid(`~q`)" class="grid_table">',[ID]))),
 forall(between(SV,EV,V),
    with_tag('tr',((
    forall(between(SH,EH,H),
     (( must_det_ll((once((hv_cg_value(Grid,CG,H,V);CG=BGC)), 
       only_color_data_or_atom(CG,Color),
       into_html_color(Color,HTMLColor))),
         format('<td bgcolor="~w" tooltip="~w">',[HTMLColor,CG]),
          catch(print_hg1(CG),E,writeln(CG=E)),write('</td>')))))))),
 ignore((nonvar(ID),write('</table>'))).


into_html_color(Color,lime):- plain_var(Color),!.
into_html_color(Color,white):- var(Color),!.
into_html_color(bg,'#123').
into_html_color(wbg,'#321').
into_html_color(fg,'#456').
into_html_color(wfg,'#654').
into_html_color(green,'#50D050').
into_html_color(Color,Color).

print_hg1(Wbg):- Wbg==bg, write('&nbsp;').
print_hg1(Wbg):- Wbg==wbg, write('&nbsp;').
print_hg1(Wbg):- is_black(Wbg), write('&nbsp;').
print_hg1(Wbg):- Wbg==' ', write('&nbsp;').
print_hg1(Wbg):- is_fg_color(Wbg),is_real_color(Wbg), write('&nbsp;').
print_hg1(Wbg):- atom(Wbg),atom_concat('#',L,Wbg),L\=='', write(' ').
print_hg1(X):- print_g1(X),!.

/*
pri=======================nt_grid_http_bound(BGC,SH,SV,EH,EV,Grid):- 
  arc_html_format(`<code>tbody td:nth-of-type(odd){ background:rgba(255,255,136,0.5); }</code>`),
   output_html(table([ class([table, 'table-striped']), 
             style('width:auto; margin-left:2em') ],
           [ tr(th(colspan(EH), ['Table for ', 'This'])),
             \ mforeach(between(SV,EV,V),
                      html(tr([ \ mforeach((between(SH,EH,H),once(hv_cg_value(Grid,CG,H,V);CG=BGC), 
                         wots(Cell,(print_g1(cpwui0,CG)))),
                                     html(td([class('mc-10'),style('text-align:center; width:11px;')], html_echo(Cell) ))) ])))
           ])),!.

*/



write_ddm(Title,Goal):- 
 write('<li class="dropdown"><a href="javascript:void(0)" class="dropbtn">'),wqs(Title),
 write('</a><div class="dropdown-content">'),call(Goal),write('</div></li>').
term_to_www_encoding(Goal,A):- with_output_to(string(S),writeq(Goal)),www_form_encode(S,A).

write_http_link(Info,Goal):- nonvar(Goal), %toplevel_pp(PP), %first_current_example_num(ExampleNum),
  get_current_test_atom(TestAtom), %get_current_test(TestID), term_to_www_encoding(TestID,TestAtom), %in_pp(PP),  
  term_to_www_encoding(Goal,CmdAtom),
  sformat(SO,'<a href="?cmd=~w&task=~w" target="_top">~w</a>~n',[CmdAtom,TestAtom,Info]),!,
  our_pengine_output(SO).


write_csection(Goal):- write_csection(Goal,Goal).
write_csection(Title,Goal):- write_csection(Title,Goal,true),!.

write_csection(Title,Goal,Showing):- gensym(accordian_,Sym),
 arc_html_format([
'<input type="checkbox" id="',Sym,'" class="hidecontent"><label for="',Sym,'">',wqs(Title),'</label>',
'<div class="content hidecontent">', call(Goal),
 format('<label for="~w" style="right: 0; position: relative; width: 8px">x</label>',[Sym]),
'</div>']),
 ignore((Showing==true->format('<script>document.getElementById("~w").click();</script>',[Sym]))).

arcproc_iframe(Request):- 
 format('Content-type: text/html~n~n',[]),!,
  set_http_debug_error(true),
  intern_arc_request_data(Request),
 with_toplevel_pp(http,handler_arcproc_iframe(Request)),!.
handler_arcproc_iframe(_Request):-
  sccs(write_arc_start(Where), call_current_arc_cmd(icmd), write_arc_end(Where)).

arcproc_main(Request):- 
 format('Content-type: text/html~n~n',[]),!,
  set_http_debug_error(true),
  intern_arc_request_data(Request),
 with_toplevel_pp(http,handler_arcproc_main(Request)),!.
handler_arcproc_main(_Request):-
  sccs(write_arc_start(Where),(call_current_arc_cmd(cmd)->true;main_no_cmd), write_arc_end(Where)).

arcproc_right(Request):- 
 format('Content-type: text/html~n~n',[]),!,
  update_changes,
  set_http_debug_error(true),
 with_toplevel_pp(http,handler_logicmoo_right(Request)),!.
handler_logicmoo_right(Request):- 
   %set_http_debug_error(false),
   /*current_output(Out),
   current_input(Out),
   open_null_stream(Err),
   set_prolog_IO(In,Out,Err),   */
   %set_html_stream_encoding(utf8),
   %set_stream(Out, encoding(utf8)),  
   intern_arc_request_data(Request),
   write_arc_start(Where),
   handler_logicmoo_menu,
  write('<div id="main">'),  
  write('<span style="font-size:20px;cursor:pointer;color: white; top: 0; left: 0; position: fixed" onclick="toggleNavL(\'mySideNavL\')">&#9776; Test Suites</span>'),
  write_csection("Accordion One Heading",(write('<p>Content for first Accordion.</p>'),write_csection("Accordion Two Heading",write('<p>Content for Second Accordion.</p>'),true)),true),
  with_tag_style('ul','right: 300px; top: 0px', (
    %write_ddm('Suite Menu',ignore(report_suites)),
    %write_ddm('Test Menu',ignore(with_pre(test_webui_menu))),    
    nop((write_ddm('Test Cases',ignore(offer_testcases)),
    write_ddm('Session Info',
      (ignore(show_http_session),
       ignore((get_http_current_request(Request))),
       pp(Request))))))),!,
    write('<span style="font-size:20px;cursor:pointer;color: white" onclick="toggleNavL(\'mySideNavL\')">&#9776; Test Suites</span>'),
    write('<span style="font-size:20px;cursor:pointer;color: white" onclick="toggleNavR(\'mySideNavR\')">&#9776; Task Operations</span>'),
  write('<span style="font-size:20px;cursor:pointer;color: white; top: 0; right: 0; position: fixed" onclick="toggleNavR(\'mySideNavR\')">&#9776; Task Operations</span>'),
   
   %begin_arc_html_request(right,Request),
   %arc_html_format([handler_logicmoo_right,write_end_html]))
   %arc_html_format( 
   write('<style type="text/css">html, body{ font-size: xx-small; background-color: #333; color: white; } td { font-size: small; }</style>'),

   %write_ddm('Test Menu',ignore(test_webui_menu)),
  
  ignore(arc_http_nav_menu),  
  %write('  <iframe id="main" src="arcproc_iframe?" class="main_iframe"/>'),
  ignore(call_current_arc_cmds),
   %ignore(print_test),
   %ignore(show_console_info),   
 %  ignore(call_current_arc_cmds),
   write('<hr>'),
   %write_csection(edit1term),
   %write_csection(show_http_session),
   write('</div>'),
   write_arc_end(Where),
   !.

click_grid:- get_now_cmd(grid,TG),click_grid(TG),!,main_no_cmd.
click_grid:- main_no_cmd.

click_grid(G):- writeq(G),nl,fail.
click_grid(TG):- \+ is_grid(TG),into_grid(TG,G),G\==TG,!,click_grid(G).
click_grid(G):- is_grid(G),print_grid(G),set_current_test(G).


set_html_component(_Name,_Value):-  \+ wants_html,!.
set_html_component(Name,Value):- 
  write_arc_start(Where),
  format(Where,'<script> window.setUrlParam("~w","~w"); window.setComponent("~w","~w");</script>',[Name,Value,Name,Value]).


main_no_cmd:-   
   ignore(show_console_info),   
   ignore(print_test),
   write('<hr>'),
   write_csection(edit1term,edit1term,false),
   show_http_session.

get_now_cmd(Cmd,Prolog):- get_param_req(Cmd,Call),url_decode_term(Call,Prolog).
%get_now_cmd(Cmd,Prolog):- get_http_current_request(Request), member(request_uri(List),Request),request_uri('/arc_web_interface.html?dump_from_pairmode),url_decode_term(Call,Prolog),!.

call_current_arc_cmds_pp:- with_toplevel_pp(http,call_current_arc_cmds).

%call_current_arc_cmds:- get_now_cmd('cmd',Prolog), dmsg(call_current_arc_cmds(cmd)=Prolog), trace, !,invoke_arc_cmd(Prolog).
call_current_arc_cmds:- luser_getval('cmd',Prolog), dmsg(call_current_arc_cmds(cmd)=Prolog), !,invoke_arc_cmd(Prolog).
call_current_arc_cmds:- print_test,!. %,print_all_info_for_test,do_web_menu_key('t'),!.
/*
call_current_arc_cmds:- 
 call_current_arc_cmd(cmd),
 call_current_arc_cmd(cmd2),
 call_current_arc_cmd(footer_cmd).
*/
call_current_arc_cmd(Var):-
   ignore((get_now_cmd(Var,Prolog),        
   dmsg(call_current_arc_cmd(Var)=Prolog),invoke_arc_cmd(Prolog))).



arc_http_nav_menu:- 
  with_pre((get_test_cmd(Prolog),
  print_menu_cmd1(prev_test),
  print_menu_cmd1(print_all_info_for_test),
  print_menu_cmd1((next_test)), 
  print_menu_cmd1(( Prolog)))),!.

pgo(N):- true, into_grid(N,O),!,print_grid(O).

show_console_info:-
  in_pp(PP),pp(in_pp(PP)),!.

/*

arc_script_header:- 
  use_module(library(xlisting/xlisting_web)),
  use_module(library(xlisting/xlisting_web_server)),
  arc_script_header_pt2.

arc_script_header_pt2:- 
  arc_html_format('<meta name="viewport" content="width=device-width, initial-scale=1.0">
<meta name="description" content="Prolog XListing for Logicmoo Code">
<meta name="author" content="logicmoo@gmail.com">
<meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1">
<link rel="stylesheet" type="text/css" href="/swish/css/menu.css">
<link rel="stylesheet" type="text/css" href="/swish/css/cliopatria.css">
<script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>
<script type="text/javascript">window.name="lm_xref"; </script>  
<script data-main="/swish/js/swish" src="/node_modules/requirejs/require.js"></script>
<script type="text/javascript" src="/www/yui/2.7.0/build/utilities/utilities.js"></script>
<script type="text/javascript" src="https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/jquery-ui.min.js"></script>
<script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/jquery.qrcode/1.0/jquery.qrcode.min.js"></script>
<link rel="stylesheet" type="text/css" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">
<link rel="stylesheet" type="text/css" href="https://fonts.googleapis.com/icon?family=Material+Icons">
<script type="text/javascript" src="/swish/lm_xref/pixmapx/popupmenu/scripts/Popup-plugin.js"></script>
<link rel="stylesheet" type="text/css" href="/swish/lm_xref/pixmapx/popupmenu/styles/Popup-plugin.css">
<script type="text/javascript" src="/swish/lm_xref/pixmapx/selected/js/social.selection.js"></script>
<link rel="stylesheet" type="text/css" href="/swish/lm_xref/pixmapx/selected/css/social.selection.css">
<script type="text/javascript" src="/swish/js/cliopatria.js"></script>
<link rel="stylesheet" type="text/css" href="/swish/css/butterfly_term.css">
<script type="text/javascript" href="/swish/js/butterfly_term.js"></script>
<link rel="stylesheet" type="text/css" href="/swish/css/term.css">
<script src="https://cdnjs.cloudflare.com/ajax/libs/json2html/2.1.0/json2html.min.js"></script>').
*/

:- dynamic(was_inline_to_bfly/0).

inline_to_bfly:- was_inline_to_bfly,!.
inline_to_bfly:- asserta(was_inline_to_bfly),inline_to_bfly_html.

inline_to_bfly_html:- toplevel_pp(swish),!,ensure_collapsable_styles.
inline_to_bfly_html:- catch_log(ensure_collapsable_styles),
 arc_html_format(
'<link rel="stylesheet" type="text/css" href="/swish/css/menu.css">
<link rel="stylesheet" type="text/css" href="/swish/css/cliopatria.css">
<script src="https://unpkg.com/gojs@2.2.15/release/go.js"></script>
<script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>
<script type="text/javascript">window.name="lm_xref"; </script>
<script type="text/javascript" src="https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/jquery-ui.min.js"></script>
<script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/jquery.qrcode/1.0/jquery.qrcode.min.js"></script>

<script type="text/javascript" src="/www/yui/2.7.0/build/utilities/utilities.js"></script>
<script type="text/javascript" src="/www/yui/2.7.0/build/datasource/datasource.js"></script>
<script type="text/javascript" src="/www/yui/2.7.0/build/autocomplete/autocomplete.js"></script>

<link rel="stylesheet" type="text/css" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">
<link rel="stylesheet" type="text/css" href="https://fonts.googleapis.com/icon?family=Material+Icons">
<script type="text/javascript" src="/swish/lm_xref/pixmapx/popupmenu/scripts/Popup-plugin.js"></script>
<link rel="stylesheet" type="text/css" href="/swish/lm_xref/pixmapx/popupmenu/styles/Popup-plugin.css">
<script type="text/javascript" src="/swish/lm_xref/pixmapx/selected/js/social.selection.js"></script>
<link rel="stylesheet" type="text/css" href="/swish/lm_xref/pixmapx/selected/css/social.selection.css">
<script type="text/javascript" src="/swish/js/cliopatria.js"></script>
<link rel="stylesheet" type="text/css" href="/swish/css/butterfly_term.css">
<script type="text/javascript" href="/swish/js/butterfly_term.js"></script>
<link rel="stylesheet" type="text/css" href="/swish/css/term.css">
<!-- -->
<script data-main="/swish/js/swish" src="/node_modules/requirejs/require.js"></script> 
').

arc_script_header2:- 
  arc_html_format((('<script src="https://code.jquery.com/jquery-3.6.0.min.js" crossorigin="anonymous"></script>
<script src="https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/jquery-ui.min.js"></script> <!-- necessary for the "draggable" ui  -->
<script src="/swish/lm_xref/pixmapx/popupmenu/scripts/Popup-plugin.js"></script>
<script src="/swish/lm_xref/pixmapx/popupmenu/scripts/Example.js"></script>

<link rel="shortcut icon" href="/static/images/favicon.png?">
<link rel="stylesheet" type="text/css" href="/swish/css/menu.css">
<link rel="stylesheet" type="text/css" href="/swish/css/cliopatria.css">
<script type="text/javascript" src="/swish/js/cliopatria.js"></script>
<link type="text/css" rel="stylesheet" href="/swish/css/term.css">
<link type="text/css" rel="stylesheet" href="/swish/css/butterfly_term.css">

<link rel="stylesheet" type="text/css" href="/www/yui/2.7.0/build/autocomplete/assets/skins/sam/autocomplete.css">
<script type="text/javascript" src="/www/yui/2.7.0/build/utilities/utilities.js"></script>
<script type="text/javascript" src="/www/yui/2.7.0/build/datasource/datasource.js"></script>
<script type="text/javascript" src="/www/yui/2.7.0/build/autocomplete/autocomplete.js"></script>

<script src="https://cdnjs.cloudflare.com/ajax/libs/jquery.qrcode/1.0/jquery.qrcode.min.js"></script>
<script src="/swish/lm_xref/pixmapx/selected/js/social.selection.js"></script>
<link rel="stylesheet" type="text/css" href="/swish/lm_xref/pixmapx/selected/css/social.selection.css">
<link rel="stylesheet" type="text/css" href="/swish/lm_xref/pixmapx/selected/css/example.css">
<link rel="stylesheet" type="text/css" href="/swish/lm_xref/pixmapx/popupmenu/styles/Popup-plugin.css">
<link rel="stylesheet" type="text/css" href="/swish/lm_xref/pixmapx/popupmenu/styles/Example.css">
<!--Use either font-awesome icons or Google icons with these links. Other icons could also be used if preferred-->
<link rel="stylesheet" href="https://fonts.googleapis.com/icon?family=Material+Icons">
<script data-main="/swish/js/swish" src="/node_modules/requirejs/require.js"></script>'))).

%http_debug_console:- current_prolog_flag(gui_tracer,true),!,guitracer.
http_debug_console:- 
  nodebug,notrace,
  current_output(Out),
  nl(Out),flush_output(Out),
  attach_console,
  current_input(In),
  current_output(Err),
  set_prolog_IO(In,Out,Err).
   %gtrace.

invoke_arc_cmd(Key):- \+ sensical_term(Key),!.
invoke_arc_cmd(Key):- atom_to_term_safe(Key,Prolog,_Vs), Prolog\=@=Key,!,invoke_arc_cmd(Prolog).
invoke_arc_cmd(Prolog):- %nonvar(Prolog),
   current_predicate(_,Prolog),
   asserta_new(xlisting_whook:offer_testcase(Prolog)),!,
   call(Prolog).
invoke_arc_cmd(Key):- do_menu_key(Key).

arc_weto(G):- call(G).

%:- luser_default(cmd,print_test).
:- luser_default(cmd,ndividuator). 
:- luser_default(footer_cmd,statistics).

current_arc_cmd(Prolog):- current_arc_cmd('cmd',Prolog).
%current_arc_cmd(cmd,Prolog):- luser_getval(cmd,Prolog).
%current_arc_cmd(cmd,Prolog):- luser_getval(cmd,Prolog).
current_arc_cmd(V,Prolog):- luser_getval(V,Prolog).
%current_arc_cmd(footer_cmd,Prolog):- (\+ current_arc_cmd(cmd,menu) -> luser_getval(footer_cmd,Prolog,menu) ; luser_getval(footer_cmd,Prolog,edit1term)).


%muarc:test_arcui

 % our_pengine_output(`<script src="https://unpkg.com/gojs/release/go-debug.js"></script>`).

:-   ignore((predicate_property(phil:'$exported_op'(_,_,_),(discontiguous)),
  \+ predicate_property(phil:'$exported_op'(_,_,_),number_of_clauses(_)),
     abolish(phil:'$exported_op',3))),
  ignore((predicate_property(rdf11:'$exported_op'(_,_,_),(discontiguous)),
\+ predicate_property(rdf11:'$exported_op'(_,_,_),number_of_clauses(_)),
  abolish(rdf11:'$exported_op',3))),
     ignore((predicate_property(lemur:'$exported_op'(_,_,_),(discontiguous)),
  \+ predicate_property(lemur:'$exported_op'(_,_,_),number_of_clauses(_)),
     abolish(lemur:'$exported_op',3))).

:- include(kaggle_arc_ui_html_go1).
:- include(kaggle_arc_ui_html_go2).
/*

:- abolish(lemur:'$exported_op',3).
:- abolish(rdf11:'$exported_op',3).
*/
:- include(kaggle_arc_footer).

