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
  invent_header(Goal,Title),
  collapsible_section(Type,Title,toplevel,Goal).

avacp(Vars):-  prolog_current_choice(Chp),'$attvars_after_choicepoint'(Chp, Vars).

print_title(Var):- (var(Var);Var==[]),!.
print_title([L|List]):- is_list(List), !, print_title(L),write(' '),print_title(List).
print_title(Title):- trim_newlines(ppt(Title)).

:- meta_predicate(collapsible_section(+,+,0)).
collapsible_section(Type,Title,Goal):-
  collapsible_section(Type,Title,false,Goal).

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
                                      tabbed_print_im(Depth+2,old_write_expandable(Showing,Tag=Title,Goal))), 
                     format('~N~w\u00A1mu~w\u00A1 ',[Ident, Tag])).

with_tagged(Tag,Goal):- 
  once(nb_current('$collapsible_section',Was);Was=[]), length(Was,Depth),!,wots(Ident,dash_chars(Depth,' ')),
  setup_call_cleanup(
    bfly_html_goal(format('~w<~w> ~N',[Ident,Tag])),
    locally(b_setval('$collapsible_section',[h(Tag)|Was]),tabbed_print_im(Depth+2,(Goal))),
    bfly_html_goal(format('~w</~w> ',[Ident,Tag]))).


title_to_html(Title,HtmlTitle):- with_pp(plain,into_attribute(Title,HtmlTitle)),!.


old_write_expandable(Showing,Title,Goal):- 
   setup_call_cleanup(flag('$old_write_expandable_depth',Depth,Depth+1),
   in_expandable(Showing,Title,Goal),
   flag('$old_write_expandable_depth',_,Depth)).

expandable_inlines:- expandable_mode(javascript).
expandable_mode(How):- var(How),!,luser_getval(expansion,How).
expandable_mode(How):- luser_getval(expansion,V),!,How==V.

:- luser_default(expansion,javascript).
:- luser_setval(expansion,bfly).

in_expandable(_Showing,_Title,Goal):- expandable_inlines, !, (call_maybe_det(Goal,Det),((Det==true-> ! ; true))).
in_expandable(Showing,Title,Goal):- Showing==always,!,ignore(ppt(Title)),(call_maybe_det(Goal,Det),((Det==true-> ! ; true))).
in_expandable(_Show,  Title,Goal):- flag('$old_write_expandable_depth',X,X), X>2, in_expandable(always,Title,Goal).
in_expandable(Showing,Title,Goal):- (Showing==toplevel;Showing==maybe), flag('$old_write_expandable_depth',X,X), X==1,!,in_expandable(true,Title,Goal).
in_expandable(Showing,Title,Goal):- (Showing==maybe, flag('$old_write_expandable_depth',X,X), X=<2), !, ignore(ppt(Title)),!,in_expandable(true,Title,Goal).
in_expandable(Showing,Title,Goal):- title_to_html(Title,HtmlTitle),!,
 (Showing == true -> Class=panel_shown; Class=panel_hidden),
 (Showing == true -> Click='collapse/expand'; Click='expand/collapse'),
 setup_call_cleanup(format(
  '<button class="accordion">~w (click to ~w)</button><div class="~w">',[HtmlTitle,Click,Class]),
  (call_maybe_det(Goal,Det),((Det==true-> ! ; true))),
  format('</div>',[])),
 flush_tee_maybe,
 (Det==true-> ! ; true).

call_maybe_det(Goal,Det):- true,call(Goal),deterministic(Det),true.

old_in_expandable(Showing,Title,Goal):- 
 on_xf_ignore_flush(ensure_colapable_styles), 
 (Showing -> PX='128'; PX='600'),
 (Showing -> Exp=''; Exp='collapsed-c'),
  inline_html_format([
   '<pre><button type="button" class="collapsible">',Title,' (click to un/expand)</button>',
   '<div class="',write(Exp),'" style="max-height: ',PX,'px"><pre>',
   call(Goal),'</pre></div></pre>']).


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

%:- http_handler('/swish/arc/', swish_arc, [prefix]).

user:file_search_path(arc,  AbsolutePath):- arc_sub_path('.',AbsolutePath).

%:- http_handler('/swish/muarc/swish_config.json', swish_reply_config_root,[priority(200)]).
:- http_handler('/swish/muarc/arcproc_left', arcproc_left, [prefix]).
:- http_handler('/arcproc_left', arcproc_left, [prefix]).
:- http_handler('/arcproc_right', arcproc_right, [prefix,chunked]).
:- http_handler('/swish/muarc/arcproc_right', arcproc_right, [prefix,chunked]).
:- http_handler('/swish/arc/', swish_arc, [prefix]).
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
  unsetenv('DISPLAY'),
  no_xdbg_flags,
  no_x_flags,
  nop((set_prolog_flag(xpce,true))).

%:- no_web_dbg.

intern_arc_request_data(Request):-
  intern_request_data(Request),
  save_in_luser(Request).

save_in_luser(NV):- \+ compound(NV),!.
save_in_luser(NV):- is_list(NV),!,must_maplist(save_in_luser,NV),!.
save_in_luser(NV):- NV=..[N,V],!,save_in_luser(N,V),!.
save_in_luser(N=V):- save_in_luser(N,V),!.
save_in_luser(media(_,_,_,_)):-!.
save_in_luser(NV):- dmsg(not_save_in_session(NV)),!.

save_in_luser(N,V):- luser_setval(N,V), luser_default(N,V), 
  ignore((is_list(V),last(V,E),compound(E),save_in_luser(V))).



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
   as_if_webui(((begin_arc_html_request(left,Request),
      arc_html_format([handler_logicmoo_left,write_end_html])))),!.
arcproc_left(Request):- xlisting_web:handler_logicmoo_cyclone(Request),!.


arcproc_right(Request):- 
 format('Content-type: text/html~n~n',[]),
 notrace((with_toplevel_pp(http,handler_logicmoo_right(Request)))),!.
arcproc_right(Request):- swish_arc(Request),!.

arc_html_format(TextAndGoal):- wants_html,!,arc_inline_html_format(TextAndGoal).
arc_html_format(TextAndGoal):- bfly_in_out(call(call,inline_html_format(TextAndGoal))).

arc_inline_html_format([H|T]):- is_codelist([H|T]),!,sformat(S,'~s',[[H|T]]),!,arc_inline_html_format(S).
arc_inline_html_format([H|T]):- is_charlist([H|T]),!,sformat(S,'~s',[[H|T]]),!,arc_inline_html_format(S).
arc_inline_html_format(TextAndGoal):- is_list(TextAndGoal),!,maplist(arc_inline_html_format,TextAndGoal).
%arc_inline_html_format(Msg):- flush_output_safe, wdmsg(call(Msg)),fail.
arc_inline_html_format(TextAndGoal):- inline_html_format(TextAndGoal),flush_output_safe.

% arc_find_tests(menu):- ignore(menu).
arc_find_tests(F):- find_tests(F).

:- dynamic(xlisting_whook:offer_testcase/1).
:- multifile(xlisting_whook:offer_testcase/1).
xlisting_whook:offer_testcase(F):- arc_find_tests(F).

handler_logicmoo_left:- handler_logicmoo_menu,!.
handler_logicmoo_menu:-   
 set_prolog_flag(gui_tracer,false),
 as_if_webui(arc_html_format([
   write('<pre>'),
   ignore(test_webui_menu),
   ignore(arc_http_nav_menu),
   ignore(offer_testcases),
   ignore(show_http_session),
   ignore((get_http_current_request(Request))),
   pp(Request),
   write('</pre>')])).

set_http_debug_error(False):- 
  set_prolog_flag(gui_tracer,false),
  set_prolog_flag(debug,false),
  set_prolog_flag(debug_on_error,False),
  set_prolog_flag(debug_on_interrupt,False),
  set_prolog_flag(determinism_error,silent),
  set_prolog_flag(report_error,False),
  nop((False==false->set_prolog_flag(on_error,halt);set_prolog_flag(on_error,status))).


%handler_logicmoo_arc:- as_if_webui(arc_html_format([call(handler_logicmoo_menu)])).
handler_logicmoo_right(Request):- 
   set_http_debug_error(false),
   intern_arc_request_data(Request),
   %begin_arc_html_request(right,Request),
   %arc_html_format([handler_logicmoo_right,write_end_html]))
   arc_html_format( 
   [write('<pre>'),
   ignore(flush_output),
   ignore(arc_http_nav_menu),
   ignore(flush_output),
   notrace(ignore(call_current_arc_cmds)),
   %ignore(print_test),
   %ignore(show_console_info),   
 %  ignore(call_current_arc_cmds),
   ignore(flush_output),
   write('<hr>'),
   invoke_arc_cmd(edit1term),
   ignore(show_http_session),
   write('</pre>')]), !.

get_now_cmd(Cmd,Prolog):- httpd_wrapper:http_current_request(Request), member(search(List),Request),member(Cmd=Call,List),url_decode_term(Call,Prolog),!.
%get_now_cmd(Cmd,Prolog):- get_param_req(Cmd,Call),url_decode_term(Call,Prolog).
%get_now_cmd(Cmd,Prolog):- get_http_current_request(Request), member(request_uri(List),Request),request_uri(/arc_web_interface.html?dump_from_pairmode),url_decode_term(Call,Prolog),!.

call_current_arc_cmds_pp:- with_toplevel_pp(http,call_current_arc_cmds).

call_current_arc_cmds:- get_now_cmd(cmd,Prolog),!,call(user:Prolog),!.
call_current_arc_cmds:- print_test,!,print_all_info_for_test,do_web_menu_key('t'),!.
call_current_arc_cmds:- 
 %call_current_arc_cmd(tc_cmd),
 call_current_arc_cmd(cmd),
 call_current_arc_cmd(cmd2),
 call_current_arc_cmd(footer_cmd).

call_current_arc_cmd(Var):-
   current_arc_cmd(Var,Prolog),        
   dmsg(Var=Prolog),invoke_arc_cmd(Prolog).



arc_http_nav_menu:- 
  current_arc_cmd(tc_cmd,Prolog),
  print_menu_cmd1((prev_test,Prolog)),
  print_menu_cmd1((next_test,Prolog)),
  print_menu_cmd1((Prolog)),!.
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

invoke_arc_cmd(Prolog):-
   nonvar(Prolog),
   asserta_new(xlisting_whook:offer_testcase(Prolog)), !,
   catch(arc_weto(Prolog),E,wdmsg(E)),!.

arc_weto(G):- call(G).

%:- luser_default(cmd,print_test).
:- luser_default(tc_cmd,ndividuator).
:- luser_default(footer_cmd,statistics).

current_arc_cmd(Prolog):- current_arc_cmd(cmd,Prolog).
%current_arc_cmd(cmd,Prolog):- luser_getval(cmd,Prolog).
%current_arc_cmd(tc_cmd,Prolog):- luser_getval(tc_cmd,Prolog).
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


