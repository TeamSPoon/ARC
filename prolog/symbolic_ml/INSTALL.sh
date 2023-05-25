
symbolic_here () {
files=(../kaggle_arc/*$2*pl)
   ln -s $files $1$2.pl
}


symbolic_here arc_bin '_deepening'
symbolic_here arc_bin '_footer'
symbolic_here arc_bin '_header'
symbolic_here arc_bin '_module'
symbolic_here arc_bin '_ndif'
symbolic_here arc_bin '_precompiler'
symbolic_here arc_bin '_test_cache'
symbolic_here arc_bin '_test_easy'
symbolic_here arc_bin '_test_favs'
symbolic_here arc_bin '_test_iface'
symbolic_here arc_bin '_test_loader'
symbolic_here arc_bin '_test_old'
symbolic_here arc_bin '_typecheck'
symbolic_here arc_bin '_ui_ansi'
symbolic_here arc_bin_ui '_explaination'
symbolic_here arc_bin '_ui_html_d3'
symbolic_here arc_bin '_ui_html_go1'
symbolic_here arc_bin '_ui_html_go2'
symbolic_here arc_bin '_ui_html'
symbolic_here arc_bin '_ui_html_wss'
symbolic_here arc_bin '_utils'



symbolic_here arc_exe '_two'
symbolic_here arc_exe '_simple'

symbolic_here arc_lib_mod '_afc'
symbolic_here arc_lib_mod '_alephlib'
symbolic_here arc_lib_mod '_aleph'
symbolic_here arc_lib_mod '_fwd.pfc'
symbolic_here arc_lib_mod '_fwd_sanity.pfc'
symbolic_here arc_lib_mod '_metagol'
symbolic_here arc_lib_mod '_oskar'
symbolic_here arc_lib_mod '_pfc'
#symbolic_here arc_lib_old '_individuation_pbox_2'
symbolic_here arc_lib_old '_intruder'



symbolic_here () {
files=(../kaggle_arc/*$2*pl)
  ln -s $files $1$2_v1.pl
  cp COPYWRITE $1$2_v2.pl
  echo " :- ensure_loaded($1$2_v1). " >> $1$2_v2.pl
}

symbolic_here arc_lib_old '_learning'

symbolic_here arc_lib_bin '_interpreter'
symbolic_here arc_lib_bin '_imageproc'
symbolic_here arc_lib_bin '_symmetry'
symbolic_here arc_lib_bin '_imagens'

symbolic_here arc_lib_mod '_grid_size'
symbolic_here arc_lib_mod '_heuristics'
symbolic_here arc_lib_mod '_recognise'

symbolic_here arc_usr_share '_boards'
symbolic_here arc_usr_share '_db'
symbolic_here arc_usr_share '_reduce'
symbolic_here arc_usr_share '_skels'
symbolic_here arc_usr_share '_individuation_pbox'

symbolic_here arc_usr_slib '_domaintypes'
symbolic_here arc_usr_slib '_generalization'
symbolic_here arc_usr_slib '_physics'

symbolic_here arc_usr_sys '_individuation'
symbolic_here arc_usr_sys '_object'
symbolic_here arc_usr_sys '_howdiff'
symbolic_here arc_usr_sys '_prior_groups'
symbolic_here arc_usr_sys '_transrules'
symbolic_here arc_usr_sys '_uniqueness'

