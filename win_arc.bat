
set SWI_PROLOG_HOME=c:/program files/swipl

"%SWI_PROLOG_HOME%/bin/swipl.exe" -g "pack_install(predicate_streams)" -t halt
"%SWI_PROLOG_HOME%/bin/swipl.exe" -g "pack_upgrade(predicate_streams)" -t halt
"%SWI_PROLOG_HOME%/bin/swipl.exe" -g "pack_install(dictoo)" -t halt
"%SWI_PROLOG_HOME%/bin/swipl.exe" -g "pack_upgrade(dictoo)" -t halt
"%SWI_PROLOG_HOME%/bin/swipl.exe" -g "pack_install(logicmoo_utils)" -t halt
"%SWI_PROLOG_HOME%/bin/swipl.exe" -g "pack_upgrade(logicmoo_utils)" -t halt
"%SWI_PROLOG_HOME%/bin/swipl.exe" -g "pack_install(logicmoo_webui)" -t halt
"%SWI_PROLOG_HOME%/bin/swipl.exe" -g "pack_upgrade(logicmoo_webui)" -t halt

cd prolog
cd kaggle_arc

call "%SWI_PROLOG_HOME%/bin/swipl-win.exe" -l kaggle_arc.pl --

