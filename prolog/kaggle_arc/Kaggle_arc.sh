#!/bin/bash

SCRIPT=$(readlink -f $0)
export ARC_DIR=$(dirname $SCRIPT)
echo ARC_DIR=$ARC_DIR

if [[ $# -gt 0 ]] ; then
   fuser -n tcp -k 17666
   fuser -n tcp -k 17666
fi

export LM_ARC_BASE=$1
echo LM_ARC_BASE=$LM_ARC_BASE

cd $ARC_DIR
rm -rf out
git checkout out

if id -u "norights" >/dev/null 2>&1; then
 sudo -u norights bash -l -c "cd ${ARC_DIR} ; pwd ;  swipl ./kaggle_arc.pl -t bfly_starup"  || stty sane
else
 bash -l -c "cd ${ARC_DIR} ; pwd ;  swipl ./kaggle_arc.pl -t 'user:bfly_startup' -- ${*}"  || stty sane
fi

if [[ $# -gt 0 ]] ; then
   fuser -n tcp -k 7771
   fuser -n tcp -k 17666
fi

killall -9 xterm

stty sane


