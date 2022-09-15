#!/bin/bash

SCRIPT=$(readlink -f $0)
export ARC_DIR=$(dirname $SCRIPT)
echo ARC_DIR=$ARC_DIR
cd $ARC_DIR
rm -rf out
git checkout out
bash -l -c "cd ${ARC_DIR} ; ls ;  swipl ./kaggle_arc.pl -t bfly_starup"  || stty sane
killall -9 xterm
stty sane


