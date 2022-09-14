#!/bin/bash

rm -rf out
git checkout out
swipl -l kaggle_arc || stty sane
killall -9 xterm
stty sane


