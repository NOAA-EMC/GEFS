#!/bin/sh
set -x -e
EXECdir=../exec
[ -d $EXECdir ] || mkdir $EXECdir
for dir in *.fd; do
 cd $dir
 make install
 cd ..
done


