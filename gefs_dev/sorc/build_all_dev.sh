#!/bin/sh
set -ex

build_dir=`pwd`
logs_dir=$build_dir/logs
if [ ! -d $logs_dir  ]; then
  echo "Creating logs folder"
  mkdir $logs_dir
fi

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  echo "Creating ../exec folder"
  mkdir ../exec
fi

#------------------------------------
# INCLUDE PARTIAL BUILD 
#------------------------------------

. ./partial_build_dev.sh

#------------------------------------
# build global_enspqpf
#------------------------------------
$Build_global_enspqpf && {
echo " .... Building global_enspqpf - 01 .... "
./build_global_enspqpf.sh > $logs_dir/build_global_enspqpf.log 2>&1
}

#------------------------------------
# build global-workflow
#------------------------------------
#if [[ -d global-workflow.fd ]] ; then
#    if [[ -L global-workflow.fd ]] ; then
#        echo " ... You don't need to build global-workflow because global-workflow.fd was linked from other directiory!"
#    else
#        echo " .... Building global-workflow .... "
#        ./build_global-workflow.sh > $logs_dir/build_global-workflow.log 2>&1
#    fi
#fi

echo;echo " .... Build dev system finished .... "

exit 0

