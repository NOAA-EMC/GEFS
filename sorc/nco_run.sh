#!/bin/ksh
set -ex

sWS=`pwd`
echo $sWS

while getopts m:e: option
do
    case "${option}"
    in
        m) machine=${OPTARG};;
        e) RunEnvir=${OPTARG};;
    esac
done

machine=${machine:-wcoss2}
RunEnvir=${RunEnvir:-nco}

./checkout.sh  > checkout.log 2>&1

./build_all.sh > build_all.log 2>&1

# for Link
cd $sWS/../sorc
if [ $machine = "hera" ]; then
    ./link_gefs.sh -e emc -m hera  > link_gefs.log 2>&1
elif [ $machine = "cray" ]; then
    ./link_gefs.sh -e $RunEnvir -m cray  > link_gefs.log 2>&1
elif [ $machine = "dell" ]; then
    ./link_gefs.sh -e $RunEnvir -m dell  > link_gefs.log 2>&1
elif [ $machine = "wcoss2" ]; then
    ./link_gefs.sh -e $RunEnvir -m wcoss2 > link_gefs.log 2>&1
fi

