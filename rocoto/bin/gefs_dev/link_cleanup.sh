#!/bin/sh
set -ex

while getopts a: option
do
    case "${option}"
    in
        a) CleanAll=${OPTARG};;

    esac

done


CleanAll=${CleanAll:-no}

LINK="ln -fs"
#[[ $RUN_ENVIR = nco ]] && LINK="cp -rp"

pwd=$(pwd -P)

pwd0=$(readlink -e $pwd/../../../)
echo $pwd0

for f0 in modulefiles/gefs sorc jobs ush scripts gempak/ush gempak/fix util/ush; do

    for f in $f0/*; do
        #echo $f
        if [ $CleanAll = "yes" ]; then    
            if [ -L $pwd0/$f ]; then
                echo $pwd0/$f
                rm -rf $pwd0/$f
            fi
        else
            cd $pwd0/$f0
            #$LINK $pwd/$f .
            iNum=`echo "$f0" |tr "/" "\n"|wc -l`
            echo $iNum
            sPrefix=""
            if [ $iNum -eq 1 ]; then
                sPrefix="../"
            elif [ $iNum -eq 2 ]; then
                sPrefix="../../"
            elif [ $iNum -eq 3 ]; then
                sPrefix="../../../"
            elif [ $iNum -eq 4 ]; then
                sPrefix="../../../../"
            elif [ $iNum -eq 5 ]; then
                sPrefix="../../../../../"
            fi
            $LINK ${sPrefix}rocoto/bin/gefs_dev/$f .

            cd $pwd
        fi 
    done

done

f0=util
f=${f0}/sorc
if [ $CleanAll = "yes" ]; then
    if [ -L $pwd0/$f ]; then
        echo $pwd0/$f
        rm -rf $pwd0/$f
    fi
else
    cd $pwd0/$f0
     $LINK ../rocoto/bin/gefs_dev/$f .
    cd $pwd
fi
