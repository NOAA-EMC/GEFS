#!/bin/sh
set -xu

topdir=$(pwd)
echo $topdir

logs_dir=${topdir}/logs
if [ ! -d $logs_dir  ]; then
  echo "Creating logs folder"
  mkdir $logs_dir
fi

echo Global_workflow checkout ...
if [[ ! -d global-workflow.fd ]] ; then
    rm -f ${logs_dir}/checkout-global-workflow.log
    git clone --recursive gerrit:global-workflow global-workflow.fd >>  ${logs_dir}/checkout-global-workflow.log 2>&1
    cd global-workflow.fd
    git checkout fb95cd3c
    cd sorc
    ./checkout.sh
    ERR=$?
    if [ $ERR = 0 ]; then
        msg="Checkout global-workflow-1 normally"
    else
        msg="Checkout global-workflow-1 failed!"
    fi

    cd fv3gfs.fd/FV3
    git checkout 6a95e93
    ERR=$?
    if [ $ERR = 0 ]; then
        msg="Checkout global-workflow-2 normally"
    else
        msg="Checkout global-workflow-2 failed!"
    fi


    cd ../../gfs_post.fd
    git checkout 8d5deeab0b2acce369af5617956f2b07a51cc342
    ERR=$?    

    if [ $ERR = 0 ]; then
        msg="Checkout global-workflow normally"
    else
        msg="Checkout global-workflow failed!"
    fi

    cd ${topdir}

    exit $ERR
else
    echo 'Skip. Directory global_workflow.fd already exists.'
fi

exit 0
