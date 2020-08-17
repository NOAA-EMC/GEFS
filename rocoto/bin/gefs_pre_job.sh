#! /bin/ksh
set -x -e
date
# . $GEFS_ROCOTO/bin/gefs_load_modules.inc
export expid=${EXPID}
export SOURCEDIR=${SOURCEDIR}
export WORKDIR=${WORKDIR}

if [[ "$WHERE_AM_I" == cray ]] ; then
    #. $GEFS_ROCOTO/bin/cray/common.sh
    #. $SOURCEDIR/versions/gefs_cray.ver
    . $GEFS_ROCOTO/bin/gefs_load_modules.inc

    . $GEFS_ROCOTO/parm/setbase
    . $GEFS_ROCOTO/parm/gefs_config
    . $GEFS_ROCOTO/parm/gefs_dev.parm

elif [[ "$WHERE_AM_I" == hera ]]; then
    # Temporary setting to test common settings
    #. $GEFS_ROCOTO/bin/hera/common.sh
    #. $SOURCEDIR/versions/gefs_hera.ver
    . $GEFS_ROCOTO/bin/gefs_load_modules.inc
    . $GEFS_ROCOTO/parm/setbase
    . $GEFS_ROCOTO/parm/gefs_config
    . $GEFS_ROCOTO/parm/gefs_dev.parm

fi

# to test and will be deleted after finalized all tasks

#. $GEFS_ROCOTO/parm/setbase
#. $GEFS_ROCOTO/parm/gefs_config
#. $GEFS_ROCOTO/parm/gefs_dev.parm

"$@"
