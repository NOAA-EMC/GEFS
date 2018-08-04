#! /bin/ksh
set -x -e
date
. $GEFS_ROCOTO/bin/gefs_load_modules.inc
export expid=${EXPID}
export SOURCEDIR=${SOURCEDIR}
export WORKDIR=${WORKDIR}

. $GEFS_ROCOTO/parm/gefs_dev.parm
. $GEFS_ROCOTO/parm/setbase
. $GEFS_ROCOTO/parm/gefs_config
#. $GEFS_ROCOTO/parm/gefs_dev.parm
exec "$@"
