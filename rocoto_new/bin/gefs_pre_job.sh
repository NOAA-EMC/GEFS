#! /bin/ksh
set -x -e
date
. $GEFS_ROCOTO/bin/gefs_load_modules.inc
exec "$@"
