#! /bin/ksh
set -x -e
date
cwd=`pwd`
. $cwd/gefs_load_modules.inc
exec "$@"
