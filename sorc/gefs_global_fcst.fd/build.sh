#! /bin/bash
echo "`date`   `pwd`   $0 $*   begin"

set -eu

dir=gefs_global_fcst.fd

echo "`date`   `pwd`   Compiling $dir   before"
./makefile.sh
echo "`date`   `pwd`   Compiling $dir   after"

echo "`date`   `pwd`   $0 $*   end"

