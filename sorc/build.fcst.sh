#! /bin/bash
echo "`date`   `pwd`   $0 $*   begin"

set -eu

if [ ! -d "../exec" ]; then

    echo "Creating ../exec folder"
    mkdir ../exec

fi

cd esmf.3_1_0rp5/src
echo "`date`   `pwd`   Compiling esmf.3.1.0rp5    before"
./makeit
echo "`date`   `pwd`   Compiling esmf.3.1.0rp5    after"
cd ../../


for dir in global_chgres.fd global_sfchdr.fd  global_sighdr.fd gefs_global_fcst.fd ; do  #global_chgres.fd global_cycle.fd  global_sfchdr.fd  global_sighdr.fd gefs_global_fcst.fd ; do
    cd $dir
    echo "`date`   `pwd`   Compiling $dir   before"
    ./makefile.sh
    echo "`date`   `pwd`   Compiling $dir   after"
    cd ../
done

echo "`date`   `pwd`   $0 $*   end"

