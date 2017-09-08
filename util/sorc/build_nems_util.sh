SHELL=/bin/sh
set -x

#####################################################################################
# 02/06/2017 Xiaqiong.Zhou@noaa.gov:   to build nemsio_get  on wcoss in GEFS
#####################################################################################


set -x -e
EXECdir=../exec
[ -d $EXECdir ] || mkdir $EXECdir

. /usrx/local/Modules/3.2.10/init/sh

 module load ./module_nemsutil.wcoss
module list

curdir=`pwd`

for prog in nemsio_get.fd ;do

cd ${curdir}/$prog
make -f makefile
done
