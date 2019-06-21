SHELL=/bin/sh
set -x

#####################################################################################
# 02/06/2017 Xiaqiong.Zhou@noaa.gov:   to build nemsio_get  on wcoss in GEFS
#####################################################################################


set -x -e
EXECdir=../exec
[ -d $EXECdir ] || mkdir $EXECdir

if [ ! -z $MODULESHOME ]; then
    . $MODULESHOME/init/sh
else
    . /opt/modules/default/init/sh
fi


 module load ./module_nemsutil.wcoss
module list

curdir=`pwd`

for prog in nemsio_get.fd ;do

cd ${curdir}/$prog
make -f makefile
done
