!/bin/sh
set -x -e

if [[ -d /lfs3 ]] ; then
    # We are on NOAA Jet
    machine="jet"
elif [[ -d /scratch3 ]] ; then
    # We are on NOAA Theia
    machine="theia"
elif [[ -d /gpfs/hps && -e /etc/SuSE-release ]] ; then
    # We are on NOAA Luna or Surge
    machine="cray"
elif [[ -L /usrx && "$( readlink /usrx 2> /dev/null )" =~ dell ]] ; then
    # We are on NOAA Venus or Mars
    machine="wcoss_dell_p3"
elif [[ -d /dcom && -d /hwrf ]] ; then
    # We are on NOAA Tide or Gyre
    machine="wcoss"
elif [[ -d /glade ]] ; then
    # We are on NCAR Yellowstone
    machine="yellowstone"
elif [[ -d /lustre && -d /ncrc ]] ; then
    # We are on GAEA.
    machine=gaea
else
    echo WARNING: UNKNOWN PLATFORM 1>&2
    exit 99
fi


EXECdir=../exec
[ -d $EXECdir ] || mkdir $EXECdir
for dir in gefs_vortex_separate.fd gefs_vortex_combine.fd global_sigzvd.fd global_ensadd.fd global_enspqpf.fd gefs_ensstat.fd global_ensppf.fd global_enscvprcp.fd global_enspvrfy.fd global_enssrbias.fd global_enscqpf.fd global_enscvt24h.fd global_ensrfmat.fd ; do
	cd $dir
    if [ $dir != 'global_enspvrfy.fd' -a $machine = 'wcoss_dell_p3' ]; then
	    make install
    fi
	cd ..
done
EXECdir=../util/exec
[ -d $EXECdir ] || mkdir $EXECdir
for dir in ../util/sorc/gettrk.fd ../util/sorc/overenstr.grib.fd ../util/sorc/getnsttf.fd; do
	cd $dir
	make install
	cd ../../../sorc
done
EXECdir=../exec
[ -d $EXECdir ] || mkdir $EXECdir
for dir in gefs_anom2_fcst.fd gefs_nstgen.fd; do
    cd $dir
    make install
    cd ..
done
