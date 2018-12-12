!/bin/sh
set -x -e

EXECdir=../exec
[ -d $EXECdir ] || mkdir $EXECdir
for dir in gefs_vortex_separate.fd gefs_vortex_combine.fd global_sigzvd.fd global_ensadd.fd global_enspqpf.fd gefs_ensstat.fd global_ensppf.fd global_enscvprcp.fd global_enspvrfy.fd global_enssrbias.fd global_enscqpf.fd global_enscvt24h.fd global_ensrfmat.fd ; do
	cd $dir
    make install
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
for dir in gefs_nstgen.fd; do
    cd $dir
    make install
    cd ..
done
