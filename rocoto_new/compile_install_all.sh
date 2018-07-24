#!/bin/bash

sWS=`pwd`
echo $sWS

while getopts c:a:r:m:f: option
do
    case "${option}"
    in
        c) CompileCode=${OPTARG};;
        a) CleanAll=${OPTARG};;
        r) RunRocoto=${OPTARG};;
        m) machine=${OPTARG};;
        f) userConfigFile=${OPTARG};;
    esac
done

CompileCode=${CompileCode:-no}
CleanAll=${CleanAll:-no}
RunRocoto=${RunRocoto:-no}
machine=${machine:-nomachine}
userConfigFile=${userConfigFile:-user_full.conf}

if [ $machine = "nomachine" ]; then
    if [ -d /scratch4/NCEPDEV ]; then
        machine=theia
    elif [ -d /gpfs ]; then
        if [ -f /etc/SuSE-release ]; then
            machine=cray
        fi
        if [ $machine = "nomachine" ]; then
            if [ $SITE = SURGE ]; then
                machine=cray
            fi
        fi
    fi
fi

echo $CompileCode
echo $CleanAll
echo $RunRocoto
echo $machine
echo $userConfigFile


if [ $CompileCode = "yes" ]; then
    cd $sWS/../sorc

    ## mkdir folds
    mkdir ../exec
    mkdir ../util/exec

    ## Deal with the modules
    module purge
    module use ./

    if [ $machine = theia ]; then
        echo "You are running on Theia!"
        module load Module_gefs_v12_theia
    elif [ $machine = cray ]; then
        echo "You are running on Cray!"
        module load Module_gefs_v12_cray
    elif [ $machine = wcoss ]; then
        echo "You are running on wcoss!"
        module load Module_gefs_v12_wcoss
    else
        echo "You are running on some platform we didn't support, please check it!"
        exit
    fi

    ## Build the code
    ./build.sh

    ## Install GEFS
    ./install.sh
fi

# for cleanning
if [ $CleanAll = "yes" ]; then
  echo "Cleaning ..."

    cd $sWS/../sorc

    for dir in gefs_vortex_separate.fd gefs_vortex_combine.fd global_sigzvd.fd  global_ensadd.fd  global_enspqpf.fd  gefs_ensstat.fd  global_ensppf.fd ; do
          cd $dir
          make clean
          cd ..
    done

    export LIBS="${G2_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}"

    for dir in global_enscvprcp.fd  global_enspvrfy.fd  global_enssrbias.fd global_enscqpf.fd  global_enscvt24h.fd  global_ensrfmat.fd ; do
          cd $dir
          make clean
          cd ..
    done

    for dir in ../util/sorc/gettrk.fd ../util/sorc/overenstr.grib.fd ../util/sorc/getnsttf.fd; do
          cd $dir
          make clean
          cd ../../../sorc
    done

    cd $sWS/../sorc
  rm -rf ../exec
  rm -rf ../util/exec


fi

# for rocoto

if [ $RunRocoto = "yes" ]; then
    cd $sWS
    if [ $machine = "theia" ]; then
        module load rocoto
    elif [ $machine = "cray" ]; then
        . /opt/modules/3.2.10.3/init/sh
        module use /usrx/local/emc_rocoto/modulefiles
        module load xt-lsfhpc
        module load rocoto
        module load python
    fi
    ./py/run_to_get_all.py  $userConfigFile
fi


