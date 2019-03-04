#!/bin/bash

sWS=`pwd`
echo $sWS

while getopts c:a:r:m:f:b:d: option
do
    case "${option}"
    in
        c) CompileCode=${OPTARG};;
        a) CleanAll=${OPTARG};;
        r) RunRocoto=${OPTARG};;
        m) machine=${OPTARG};;
        f) userConfigFile=${OPTARG};;
        b) AddCrontabToMyCrontab=${OPTARG};;
        d) DeleteCrontabFromMyCrontab=${OPTARG};;
    esac
done

CompileCode=${CompileCode:-no}
CleanAll=${CleanAll:-no}
RunRocoto=${RunRocoto:-no}
machine=${machine:-nomachine}
userConfigFile=${userConfigFile:-user_full.conf}
AddCrontabToMyCrontab=${AddCrontabToMyCrontab:-no}
DeleteCrontabFromMyCrontab=${DeleteCrontabFromMyCrontab:-no}

if [ $machine = "nomachine" ]; then
    if [ -d /scratch4/NCEPDEV ]; then
        machine=theia
    elif [[ -d /mnt/lfs2 && -d /mnt/lfs3 ]] ; then # Jet
        machine=jet
    elif [[ -d /gpfs/hps3 && -e /etc/SuSE-release ]]; then # Luna or Surge
        machine=cray
    elif [[ -d /dcom && -d /hwrf ]] ; then # Tide or Gyre
        machine=wcoss
    elif [[ -L /usrx && "$( readlink /usrx 2> /dev/null )" =~ dell ]] ; then # We are on NOAA Mars or Venus
        machine=wcoss_dell_p3
    else
        echo "This is not supported by this script!"
        exit 55
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
    elif [ $machine = jet ]; then
        echo "You are running on Jet!"
        module load Module_gefs_v12_jet
    elif [ $machine = cray ]; then
        echo "You are running on Cray!"
        module load Module_gefs_v12_cray
    elif [ $machine = wcoss ]; then
        echo "You are running on wcoss!"
        module load Module_gefs_v12_wcoss
    elif [ $machine = wcoss_dell_p3 ]; then
        echo "You are running on wcoss_dell_p3!"
        module load Module_gefs_v12_wcoss_dell_p3
    else
        echo "You are running on some platform we didn't support, please check it!"
        exit
    fi

    ## Build the code
    ./build.sh

    ## Install GEFS
    ./install.sh

    cd $sWS/../
    rm -rf fix
    if [ $machine = "theia" ]; then
        /bin/ln -sf /scratch4/NCEPDEV/ensemble/noscrub/common/git/fv3gefs/fix fix
    elif [ $machine = "cray" ]; then
        /bin/ln -sf /gpfs/hps3/emc/ensemble/noscrub/emc.enspara/common/git/fv3gefs/fix fix
    elif [ $machine = "wcoss_ibm" ]; then
        /bin/ln -sf /ensemble/noscrub/Walter.Kolczynski/gefs-fixed fix
    elif [ $machine = "wcoss_dell_p3" ]; then
        /bin/ln -sf /gpfs/dell2/emc/verification/noscrub/emc.enspara/common/git/fv3gefs/fix fix
    fi
fi

# for cleanning
if [ $CleanAll = "yes" ]; then
    echo "Cleaning ..."
    
    rm -rf gefs.xml
    rm -rf cron_rocoto
    rm -rf tasks

    cd $sWS/../sorc

    for dir in gefs_vortex_separate.fd gefs_vortex_combine.fd global_sigzvd.fd  global_ensadd.fd  global_enspqpf.fd  gefs_ensstat.fd  global_ensppf.fd ; do
        cd $dir
        make clean
        cd ..
    done

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
    for dir in gefs_anom2_fcst.fd gefs_nstgen.fd ; do
        cd $dir
        make clean
        cd ..
    done    


    cd ${sWS}/../sorc
    rm -rf ../exec
    rm -rf ../util/exec
    rm -rf ../fix

fi # for CleanAll

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
    elif [ $machine = "wcoss_dell_p3" ]; then
        . /usrx/local/prod/lmod/lmod/init/sh
        module use /gpfs/dell3/usrx/local/dev/emc_rocoto/modulefiles
        module load lsf/10.1
        module load ruby/2.5.1
        module load rocoto/complete
        module load python/2.7.14        
    fi
    ./py/run_to_get_all.py  $userConfigFile
    
    echo "Generated xml and/or ent and updated bin file!"
fi # For RunRocoto

#echo "crontab"
# For Crontab
if [ $AddCrontabToMyCrontab = "yes" ]; then
    cd $sWS
    if [ $machine = "theia" ]; then
        echo "Not ready on theia"
    elif [ $machine = "cray" ]; then
        echo "Not ready on cray"
    elif [ $machine = "wcoss_dell_p3" ]; then
        py/add_crontab.py
        echo "Added crontab to system!"
    elif [ $machine = "jet" ]; then
        py/add_crontab.py
        crontab ~/cron/mycrontab
        echo "Added crontab to system!"
    fi
fi

if [ $DeleteCrontabFromMyCrontab = "yes" ]; then
    cd $sWS
    if [ $machine = "theia" ]; then
        echo "Not ready on theia"
    elif [ $machine = "cray" ]; then
        echo "Not ready on cray"
    elif [ $machine = "wcoss_dell_p3" ]; then
        py/del_crontab.py
        echo "Deleted crontab to system!"
    elif [ $machine = "jet" ]; then
        crontab -r
        py/del_crontab.py
        crontab ~/cron/mycrontab
        echo "Deleted crontab to system!"
    fi
fi
