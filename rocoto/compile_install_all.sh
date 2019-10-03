#!/bin/bash
set -eu

sWS=`pwd`
echo $sWS

while getopts c:a:r:m:f:b: option
do
    case "${option}"
    in
        c) CompileCode=${OPTARG};;
        a) CleanAll=${OPTARG};;
        r) RunRocoto=${OPTARG};;
        m) machine=${OPTARG};;
        f) userConfigFile=${OPTARG};;
        b) AddCrontabToMyCrontab=${OPTARG};;
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
    if [ -d /scratch1/NCEPDEV ]; then
        machine=hera
    elif [ -d /scratch3/NCEPDEV ]; then
        machine=theia
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

    ## Build the code and install
    ./build_all.sh

    cd $sWS/../sorc
    if [ $machine = "theia" ]; then
        ./link_gefs.sh -e emc -m theia
    elif [ $machine = "hera" ]; then
        ./link_gefs.sh -e emc -m hera
    elif [ $machine = "cray" ]; then
        ./link_gefs.sh -e emc -m cray
    elif [ $machine = "wcoss_ibm" ]; then
        ./link_gefs.sh -e emc -m ibm
    elif [ $machine = "wcoss_dell_p3" ]; then
        ./link_gefs.sh -e emc -m dell
    fi
fi

# for cleanning
if [ $CleanAll = "yes" ]; then
    echo "Cleaning ..."
    
    rm -rf gefs.xml
    rm -rf cron_rocoto
    rm -rf tasks

    cd $sWS/../sorc

    rm -rf logs

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
    rm -f ../fix

fi # for CleanAll

# for rocoto

if [ $RunRocoto = "yes" ]; then
    cd $sWS
    if [ $machine = "hera" ]; then
        module load intel/18.0.5.274
        module load rocoto/1.3.1
        module load contrib
        module load anaconda/anaconda3-5.3.1
 
    elif [ $machine = "theia" ]; then
        module load rocoto/1.3.1
        module load intelpython/3.6.1.0

    elif [ $machine = "wcoss_ibm" ]; then
        module load ibmpe ics lsf
        module load python/3.6.3
        module use /usrx/local/emc_rocoto/modulefiles
        module load rocoto

    elif [ $machine = "cray" ]; then
        . /opt/modules/3.2.10.3/init/sh
        module use /usrx/local/emc_rocoto/modulefiles
        module load xt-lsfhpc
        module load rocoto
        module load python/3.6.3

    elif [ $machine = "wcoss_dell_p3" ]; then
        . /usrx/local/prod/lmod/lmod/init/sh
        module use /gpfs/dell3/usrx/local/dev/emc_rocoto/modulefiles
        module load lsf/10.1
        module load ruby/2.5.1
        module load rocoto/complete
        module load python/3.6.3       
    fi
    ./py/run_to_get_all.py  $userConfigFile
    
    echo "Generated xml and/or ent and updated bin file!"
fi # For RunRocoto

#echo "crontab"
# For Crontab
if [ $AddCrontabToMyCrontab = "yes" ]; then
    cd $sWS
    if [ $machine = "hera" ]; then
        if [ -f $HOME/cron/mycrontab ]; then
            echo "Adding crontab to $HOME/cron/mycrontab!" 
        else 
            mkdir $HOME/cron
            touch $HOME/cron/mycrontab
        fi
    
        py/add_crontab.py
        crontab $HOME/cron/mycrontab
        echo "Added crontab to $HOME/cron/mycrontab!"

    elif [ $machine = "theia" ]; then
        if [ -f $HOME/cron/mycrontab ]; then
            echo "Adding crontab to $HOME/cron/mycrontab!" 
        else
            mkdir $HOME/cron
            touch $HOME/cron/mycrontab
        fi
   
        py/add_crontab.py
        crontab $HOME/cron/mycrontab
        echo "Added crontab to $HOME/cron/mycrontab!"

    elif [ $machine = "wcoss_ibm" ]; then
        if [ -f $HOME/cron/mycrontab ]; then
            echo "Adding crontab to $HOME/cron/mycrontab!" 
        else
            mkdir $HOME/cron
            touch $HOME/cron/mycrontab
        fi

        py/add_crontab.py
        crontab $HOME/cron/mycrontab
        echo "Added crontab to $HOME/cron/mycrontab!"

    elif [ $machine = "cray" ]; then
        if [ -f $HOME/cron/mycrontab ]; then
            echo "Adding crontab to $HOME/cron/mycrontab!" 
        else
            mkdir $HOME/cron
            touch $HOME/cron/mycrontab
        fi

        py/add_crontab.py
        crontab $HOME/cron/mycrontab
        echo "Added crontab to $HOME/cron/mycrontab!"

    elif [ $machine = "wcoss_dell_p3" ]; then
        py/add_crontab.py
        echo "Added crontab to $HOME/cron/mycrontab!"
    fi
fi
