#!/bin/bash
set -eu #x

sWS=$(pwd)
echo $sWS

while getopts c:a:r:m:f:b:e:s:l:o: option
do
    case "${option}"
    in
        c) CompileCode=${OPTARG};;
        a) CleanAll=${OPTARG};;
        r) RunRocoto=${OPTARG};;
        m) machine=${OPTARG};;
        f) userConfigFile=${OPTARG};;
        b) AddCrontabToMyCrontab=${OPTARG};;
        e) RunEnvir=${OPTARG};;
        s) Structure=${OPTARG};;
        l) Link=${OPTARG};;
        o) Operation=${OPTARG};;
    esac
done

CompileCode=${CompileCode:-no}
CleanAll=${CleanAll:-no}
RunRocoto=${RunRocoto:-no}
machine=${machine:-nomachine}
userConfigFile=${userConfigFile:-user_full.conf}
AddCrontabToMyCrontab=${AddCrontabToMyCrontab:-no}
DeleteCrontabFromMyCrontab=${DeleteCrontabFromMyCrontab:-no}
RunEnvir=${RunEnvir:-emc}
Structure=${Structure:-no} # dev (use HOMEDIR to link), prod (clone global-workflow from vlab), no (use the original structure)
Link=${Link:-no}
Operation=${Operation:-no} # ecflow, rocoto, lsf

if [ $machine = "nomachine" ]; then
    if [ -d /scratch1/NCEPDEV ]; then
        machine=hera
    elif [[ -d /apps/prod ]]; then # WCOSS2
        machine=wcoss2
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
echo $RunEnvir
echo ${Structure}
echo ${Link}

if [ $CompileCode = "yes" ]; then
    Link=yes
fi


if [ $CompileCode = "yes" ]; then
    cd $sWS/../sorc

    if [[ $Structure == "dev" ]]; then
        echo "...working on it ..."
        if [[ $machine == "hera" ]]; then
            sHeader='/scratch2/NCEPDEV/ensemble'
        elif [[ $machine == "wcoss2" ]]; then
            sHeader='/lfs/h'
        fi
        sHOMEDIR=$(grep 'export HOMEDIR=${HOMEDIR:-'${sHeader} -r ${sWS}/parm/setbase | sed 's/export HOMEDIR=${HOMEDIR:-//g'| sed 's/}//g')
        echo $sHOMEDIR

        if [[ -L global-workflow.fd ]] ; then
            rm global-workflow.fd
        elif [[ -d global-workflow.fd ]] ; then
            rm -rf global-workflow.fd
        fi
        ln -sf $sHOMEDIR global-workflow.fd

    elif [[ $Structure == "prod" ]]; then
        # Checkout the global-workflow if needed
        if [[ -d global-workflow.fd ]] ; then
            rm -rf global-workflow.fd
        fi
        ./checkout.sh
    fi

    ## Build the code and install
    ./build_all.sh

fi


# for Link
if [ $Link = "yes" ]; then
    cd $sWS/../sorc
    if [ $machine = "hera" ]; then
        ./link_gefs.sh -e emc -m hera
    elif [ $machine = "wcoss2" ]; then
        ./link_gefs.sh -e $RunEnvir -m wcoss2
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

    if [[ -L global-workflow.fd ]] ; then
        rm global-workflow.fd
    elif [[ -d global-workflow.fd ]] ; then
        rm -rf global-workflow.fd
    fi
    
    for dir in global_ensadd.fd  global_enspqpf.fd  gefs_ensstat.fd  global_ensppf.fd ; do
        if [ -f $dir ]; then
            cd $dir
            make clean
            cd ..
        fi
    done

    for dir in global_enscvprcp.fd  global_enspvrfy.fd  global_enssrbias.fd global_enscqpf.fd  global_enscvt24h.fd  global_ensrfmat.fd ; do
        if [ -f $dir ]; then
            cd $dir
            make clean
            cd ..
        fi
    done

    for dir in ../util/sorc/overenstr.grib.fd; do
        if [ -f $dir ]; then
            cd $dir
            make clean
            cd ../../../sorc
        fi
    done
    for dir in gefs_anom2_fcst.fd gefs_nstgen.fd ; do
        if [ -f $dir ]; then
            cd $dir
            make clean
            cd ..
        fi
    done    


    cd ${sWS}/../sorc
    rm -rf ../exec
    rm -rf ../util/exec
    rm -rf ../fix/fix_*
    rm -rf ../fix/product

    # Clean the new links
    rm -rf global-workflow.fd
    rm -rf ../parm/parm_fv3diag
    rm -rf ../parm/post
    rm -rf ../parm/product
    rm -f ../ush/gfs_nceppost.sh
    rm -f ../ush/global_chgres.sh
    rm -f ../ush/global_chgres_driver.sh
    rm -f ../ush/wave_*
    rm -f ../ush/merge_fv3_chem_tile.py

    rm -f ../parm/prep_chem_sources.inp.IN
    rm -f ../scripts/exglobal_prep_chem.sh
    rm -f ../scripts/exwave_*
    rm -f ../scripts/exglobal_fcst_nemsfv3gfs.sh
    rm -rf ../env

fi # for CleanAll

# for rocoto
if [ $RunRocoto = "yes" ]; then
    cd $sWS
    if [ $machine = "hera" ]; then
        module load intel/18.0.5.274
        module load rocoto/1.3.1
        module load contrib
        module load intelpython/3.6.8
 
    elif [ $machine = "wcoss2" ]; then

        module purge
        module load envvar/1.0

        module load PrgEnv-intel/8.1.0
        module load craype/2.7.10
        module load intel/19.1.3.304
        module load cray-mpich/8.1.9

        module load python/3.8.6

        module use /apps/ops/test/nco/modulefiles/
        module load core/rocoto/1.3.5

    fi
    ./py/run_pyGEFS.py -r yes -f $userConfigFile
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

    elif [ $machine = "wcoss2" ]; then
        if [ -f $HOME/cron/mycrontab ]; then
            echo "Adding crontab to $HOME/cron/mycrontab!"
        else
            mkdir $HOME/cron
            touch $HOME/cron/mycrontab
        fi

        py/add_crontab.py
        crontab $HOME/cron/mycrontab
        echo "Added crontab to $HOME/cron/mycrontab!"

    fi
fi
