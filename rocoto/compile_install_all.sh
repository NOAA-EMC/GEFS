#!/bin/bash
set -eu #x

sWS=$(pwd)
echo ${sWS}

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

if [ ${machine} = "nomachine" ]; then
  if [ -d /scratch1/NCEPDEV ]; then
    machine=hera
  elif [[ -d /apps/prod ]]; then # WCOSS2
    machine=wcoss2
  else
    echo "This is not supported by this script!"
    exit 55
  fi
fi

echo ${CompileCode}
echo ${CleanAll}
echo ${RunRocoto}
echo ${machine}
echo ${userConfigFile}
echo ${RunEnvir}
echo ${Structure}
echo ${Link}

if [ ${CompileCode} = "yes" ]; then
  Link=yes
fi


if [ ${CompileCode} = "yes" ]; then
  cd ${sWS}/../sorc

  if [[ ${Structure} == "dev" ]]; then
    echo "...working on it ..."
    if [[ ${machine} == "hera" ]]; then
      sHeader='/scratch2/NCEPDEV/ensemble'
    elif [[ ${machine} == "wcoss2" ]]; then
      sHeader='/lfs/h'
    fi
    sHOMEDIR=$(grep 'export HOMEDIR=${HOMEDIR:-'${sHeader} -r ${sWS}/parm/setbase | sed 's/export HOMEDIR=${HOMEDIR:-//g'| sed 's/}//g')
    echo ${sHOMEDIR}

    if [[ -L global-workflow.fd ]] ; then
      rm global-workflow.fd
    elif [[ -d global-workflow.fd ]] ; then
      rm -rf global-workflow.fd
    fi
    ln -sf ${sHOMEDIR} global-workflow.fd

  elif [[ ${Structure} == "prod" ]]; then
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
if [ ${Link} = "yes" ]; then
  cd ${sWS}/../sorc
  if [ ${machine} = "hera" ]; then
    ./link_gefs.sh -e emc -m hera
  elif [ ${machine} = "wcoss2" ]; then
    ./link_gefs.sh -e ${RunEnvir} -m wcoss2
  fi
fi


# for cleanning
if [ ${CleanAll} = "yes" ]; then
  echo "Cleaning ..."

  cd ${sWS}
  rm -rf ../exec
  rm -rf ../util/exec
  rm -rf ../env

  rm -rf gefs.xml
  rm -rf gefs*.db
  rm -rf cron_rocoto
  rm -rf tasks
  rm -rf logs

  cd ${sWS}/../sorc
  rm -rf logs
  rm -rf ufs_model.fd
  if [[ -L global-workflow.fd ]] ; then
    rm global-workflow.fd
  elif [[ -d global-workflow.fd ]] ; then
    rm -rf global-workflow.fd
  fi

  for sDir in global_ensadd \
              gefs_ensstat \
              global_ensppf \
              wave_stat \
              gefs_nemsio2nc \
              global_enscqpf \
              global_enscvprcp \
              global_enssrbias \
              global_enscvt24h \
              global_enspqpf \
              global_enspvrfy \
              global_ensrfmat \
              global_enssrbias; do
    cd ${sWS}/../sorc
    if [ -f ${sDir}.fd ]; then
      cd ${sDir}.fd
      make clean
    fi
  done

  cd ${sWS}/../util/sorc
  for sDir in overenstr.grib.fd; do
    if [ -f ${sDir} ]; then
      cd ${sDir}
      make clean
    fi
  done

  cd ${sWS}/../fix
  for sDir in product \
              fix_emission fix_gefs fix_wave \
              am aer lut orog chem ugwd wave; do
    rm -rf ${sDir}
  done

  cd ${sWS}/../parm
  for sDir in parm_fv3diag post product; do
    rm -rf ${sDir}
  done

  cd ${sWS}/../scripts
  for sFile in `ls exgfs*` exglobal_forecast.sh; do
    rm -rf ${sFile}
  done

  cd ${sWS}/../ush
  for sFile in preamble.sh cplvalidate.sh `ls forecast_* ` `ls wave_*` merge_fv3_aerosol_tile.py chgres_cube.sh nems_configure.sh `ls parsing_*.sh`; do
    rm -rf ${sFile}
  done

fi # for CleanAll

# for rocoto
if [ $RunRocoto = "yes" ]; then
  cd $sWS
  if [ $machine = "hera" ]; then
    module load intel/18.0.5.274
    module load rocoto/1.3.3
    module load contrib
    module load anaconda/latest
 
  elif [ $machine = "wcoss2" ]; then

    module purge
    module load envvar/1.0

    module load PrgEnv-intel/8.3.3
    module load craype/2.7.17
    module load intel/19.1.3.304
    module load cray-mpich/8.1.9

    module load python/3.8.6

    module use /apps/ops/test/nco/modulefiles/
    module load core/rocoto/1.3.5

  fi
  ./py/run_pyGEFS.py -r yes -f ${userConfigFile}
  echo "Generated xml and/or ent and updated bin file!"
fi # For RunRocoto

# For Crontab
if [ ${AddCrontabToMyCrontab} = "yes" ]; then
  cd ${sWS}
  sFile=${HOME}/cron/mycrontab
  if [ -f ${sFile} ]; then
    echo "Adding crontab to ${sFile}!"
  else
    mkdir ${HOME}/cron
    touch ${sFile}
  fi

  py/add_crontab.py
  crontab ${sFile}
  echo "Added crontab to ${sFile}!"

fi
