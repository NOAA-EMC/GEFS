#!/bin/ksh
#BSUB -J FS2019040200010.190403111947
#BSUB -P GEN-T2O
#BSUB -o /gpfs/dell2/ptmp/Bo.Cui/o/GEFS_V11.3.0/com/output/dev/20190402/jgefs_avgspr_gempak_00.o%J 
#BSUB -e /gpfs/dell2/ptmp/Bo.Cui/o/GEFS_V11.3.0/com/output/dev/20190402/jgefs_avgspr_gempak_00.o%J
#BSUB -cwd /gpfs/dell2/ptmp/Bo.Cui/o/GEFS_V11.3.0/tmpnwprd
#BSUB -W 00:30
#BSUB -q "debug" 
#BSUB -n 4
#BSUB -R span[ptile=4]
#BSUB -R affinity[core]
#BSUB -x  
#

# EXPORT list here
set -x

NWROOT=/gpfs/dell2/emc/retros/noscrub/Bo.Cui/GEFS_V11.3.0/nwdev
export cyc=00    
export PDY=20190402

#%include <head.h>
#%include <envir-p2.h>

#export cyc=%CYC%
#export cyc_fcst=%CYC%

########################
# add the following part
########################

  . /usrx/local/prod/lmod/lmod/init/ksh
  module use /gpfs/dell1/nco/ops/nwprod/modulefiles
  module load prod_util/1.1.0
  module load grib_util/1.1.0
  module load util_shared/1.1.0

  module load EnvVars/1.0.2
  module load ips/18.0.1.163
  module load lsf/10.1
  module load impi/18.0.1
  module load CFP/2.0.1
  export USE_CFP=YES


# CALL executable job script here

VERSION_FILE=$NWROOT/versions/gefs.ver
if [ -f $VERSION_FILE ]; then
  . $VERSION_FILE
else
  ecflow_client --msg="***JOB ${ECFNAME} ERROR: Version File $VERSION_FILE does not exist ***"
  ecflow_client --abort
  exit
fi

$NWROOT/jobs/JGEFS_AVGSPR_GEMPAK
#$NWROOT/gefs.${gefs_ver}/jobs/JGEFS_AVGSPR_GEMPAK

%include <tail.h>
%manual
######################################################################

# Purpose:
# to convert GEFS AVG and SPR pgrb to gempak format
#
######################################################################

######################################################################
# Job specific troubleshooting instructions:
#  see generic troubleshoot manual page
#
######################################################################

# include manual page below
%end
