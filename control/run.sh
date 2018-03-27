#!/bin/ksh
#
#BSUB -J hpss.19990113
#BSUB -P GEN-T2O
#BSUB -o /gpfs/hps/ptmp/emc.enspara/Hong.Guan/o/GEFS_ext_FV3/com/output/dev/19990113/hpss.19990113.o1
#BSUB -e /gpfs/hps/ptmp/emc.enspara/Hong.Guan/o/GEFS_ext_FV3/com/output/dev/19990113/hpss.19990113.o1
#BSUB -cwd /gpfs/hps/ptmp/emc.enspara/Hong.Guan/o/GEFS_ext_FV3/tmpnwprd
#BSUB -W 03:00
#BSUB -L /bin/ksh
#BSUB -q dev_transfer
#BSUB -R "rusage[mem=1650]"
##BSUB -R affinity[core]
export mmdd=19990113
#export mem=
#echo mem=
echo mmdd=19990113
export TDIR=/NCEPDEV/emc-ensemble/5year/emc.enspara/Hong.Guan/GEFS
export BDIR=/gpfs/hps/ptmp/emc.enspara/Hong.Guan/o/GEFS
export task=GEFS
/gpfs/hps3/emc/ensemble/save/emc.enspara/Hong.Guan/GEFS/nwdev/control/HPSS_ARCH_CYCLE.sh 19990113 19990113 com write "00" 
