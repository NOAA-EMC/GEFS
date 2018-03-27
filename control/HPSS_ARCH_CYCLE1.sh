if [ $# -lt 5 ]; then
  echo $0 requires 5 arguments: stymd + edymd + data type + test id + action + TT
  echo Example : $0 20091201 20091201 com/nwges Ic write/read "00 06 12 18"
  exit
fi

source /u/emc.enspara/.profile.Hong.Guan
ls /usrx/local/prod/hpss/htar

ndate=/gpfs/hps/nco/ops/nwprod/prod_util.v1.0.23/exec/ndate
BDIR=$BDIR
echo BDIR=$BDIR
#Define variables
TASK=$expid
stymd=$1
edymd=$2
type=$3
act=$4
TT=$5

#Define tardir   and filedir
#TDIR=/hpssuser/g01/$LOGNAME/$TASK/$type/$id
echo TDIR=$TDIR
 TDIR=$TDIR/$TASK/$type

 case $type in
  'nwges') tardir=$TDIR;;
  'com') tardir=$TDIR;;
 esac

if [ $act = 'write' ]; then
 case $type in
  'com')  filedir=$BDIR/com/gens/dev/;;
  'nwges') filedir=$BDIR/nwges/dev/;;
 esac
# 'com')  filedir=$BDIR/$task/o/tf$id/com/gens/dev/;;
# 'nwges') filedir=$BDIR/$task/o/tf$id/nwges/dev/;;
 echo $tardir $filedir
else
 case $type in
  'com')  filedir=/ptmp/wx20dh/$TASK\COM/$id/;;
  'nwges') filedir=/ptmp/wx20dh/$TASK\NWGS/$id;;
 esac
fi

if [ -d $filedir ]; then
 cd $filedir
else
 echo $filedir does not exist!!!
 exit
fi

mmdd=$stymd
while [ $mmdd -le $edymd ]; do

 case $type in
  'com') tobecopiedfiles=gefs.$mmdd; tmpdir=ens.$mmdd;;
  'nwges') tobecopiedfiles=gefs.$mmdd;;
 esac

if [ $act = 'write' ]; then
#WRITING FILES

#WRITE nwges files
 if [ $type = 'nwges' ]; then
  pwd
  for tt in $TT
  do
   htar -P -Y 222 -cvf $tardir/ens.$mmdd\_$tt\.tar $tobecopiedfiles\/*t$tt\z.sanl* $tobecopiedfiles\/gec00.t$tt\z.sfcanl
  done
 fi

#WRITE com directories
 if [ $type = 'com' ]; then
  pwd
  for tt in $TT
  do
   for sub in  pgrb2a1p0 ensstat 
   do  
    htar -P -Y 222 -cvf $tardir/$tt\/ens.$mmdd\_$sub\.tar $tobecopiedfiles\/$tt\/$sub\/ 
   done
  done
 fi

else
#READING FILES

#READ nwges files
 if [ $type = 'nwges' ]; then
  for tt in $TT
  do
   htar -P -Y 222 -xvf $tardir/ens.$mmdd_$tt\.tar
  done
 fi

 if [ $type = 'com' ]; then
  for tt in $TT
  do
   for sub in pgrb2a ensstat sflux 
   do  
    htar -P -Y 222 -xvf $tardir/$tt\/ens.$mmdd\_$sub\.tar 
   done
  done
 fi

fi   #($act)

ymdh=$mmdd\00
mmdd=`$ndate +24 $ymdh | cut -c1-8`

done
