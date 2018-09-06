#!/bin/ksh
#####################################################################
echo "-----------------------------------------------------"
echo " gefs_prdgen_gfs.sh"
echo " originally named exglobal_post_gfs.sh.sms"
echo " and then exgefs_prdgen_gfs.sh.sms"
echo " Feb 06 - Wobus - new script to convert gfs to pgrba"
echo " Jul 11 - Wobus - rename to exgefs_prdgen_gfs.sh.sms"
echo " Oct 14 - Hou -  Createded exgefs_prdgen_gfs_grb2.sh.sms following"
echo "                 exgefs_prdgen_gfs.sh.sms but for grib2 input files"
echo " Jan 15 - Hou -  renamed exgefs_prdgen_gfs.sh.ecf"
echo " April 10 - Hou - moved to ush and renamed gefs_prdgen_gfs.sh"
echo "-----------------------------------------------------"
#####################################################################

set -xa

#export WGRIB=${WGRIB:-$EXECgrib/wgrib}
#export GRBINDEX=${GRBINDEX:-$EXECgrib/grbindex}
#export COPYGB=${COPYGB:-$EXECgrib/copygb}
#export WGRIB2=${WGRIB2:-$EXECgrib/wgrib2}
#export GRB2INDEX=${GRB2INDEX:-$EXECgrib/grb2index}
#export COPYGB2=${COPYGB2:-$EXECgrib/copygb2}
#export CNVGRIB=${CNVGRIB:-$EXECgrib/cnvgrib21_gfs}

export ENSADD=${ENSADD:-$USHgefs/global_ensadd.sh}
export TRANSG=${TRANSG:-$USHgefs/gefs_transfer_gfs.sh}

echo WGRIB=$WGRIB
echo WGRIB2=$WGRIB2
echo COPYGB=$COPYGB
echo COPYGB2=$COPYGB2
echo CNVGRIB=$CNVGRIB
echo GRBINDEX=$GRBINDEX
echo GRB2INDEX=$GRB2INDEX
echo ENSADD=$ENSADD

parm00=$PARMgefs/gefs_pgrb2a_f00.parm
parmhh=$PARMgefs/gefs_pgrb2a_fhh.parm

case $jobgrid in
  1p0) dirsuf=
       filsuf=
       filetail=
       GRID=
       grid=${grid1p0}
       ;;
  2p5) dirsuf=lr
       filsuf=
       filetail=.2
       GRID=
       grid=${grid2p5}
       ;;
  0p5) dirsuf=p5
       filsuf=.0p50.
       filetail=
       GRID=_0P5
       grid=${gridp5}
       ;;
esac

# JY export makegrb2i=no
export makegrb2i=yes

# set variables for ensemble PDS header
(( e1 = 0 ))
(( e2 = 0 ))

############################################################
#  Define Variables:
#  -----------------
#  SHOUR        is the starting forecast hour. normally 0 except for restarts.
#  FHOUR        is the ending forecast hour.
#  FHINC        is the increment hour for each forecast steps.
#  FH           is the current forecast hour.
#  SLEEP_TIME   is the number of seconds to sleep before exiting with error.
#  SLEEP_INT    is the number of seconds to sleep between restrt file checks.
#  restart_file is the name of the file to key off of to kick off post.
############################################################

############################################################
# Post Analysis Files before starting the Forecast Post
############################################################
if test -f $COMINgfs/${RUN}.t${cyc}z.master.grb2anl -a ${SHOUR} -eq 0 -a "$jobgrid" != '2p5'
then

   # RLW 20110725 more complete cleanup of temporary files
   rm -f master_grb2file
   rm -f tmpfile
   rm -f pgbafile pgbaifile
   rm -f pgb2afile pgb2aifile

   parmlist=$parm00 
   ln -s $COMINgfs/${RUN}.t${cyc}z.master.grb2anl master_grb2file
   $WGRIB2 -s master_grb2file |grep -F -f $parmlist |$WGRIB2 master_grb2file -i -grib tmpfile
   $COPYGB2 -g "${grid}" -i0 -x tmpfile pgb2afile
   $GRB2INDEX pgb2afile pgb2aifile
   $ENSADD $e1 $e2 pgb2afile epgbafile
   mv epgbafile pgb2afile
      if [[ "$makepgrb1" = "yes" ]]; then
   $CNVGRIB -g21 pgb2afile pgbafile
   $GRBINDEX pgbafile pgbaifile
      fi

   if test "$SENDCOM" = 'YES'
   then
      #
      # Save Pressure GRIB/Index files
      #
      if [[ "$makepgrb2" = "yes" ]]; then
       mv pgb2afile $COMOUT/$cyc/pgrb2a$dirsuf/ge${RUN}.${cycle}.pgrb2a$filsuf\anl
	   testfile=$COMOUT/$cyc/pgrb2a$dirsuf/ge${RUN}.${cycle}.pgrb2a$filsuf\anl
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
       if [[ "$makegrb2i" = "yes" ]]; then
	  mv pgb2aifile $COMOUT/$cyc/pgrb2a$dirsuf/ge${RUN}.${cycle}.pgrb2a$filsuf\anl.idx
	       testfile=$COMOUT/$cyc/pgrb2a$dirsuf/ge${RUN}.${cycle}.pgrb2a$filsuf\anl.idx
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
       fi
      fi

#################################### CHECK DBN ALERTS ###########
      if test "$SENDDBN" = 'YES'
      then
         if test "$NET" = 'gens'
         then
	   MEMBER=GFS
	   if [[ "$makepgrb2" = "yes" ]]; then
	     $DBNROOT/bin/dbn_alert MODEL ENS_PGB2A${GRID}_$MEMBER $job $COMOUT/$cyc/pgrb2a$dirsuf/ge${RUN}.${cycle}.pgrb2a$filsuf\anl
	   fi
	   #if [[ "$makegrb2i" = "yes" ]]; then
	   #    $DBNROOT/bin/dbn_alert MODEL ENS_PGB2AI${GRID}_$MEMBER $job $COMOUT/$cyc/pgrb2a$dirsuf/ge${RUN}.${cycle}.pgrb2a$filsuf\ianl
	   #fi
         fi
      fi
   fi
fi

SLEEP_LOOP_MAX=`expr $SLEEP_TIME / $SLEEP_INT`

(( fhr=SHOUR))
if (( fhr == 0 )); then
  fhr=00
fi
export fhr

if test $fhr -lt 10 -a $fhr -gt 0
then
   export fhr="0$fhr"
fi

############################################################
# Loop Through the Post Forecast Files 
############################################################
while test $fhr -le $FHOUR
do

if test $fhr -lt 100
then
   export mfhr="0$fhr"
else
   export mfhr=$fhr
fi

    ###############################
    # Start Looping for the 
    # existence of the restart files
    ###############################
    #set +x
    set -x
    export pgm="postcheck"
    ic=1

    while [ $ic -le $SLEEP_LOOP_MAX ]
    do
#      if [[ -s $restart_file_a$fhr ]] && [[ -s $restart_file_b$fhr ]]; then
       if test -f $COMINgfs/${RUN}.t${cyc}z.master.grb2f$mfhr
       then
	  found=yes
          break
       else
          ic=`expr $ic + 1`
          sleep $SLEEP_INT
       fi
       if (( fhr > 180 )) && (( fhr % 12 == 6 )); then
	 if (( ic > 2 )) && (( ic < SLEEP_LOOP_MAX )); then
	   echo fhr=$fhr not expected
	   found=no
	   break
	 fi
       fi
       ###############################
       # If we reach this point assume
       # fcst job never reached restart 
       # period and error exit
       ###############################
       if [ $ic -eq $SLEEP_LOOP_MAX ]
       then
          export err=9
          err_chk
       fi
    done
    #set -x

  if [[ "$found" = yes ]]; then

    # RLW 20110725 more complete cleanup of temporary files
    rm -f master_grb2file
    rm -f tmpfile
    rm -f pgbafile pgbaifile
    rm -f pgb2afile pgb2aifile

    msg="Starting prdgen for fhr=$fhr"
    postmsg "$jlogfile" "$msg"

    if [ $fhr -eq 0 ]
    then
      parmlist=$parm00 
    else
      parmlist=$parmhh 
    fi

    ln -s $COMINgfs/${RUN}.t${cyc}z.master.grb2f${mfhr} master_grb2file

    $WGRIB2 -s master_grb2file |grep -F -f $parmlist |$WGRIB2 master_grb2file -i -grib tmpfile_tem
   rm ex-list*
   if (( fhr > 6 )); then
    $WGRIB2 -s tmpfile_tem | grep "APCP" | grep "0-" > ex-list1
   fi
    $WGRIB2 -s tmpfile_tem | grep -e CSN -e CIC -e CFR -e CRA | grep "hour fcst" > ex-list
    cat ex-list1 >> ex-list
    $WGRIB2 -s tmpfile_tem | grep -v -f ex-list | $WGRIB2 -i tmpfile_tem -grib tmpfile
    if [[ x$fhoroglist != x ]]; then
      for fhorog in $fhoroglist
      do
	if (( fhr == fhorog )); then
	  $WGRIB2 -s master_grb2file |grep 'HGT:surface' |$WGRIB2 master_grb2file -i -append -grib tmpfile 
	fi
      done
    fi

    $COPYGB2 -g "${grid}" -i0 -x tmpfile pgb2afile
    $GRB2INDEX pgb2afile pgb2aifile
    $ENSADD $e1 $e2 pgb2afile epgbafile
    mv epgbafile pgb2afile
      if [[ "$makepgrb1" = "yes" ]]; then
    $CNVGRIB -g21 pgb2afile pgbafile
    $GRBINDEX pgbafile pgbaifile
      fi
    
 if [[ $jobgrid == 0p5 ]]; then
#For the 0.5 degree grid pgrb files, name them with 3-digit (0-999) fcst hours 
    if test $fhr -lt 100
    then
      pgfhr=0$fhr
    else  
      pgfhr=$fhr
    fi
 else
#For the 1.0 and 2.5 degree grid pgrb files, name them with 2 digit (00-99)or  3-digit fcst hours 
     pgfhr=$fhr
 fi

    $TRANSG pgrb2a$dirsuf pgrb2a$filsuf

    if test $SENDCOM = "YES"
    then
       if test $fhr -lt 100
       then
          pad="0"
       else
	pad=""
       fi
       echo "$PDY$cyc$pad$fhr" > $COMOUT/$cyc/misc/gfs/ge${RUN}.t${cyc}z.control.$fhr$jobgrid
    fi

  fi
# if not found, come here to increment

 if [[ $jobgrid == 0p5 ]] && [[ $fhr == $fhmaxh ]]; then
  FHINC=6
 fi 
  export fhr=`expr $fhr + $FHINC`
  if test $fhr -lt 10
  then
     export fhr="0$fhr"
  fi
done

cat $pgmout

########################################################

echo `date` $0 end
msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################
