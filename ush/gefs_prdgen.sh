#!/bin/ksh
#####################################################################
echo "-----------------------------------------------------"
echo " Script: gefs_prdgen.sh" 
echo " "
echo " Purpose - Perform interpolation and GRIB2 conversion"
echo "           on master GRIB files"
echo "           for one member and one time step."
echo "           Move posted files to /com"
echo "           Alert posted files to DBNet"
echo " "
echo " History - "
echo "    Wobus   - 8/28/07 - New "
echo "    Wobus   - 7/30/10 - move 180-192hr products to pgrbd"
echo "    Hou     - 7/31/14 - adopted for grib2 based processing "
echo "    Meng    - 11/17/16 - Use neighbor interpolation for ICSEV " 
echo "    Meng    - 03/09/17 - Remove grib1, PGRBC and PGRBD generation, "
echo "                         and use unify version to generate all grids"
echo "-----------------------------------------------------"
#####################################################################
set -xa

anlflag=$anlflag
ffhr=$ffhr
fhr=$fhr

case $jobgrid in
   1p0) grid=$grid1p0;;
   2p5) grid=$grid2p5;;
   0p5) grid=$gridp5;;
esac

#export WGRIB=${WGRIB:-$EXECgrib/wgrib}
#export GRBINDEX=${GRBINDEX:-$EXECgrib/grbindex}
#export COPYGB=${COPYGB:-$EXECgrib/copygb}
#export WGRIB2=${WGRIB2:-$EXECgrib/wgrib2}
#export GRB2INDEX=${GRB2INDEX:-$EXECgrib/grb2index}
#export COPYGB2=${COPYGB2:-$EXECgrib/copygb2}
#export CNVGRIB=${CNVGRIB:-$EXECgrib/cnvgrib21_gfs}

echo settings in $0 gefsmachine=$gefsmachine
echo settings in $0 WGRIB=$WGRIB
echo settings in $0 WGRIB2=$WGRIB2
echo settings in $0 GRBINDEX=$GRBINDEX
echo settings in $0 GRB2INDEX=$GRB2INDEX
echo settings in $0 COPYGB=$COPYGB
echo settings in $0 COPYGB2=$COPYGB2
echo settings in $0 CNVGRIB=$CNVGRIB

R1=`echo $RUN|cut -c1-3`
R2=`echo $RUN|cut -c4-5`
case $R1 in
  (gec) if (( R2 == 0 )); then 
           (( e1 = 1 ))
	   (( e2 = 2 ))
	fi;;
  (gen) (( e1 = 2 ))
	(( e2 = R2 ));;
  (gep) (( e1 = 3 ))
	(( e2 = R2 ));;
  (*)   (( e1 = 0 ))
	(( e2 = 0 ))
	echo unrecognized RUN=$RUN R1=$R1 R2=$R2 ;;
esac

msg="Starting post for member=$member ffhr=$ffhr"
postmsg "$jlogfile" "$msg"

####################################
# Step I: Create pgrb2 files 
####################################
if [[ -s $DATA/pgrb2$ffhr$cfsuffix ]] && \
   [[ -s $DATA/pgrb2i$ffhr$cfsuffix ]] && \
   [[ $overwrite = no ]]; then
   echo `date` 1x1 pgrb2 processing skipped for $RUN $ffhr
else
#   $COPYGB2 -g "${grid}" -i0 -x $COMIN/$cyc/master/$RUN.$cycle.master.grb2$ffhr$cfsuffix pgb2file.$ffhr$cfsuffix
   masterfile=$RUN.$cycle.master.grb2$ffhr$cfsuffix
   cp $COMIN/$cyc/master/$masterfile $masterfile
   if [[ -s icesv.grb2 ]]; then rm -f icesv.grb2; fi
   if [[ -s others.grb2 ]]; then rm -f others.grb2; fi
   $WGRIB2 -s $masterfile | grep ":ICSEV:" | wgrib2 -i $masterfile -grib icesv.grb2
   $WGRIB2 -s $masterfile | grep -v ":ICSEV:" | wgrib2 -i $masterfile -grib others.grb2
   $COPYGB2 -g "${grid}" -i0 -x others.grb2 pgb2file.$ffhr$cfsuffix
   $COPYGB2 -g "${grid}" -i2 -a -x icesv.grb2 pgb2file.$ffhr$cfsuffix
   echo `date` pgrb2 $jobgrid grbfile $ffhr completed

######################################################
# STEP II: Split the pgb2file into pgrb2a, pgrb2b parts
######################################################
   if (( fhr == 0 ))
   then
     hsuffix="00"
   else
     hsuffix="hh"
   fi

#  set +x

   excludestring='180-192hr'

   parmlist=$PARMgefs/gefs_pgrb2a_f${hsuffix}.parm
   $WGRIB2 -s pgb2file.$ffhr$cfsuffix | \
       grep -F -f $parmlist | \
       grep -v -F $excludestring | \
       $WGRIB2 pgb2file.$ffhr$cfsuffix -s -i -grib pgb2afile.$ffhr$cfsuffix
   if [[ x$fhoroglist != x ]]; then
      for fhorog in $fhoroglist
      do
	if (( fhr == fhorog )); then
	  $WGRIB2 -s pgb2file.$ffhr$cfsuffix | grep 'HGT:surface' | $WGRIB2 pgb2file.$ffhr$cfsuffix -i -append -grib pgb2afile.$ffhr$cfsuffix
	fi
      done
   fi
   $WGRIB2 -s pgb2afile.$ffhr$cfsuffix > pgb2afile.${ffhr}${cfsuffix}.idx

   parmlist2=$PARMgefs/gefs_pgrb2ab_f${hsuffix}.parm
   $WGRIB2 -s pgb2file.$ffhr$cfsuffix | \
       grep -F -f $parmlist2 | \
       grep -v -F -f $parmlist | \
       grep -v -F $excludestring | \
       $WGRIB2 pgb2file.$ffhr$cfsuffix -s -i -grib pgb2bfile.$ffhr$cfsuffix
   $WGRIB2 -s pgb2bfile.$ffhr$cfsuffix > pgb2bfile.${ffhr}${cfsuffix}.idx

   ##############################################
   # Save Data
   ##############################################
   if test "$SENDCOM" = 'YES'
   then
      #
      # Save Pressure GRIB/Index files
      #
      mv pgb2afile.$ffhr$cfsuffix $fileaout
          if [[ ! -s $fileaout ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
      mv pgb2bfile.$ffhr$cfsuffix $filebout
			testfile=$filebout
          if [[ ! -s $filebout ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
      if [[ "$makegrb2i" = "yes" ]]; then
	mv pgb2afile.$ffhr$cfsuffix.idx $fileaouti
          if [[ ! -s $fileaouti ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
	mv pgb2bfile.$ffhr$cfsuffix.idx $filebouti
          if [[ ! -s $filebouti ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
      fi

      ####################################
      # Send DBNET Alets
      ####################################
      if [[ "$jobgrid" = "1p0" ]]; then
      ###############################################################################
      # Send DBNet alerts for PGB2A at 1p0 at 6 hour increments for all forecast hours
      # Do for 00, 06, 12, and 18Z cycles.
      ###############################################################################
      if test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a ` expr $cyc % 6 ` -eq 0
      then
	if test `echo $RUN | cut -c1-2` = "ge"
	then
	  MEMBER=`echo $RUN | cut -c3-5 | tr '[a-z]' '[A-Z]'`
	  if [[ $fhr -ge 0 && $fhr -le $fhmax && ` expr $fhr % 6 ` -eq 0 && ! -n "$cfsuffix" ]]
	  then
	    $DBNROOT/bin/dbn_alert MODEL ENS_PGB2A_$MEMBER $job $fileaout
	    $DBNROOT/bin/dbn_alert MODEL ENS_PGB2A_${MEMBER}_WIDX $job $fileaouti
	  fi
	fi
      fi

      ###############################################################################
      # Send DBNet alerts for PGB2B at 1p0 at 6 hour increments for up to 84 hours
      # Do for 00Z and 12Z only
      ###############################################################################
       if test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a "$NET" = 'gens'
       then
         if test `echo $RUN | cut -c1-2` = "ge" -a ! -n "$cfsuffix"
	 then
	  MEMBER=`echo $RUN | cut -c3-5 | tr '[a-z]' '[A-Z]'`
	# if [[ $fhr -ge 0 && $fhr -le 84 && ` expr $fhr % 6 ` -eq 0 && ! -n "$cfsuffix" ]]
	# then
	    $DBNROOT/bin/dbn_alert MODEL ENS_PGB2B_$MEMBER $job $filebout
	    $DBNROOT/bin/dbn_alert MODEL ENS_PGB2B_${MEMBER}_WIDX $job $filebouti
	# fi
         fi

       fi
      fi #1p0
   
      if [[ "$jobgrid" = "2p5" ]]; then
      ######################################################################################
      # Send DBNet alerts for PGBA2 at 6 hour increments for all forecast hours
      # Do for 00, 06, 12, and 18Z cycles.
      ######################################################################################
      if test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a ` expr $cyc % 6 ` -eq 0
      then
        if test `echo $RUN | cut -c1-2` = "ge"
        then
          MEMBER=`echo $RUN | cut -c3-5 | tr '[a-z]' '[A-Z]'`
          if test "$DO_LOW_RES" = 'YES' -a ` expr $fhr % 6 ` -eq 0 -a ! -n "$cfsuffix"
          then
            $DBNROOT/bin/dbn_alert MODEL ENS_PGB2A2_$MEMBER $job $fileaout
            if [[ "$makegrb2i" = "yes" ]]; then
            $DBNROOT/bin/dbn_alert MODEL ENS_PGB2A2_${MEMBER}_WIDX $job $fileaouti
            fi
          fi
        fi
      fi

      ######################################################################################
      # Send DBNet alerts for PGB2B at 6 hour increments for forecast hours 0 - 84 and
      # for PGBB2 at 6 hour increments for forecast hours 90 through 384.
      # Do for 00 and 12Z cycles.
      ######################################################################################

      if test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a ` expr $cyc % 12 ` -eq 0
      then
        if test `echo $RUN | cut -c1-2` = "ge"
        then
          MEMBER=`echo $RUN | cut -c3-5 | tr '[a-z]' '[A-Z]'`
          if test "$DO_LOW_RES" = 'YES' -a ` expr $fhr % 6 ` -eq 0 -a $fhr -ge 90 -a ! -n "$cfsuffix"
          then
            $DBNROOT/bin/dbn_alert MODEL ENS_PGB2B2_$MEMBER $job $filebout
            if [[ "$makegrb2i" = "yes" ]]; then
            $DBNROOT/bin/dbn_alert MODEL ENS_PGB2B2_${MEMBER}_WIDX $job $filebouti
            fi
          fi
        fi
      fi
          
      fi #2p5

      if [[ "$jobgrid" = "0p5" ]]; then
      ###############################################################################
      # Send DBNet alerts for  PGB2A at 3 hour increments for all forecast hours
      # Do for 00, 06, 12, and 18Z cycles.
      ###############################################################################
      if test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a ` expr $cyc % 6 ` -eq 0
      then
        if test `echo $RUN | cut -c1-2` = "ge"
        then
          MEMBER=`echo $RUN | cut -c3-5 | tr '[a-z]' '[A-Z]'`
          if [[ $fhr -ge 0 && $fhr -le $fhmax && ` expr $fhr % $FHINCP5 ` -eq 0 && ! -n "$cfsuffix" ]]
          then
            $DBNROOT/bin/dbn_alert MODEL ENS_PGB2A_0P5_$MEMBER $job $fileaout
            $DBNROOT/bin/dbn_alert MODEL ENS_PGB2A_0P5_${MEMBER}_WIDX $job $fileaouti
          fi
        fi
      fi

      ###############################################################################
      # Send DBNet alerts for PGB2B at 3 hour increments for up to 84 hours
      # Do for 00Z and 12Z only
      ###############################################################################
       if test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a "$NET" = 'gens'
       then
         if test `echo $RUN | cut -c1-2` = "ge" -a ! -n "$cfsuffix"
         then
          MEMBER=`echo $RUN | cut -c3-5 | tr '[a-z]' '[A-Z]'`
            $DBNROOT/bin/dbn_alert MODEL ENS_PGB2B_0P5_$MEMBER $job $filebout
            $DBNROOT/bin/dbn_alert MODEL ENS_PGB2B_0P5_${MEMBER}_WIDX $job $filebouti
         fi
       fi
          
      fi #0p5

   fi #$SENDCOM
echo `date` pgrb2a $jobgrid sendcom $ffhr completed
fi

########################################################
echo `date` $sname $member $partltr $cfsuffix $fsuffix $jobgrid GRIB end on machine=`uname -n`
msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################
