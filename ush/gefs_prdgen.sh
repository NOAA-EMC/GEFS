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
echo "-----------------------------------------------------"
#####################################################################
set -xa

anlflag=$anlflag
ffhr=$ffhr
fhr=$fhr

#export WGRIB=${WGRIB:-/nwprod/util/exec/wgrib}
#export GRBIDX=${GRBIDX:-/nwprod/util/exec/grbindex}
export ENSADD=${ENSADD:-$USHgefs/global_ensadd.sh}
#export CNVGRIB=${CNVGRIB:-/nwprod/util/exec/cnvgrib}
#export COPYGB=${COPYGB:-/nwprod/util/exec/copygb}
#export WGRIB2=${WGRIB2:-/nwprod/util/exec/wgrib2}
export GRBIDX=$GRBINDEX

echo settings in $0 gefsmachine=$gefsmachine
echo settings in $0 WGRIB=$WGRIB
echo settings in $0 GRBIDX=$GRBIDX
echo settings in $0 ENSADD=$ENSADD
echo settings in $0 COPYGB=$COPYGB
echo settings in $0 CNVGRIB=$CNVGRIB
echo settings in $0 WGRIB2=$WGRIB2


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
# Step I: Create 1x1 pgrb files 
####################################
if [[ -s $DATA/pgrb$ffhr$cfsuffix ]] && \
   [[ -s $DATA/pgrbi$ffhr$cfsuffix ]] && \
   [[ $overwrite = no ]]; then
   echo `date` 1x1 pgrb processing skipped for $RUN $ffhr
else
   $COPYGB -g3 -i0 -x $COMIN/$cyc/master/$RUN.$cycle.master.grb$ffhr$cfsuffix pgbfile.$ffhr$cfsuffix
   $ENSADD $e1 $e2 pgbfile.$ffhr$cfsuffix pgbifile.$ffhr$cfsuffix epgbfile.$ffhr$cfsuffix
   if [[ "$addgrb1id" = "yes" ]]; then
     mv epgbfile.$ffhr$cfsuffix pgbfile.$ffhr$cfsuffix
     if [[ "$makegrb1i" = "yes" ]]; then
       $GRBIDX pgbfile.$ffhr$cfsuffix pgbifile.$ffhr$cfsuffix
     fi
   fi
   echo `date` pgrba 1x1 grbfile $ffhr completed

   ######################################################
   # Split the pgbfile into pgrba, pgrbb and pgrbd parts
   ######################################################
   if (( fhr == 0 ))
   then
     hsuffix="00"
   else
     hsuffix="hh"
   fi

   set +x

   excludestring='180-192hr'

   (parmlist=$PARMgefs/gefs_pgrba_f${hsuffix}.parm
   $WGRIB -s pgbfile.$ffhr$cfsuffix | \
       grep -F -f $parmlist | \
       grep -v -F $excludestring | \
       $WGRIB pgbfile.$ffhr$cfsuffix -s -grib -i -o pgbafile.$ffhr$cfsuffix
   if [[ x$fhoroglist != x ]]; then
      for fhorog in $fhoroglist
      do
	if (( fhr == fhorog )); then
	  $WGRIB -s pgbfile.$ffhr$cfsuffix | grep 'HGT:sfc' | $WGRIB pgbfile.$ffhr$cfsuffix -i -grib -append -o pgbafile.$ffhr$cfsuffix
	fi
      done
   fi
   $GRBIDX pgbafile.$ffhr$cfsuffix pgbaifile.$ffhr$cfsuffix) &

   (parmlist=$PARMgefs/gefs_pgrbb_f${hsuffix}.parm
   $WGRIB -s pgbfile.$ffhr$cfsuffix | \
       grep -F -f $parmlist | \
       grep -v -F $excludestring | \
       $WGRIB pgbfile.$ffhr$cfsuffix -s -grib -i -o pgbbfile.$ffhr$cfsuffix
   $GRBIDX pgbbfile.$ffhr$cfsuffix pgbbifile.$ffhr$cfsuffix)&

   if [[ $CREATE_PGRBD = YES ]]; then
   (parmlista=$PARMgefs/gefs_pgrba_f${hsuffix}.parm
   parmlistb=$PARMgefs/gefs_pgrbb_f${hsuffix}.parm
   $WGRIB -s pgbfile.$ffhr$cfsuffix | \
       grep -v -F -f $parmlista | \
       grep -v -F -f $parmlistb | \
       $WGRIB pgbfile.$ffhr$cfsuffix -s -grib -i -o pgbdfile.$ffhr$cfsuffix
   $WGRIB -s pgbfile.$ffhr$cfsuffix | \
       grep -F -f $parmlista | \
       grep -F $excludestring | \
       $WGRIB pgbfile.$ffhr$cfsuffix -s -grib -i -append -o pgbdfile.$ffhr$cfsuffix
   $WGRIB -s pgbfile.$ffhr$cfsuffix | \
       grep -F -f $parmlistb | \
       grep -F $excludestring | \
       $WGRIB pgbfile.$ffhr$cfsuffix -s -grib -i -append -o pgbdfile.$ffhr$cfsuffix
   $GRBIDX pgbdfile.$ffhr$cfsuffix pgbdifile.$ffhr$cfsuffix)&
   fi
   set -x

   wait

   if test "$SENDCOM" = 'YES'
   then
      #
      # Save Pressure GRIB/Index files
      #
      mv pgbafile.$ffhr$cfsuffix $COMOUT/$cyc/pgrba/${RUN}.${cycle}.pgrba$ffhr$cfsuffix
                        testfile=$COMOUT/$cyc/pgrba/${RUN}.${cycle}.pgrba$ffhr$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
      mv pgbbfile.$ffhr$cfsuffix $COMOUT/$cyc/pgrbb/${RUN}.${cycle}.pgrbb$ffhr$cfsuffix
                        testfile=$COMOUT/$cyc/pgrbb/${RUN}.${cycle}.pgrbb$ffhr$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi

   if [[ $CREATE_PGRBD = YES ]]; then
      mv pgbdfile.$ffhr$cfsuffix $COMOUT/$cyc/pgrbd/${RUN}.${cycle}.pgrbd$ffhr$cfsuffix
                        testfile=$COMOUT/$cyc/pgrbd/${RUN}.${cycle}.pgrbd$ffhr$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
   fi
      if [[ "$makegrb1i" = "yes" ]]; then
	mv pgbaifile.$ffhr$cfsuffix $COMOUT/$cyc/pgrba/${RUN}.${cycle}.pgrbai$ffhr$cfsuffix
                           testfile=$COMOUT/$cyc/pgrba/${RUN}.${cycle}.pgrbai$ffhr$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
	mv pgbbifile.$ffhr$cfsuffix $COMOUT/$cyc/pgrbb/${RUN}.${cycle}.pgrbbi$ffhr$cfsuffix
			   testfile=$COMOUT/$cyc/pgrbb/${RUN}.${cycle}.pgrbbi$ffhr$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
   if [[ $CREATE_PGRBD = YES ]]; then
	mv pgbdifile.$ffhr$cfsuffix $COMOUT/$cyc/pgrbd/${RUN}.${cycle}.pgrbdi$ffhr$cfsuffix
   fi
      fi

      ###############################################################################
      # Send DBNet alerts for PGBA and PGBA2 at 6 hour increments for all forecast hours
      # Do for 00, 06, 12, and 18Z cycles.
      ###############################################################################
      if test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a ` expr $cyc % 6 ` -eq 0
      then
	if test `echo $RUN | cut -c1-2` = "ge"
	then
	  MEMBER=`echo $RUN | cut -c3-5 | tr '[a-z]' '[A-Z]'`
	  if [[ $fhr -ge 0 && $fhr -le $fhmax && ` expr $fhr % 6 ` -eq 0 && ! -n "$cfsuffix" ]]
	  then
	    $DBNROOT/bin/dbn_alert MODEL GEFS_LEGACY $job $COMOUT/$cyc/pgrba/${RUN}.${cycle}.pgrba$ffhr$cfsuffix
	  fi
	fi
      fi

      ###############################################################################
      # Send DBNet alerts for PGBB and PGBB2 at 6 hour increments for up to 84 hours
      # Do for 00Z and 12Z only
      ###############################################################################
      #if test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a ` expr $cyc % 12 ` -eq 0
      #then
        #if test `echo $RUN | cut -c1-2` = "ge"
	#then
	# MEMBER=`echo $RUN | cut -c3-5 | tr '[a-z]' '[A-Z]'`
	# if [[ $fhr -ge 0 && $fhr -le 84 && ` expr $fhr % 6 ` -eq 0 && ! -n "$cfsuffix" ]]
	# then
	#   $DBNROOT/bin/dbn_alert MODEL GEFS_LEGACY $job $COMOUT/$cyc/pgrbb/${RUN}.${cycle}.pgrbb$ffhr$cfsuffix
	# fi
      #fi
     #fi

      ###############################################################################
      # Do Not send DBNet alerts for the PGBD files at this time
      ###############################################################################
   fi
fi
echo `date` pgrba 1x1 sendcom $ffhr completed

  case $gefsmachine in
    (dell)
      fmakegb2=1
    ;;
    (wcoss)
      fmakegb2=1
    ;;
    (zeus)
      fmakegb2=0
    ;;
  esac
if (( fmakegb2 == 1 )); then
#DHOU 03/23/2012, skip grib2 files for ZEUS
######################################
# Step II: Create GRIB2A files
#####################################
if [[ -s $COMOUT/$cyc/pgrb2a/${RUN}.${cycle}.pgrb2a$ffhr$cfsuffix ]] && \
   [[ -s $COMOUT/$cyc/pgrb2a/${RUN}.${cycle}.pgrb2ai$ffhr$cfsuffix ]] && \
   [[ $overwrite = no ]]; then
   echo `date` 1x1 pgrb2a processing skipped for $RUN $ffhr
else
   FILEA=$COMIN/$cyc/pgrba/${RUN}.${cycle}.pgrba$ffhr$cfsuffix
   $CNVGRIB -g12 -p40 $FILEA pgb2afile.$ffhr$cfsuffix
   $WGRIB2 -s pgb2afile.$ffhr$cfsuffix > pgb2afile.${ffhr}${cfsuffix}.idx

   if test "$SENDCOM" = 'YES'
   then
     #
     # Save Pressure GRIB/Index files
     #
     mv pgb2afile.$ffhr$cfsuffix $COMOUT/$cyc/pgrb2a/${RUN}.${cycle}.pgrb2a$ffhr$cfsuffix
                        testfile=$COMOUT/$cyc/pgrb2a/${RUN}.${cycle}.pgrb2a$ffhr$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
     mv pgb2afile.${ffhr}${cfsuffix}.idx $COMOUT/$cyc/pgrb2a/${RUN}.${cycle}.pgrb2a${ffhr}${cfsuffix}.idx
			        testfile=$COMOUT/$cyc/pgrb2a/${RUN}.${cycle}.pgrb2a${ffhr}${cfsuffix}.idx
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi

     if test "$SENDDBN_GB2" = 'YES'
     then
       if test "$NET" = 'gens'
       then
         if test `echo $RUN | cut -c1-2` = "ge" -a ! -n "$cfsuffix"
         then
           MEMBER=`echo $RUN | cut -c3-5 | tr '[a-z]' '[A-Z]'`
           $DBNROOT/bin/dbn_alert MODEL GEFS_LEGACY $job $COMOUT/$cyc/pgrb2a/${RUN}.${cycle}.pgrb2a$ffhr$cfsuffix
	   if test "$SENDDBN_GB2_IDX" = 'YES'
	   then
	     $DBNROOT/bin/dbn_alert MODEL GEFS_LEGACY $job \
	       $COMOUT/$cyc/pgrb2a/${RUN}.${cycle}.pgrb2a${ffhr}${cfsuffix}.idx
	   fi
         fi
       fi
     fi
   fi
fi

###########################################
# STEP III: Create GRIB2B and GRIB2C files
###########################################
if [[ -s $COMOUT/$cyc/pgrb2b/${RUN}.${cycle}.pgrb2b$ffhr$cfsuffix ]] && \
   [[ -s $COMOUT/$cyc/pgrb2b/${RUN}.${cycle}.pgrb2bi$ffhr$cfsuffix ]] && \
   [[ $overwrite = no ]]; then
   echo `date` 1x1 pgrb2b processing skipped for $RUN $ffhr
else
   FILEB=$COMIN/$cyc/pgrbb/${RUN}.${cycle}.pgrbb$ffhr$cfsuffix
   $CNVGRIB -g12 -p40 $FILEB pgb2bfile.$ffhr$cfsuffix
   $WGRIB2 -s pgb2bfile.$ffhr$cfsuffix > pgb2bfile.${ffhr}${cfsuffix}.idx

   if test "$SENDCOM" = 'YES'
   then
      #
      # Save Pressure GRIB/Index files
      #
      mv pgb2bfile.$ffhr$cfsuffix $COMOUT/$cyc/pgrb2b/${RUN}.${cycle}.pgrb2b$ffhr$cfsuffix
                         testfile=$COMOUT/$cyc/pgrb2b/${RUN}.${cycle}.pgrb2b$ffhr$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
      mv pgb2bfile.${ffhr}${cfsuffix}.idx $COMOUT/$cyc/pgrb2b/${RUN}.${cycle}.pgrb2b${ffhr}${cfsuffix}.idx
				 testfile=$COMOUT/$cyc/pgrb2b/${RUN}.${cycle}.pgrb2b${ffhr}${cfsuffix}.idx
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi

      if test "$CREATE_TIGGE" = 'YES'
      then
	 if (( fhr == 0 )); then
	   parmlist=${PARMgefs}/gefs_pgrbc_f00.parm
	 else
	   parmlist=${PARMgefs}/gefs_pgrbc_fhh.parm
	 fi
         set +x
	 $WGRIB $COMOUT/$cyc/pgrbb/${RUN}.${cycle}.pgrbb$ffhr$cfsuffix | \
	   grep -F -f $parmlist | \
	   $WGRIB -i -grib -o pgbcfile.$ffhr$cfsuffix $COMOUT/$cyc/pgrbb/${RUN}.${cycle}.pgrbb$ffhr$cfsuffix
         set -x
	 $CNVGRIB -g12 -p40 pgbcfile.$ffhr$cfsuffix pgb2cfile.$ffhr$cfsuffix
	 mv pgb2cfile.$ffhr$cfsuffix $COMOUT/$cyc/pgrb2c/${RUN}.${cycle}.pgrb2c$ffhr$cfsuffix
                            testfile=$COMOUT/$cyc/pgrb2c/${RUN}.${cycle}.pgrb2c$ffhr$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
	 rm pgbcfile.$ffhr$cfsuffix
      fi

      if test "$SENDDBN_GB2" = 'YES'
      then
        if test "$NET" = 'gens'
        then
          if test `echo $RUN | cut -c1-2` = "ge" -a ! -n "$cfsuffix"
          then
            MEMBER=`echo $RUN | cut -c3-5 | tr '[a-z]' '[A-Z]'`
            $DBNROOT/bin/dbn_alert MODEL GEFS_LEGACY $job $COMOUT/$cyc/pgrb2b/${RUN}.${cycle}.pgrb2b$ffhr$cfsuffix
	    if test "$SENDDBN_GB2_IDX" = 'YES'
	    then
	      $DBNROOT/bin/dbn_alert MODEL GEFS_LEGACY $job \
                   $COMOUT/$cyc/pgrb2b/${RUN}.${cycle}.pgrb2b$ffhr${cfsuffix}.idx
	    fi
            
            if test "$CREATE_TIGGE" = 'YES'
            then
              $DBNROOT/bin/dbn_alert MODEL GEFS_LEGACY $job $COMOUT/$cyc/pgrb2c/${RUN}.${cycle}.pgrb2c$ffhr$cfsuffix
            fi
          fi
        fi
      fi
    fi
fi

   if [[ $CREATE_PGRBD = YES ]]; then
###############################
# STEP IV: Create GRIB2D files
###############################
if [[ -f $COMOUT/$cyc/pgrb2d/${RUN}.${cycle}.pgrb2d$ffhr$cfsuffix ]] && \
   [[ -f $COMOUT/$cyc/pgrb2d/${RUN}.${cycle}.pgrb2di$ffhr$cfsuffix ]] && \
   [[ $overwrite = no ]]; then
   echo `date` 1x1 pgrb2d processing skipped for $RUN $ffhr
else

   FILED=$COMIN/$cyc/pgrbd/${RUN}.${cycle}.pgrbd$ffhr$cfsuffix

   $CNVGRIB -g12 -p40 $FILED pgb2dfile.$ffhr$cfsuffix

   if test "$SENDCOM" = 'YES'
   then
      #
      # Save Pressure GRIB/Index files
      #
      mv pgb2dfile.$ffhr$cfsuffix $COMOUT/$cyc/pgrb2d/${RUN}.${cycle}.pgrb2d$ffhr$cfsuffix
			 testfile=$COMOUT/$cyc/pgrb2d/${RUN}.${cycle}.pgrb2d$ffhr$cfsuffix
          if [[ ! -s $testfile ]]; then
            msg="FATAL ERROR: $testfile WAS NOT WRITTEN"
            echo "`date`    $msg"
            postmsg "$jlogfile" "$msg"
            export err=1
            err_chk
          fi
   fi
fi
   fi

fi #(0=1 foe ZEUS, skip grib2 files)
########################################################
echo `date` $sname $member $partltr $cfsuffix $fsuffix 1x1 GRIB end on machine=`uname -n`
msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################
