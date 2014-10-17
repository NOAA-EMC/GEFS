#!/bin/sh

#####################################################################
echo "-----------------------------------------------------"
echo " Script: global_transfer.sh" 
echo " "
echo " Purpose - Copy Global Posts to /nwges and /com"
echo "           Alert posted files to DBNet"
echo " "
echo " History - "
echo "    Facey   - 9/10/96 - Implemented on C90"
echo "    Michaud - 6/03/99 - Converted to IBM SP"
echo "    Wobus   - 8/25/03 - Changes for Ens 4cyc x 180hr "
echo "    Wobus   - 6/08/05 - Changes for 6-hour breeding "
echo "-----------------------------------------------------"
#####################################################################
set -xa
 
if test "$SAVEGES" = "YES" -a $fhr -le 15
then
  (cp $COMOUT/$cyc/sfcsig/${RUN}.t${cyc}z.sf$fhr$cfsuffix $GESdir/${RUN}.${cycle}.sf$fhr$cfsuffix)&
  cp $COMOUT/$cyc/sfcsig/${RUN}.t${cyc}z.bf$fhr$cfsuffix $GESdir/${RUN}.${cycle}.bf$fhr$cfsuffix
  wait
  msg="Guess files for fcst hour $fhr copied to $GESdir"
  postmsg "$jlogfile" "$msg"
fi
 
if test "$SENDCOM" = "YES"
then
   #
   # Save Pressure and SFLUX GRIB/GRIB Index files
   #
   mv flxifile $COMOUT/$cyc/sflux/${RUN}.${cycle}.sfluxgrbif$fhr$cfsuffix
   mv pgbafile $COMOUT/$cyc/pgrba/${RUN}.${cycle}.pgrbaf$fhr$cfsuffix
   mv pgbbfile $COMOUT/$cyc/pgrbb/${RUN}.${cycle}.pgrbbf$fhr$cfsuffix
   if [[ "$makegrb1i" = "yes" ]]; then
     mv pgbaifile $COMOUT/$cyc/pgrba/${RUN}.${cycle}.pgrbaif$fhr$cfsuffix
     mv pgbbifile $COMOUT/$cyc/pgrbb/${RUN}.${cycle}.pgrbbif$fhr$cfsuffix
   fi
 
   # Save Sig and sfc fcst files

   if [[ $RUN = ge??? ]]
   then
     if [[ $fhr -gt 90 ]]
     then
       if [[ $fhr -gt 126 ]]
       then
	 rm $COMOUT/$cyc/sfcsig/${RUN}.${cycle}.sf$fhr$cfsuffix
	 rm $COMOUT/$cyc/sfcsig/${RUN}.${cycle}.bf$fhr$cfsuffix
       fi
       #
       # Save COM flux files
       #
       rm $COMOUT/$cyc/sflux/${RUN}.${cycle}.sfluxgrbf$fhr$cfsuffix
       rm $COMOUT/$cyc/sflux/${RUN}.${cycle}.sfluxgrbif$fhr$cfsuffix
       # remove the 3-hourly files as well
       fhrm3=$((fhr-3))
       if [ $fhrm3 -lt 10 ]; then fhrm3=0$fhrm3; fi
       rm $COMOUT/$cyc/sflux/${RUN}.${cycle}.sfluxgrbf$fhrm3$cfsuffix
       rm $COMOUT/$cyc/sflux/${RUN}.${cycle}.sfluxgrbif$fhrm3
     else
       if [[ $fhr -le 90 && $cycsuffix = true ]]
       then
          rm $COMOUT/$cyc/sflux/${RUN}.${cycle}.sfluxgrbf$fhr$cfsuffix
          rm $COMOUT/$cyc/sflux/${RUN}.${cycle}.sfluxgrbif$fhr$cfsuffix
          # remove the 3-hourly files as well
          fhrm3=$((fhr-3))
          if [ $fhrm3 -lt 10 ]; then fhrm3=0$fhrm3; fi
          rm $COMOUT/$cyc/sflux/${RUN}.${cycle}.sfluxgrbf$fhrm3$cfsuffix
          rm $COMOUT/$cyc/sflux/${RUN}.${cycle}.sfluxgrbif$fhrm3$cfsuffix
       fi
     fi
   else 
     if [[ $fhr -gt 84 && $fhr -ne 90 ]] && [[ $fhr -ne 96 && $fhr -ne 120 ]]
     then
       rm $COMOUT/$cyc/sfcsig/${RUN}.${cycle}.bf$fhr$cfsuffix
     fi
   fi
 
   # Save lowres files if we are supposed to
   #
   if test "$DO_LOW_RES" = 'YES' -a `expr $fhr % 6` -eq 0
   then
     mv pgbafile.2 $COMOUT/$cyc/pgrbalr/${RUN}.${cycle}.pgrbaf$fhr.2$cfsuffix
     mv pgbbfile.2 $COMOUT/$cyc/pgrbblr/${RUN}.${cycle}.pgrbbf$fhr.2$cfsuffix
     if [[ "$makegrb1i" = "yes" ]]; then
       mv pgbaifile.2 $COMOUT/$cyc/pgrbalr/${RUN}.${cycle}.pgrbaif$fhr.2$cfsuffix
       mv pgbbifile.2 $COMOUT/$cyc/pgrbblr/${RUN}.${cycle}.pgrbbif$fhr.2$cfsuffix
     fi
   fi
fi

#
# DBNet Alerts for gfs ensemble
#

# Send DBNet alerts for PGBA and PGBA2 at 6 hour increments for all forecast hours
# Do for 00, 06, 12, and 18Z cycles.

if test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a ` expr $cyc % 6 ` -eq 0
then
  if test `echo $RUN | cut -c1-2` = "ge"
  then

    MEMBER=`echo $RUN | cut -c3-5 | tr '[a-z]' '[A-Z]'`
    if [[ $fhr -ge 0 && $fhr -le $fhmax && ` expr $fhr % 6 ` -eq 0 && ! -n "$cfsuffix" ]]
    then
      ($DBNROOT/bin/dbn_alert MODEL ENS_PGBA_$MEMBER $job $COMOUT/$cyc/pgrba/${RUN}.${cycle}.pgrbaf$fhr$cfsuffix)&
    fi
    if test "$DO_LOW_RES" = 'YES' -a ` expr $fhr % 6 ` -eq 0 -a ! -n "$cfsuffix"
    then
      ($DBNROOT/bin/dbn_alert MODEL ENS_PGBA2_$MEMBER $job $ENS_COM/$cyc/pgrbalr/${RUN}.${cycle}.pgrbaf$fhr.2$cfsuffix)&
    fi

  fi
fi

# Send DBNet alerts for PGBB at 6 hour increments for forecast hours 0 - 84 and
# for PGBB2 at 6 hour increments for forecast hours 90 through 384.
# Do for 00 and 12Z cycles.

if test "$SENDDBN" = 'YES' -a "$NET" = 'gens' -a ` expr $cyc % 12 ` -eq 0
then
  if test `echo $RUN | cut -c1-2` = "ge"
  then

    MEMBER=`echo $RUN | cut -c3-5 | tr '[a-z]' '[A-Z]'`
    if [[ $fhr -ge 0 && $fhr -le 84 && ` expr $fhr % 6 ` -eq 0 && ! -n "$cfsuffix" ]]
    then
      ($DBNROOT/bin/dbn_alert MODEL ENS_PGBB_$MEMBER $job $COMOUT/$cyc/pgrbb/${RUN}.${cycle}.pgrbbf$fhr$cfsuffix)&
    fi
    if test "$DO_LOW_RES" = 'YES' -a ` expr $fhr % 6 ` -eq 0 -a $fhr -ge 90 -a ! -n "$cfsuffix"
    then
      ($DBNROOT/bin/dbn_alert MODEL ENS_PGBB2_$MEMBER $job $ENS_COM/$cyc/pgrbblr/${RUN}.${cycle}.pgrbbf$fhr.2$cfsuffix)&
    fi
  fi

fi

wait
exit 

