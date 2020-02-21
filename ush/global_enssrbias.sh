######################### CALLED BY EXENSCQPF ##########################
echo "------------------------------------------------"
echo "Ensemble CQPF -> global_enssrbias.sh            "
echo "------------------------------------------------"
echo "History: Feb 2004 - First implementation of this new script."
echo "AUTHOR: Yuejian Zhu (wx20yz)"

echo "         ######################################### "
echo "         ####  RUN NEW STATISTICS BIAS DIST.  #### "
echo "         ####  RUN NEW STATISTICS BIAS DIST.  #### "
echo "         ####  RUN NEW STATISTICS BIAS DIST.  #### "
echo "         ####        RUNNING ID = " $1 "       #### "
echo "         ######################################### "

RID=$1
IYMDP1=`$NDATE +24 $OBSYMD\00 | cut -c1-8`


#set -x

if [ -s STAT_RM_BIAS_$RID.dat ]; then
   cp STAT_RM_BIAS_$RID.dat OLD_STAT.dat
   iold=1  
else
   iold=0
fi

if [ -s rain_$RID.$OBSYMD ]; then
   cp rain_$RID.$OBSYMD DAY_NEWS.dat
   inew=1  
else
   inew=0
fi

set +x

if [ $inew -eq 1 ]; then
 echo " &namin " >input_stat
 echo " cfile(1)='DAY_NEWS.dat',"   >>input_stat
 echo " cfile(2)='OLD_STAT.dat',"   >>input_stat
 echo " ifile(1)=$inew,"            >>input_stat
 echo " ifile(2)=$iold,"            >>input_stat
 echo " ofile(1)='NEW_STAT.dat',"   >>input_stat
 echo " iymd=$OBSYMD,"              >>input_stat
 echo " idday=30,"                  >>input_stat
 echo " /" >>input_stat
 cat input_stat

 export pgm=global_enssrbias
 . prep_step

 startmsg

 $EXECgefs/global_enssrbias <input_stat >stat_output.$RID
 export err=$?;err_chk

 cat stat_output.$RID >> $pgmout

 mv NEW_STAT.dat STAT_RM_BIAS_$RID.$IYMDP1
else
 mv OLD_STAT.dat STAT_RM_BIAS_$RID.$IYMDP1
fi

