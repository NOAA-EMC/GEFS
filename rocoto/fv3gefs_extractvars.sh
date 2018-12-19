#!/bin/ksh

date_s=1996010400
date_e=1996010400

npert=10 #number of members to extract
foutmax=240 #last lead hour to extract 

indir=/gpfs/dell3/nco/storage/fv3gefs/HOME/emc.enspara/Hong.Guan/o/com/gens/dev/gefs.19960104/00/pgrb2ap25 #directory containing the files that we want to extract
outdirpre=/gpfs/dell2/emc/retros/noscrub/Eric.Sinsky/scripts/test #directory where we want to save the extracted files
workdir=/gpfs/dell2/emc/retros/noscrub/Eric.Sinsky/scripts #working directory (same directory as this script)
varlist=/gpfs/dell2/emc/retros/noscrub/Eric.Sinsky/scripts/shortparmlist

cdate=$date_s

rm -f $outdirpre/*

while [[ $cdate -le $date_e ]];do
 ddate=`echo $cdate | cut -c1-8`
 yyyy=`echo $cdate | cut -c1-4`
 mm=`echo $cdate | cut -c5-6`
 dd=`echo $cdate | cut -c7-8`
 hh=`echo $cdate | cut -c9-10`
 for nh in {0..$foutmax..6}; do
  fnh=`printf "%3.3d" ${nh}`
  echo "extracting f${fnh}"

  #Extract individual member files
  for imem in {0..$npert}; do
   imemh=`printf "%2.2d" ${imem}`
   if [[ $imemh == "00" ]]; then
    ensname="c${imemh}"
   else 
    ensname="p${imemh}"
   fi
   infile=$indir/ge${ensname}.t00z.pgrb2a.0p25.f${fnh}
   oufile=$outdirpre/ge${ensname}.t00z.pgrb2a.0p25.f${fnh}
   if [ -f $infile ]; then #check if input file exists before extraction
    rm -f $outfile #remove outfile if it already exists before extraction   
    $WGRIB2 $infile | grep -F -f $varlist | $WGRIB2 -i $infile -append -grib $oufile>/dev/null
   else
    echo "$infile does not exist"
   fi 
  done #member

  #Extract geavg and gespr file
  for ensname in "spr" "avg"; do
   infile=$indir/ge${ensname}.t00z.pgrb2a.0p25.f${fnh}
   oufile=$outdirpre/ge${ensname}.t00z.pgrb2a.0p25.f${fnh}
   if [ -f $infile ]; then #check if input file exists before extraction
    rm -f $outfile #remove outfile if it already exists before extraction   
    $WGRIB2 $infile | grep -F -f $varlist | $WGRIB2 -i $infile -append -grib $oufile>/dev/null
   else
    echo "$infile does not exist"
   fi
  done #member


 done #fhr
cdate=$($NDATE +168 $cdate)
done #case
