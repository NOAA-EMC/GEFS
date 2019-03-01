#!/bin/ksh

cdate=$1
DATA=$2

npert=4 #number of members to extract
foutmax=6 #last lead hour to extract 

nemsio2grb=$EXECacc/gfsnemsio2grb

cp -p $FIXens_acc/template_512.grb $DATA
cd $DATA

ddate=`echo $cdate | cut -c1-8`
echo $ddate,$cdate

# COMIN_00and03 and COMIN_master are directory containing the files that we want to extract
#Extract c00 files for f000 and f003 from PSD nemsio data
export grid_new="40 6 0 0 0 0 0 0 1536 768 0 0 89820709 0 48 -89820709 359765625 234375 384 0"

    for fnh in 00 03; do
      echo "extracting f${fnh}"
      infile=bfg_${PDY}06_fhr${fnh}_control2
      oufile=$DATA/gec00.t00z.master.grb2f0${fnh}
      echo $COMIN_00and03/$infile
      if [ -f $COMIN_00and03/$infile ]; then #check if input file exists before extraction
        rm -f $outfile #remove outfile if it already exists before extraction   
        cp $COMIN_00and03/$infile .
#    $nemsio2grb $infile a
        $nemsio2grb $infile 
        copygb2 -g "$grid_new" -x $infile.grb $oufile
      else
        echo "$infile does not exist"
      fi 
    done

  #Extract individual member files for f006 master data
  fnh=006
  for imem in {00..$npert}; do
   imemh=`printf "%2.2d" ${imem}`
   if [[ $imemh == "00" ]]; then
    ensname="c${imemh}"
   else 
    ensname="p${imemh}"
   fi

   echo $ensname
   echo "extracting f${fnh}"
    infile=$COMIN_master/ge${ensname}.t00z.master.grb2f${fnh}
    oufile=$DATA/ge${ensname}.t00z.master.grb2f${fnh}
    echo $infile
    echo $oufile

   if [ -f $infile ]; then #check if input file exists before extraction
#    rm -f $outfile/ge${ensname}.t00z.master.grb2f${fnh}  #remove outfile if it already exists before extraction   
#    $WGRIB2 $infile | grep -F -f $varlist3 | $WGRIB2 -i $infile -append -grib $oufile>/dev/null
    $WGRIB2 $infile | grep ":DLWRF:surface" | $WGRIB2 -i $infile -append -grib $oufile>/dev/null
    $WGRIB2 $infile | grep ":ULWRF:surface" | $WGRIB2 -i $infile -append -grib $oufile>/dev/null
    $WGRIB2 $infile | grep ":DSWRF:surface" | $WGRIB2 -i $infile -append -grib $oufile>/dev/null
    $WGRIB2 $infile | grep ":USWRF:surface" | $WGRIB2 -i $infile -append -grib $oufile>/dev/null
    $WGRIB2 $infile | grep ":USWRF:top" | $WGRIB2 -i $infile -append -grib $oufile>/dev/null
    $WGRIB2 $infile | grep ":ULWRF:top" | $WGRIB2 -i $infile -append -grib $oufile>/dev/null
    $WGRIB2 $infile | grep ":UFLX:surface" | $WGRIB2 -i $infile -append -grib $oufile>/dev/null
    $WGRIB2 $infile | grep ":VFLX:surface" | $WGRIB2 -i $infile -append -grib $oufile>/dev/null
    $WGRIB2 $infile | grep ":SHTFL:surface" | $WGRIB2 -i $infile -append -grib $oufile>/dev/null
    $WGRIB2 $infile | grep ":LHTFL:surface" | $WGRIB2 -i $infile -append -grib $oufile>/dev/null
    $WGRIB2 $infile | grep ":PRATE:surface" | $WGRIB2 -i $infile -append -grib $oufile>/dev/null
    $WGRIB2 $infile | grep ":CPRAT:surface" | $WGRIB2 -i $infile -append -grib $oufile>/dev/null
    $WGRIB2 $infile | grep ":ALBDO:surface" | $WGRIB2 -i $infile -append -grib $oufile>/dev/null
    $WGRIB2 $infile | grep ":TCDC:entire" | $WGRIB2 -i $infile -append -grib $oufile>/dev/null
    $WGRIB2 $infile | grep ":TCDC:boundary" | $WGRIB2 -i $infile -append -grib $oufile>/dev/null
    $WGRIB2 $infile | grep ":TCDC:high" | $WGRIB2 -i $infile -append -grib $oufile>/dev/null
    $WGRIB2 $infile | grep ":TCDC:middle" | $WGRIB2 -i $infile -append -grib $oufile>/dev/null
    $WGRIB2 $infile | grep ":TCDC:low" | $WGRIB2 -i $infile -append -grib $oufile>/dev/null
    $WGRIB2 $infile | grep ":GFLUX:surface" | $WGRIB2 -i $infile -append -grib $oufile>/dev/null
    $WGRIB2 $infile | grep ":U-GWD:surface" | $WGRIB2 -i $infile -append -grib $oufile>/dev/null
    $WGRIB2 $infile | grep ":V-GWD:surface" | $WGRIB2 -i $infile -append -grib $oufile>/dev/null

   else
    echo "$infile does not exist"
   fi 

  done #member

#f [ -d "$COMOUT" ]
# then
#   export file_dir=$DATA/
#   rm -fr input_directory_log_${ddate}.txt
#   echo "data found in $file_dir for $ddate" >> $DATA/input_directory_log_${ddate}.txt
# fi


 export out_dir1=$COMOUT/master0306/
 export out_dir=$DATA/
 export exec_dir=$EXECacc
 export sorc_dir=$SORCacc
 export sorc_name=gefs_6h_ave_1mem


 mkdir -p $out_dir1
 rm -f ${out_dir1}

 mkdir -p $out_dir

 for mem in {00..$npert}; do

   memh=`printf "%2.2d" ${mem}`
   if [[ $memh == "00" ]]; then
    export ens_mem="gec${memh}"
   else
    export ens_mem="gep${memh}"
   fi

   cd $sorc_dir
     compile_gefs_1mem_p1.sh
   cd $DATA

   $exec_dir/$sorc_name.exe

#output f06
   infile=$COMIN_master/${ens_mem}.t00z.master.grb2f006
   wgrib2 $infile -not "(ULWRF|USWRF)" -not "(DLWRF|DSWRF|UFLX|VFLX|SHTFL|LHTFL|PRATE|CPRAT|ALBDO|GFLUX|U-GWD|V-GWD)" -not "TCDC:(low|middle|high|entire|boundary)" -grib out1.grb2
   cat out1.grb2 $ens_mem.t00z.pgrb2af006 > out2.grb

   mv $COMIN_master/${ens_mem}.t00z.master.grb2f006 $COMIN_master/${ens_mem}.t00z.master.grb2f006_org
   mv $COMIN_master/${ens_mem}.t00z.master.grb2if006 $COMIN_master/${ens_mem}.t00z.master.grb2if006_org

   mv out2.grb $COMIN_master/${ens_mem}.t00z.master.grb2f006
   $GRB2INDEX $COMIN_master/$ens_mem.t00z.master.grb2f006  $COMIN_master/$ens_mem.t00z.master.grb2if006

   rm -fr out1.grb2 out2.grb
   

#output f03
   infile=$COMIN_master/${ens_mem}.t00z.master.grb2f003
   wgrib2 $infile -not "(ULWRF|USWRF)" -not "(DLWRF|DSWRF|UFLX|VFLX|SHTFL|LHTFL|PRATE|CPRAT|ALBDO|GFLUX|U-GWD|V-GWD)" -not "TCDC:(low|middle|high|entire|boundary)" -grib out1.grb2
   cat out1.grb2 gec00.t00z.pgrb2af03 > out2.grb

   mv $COMIN_master/${ens_mem}.t00z.master.grb2f003 $COMIN_master/${ens_mem}.t00z.master.grb2f003_org
   mv $COMIN_master/${ens_mem}.t00z.master.grb2if003 $COMIN_master/${ens_mem}.t00z.master.grb2if003_org

   mv out2.grb $COMIN_master/${ens_mem}.t00z.master.grb2f003
   $GRB2INDEX $COMIN_master/$ens_mem.t00z.master.grb2f003  $COMIN_master/$ens_mem.t00z.master.grb2if003

   rm -fr out1.grb2 out2.grb

 done #member

#rm -fr ${out_dir1}*.t00z.pgrb2af*
