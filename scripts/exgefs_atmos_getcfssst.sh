#!/bin/ksh

echo "$(date -u) begin ${.sh.file}"

set -x
if [[ ${STRICT:-NO} == "YES" ]]; then
    set -eu
fi

# Set environment.
VERBOSE=${VERBOSE:-"YES"}
if [ $VERBOSE = "YES" ]; then
    echo $(date) EXECUTING ${.sh.file} $* >&2
    set -x
fi

if [[ $SENDCOM == "YES" ]]; then
    export SSTDIR=${SSTDIR:-$COMOUT/cfssst}
else
    export SSTDIR=${SSTDIR:-$DATA/cfssst}
fi
mkdir -m 775 -p $SSTDIR

# Utilities
NCP=${NCP:-"/bin/cp -p"}
NLN=${NLN:-"/bin/ln -sf"}
NMV=${NMV:-"/bin/mv -uv"}

export HOMEgfs=${HOMEgfs:-$HOMEgefs}
export EXECgfs=$HOMEgfs/exec

# Executables.
if [[ -z "$EXECgefs" ]]; then
    echo "FATAL ERROR in ${.sh.file}: GEFS executable directory $EXECgefs does not exist"
    exit 5
fi

export GEFS_ANOM2_FCST=${GEFS_ANOM2_FCST:-$EXECgefs/gefs_anom2_fcst}
export GEFS_NSTGEN=${GEFS_NSTGEN:-$EXECgefs/gefs_nstgen}

################################################################################
# Preprocessing
if [ ! -d $DATA ]; then mkdir -p $DATA; fi
cd $DATA || exit 99

#################################################################################
# Run
#=====

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
export filenamein_CFSv2=tmpsfc.grb2 #tmpsfc.01.$PDY$cyc.daily.grb2
export filenamein_nst=sfcanl.nc
export filenameout_nst=Tsfc.grb     #gfs.t00z.Tsfc.$PDY.grb  #-- is sstanlfile

#Link Real-time CFSv2
HFcfsMax=120  # 5 day backword search for cfs fcst
HFcfs=24
PDYcfs=$PDY
while [ $HFcfs -le $HFcfsMax ] ; do
    PDYcfscfc=$($NDATE -${HFcfs} ${PDY}${cyc})
    PDYcfs=$(echo $PDYcfscfc | cut -c1-8)

    COMINcfs2=${COMINcfs}${PDYcfs}/${cyc}
    if [ -d $COMINcfs2 ]; then
        sFile=${COMINcfs2}/time_grib_01/tmpsfc.01.${PDYcfs}${cyc}.daily.grb2
        if [[ -f $sFile ]]; then
            $NLN $sFile $filenamein_CFSv2
            break
        fi
    fi

    HFcfs=$(expr $HFcfs + 24) 
done

if [ $HFcfs -gt $HFcfsMax ];  then
    echo "FATAL ERROR in ${.sh.file}: Real-time CFSv2 does not exist: $sFile"
    exit 92
fi

#Link Real-time nst file
sFile=$COMINgfs/gfs.${cycle}.sfcanl.nc
if [[ -f $sFile ]]; then
    test_tref=$(ncdump -h $sFile | grep tref)
    if [ -z $test_tref ]; then
        echo "FATAL ERROR in ${.sh.file}: Real-time nst does not exist in $sFile"
        exit 93
    fi
    $NLN $sFile $filenamein_nst
else
    echo "FATAL ERROR in ${.sh.file}: gfs surface analysis $sFile does not exist"
    exit 93
fi

export err=$?
if (( err != 0 )); then
    echo "FATAL ERROR in ${.sh.file}: link sst files FAILED!"
    exit 93
fi
echo "real-time data link ends at $(date)"

echo "read nst file to save tref begin"

$GEFS_NSTGEN $filenamein_nst $filenameout_nst
export err=$?
if (( err !=0 )); then
    echo "FATAL ERROR in ${.sh.file}: Reading sfcanl file $filenamein_nst to save tref to $filenameout_nst failed!"
    exit 94
fi

echo "read nst file to save tref end!!!"

#==============================================================================================
echo "2-tiered sst generation begin"

export sstoutdir="outdata"
mkdir -p $sstoutdir
# temporary directories for climm and cfsr (analysis)
tmpraw=raw
mkdir -p $tmpraw
tmpclim=clim
mkdir -p $tmpclim
tmpcfsr=cfsr
mkdir -p $tmpcfsr


# -- file with parameters kpds8, kpds9 and kpds10
kpdsfile=kpdslist.txt

export fn_rawfc=$tmpraw/TMPsfc.${PDY}${cyc}.24hr.grb
export fn_anom_fc=$sstoutdir/TMPsfc.${PDY}${cyc}.24hr.anom.grb


# Unit files
un_climm=55
un_climo=57
un_rtgan=59

NFH=840  # 35 days. Fixed. The fortran code is expecting this.

grid='255 0 1440 721 90000 0 128 -90000 359750 250 250 0'

export grid0p25="latlon 0:1440:0.25 90:721:-0.25"
export option1=' -set_grib_type same -new_grid_winds earth '
export option21=' -new_grid_interpolation bilinear'

#   -------------------- Main Loop -------------------------------
mm=$(echo ${PDY} | cut -c5-6)
dd=$(echo ${PDY} | cut -c7-8)
hh=${cyc}

>$fn_rawfc

# RTG analysis for initial time required by GFS
anlfileout=anl_sst_grb_latlon.$PDY
$COPYGB -g"$grid" -x $filenameout_nst $anlfileout
export err=$?
if (( err !=0 )); then
    echo "FATAL ERROR in ${.sh.file}: $filenameout_nst; $anlfileout!"
    exit 96
fi
cat $anlfileout > $fn_rawfc    # This is lead 0 forecast

# CFS forecast data
# Raw flx forecast
# it is trimmed to obtain data every 24h
rawgb2trimdir=$DATA/CFSBC_inputs_0p25 #/cfsbcinput.${PDY}${cyc}
mkdir -p $rawgb2trimdir
rawgb2trim_short=$rawgb2trimdir/tmpsfc.01.${PDY}${cyc}.daily.short.grb2
rawgb2trim=$rawgb2trimdir/tmpsfc.01.${PDY}${cyc}.daily.grb2
if [ ! -f $rawgb2trim  ]; then
    #$COPYGB2 -g "$grid_new" -x  $filenamein_CFSv2 $rawgb2trim
    $WGRIB2 $filenamein_CFSv2 -for_n 1:150 -grib $rawgb2trim_short
    $WGRIB2  $rawgb2trim_short $option1 $option21 -new_grid $grid0p25 $rawgb2trim
    export err=$?
    if (( err !=0 )); then
        echo "FATAL ERROR in ${.sh.file}: WGRIB2 failed on $filenamein_CFSv2; $rawgb2trim!"
        exit 96
    fi
fi

rawgb2=$tmpraw/TMPsfc.${PDY}${cyc}.24h.grb2
if [ -s $rawgb2 ] ; then
    rm -f $rawgb2
fi
#FH=24
FH=$(expr 24 + ${HFcfs})
NFHcfs=$(expr ${HFcfs} + ${NFH})
echo $FH
until [ $FH -gt $NFHcfs ] ; do
    $WGRIB2 $rawgb2trim -append -if "TMP:surface:$FH hour fcst:" -grib $rawgb2 > /dev/null
    FH=$(expr $FH + 24)
done
# convert from grib2 to grib and change resolution
rawgb=$tmpraw/TMPsfc.${PDY}${cyc}.fcs.grb
$CNVGRIB -g21 $rawgb2 $rawgb
if [ $? -eq 0 ] ; then rm $rawgb2 ; fi
cat $rawgb >> $fn_rawfc

# Model climatology
# daily one-season forecast must be trimmed to 35 days every 24h
# Files do not include f00h
CFSv2_clim_dir=${CFSv2_clim_dir:-${FIXgefs}/sstclim}

climmgb2trim=${CFSv2_clim_dir}/${hh}/tmpsfc.$mm.$dd.${hh}Z.mean.clim.daily.grb2
if [ ! -s $climmgb2trim ] ; then
    echo "FATAL ERROR in ${.sh.file}: $climmgb2trim does not exist!"
    exit 89
fi
climmgb2=$tmpclim/tmpsfc.${PDY}${cyc}.mean.clim.daily.grb2
if [ -s $climmgb2 ] ; then
    rm -f $climmgb2
fi
FH=24
until [ $FH -gt $NFH ] ; do
    $WGRIB2 $climmgb2trim -append -if "TMP:surface:$FH hour fcst:" -grib $climmgb2 > /dev/null
    FH=$(expr $FH + 24)
done

climmgb=$tmpclim/tmpsfc.${PDY}${cyc}.mean.clim.daily.grb
>$climmgb
$CNVGRIB -g21 $climmgb2 $climmgb
cat $anlfileout $climmgb > $climmgb.plus  # This is to add rtg analysis as lead 0 to the hindcast
echo "$climmgb.plus: $climmgb.plus"
$WGRIB $climmgb.plus -d all -ieee -o fort.$un_climm
if [ $? -eq 0 ] ; then
    rm -f $climmgb2
fi

#   # Observations (analysis). RTG i.c. are persisted 35 days
echo "$anlfileout: $anlfileout"
$WGRIB $anlfileout -d all -ieee -o fort.$un_rtgan
#
# CFSR must be trimmed to the 35 days of the forecasts
climogb2trim=${climogb2trim:-${FIXgefs}/sstclim/cfsr/tmpsfc.cfsr.mean.clim.daily.1999.2010.grb2}
if [ ! -s $climogb2trim ] ; then
    echo "FATAL ERROR in ${.sh.file}: $climogb2trim does not exist!"
    exit 90
fi
climogb2=$tmpcfsr/tmpsfc.cfsr.${PDY}${cyc}.clim.daily.grb2
cfsr_date=2000${mm}${dd}${hh}
if [ -s $climogb2 ] ; then
    rm -f $climogb2
fi
FH=0
until [ $FH -gt $NFH ] ; do
    $WGRIB2 $climogb2trim -append -if ":d=$cfsr_date:" -grib $climogb2 > /dev/null
    FH=$(expr $FH + 24)
    cfsr_date=$($NDATE +24 $cfsr_date)
    yyn=$(echo $cfsr_date | cut -c1-4)
    #      echo " reanalysis: year $yyn "
    if [ $yyn = 2001 ] ; then
        mmn=$(echo $cfsr_date | cut -c5-6)
        ddn=$(echo $cfsr_date | cut -c7-8)
        cfsr_date=2000${mmn}${ddn}${hh}
    fi
done
climogb=$tmpcfsr/tmpsfc.cfsr.${PDY}${cyc}.clim.daily.grb
$CNVGRIB -g21 $climogb2 $climogb
echo "climogb: $climogb"
$WGRIB $climogb -d all -ieee -o fort.$un_climo
export err=$?
if [ $? -eq 0 ] ;
    then rm -f $climmgb2
else
    echo "FATAL ERROR in ${.sh.file}: CNVGRIB failed for $climogb"
fi

#
# -- Generate kpds -- =========================================================
#  Generate kpds starting from this and going through the 35 days forecast
>$kpdsfile
kdate=${PDY}${cyc}
FH=0
until [ $FH -gt $NFH ] ; do
    #     kdate=$($NDATE +24 $kdate)
    kcc=$(echo $kdate | cut -c1-2)
    kyy=$(echo $kdate | cut -c3-4)
    kpds8=$(echo $kdate | cut -c3-4)
    if [ "$kpds8" -eq "00" ] ; then
        kpds8="100"
    fi
    kpds9=$(echo $kdate | cut -c5-6)
    kpds10=$(echo $kdate | cut -c7-8)

    if [ "$kcc" -eq "19" ] ; then
        kpds21="20"
    elif [ "$kcc" -eq "20" ] ; then
        kpds21="21"
    fi

    if [ "$kyy" -eq "00" ] ; then
        kpds21="20"
    fi

    echo $kpds8 $kpds9 $kpds10 $kpds21 >> $kpdsfile

    kdate=$($NDATE +24 $kdate)

    FH=$(expr $FH + 24)
done
echo "$kpdsfile: $kpdsfile"
#
#  Run the bias correction
# -- Uses persistence with growing weight towards CFSv2 as lead time increases NCEP grib T126 (Gaussian 384*190)
echo "fn_anom_fc: $fn_anom_fc"
if [ -s $fn_anom_fc ] ; then
    rm -f $fn_anom_fc
fi

echo "$fn_rawfc $un_climm $un_climo $un_rtgan $fn_anom_fc $kpdsfile"
$GEFS_ANOM2_FCST $fn_rawfc $un_climm $un_climo $un_rtgan $fn_anom_fc $kpdsfile $HFcfs
export err=$?
if [[ $err != 0 ]]; then
    echo "FATAL ERROR in ${.sh.file}: Bias correction failed in $GEFS_ANOM2_FCST"
    exit $err
fi

$NCP $fn_anom_fc $SSTDIR/TMPsfc.${PDY}${cyc}.24hr.anom.grb
export err=$?

if [[ $err != 0 ]]; then
    echo "FATAL ERROR in ${.sh.file}: Could not copy $fn_anom_fc to $SSTDIR"
    exit $err
fi

echo "$(date -u) end ${.sh.file}"

exit 0
