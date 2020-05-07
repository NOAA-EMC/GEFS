######################### CALLED BY EXENSCQPF ##########################
echo "------------------------------------------------"
echo "Ensemble CQPF -> global_enspvrfy.sh -> global_enswgrpsh    "
echo "------------------------------------------------"
echo "History: Feb 2004 - First implementation of this new script."
echo "AUTHOR: Yuejian Zhu (wx20yz)"
echo "History: Nov 2014 - Grib2 code conversion"
echo "AUTHOR: Yan Luo (wx22lu)"

set -x

IYMD=$(echo $1 | cut -c1-8)    
FHOUR=$2
ICYC=$3      # 1, 24 hours : 2, every 12 hours : 4, every 6 hours
RUNID=$4

 if [ $FHOUR -eq 00 ]; then
  FLUX=$DATA/qpf_$RUNID.$IYMD\00                   
  $WGRIB2 -match "12-36 hour"  $FLUX -grib precip.$RUNID
 else
  IYMD1=$($NDATE -$FHOUR $IYMD\12 | cut -c1-8)
  case $FHOUR in
   36) outime=12_36;  grptime=12-36;;
   60) outime=36_60;  grptime=36-60;;
   84) outime=60_84;  grptime=60-84;;
  108) outime=84_108; grptime=84-108;;
  132) outime=108_132;grptime=108-132;;
  156) outime=132_156;grptime=132-156;;
  180) outime=156_180;grptime=156-180;;
  204) outime=180_204;grptime=180-204;;
  228) outime=204_228;grptime=204-228;;
  252) outime=228_252;grptime=228-252;;
  276) outime=252_276;grptime=252-276;;
  300) outime=276_300;grptime=276-300;;
  324) outime=300_324;grptime=300-324;;
  348) outime=324_348;grptime=324-348;;
  372) outime=348_372;grptime=348-372;;
  esac

  FLUX=$DATA/qpf_$RUNID.$IYMD1\00                   
  PTMP=$DATA/$RUNID\_$IYMD1\00_$outime
  $WGRIB2 -match "${grptime} hour" $FLUX -grib $PTMP
 fi


