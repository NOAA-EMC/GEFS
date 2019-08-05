#!/bin/sh
#
# Metafile Script : gefs_avgspr_meta.sh
#
# Log :
# J. Carr/PMB      12/25/2004     Pushed into production.
# A. Robson/HPC    07/06/2005     Changed map line thickness to 1.
# Luke Lin/NCO     02/16/2006     Modified for gefs
# M. Klein/HPC     03/13/2007     Changed South American area and fixed error in PMSL spread.
# M. Klein/HPC     07/23/2007     Add an Alaska area...but only plot 500 and PMSL data.
#                                 Also modify order of parameters written for nam and sam area.
# M. Klein/HPC     01/22/2008     Add larger GAREA for Alaska desk support.
# M. Klein/WPC     10/30/2013     For CPC, add the 6-10 and 8-14 day QPFs.
#
# Set Up Local Variables
#
set -x
export PS4='gefs_avgspr:$SECONDS + '
mkdir $DATA/gefs_avgspr
cd $DATA/gefs_avgspr
sh $utilscript/setup.sh
cp $FIXgempak/datatype.tbl datatype.tbl

mdl=gefs_avgspr
MDL=GEFS_AVGSPR
PDY2=`echo $PDY | cut -c3-`

gdattim_6to10=""
gdattim_8to14=""

for area in natl mpac
do
    garea=${area}
    proj=" "

    metaname="gefs_avgspr_${area}_${cyc}.meta"
    device="nc | ${metaname}"

    export pgm=gdplot2_nc;. prep_step; startmsg

gdplot2_nc << EOF
GDATTIM  = f00-f240-12
GAREA    = ${garea}
PROJ     = ${proj}
DEVICE   = ${device}
PANEL    = 0
TEXT     = 1/21//hw
MAP      = 31/1/1
LATLON	 = 18/2/1/10;10
gdfile   = F-GEFSAVG | ${PDY2}/${cyc}00
GLEVEL   = 500:1000!500:1000!0
GVCORD   = pres!pres!none
PANEL    = 0
SKIP     = 0
SCALE    = -1!-1!0
GDPFUN   = ldf(hght) ! ldf(hght) !sm5s(pmsl)
TYPE     = c
CONTUR   = 2
CINT     = 6/0/540 ! 6/546/999        ! 4
LINE     = 6/3/2   ! 2/3/2            ! 20//3
FINT     =
FLINE    =
HILO     = !! 26;2/H#;L#/1018-1070;900-1012//30;30/y
HLSYM    = 2;1.5//21//hw
CLRBAR   = 1
WIND     = 0
REFVEC   =
title    = 5/-2/~ ? GEFS_AVG PMSL, 1000-500 THICK|~${area} PMSL & 1000-500 THICK!0
TEXT     = 1/21//hw
CLEAR    = yes
list
run

gdfile   = F-GEFSSPR | ${PDY2}/${cyc}00 !F-GEFSAVG | ${PDY2}/${cyc}00
gdpfun   = sm5s(hght) !sm5s(hght)
glevel   = 500        !500
gvcord   = pres       !pres
scale    = 0          !-1
cint     = 0          !6
line     = 0          !5/1/3
fint     = 30;60;90;120;150;180;210;240;270;300;330
fline    = 0;8;23; 22; 24; 25; 18; 17; 16; 14; 11;4
clear    = yes!no
type     = f  !c
panel    = 0
hilo     =
hlsym    =
TITLE    = 5/-2/~ ? @ GEFS_AVGSPR HGHT AND SPREAD |~${area} @ MEAN AND SPREAD!0
list
run

gdpfun   = sm5s(pmsl) !sm5s(pmsl)
glevel   = 0          !0
gvcord   = none       !none
scale    = 0          !0
cint     = 0          !4
line     = 0          !5/1/3
fint     = 2; 4; 6;  8; 10; 12; 14; 16; 18; 20;22
fline    = 0;8;23; 22; 24; 25; 18; 17; 16; 14; 11;4
clear    = yes!no
type     = f  !c
panel    = 0
hilo     =
hlsym    =
TITLE   = 5/-2/~ ? GEFS_AVGSPR MSLP AND SPREAD|~${area} GEFS_AVGSPR MSLP AND SPREAD!0
list
run

exit
EOF

    export err=$?;err_chk

    #####################################################
    # GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
    # WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
    # FOR THIS CASE HERE.
    #####################################################

    ls -l ${metaname}
    export err=$?;export pgm="GEMPAK META CHECK FILE";err_chk

    if [ $SENDCOM = "YES" ] ; then
        mv ${metaname} ${COMOUT}/gefs_avgspr_${PDY}_${cyc}_${area}
        if [ $SENDDBN = "YES" ] ; then
            $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job ${COMOUT}/gefs_avgspr_${PDY}_${cyc}_${area}
        fi
    fi
done

# Make metafiles for North and South America...as well as Alaska.

for area in nam sam ak
do
    if [ ${area} = "nam" ] ; then
        garea="17.529;-129.296;53.771;-22.374"
        proj="str/90;-105;0"
        garea2="us"
        proj2=""
        fint=".01;.1;.25;.5;.75;1;1.5;2;2.5;3;4;5;6;7;8;9;10"
        fline="0;21-30;14-20;5"
        hilo="31;0/x#2/.25-10///y"
        parm="i"
	run="run"
	run2="run"
        run3=""
        run6to10="run"
        run8to14="run"
        if [ ${cyc} = "00" ] ; then
            gdattim_6to10="${PDY2}/${cyc}00F264"
            gdattim_8to14="${PDY2}/${cyc}00F360"
        elif [ ${cyc} = "06" ] ; then
            gdattim_6to10="${PDY2}/${cyc}00F258"
            gdattim_8to14="${PDY2}/${cyc}00F354"
        elif [ ${cyc} = "12" ] ; then
            gdattim_6to10="${PDY2}/${cyc}00F276"
            gdattim_8to14="${PDY2}/${cyc}00F372"
        elif [ ${cyc} = "18" ] ; then
            gdattim_6to10="${PDY2}/${cyc}00F270"
            gdattim_8to14="${PDY2}/${cyc}00F366"
        fi
	f216="f384"
	F240="F384"
    elif [ ${area} = "sam" ] ; then
        garea="-66;-127;14.5;-19"
        proj="mer"
        garea2="-66;-127;14.5;-19"
        proj2="mer"
        fint="1;5;10;15;20;25;30;35;40;45;50;55;60;65;70;75;80;85"
        fline="0;21-30;14-20;5"
        hilo="31;0/x#/10-400///y"
        parm="m"
	run="run"
	run2=""
        run3=""
        run6to10=""
        run8to14=""
	f216="f216"
	F240="F240"
    else
        garea="35.0;178.0;78.0;-94.0"
	proj="NPS"
	garea2="10.0;155.0;59.0;-72.0"
	proj2="STR/90;-160;0"
	fint=""
	fline=""
	hilo=""
	parm="i"
	run=""
	run2=""
        run3="run"
        run6to10=""
        run8to14=""
	f216="f216"
	F240="F240"
    fi
    metaname="gefs_avgspr_${PDY}_${cyc}_${area}"
    device="nc | ${metaname}"

    export pgm=gdplot2_nc;. prep_step; startmsg

gdplot2_nc << EOF
GAREA    = ${garea}
PROJ     = ${proj}
gdfile   = F-GEFSAVG | ${PDY2}/${cyc}00
gdattim  = F00-${F240}-06
device   = ${device}
GLEVEL   = 500:1000!500:1000!0
GVCORD   = pres!pres!none
PANEL    = 0
SKIP     = 0
SCALE    = -1!-1!0
GDPFUN   = ldf(hght) ! ldf(hght) !sm5s(pmsl)
TYPE     = c
CONTUR   = 2
CINT     = 6/0/540 ! 6/546/999        ! 4
LINE     = 6/3/2   ! 2/3/2            ! 20//3
FINT     =
FLINE    =
HILO     = !! 26;2/H#;L#/1018-1070;900-1012//30;30/y
HLSYM    = 2;1.5//21//hw
CLRBAR   = 1
WIND     = 0
REFVEC   =
title    = 5/-2/~ ? GEFS MEAN PMSL AND 1000-500MB THICKNESS|~PMSL & 1000-500 THICK
TEXT     = 1/21//hw
CLEAR    = yes
LATLON   = 1
run

garea    = ${garea2}
proj     = ${proj2}
title    = 5/-2/~ ? GEFS MEAN PMSL AND 1000-500MB THICKNESS|~PMSL & 1000-500 THICK LRG
${run3}

garea    = ${garea}
proj     = ${proj}
gdfile   = F-GEFSSPR | ${PDY2}/${cyc}00 !F-GEFSAVG | ${PDY2}/${cyc}00
gdpfun   = sm5s(pmsl) !sm5s(pmsl)
glevel   = 0          !0
gvcord   = none       !none
scale    = 0          !0
cint     = 0          !4
line     = 0          !5/1/3
fint     = 2; 4; 6;  8; 10; 12; 14; 16; 18; 20;22
fline    = 0;8;23; 22; 24; 25; 18; 17; 16; 14; 11;4
title    = 5/-2/~ ? GEFS MEAN AND SPREAD - PMSL |~PMSL & SPREAD
clear    = yes!no
type     = f  !c
panel    = 0
hilo     =
hlsym    =
run

garea    = ${garea2}
proj     = ${proj2}
title    = 5/-2/~ ? GEFS MEAN AND SPREAD - PMSL |~PMSL & SPREAD LRG
${run3}

garea    = ${garea}
proj     = ${proj}
gdfile   = F-GEFSSPR | ${PDY2}/${cyc}00 !F-GEFSAVG | ${PDY2}/${cyc}00
gdpfun   = sm5s(hght) !sm5s(hght)
glevel   = 500        !500
gvcord   = pres       !pres
scale    = 0          !-1
cint     = 0          !6
line     = 0          !5/1/3
fint     = 30;60;90;120;150;180;210;240;270;300;330
fline    = 0;8;23; 22; 24; 25; 18; 17; 16; 14; 11;4
title    = 5/-2/~ ? GEFS MEAN AND SPREAD - 500 MB HGT |~500 HGT & SPREAD
clear    = yes!no
type     = f  !c
panel    = 0
hilo     =
hlsym    =
run

garea    = ${garea2}
proj     = ${proj2}
title    = 5/-2/~ ? GEFS MEAN AND SPREAD - 500 MB HGT |~500 HGT & SPREAD LRG
${run3}

gdfile   = F-GEFSAVG | ${PDY2}/${cyc}00
garea    = ${garea2}
proj     = ${proj2}
GDATTIM  = f12-${f216}-06
wind     = bk0
GLEVEL   = 0
LATLON   = 0
gvcord   = none
type     = f
cint     = 0.1;0.25
line     = 32//1/0
fint     = ${fint}
fline    = ${fline}
HILO     = ${hilo}
HLSYM    = 1.5
glevel   = 0
scale    = 0
gdpfun   = p12${parm}
refvec   =
title    = 5/-2/~ ? GEFS MEAN 12-HR PCPN|~12-HR PCPN
${run2}

GDATTIM  = f24-${f216}-06
gdpfun   = p24${parm}
title    = 5/-2/~ ? GEFS MEAN 24-HR PCPN|~24-HR PCPN
${run}

GDATTIM  = f48-${f216}-06
gdpfun   = p48${parm}
title    = 5/-2/~ ? GEFS MEAN 48-HR PCPN|~48-HR PCPN
${run2}

GDATTIM  = f72-${f216}-06
gdpfun   = p72${parm}
title    = 5/-2/~ ? GEFS MEAN 72-HR PCPN|~72-HR PCPN
${run2}

GDATTIM  = f120-${f216}-06
gdpfun   = p120${parm}
title    = 5/-2/~ ? GEFS MEAN 120-HR PCPN|~120-HR PCPN
${run2}

garea    = ${garea}
proj     = ${proj}
gdattim  = ${gdattim_6to10}
gdpfun   = p114${parm}
title    = 5/-2/~ ? GEFS MEAN 6-10 DAY PCPN|~6-10 DAY PCPN
${run6to10}

gdattim  = ${gdattim_8to14}
gdpfun   = p162${parm}
title    = 5/-2/~ ? GEFS MEAN 8-14 DAY PCPN|~8-14 DAY PCPN
${run8to14}

ex
EOF

    export err=$?;err_chk

    #####################################################
    # GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
    # WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
    # FOR THIS CASE HERE.
    #####################################################

    ls -l ${metaname}
    export err=$?;export pgm="GEMPAK META CHECK FILE";err_chk

    if [ $SENDCOM = "YES" ] ; then
        mv ${metaname} ${COMOUT}/gefs_avgspr_${PDY}_${cyc}_${area}
        if [ $SENDDBN = "YES" ] ; then
            $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job ${COMOUT}/gefs_avgspr_${PDY}_${cyc}_${area}
        fi
    fi
done

exit

