#!/bin/sh
#
# substitute tracked locations into a syndata file
#
if (( $# != 5 )); then
	echo five arguments needed pdy cyc syndatafile trackunixfile outfile
	exit
fi
if [[ $1 == "test" ]]; then
	echo test mode
	cd /ptmp/$LOGNAME/d/com/avn/prod/avn.20040824
	pdy=20040824
	cyc=12
	filesyn=gblav.t12z.syndata.tcvitals.tm00
	filetrk=avn.t12z.cyclone.trackatcfunix
	fileout=test.syntrk
else
	pdy=$1
	cyc=$2
	filesyn=$3
	filetrk=$4
	fileout=$5
fi # [[ $1 == "test" ]]

echo pdy=$pdy
echo cyc=$cyc
pwd
echo filesyn=$filesyn
echo filetrk=$filetrk
echo fileout=$fileout
echo
echo $filetrk begin
cat $filetrk
echo $filetrk end
echo
echo $filesyn begin
cat $filesyn
echo $filesyn end
echo
ls -al $filesyn $filetrk $fileout 2>/dev/null
echo
rm $fileout 2>/dev/null
ls -al $filesyn $filetrk $fileout 2>/dev/null
echo
touch $fileout
ls -al $filesyn $filetrk $fileout
echo
lines=$(cat $filesyn|wc -l)
echo lines=$lines
if (( lines == 0 )); then
	touch $fileout
else
	((iline=0))
	while (( iline < lines )); do
		((iline=iline+1))
		echo
		echo iline=$iline
		insyn=$(head -$iline $filesyn | tail -1)
		echo insyn="$insyn"
		insynl=$(echo "$insyn"|wc -c)
		echo insynl=$insynl
		part1=$(echo "$insyn"|cut -c1-5)
		part2=$(echo "$insyn"|cut -c6-7)
		part3=$(echo "$insyn"|cut -c8-8)
		part4=$(echo "$insyn"|cut -c9-19)
		part5=$(echo "$insyn"|cut -c20-27)
		part6=$(echo "$insyn"|cut -c28-28)
		part7=$(echo "$insyn"|cut -c29-30)
		part8=$(echo "$insyn"|cut -c31-33)
		part9=$(echo "$insyn"|cut -c34-37)
		part10=$(echo "$insyn"|cut -c38-38)
		part11=$(echo "$insyn"|cut -c39-43)
		part12=$(echo "$insyn"|cut -c44-"$insynl")
		echo part1="$part1"
		echo part2="$part2"
		echo part3="$part3"
		echo part4="$part4"
		echo part5="$part5"
		echo part6="$part6"
		echo part7="$part7"
		echo part8="$part8"
		echo part9="$part9"
		echo part10="$part10"
		echo part11="$part11"
		echo part12="$part12"
		if [[ $part5 = $pdy ]] && [[ $part7 = $cyc ]];then
			part3u=$(echo $part3|tr 'a-z' 'A-Z')
			case $part3u in
				"L") tsa="AL" ;;
				"E") tsa="EP" ;;
				"C") tsa="CP" ;;
				"W") tsa="WP" ;;
				"O") tsa="SC" ;;
				"T") tsa="EC" ;;
				"U") tsa="AU" ;;
				"P") tsa="SP" ;;
				"S") tsa="SI" ;;
				"B") tsa="BI" ;;
				"A") tsa="NA" ;;
				*) tsa="NULL" ;;
			esac # $part3u
			echo
			echo tsa=$tsa
			echo
			if [[ $tsa = "NULL" ]]; then
				echo WE DO NOT RECOGNIZE THE REGION LETTER "part3"
				rc=9
			else
				grep "$tsa, $part2, ......................000" $filetrk
				rc=$?
			fi # [[ $tsa = "NULL" ]]
			echo rc=$rc
			if (( rc == 0 )); then
				intrk=$(grep "$tsa, $part2, ......................000" $filetrk |head -1)
				echo intrk="$intrk"
				strlat=$(echo "$intrk"|cut -c36-39|tr " " "0")
				strlon=$(echo "$intrk"|cut -c42-46|tr " " "0")
				echo strlat="$strlat"
				echo strlon="$strlon"
				if [[ $strlat = "0000" ]]; then
					echo DO NOT USE ZERO POSITION TO SUBSTITUTE FOR SYNDAT
					outsyn="$insyn"
				else
					outsyn="$part1$part2$part3$part4$part5$part6$part7$part8$strlat$part10$strlon$part12"
				fi # [[ $strlat = "0000" ]]
			else
				echo NO MATCHING ANALYSIS POSITION TO SUBSTITUTE FOR SYNDAT
				outsyn="$insyn"
			fi # (( rc == 0 ))
			echo outsyn="$outsyn"
			echo "$outsyn">>$fileout
		else
			echo
			echo $part5 $part7 DO NOT MATCH $pdy $cyc SO EXCLUDE FROM SYNDAT
		fi # [[ $part5 = $pdy ]] && [[ $part7 = $cyc ]]
		echo
	done # while (( iline < lines ))
fi # (( lines == 0 ))
ls -al $filesyn $filetrk $fileout
echo
echo $filesyn begin
cat $filesyn
echo $filesyn end
echo
echo $fileout begin
cat $fileout
echo $fileout end
echo
echo diff $filesyn $fileout begin
diff $filesyn $fileout
echo diff $filesyn $fileout end
echo
