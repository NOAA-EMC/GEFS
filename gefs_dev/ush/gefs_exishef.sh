######################## CALLED BY EXENSCQPF ##########################
echo "------------------------------------------------"
echo "exishef.sh "
echo "------------------------------------------------"
echo "History: Aug 2019 - First implementation of this new script in GEFS transition to DELL."
echo "   Following Sid Kats and Ying Lin's example "
echo "AUTHOR: Dingchen Hou (wx20dh)"

#DATA=$(pwd)
#UTILgefs=../..
#INPUTshef=/gpfs/dell1/nco/ops/dcom/prod/shef_pefiles
#PDYshef=$1
#cp -pr ${INPUTshef}/${PDYshef}.pe SHEFOUT1

echo DATA=$DATA
echo UTILgefs=$UTILgefs

mkdir $DATA/exishef
cd $DATA/exishef

#    extract the 24 hr precip reports from output of decoder
#    do not use estimated values from PDX RFC, and keep reports
#    whose value is listed as missing (if it is a revised report)

awk ' $7 ~/P/ && $8 ~/P/ {print $0}' $DATA/SHEFOUT1 | \
	awk ' $9 ~/2001|5004/ {print $0}' | awk ' $15 ~/Z|V|S|N/ {print $0}' |\
	awk ' $14 !~/-9*/ || $16 ~/1/ {print $0}' > rtemp

#    remove bad reports (val > 20)

awk '$14 < 20  {print $0}' rtemp > rfc24-precip

date "+End shef-decoder:         %c" >> shef-status-log

#    Now prepare shef output for transfer to WWB workstation and program
#    to make BUFR message from these shef output records....

#      move the ID to begining of the line, then sort by stn and time

cut -c18-25 rfc24-precip | paste -d' ' - rfc24-precip |  \
	cut -c1-26,36-80 | $UTILgefs/ush/ingest_delete-bad-dates.pl > rfc24-x

sort -k1,6 -k16,16 -k14n,14 -k7,12 rfc24-x | uniq > rfc24-xsort

#      select report closest to 12Z for stns with more than 1 report/day
#      and drop reports with values listed as missing

awk  -f $UTILgefs/ush/ingest_uniq-final.awk  rfc24-xsort  | \
awk ' $14 !~/-9*/ {print $0}' > rfc24-xuniq

#      This is a note in the SHEF script
#      If the early run of the day (1-7PM ET or 18-24 GMT), select all
#      the records from previous step and save them.  If the late run,
#      (8-11PM ET or 1-4 GMT), select only those records which do not
#      match those records previously saved in the early run.

#     Note by Dingchen Hou, 08/22/2019
#     In GEFS prod, the cqpf job rus at about 00Z+5:30 and the SHEF file
#     used if for previous day, which would be frozen after 00Z today. 
zh1=19
if [ $zh1 -gt 12 ]
then
	cp -pr rfc24-xuniq $DATA/rfc24-uniq-early-shef
else
	mv   rfc24-xuniq      rfc24-xuniq-final
	sort rfc24-uniq-early rfc24-xuniq-final | uniq -u > rfc24-xuniq
	rm   rfc24-xuniq-final
fi

#      group reports by type (2001 & 5004) so that FTP from cpc-ls-work1 can grab
#      the data for insertation into Postgres RDBMS

#awk  '$9 ~/2001/ {print $0}' rfc24-xuniq        > rfc24-x2001
#awk  '$9 ~/5004/ {print $0}' rfc24-xuniq        > rfc24-x5004
#cp   rfc24-x2001  rfc24-x2001
#cp   rfc24-x5004  rfc24-x5004

cd $DATA
