#________________________________________________________________________
# SCRIPT:   /cpc/save/Sid.Katz/shef/ush/ingest_uniq-final.awk
# LANGUAGE: awk
#-------------------------------------------------------------------------
# INITIATION METHOD:  manual or called by another script
#-------------------------------------------------------------------------
# PURPOSE:  Find report closest to 12Z from multiple reports in one day
#-------------------------------------------------------------------------
# USAGE:   awk -f ingest_uniq-final.awk input filename > output filename
#-------------------------------------------------------------------------
# INPUT FILES:  $DATA/rfc24-xsort Decoded SHEF msgs, sorted by site & time
# OUTPUT FILES: $DATA/rfc24-xuniq Input file with report closest to 12Z
# SOURCED FILES:    None
#-------------------------------------------------------------------------
# SCRIPTS USED:     None
# EXECUTABLES USED: None
#-------------------------------------------------------------------------
# COMMAND-LINE VARIABLES: See input/output file section
# LOCAL VARIABLES:
#   rec   - array to hold individual reports for a station by hour value ($5)
#   min   - array to hold the minutes part of time for the value
#   src   - array to hold the name of the office which sent the report
#   nrep  - number of reports received for a station for a specific date
#   previd- stn id variable used to determine when a new station begins
#   prday - date variable used to determine when a new date begins
#   $0    - awk id: whole input report
#   $1    - awk id: 1st field of the input report (stn id)
#   $4    - awk id: 4th field of the input report (day of the month)
#   $5    - awk id: 5th field of the input report (hour of the day)
#   $6    - awk id: 6th field of the input report (minutes of the hour)
#   $17   - awk id: 17th field of the input report (office sending msg)
#-------------------------------------------------------------------------
# AUTHOR:    Sid Katz
#-------------------------------------------------------------------------
# DATE:      13-Nov-2006
#-------------------------------------------------------------------------
# MODIFICATIONS:
#             Add doc block for WCOSS implementation 7-Feb-2013
#             Shell variable $DATA set by calling script
#_________________________________________________________________________

#   awk script to find the report closest to 12Z from multiple 
#   reports in one day.
#   replace 1st break commands with next command 7-dec-2012

{  if ($1 == previd)

     { if ($4 == prday)            # same stn and day
        { if (rec[$5] == "")       # no existing report for this hour
           {  rec[$5] = $0         # add to holding arrays
              min[$5] = $6
              src[$5] = $17
              nrep +=1 
              next                 # break
           }
          else                                # report for this hour exists
            {if ($17 != "NMC")                # not an NMC report - replace ??
              { if ($5 < 12 && $6 >= min[$5]) # closer to 12Z - replace...
                 {  rec[$5] = $0
                    min[$5] = $6
                    src[$5] = $17
                    nrep +=1 
                 }
                if ($5 > 11 && $6 <= min[$5]) # closer to 12Z - replace...
                 {  rec[$5] = $0
                    min[$5] = $6
                    src[$5] = $17
                    nrep +=1 
                 }
              } 
            }
        }               
       else                        # same stn, new day 
        { if (nrep > 1)            # for multiple reports, discard NMC reports
           { for (j=0; j<25; j++) 
             if (src[j] != "" && src[j] == "NMC" && nrep > 1)
                {rec[j] = ""; nrep --; } 
           }
          for (i=0; i<13; i++)     # find closet report to 12Z 
           {  if (rec[12-i] != "")  {printf "%s\n", rec[12-i]; break} # break
              if (rec[12+i] != "")  {printf "%s\n", rec[12+i]; break} # break
           }               
                                   # reset all the holding arrays/variables
          for (j=0; j<25; j++)      {rec[j] = ""; src[j] = ""}
          rec[$5] = $0
          min[$5] = $6
          src[$5] = $17
          prday   = $4 
          nrep    = 1
        }
     }

   else                            # new stn--write report for previous stn
     { if (nrep > 1)               # for multiple reports, discard NMC reports 
        { for (j=0; j<25; j++) 
          if (src[j] != "" && src[j] == "NMC" && nrep > 1) 
             {rec[j] = ""; nrep --; } 
        } 
       for (i=0; i<13; i++)        # find closet report to 12Z
        {   if (rec[12-i] != "")    {printf "%s\n", rec[12-i]; break} # break
            if (rec[12+i] != "")    {printf "%s\n", rec[12+i]; break} # break
        }
                                   # reset all the holding arrays/variables 
       for (j=0; j<25; j++)         {rec[j] = ""; src[j] = ""}
       rec[$5] = $0
       min[$5] = $6
       src[$5] = $17
       prday   = $4 
       nrep    = 1
       previd  = $1
     }
}

END{   if (nrep > 1)               # eof--process last stn in holding arrays
        { for (j=0; j<25; j++) 
          if (src[j] != "" && src[j] == "NMC" && nrep > 1) 
             {rec[j] = ""; nrep --; } 
        }
       for (i=0; i<13; i++)        # find closet report to 12Z 
        {   if (rec[12-i] != "")    {printf "%s\n", rec[12-i]; break}  # break
            if (rec[12+i] != "")    {printf "%s\n", rec[12+i]; break}  # break
        }
    }
