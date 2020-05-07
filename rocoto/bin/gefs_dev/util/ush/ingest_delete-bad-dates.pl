#!/usr/bin/perl
#_________________________________________________________________________
# SCRIPT:   /cpc/save/Sid.Katz/shef/ush/ingest_delete-bad-dates.pl
# LANGUAGE: perl script
#-------------------------------------------------------------------------
# INITIATION METHOD:  manual or called by another script
#-------------------------------------------------------------------------
# PURPOSE: Remove reports with future date of observations      
#-------------------------------------------------------------------------
# USAGE:   ingest_delete-bad-dates.pl input file > output file
#-------------------------------------------------------------------------
# INPUT FILES:  $DATA/rfc24-precip Decoded SHEF messages
# OUTPUT FILES: $DATA/rfc24-x Decoded SHEF messages without bad records
# SOURCED FILES:    NONE
#-------------------------------------------------------------------------
# SCRIPTS USED:     NONE
#-------------------------------------------------------------------------
# EXECUTABLES USED: NONE
#-------------------------------------------------------------------------
# COMMAND-LINE VARIABLES: See input/output file section
# LOCAL VARIABLES: NONE 
#-------------------------------------------------------------------------
# AUTHOR:    Sid Katz
#-------------------------------------------------------------------------
# DATE:       5-Jan-2009
#-------------------------------------------------------------------------
# MODIFICATIONS:
#             Add doc block for WCOSS implementation 7-Feb-2013
#             Shell variable $DATA set by calling script
#_________________________________________________________________________

##  This perl pgm removes reports whose date of observation is in the
#   future, according to the current system clock.

$today=`date "+%Y%m%d"` ;

while (<>) 
{     chop;
     ($id, $yr, $mon, $day) = split ( " ", $_);
      $daterec = ($yr * 10000) + ($mon * 100) + $day ;
      unless ( $daterec > $today)
         { printf "%s\n", $_ ; }
} 
