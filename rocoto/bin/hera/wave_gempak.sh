#!/bin/bash

# Set NCO messaging proxies
export jlogfile=/dev/null
export jobid=${job}.$$

module load gempak/7.4.2

$SOURCEDIR/jobs/JGEFS_WAVE_GEMPAK
