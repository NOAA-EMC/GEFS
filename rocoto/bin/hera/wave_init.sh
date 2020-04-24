#!/bin/bash

# Set NCO messaging proxies
export jlogfile=/dev/null
export jobid=${job}.$$

$SOURCEDIR/jobs/JGEFS_WAVE_INIT
