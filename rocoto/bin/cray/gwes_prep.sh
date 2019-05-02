#!/bin/bash

# Set NCO messaging proxies
export jlogfile=/dev/null
export jobid=${job}.$$

$SOURCEDIR/jobs/JWAVE_GWES_PREP
