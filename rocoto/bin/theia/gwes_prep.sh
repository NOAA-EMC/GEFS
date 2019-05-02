#!/bin/bash

# Set NCO messaging proxies
export jlogfile=/dev/null
export jobid=${job}.$$

export gwesmpexec_mpmd="srun -n $SLURM_NTASKS /scratch3/NCEPDEV/nwprod/util/exec/mpiserial"

$SOURCEDIR/jobs/JWAVE_GWES_PREP
