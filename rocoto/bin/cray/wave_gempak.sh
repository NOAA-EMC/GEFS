#!/bin/bash

ulimit -s unlimited
ulimit -a

module load gempak/7.3.1

$SOURCEDIR/jobs/JGEFS_WAVE_GEMPAK
