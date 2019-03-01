#!/bin/sh

module rm craype-haswell
module load craype-sandybridge

module load PrgEnv-intel
module rm intel
module load intel/16.3.210

module load nemsio-intel/2.2.3       
module load bacio-intel/2.0.1
module load w3nco-intel/2.0.6

make -f gfsnemsio2grb.make

