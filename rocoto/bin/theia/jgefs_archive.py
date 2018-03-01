#! /apps/intel/intelpython3/bin/python3

import os, shutil, glob, subprocess

# Read in environment variables and make sure they exist
ptmp = os.environ.get("WORKDIR")
if( ptmp == None ):
	print("FATAL: Environment variable WORKDIR not set")
	quit(-100)
exp_id = os.environ.get("EXPID")
if( exp_id == None ):
	print("FATAL: Environment variable EXPID not set")
	quit(-100)
date = os.environ.get("PDY")
if( date == None ):
	print("FATAL: Environment variable PDY not set")
	quit(-100)
cycle = os.environ.get("cyc")
if( cycle == None ):
	print("FATAL: Environment variable cyc not set")
	quit(-100)

yyyy = date[0:4]
mm = date[4:6]
dd = date[6:8]

hpss_path = "/NCEPDEV/emc-ensemble/2year/Walter.Kolczynski/GEFS"

destination_path = hpss_path + "/" + exp_id + "/" + yyyy + "/" + yyyy + mm
output_path = ptmp + "/com/gens/dev/gefs." + date + "/" + cycle

dirs_to_archive = ["ensstat", "pgrb2a1p0", "pgrb2a2p5", "pgrb2ap5", "tctrack"]
# dirs_to_archive = ["ensstat", "tctrack"]

# Create directory on HPSS
err_code = subprocess.run(["hsi", "mkdir", "-p", destination_path]).returncode
if(err_code != 0):
	print("FATAL: Could not create destination directory " + destination_path + " on HPSS, error code: " + str(err_code))
	quit(-1)

# Archive directories
os.chdir(output_path)
for directory in dirs_to_archive:
	tar_file = destination_path + "gefs." + date + "_" + cycle + "." + directory + ".tar"
	err_code = subprocess.run(["htar", "-cvf", tar_file, directory]).returncode
	if(err_code != 0):
		print("FATAL: Could not create " + tar_file + " on HPSS, error code: " + str(err_code))
		quit(-2)