#! /apps/intel/intelpython3/bin/python3

import os, shutil, glob

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

noscrub = "/scratch4/NCEPDEV/ensemble/noscrub/Walter.Kolczynski/GEFS"
clobber = True

destination_path = noscrub + "/" + exp_id + "/gefs." + date + "/" + cycle
output_path = ptmp + "/com/gens/dev/gefs." + date + "/" + cycle

dirs_to_keep = ["ensstat", "pgrb2a1p0", "pgrb2a2p5", "pgrb2ap5", "tctrack"]

# Output directories
for directory in dirs_to_keep:
	output_dir = output_path + "/" + directory
	destination_dir = destination_path + "/" + directory
	if ( os.path.exists(destination_dir) ):
		if(clobber):
			shutil.rmtree(destination_dir)
		else:
			print("FATAL: Destination diretory " + destination_dir + " already exists and clobber is False")
			quit(-1)

	if ( not os.path.exists(destination_path) ):
		os.makedirs(destination_path)

	shutil.copytree(output_dir, destination_dir)