#! /apps/intel/intelpython3/bin/python3

import os, shutil, glob, contextlib
import datetime
from datetime import datetime, timedelta

# Read in environment variables and make sure they exist
ptmp = os.environ.get("WORKDIR")
if( ptmp == None ):
	print("FATAL: Environment variable WORKDIR not set")
	quit(-100)
exp_id = os.environ.get("EXPID")
if( exp_id == None ):
	print("FATAL: Environment variable EXPID not set")
	quit(-100)
date_string = os.environ.get("PDY")
if( date_string == None ):
	print("FATAL: Environment variable PDY not set")
	quit(-100)
cycle = os.environ.get("cyc")
if( cycle == None ):
	print("FATAL: Environment variable cyc not set")
	quit(-100)

# Output directories that need to be removed
output_dirs = ["ensstat", "init", "misc", "pgrb2a1p0", "pgrb2alr", "pgrb2b1p0", "pgrb2bp5", "sflux", "genesis", "master", "pgrb2a", "pgrb2a2p5", "pgrb2ap5", "pgrb2b2p5", "sfcsig", "tctrack"]
output_dirs_last_cyc = ["sfcsig_enkf", "track_enkf"]

date = datetime.strptime(date_string + cycle, "%Y%m%d%H")

# Calculate information for previous cycle (needed to remove EnKF data)
date_last_cyc = date + timedelta(hours=-6)
date_string_last = date_last_cyc.strftime("%Y%m%d")
cycle_last = date_last_cyc.strftime("%H")


# Start building up directories to remove
dirs_to_remove = []

# Working directories
dirs_to_remove.append(ptmp + "/tmpnwprd/" + exp_id + date_string + cycle + "*")
dirs_to_remove.append(ptmp + "/tmpnwprd/gefs_init" + date_string + cycle + ".dev.save")

# Last cycle enkf directories
for output_dir in output_dirs_last_cyc:
	dirs_to_remove.append(ptmp + "/com/gens/dev/gefs." + date_string_last + "/" + cycle_last + "/" + output_dir)

# Output directories
for output_dir in output_dirs:
	dirs_to_remove.append(ptmp + "/com/gens/dev/gefs." + date_string + "/" + cycle + "/" + output_dir)

# Other init directories
dirs_to_remove.append(ptmp + "/nwges/dev/gefs." + date_string + "/*.t" + cycle + "z.*")
dirs_to_remove.append(ptmp + "/nwges/dev/gefs." + date_string + "/" + cycle)

# Log directory (probably want to keep these)
# dirs_to_remove.append(ptmp + "/com/output/dev/" + date_string + "/*_" + cycle + ".*.bqs3")

# jlog directory
dirs_to_remove.append(ptmp + "/com/logs/jlogfiles/jlogfile." + exp_id + date_string + cycle + "*")

for path in dirs_to_remove:
	# print(path)
	for f in glob.glob(path):
		print("Removing " + f)
		# Delete if it is a directory
		with contextlib.suppress(NotADirectoryError):
			shutil.rmtree(f)
		# Delete if it is a file
		with contextlib.suppress(FileNotFoundError):
			os.remove(f)
