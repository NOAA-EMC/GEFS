#! /usr/bin/env python3

"""
Delete all GEFS output from temporary directory (except log files)

Inputs (via environment variables):
	WORKDIR         : The base GEFS output directory (usually in ptmp)
	EXPID           : GEFS experiment ID
	PDY             : Initialization date in YYYYMMDD form
	cyc             : Initialization hour in HH form

Outputs:
	The following files/directories in WORKDIR and all files contained within will all be deleted:
		<WORKDIR>/com/gefs/dev/gefs.<PDY>/<cyc>/atmos/ensstat
		<WORKDIR>/com/gefs/dev/gefs.<PDY>/<cyc>/atmos/init
		<WORKDIR>/com/gefs/dev/gefs.<PDY>/<cyc>/atmos/misc
		<WORKDIR>/com/gefs/dev/gefs.<PDY>/<cyc>/atmos/pgrb2alr
		<WORKDIR>/com/gefs/dev/gefs.<PDY>/<cyc>/atmos/pgrb2bp5
		<WORKDIR>/com/gefs/dev/gefs.<PDY>/<cyc>/atmos/sflux
		<WORKDIR>/com/gefs/dev/gefs.<PDY>/<cyc>/atmos/genesis
		<WORKDIR>/com/gefs/dev/gefs.<PDY>/<cyc>/atmos/master
		<WORKDIR>/com/gefs/dev/gefs.<PDY>/<cyc>/atmos/pgrb2a
		<WORKDIR>/com/gefs/dev/gefs.<PDY>/<cyc>/atmos/pgrb2a2p5
		<WORKDIR>/com/gefs/dev/gefs.<PDY>/<cyc>/atmos/pgrb2ap5
		<WORKDIR>/com/gefs/dev/gefs.<PDY>/<cyc>/atmos/pgrb2b2p5
		<WORKDIR>/com/gefs/dev/gefs.<PDY>/<cyc>/atmos/sfcsig
		<WORKDIR>/com/gefs/dev/gefs.<PDY>/<cyc>/atmos/tctrack

	Additionally, the following directories for the from the previous cycle
	  (6 hours previous) and all files contained within will be deleted:
		<WORKDIR>/com/gefs/dev/gefs.<PDY_last>/<cyc_last>/atmos/sfcsig_enkf
		<WORKDIR>/com/gefs/dev/gefs.<PDY_last>/<cyc_last>/atmos/track_enkf

Error Codes:
	-100 : Required environment variable not defined

"""

import os
import shutil
import glob
from contextlib import suppress
from datetime import datetime, timedelta
from functools import partial

# Make sure print statements are flushed immediately, otherwise
#   print statments may be out-of-order with subprocess output
print = partial(print, flush=True)

# Output directories that need to be removed
output_dirs = ["gridded", "station", "stats", "rundata", "gempak"]
output_dirs_last_cyc = ["restart"]
output_dir_pattern = "{work_dir}/com/gefs/dev/gefs.%Y%m%d/%H/wave/{output_dir}"

# Read in environment variables and make sure they exist
work_dir = os.environ.get("WORKDIR")
if(work_dir is None):
	print("FATAL: Environment variable WORKDIR not set")
	quit(-100)

exp_id = os.environ.get("EXPID")
if(exp_id is None):
	print("FATAL: Environment variable EXPID not set")
	quit(-100)

pdy = os.environ.get("PDY")
if(pdy is None):
	print("FATAL: Environment variable PDY not set")
	quit(-100)

cycle = os.environ.get("cyc")
if(cycle is None):
	print("FATAL: Environment variable cyc not set")
	quit(-100)

time = datetime.strptime(pdy + cycle, "%Y%m%d%H")

print("Starting GEFS cleanup with the following settings:")
print("Work Directory : {work_dir}".format(work_dir=work_dir))
print("Experiment ID  : {exp_id}".format(exp_id=exp_id))
print(time.strftime("Date/Cycle                    : %Y%m%d_%H"))

# Calculate information for previous cycle (needed to remove EnKF data)
time_last_cyc = time + timedelta(hours=-6)

# Start building up directories to remove
dirs_to_remove = []

# Working directories
#dirs_to_remove.append(time.strftime("{work_dir}/tmpnwprd/{exp_id}_%Y%m%d%H_*".format(work_dir=work_dir, exp_id=exp_id)))
#dirs_to_remove.append(time.strftime("{work_dir}/tmpnwprd/gefs_init_%Y%m%d%H.dev.save".format(work_dir=work_dir)))

# Last cycle enkf directories
for output_dir in output_dirs_last_cyc:
	dirs_to_remove.append(time_last_cyc.strftime(output_dir_pattern.format(work_dir=work_dir, output_dir=output_dir)))

# Output directories
for output_dir in output_dirs:
	dirs_to_remove.append(time.strftime(output_dir_pattern.format(work_dir=work_dir, output_dir=output_dir)))

# Other init directories
#dirs_to_remove.append(time.strftime("{work_dir}/nwges/dev/gefs.%Y%m%d/*.t%Hz.*".format(work_dir=work_dir)))
#dirs_to_remove.append(time.strftime("{work_dir}/nwges/dev/gefs.%Y%m%d/%H".format(work_dir=work_dir)))

# Log directory (probably want to keep these)
# dirs_to_remove.append(work_dir + "/com/output/dev/" + pdy + "/*_" + cycle + ".*.bqs3")

# jlog directory
#dirs_to_remove.append(time.strftime("{work_dir}/com/logs/jlogfiles/jlogfile.{exp_id}%Y%m%d%H*".format(work_dir=work_dir, exp_id=exp_id)))

for path in dirs_to_remove:
	# print(path)
	for f in glob.glob(path):
		print("Removing " + f)
		# Delete if it is a directory
		with suppress(NotADirectoryError):
			shutil.rmtree(f)
		# Delete if it is a file
		with suppress(FileNotFoundError):
			os.remove(f)
