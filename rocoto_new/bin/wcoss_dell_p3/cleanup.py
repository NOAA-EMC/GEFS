#!/usr/bin/env python
###!/usrx/local/dev/packages/python/2.7.14/bin/python

##! /usrx/local/prod/python/2.7.13/bin/python

##########################################################
# Delete all GEFS output from temporary directory (except log files)
#
# Inputs (via environment variables):
# 	WORKDIR         : The base GEFS output directory (usually in ptmp)
#   EXPID           : GEFS experiment ID
# 	PDY             : Initialization date in YYYYMMDD form
# 	cyc             : Initialization hour in HH form
#
# Outputs:
# 	The following files/directories in WORKDIR and all files contained within will all be deleted:
#		<WORKDIR>/tmpnwprd/<EXPID><PDY><cyc>*
#		<WORKDIR>/tmpnwprd/gefs_init_<PDY><cyc>.dev.save
#		<WORKDIR>/com/gens/dev/gefs.<PDY>/<cyc>/ensstat
#		<WORKDIR>/com/gens/dev/gefs.<PDY>/<cyc>/init
#		<WORKDIR>/com/gens/dev/gefs.<PDY>/<cyc>/misc
#		<WORKDIR>/com/gens/dev/gefs.<PDY>/<cyc>/pgrb2a1p0
#		<WORKDIR>/com/gens/dev/gefs.<PDY>/<cyc>/pgrb2alr
#		<WORKDIR>/com/gens/dev/gefs.<PDY>/<cyc>/pgrb2b1p0
#		<WORKDIR>/com/gens/dev/gefs.<PDY>/<cyc>/pgrb2bp5
#		<WORKDIR>/com/gens/dev/gefs.<PDY>/<cyc>/sflux
#		<WORKDIR>/com/gens/dev/gefs.<PDY>/<cyc>/genesis
#		<WORKDIR>/com/gens/dev/gefs.<PDY>/<cyc>/master
#		<WORKDIR>/com/gens/dev/gefs.<PDY>/<cyc>/pgrb2a
#		<WORKDIR>/com/gens/dev/gefs.<PDY>/<cyc>/pgrb2a2p5
#		<WORKDIR>/com/gens/dev/gefs.<PDY>/<cyc>/pgrb2ap5
#		<WORKDIR>/com/gens/dev/gefs.<PDY>/<cyc>/pgrb2b2p5
#		<WORKDIR>/com/gens/dev/gefs.<PDY>/<cyc>/sfcsig
#		<WORKDIR>/com/gens/dev/gefs.<PDY>/<cyc>/tctrack
#		<WORKDIR>/nwges/dev/gefs.<PDY>/*.t<cyc>z.*
#		<WORKDIR>/nwges/dev/gefs.<PDY>/<cyc>
#		<WORKDIR>/com/logs/jlogfiles/jlogfile.<EXPID><PDY><cyc>*
#
#	Additionally, the following directories for the from the previous cycle (6 hours previous)
#	  and all files contained within will be deleted:
# 		<WORKDIR>/com/gens/dev/gefs.<PDY_last>/<cyc_last>/sfcsig_enkf
# 		<WORKDIR>/com/gens/dev/gefs.<PDY_last>/<cyc_last>/track_enkf
#
# Error Codes:
# 	-100 : Required environment variable not defined
#
##########################################################

import os, shutil, glob
import datetime
from datetime import datetime, timedelta

# Read in environment variables and make sure they exist
work_dir = os.environ.get("WORKDIR")
if( work_dir == None ):
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

print("Starting GEFS cleanup with the following settings:")
print("Work Directory : " + work_dir)
print("Experiment ID  : " + exp_id)
print("Date/Cycle     : " + date_string + "_" + cycle)

# Output directories that need to be removed
output_dirs = ["ensstat", "init", "misc", "pgrb2alr", "sflux", "genesis", "master", "pgrb2ap25", "pgrb2bp25", "pgrb2ap5", "pgrb2bp5", "pgrb2a2p5", "pgrb2b2p5", "pgrb2a", "sfcsig", "tctrack"]
output_dirs_last_cyc = ["sfcsig_enkf", "track_enkf"]

date = datetime.strptime(date_string + cycle, "%Y%m%d%H")

# Calculate information for previous cycle (needed to remove EnKF data)
date_last_cyc = date + timedelta(hours=-6)
date_string_last = date_last_cyc.strftime("%Y%m%d")
cycle_last = date_last_cyc.strftime("%H")

# Start building up directories to remove
dirs_to_remove = []

# Working directories
dirs_to_remove.append(work_dir + "/tmpnwprd/" + exp_id + date_string + cycle + "*")
dirs_to_remove.append(work_dir + "/tmpnwprd/gefs_init_" + date_string + cycle + ".dev.save")

# Last cycle enkf directories
for output_dir in output_dirs_last_cyc:
	dirs_to_remove.append(work_dir + "/com/gens/dev/gefs." + date_string_last + "/" + cycle_last + "/" + output_dir)

# Output directories
for output_dir in output_dirs:
	dirs_to_remove.append(work_dir + "/com/gens/dev/gefs." + date_string + "/" + cycle + "/" + output_dir)

# Other init directories
dirs_to_remove.append(work_dir + "/nwges/dev/gefs." + date_string + "/*.t" + cycle + "z.*")
dirs_to_remove.append(work_dir + "/nwges/dev/gefs." + date_string + "/" + cycle)

# Log directory (probably want to keep these)
# dirs_to_remove.append(work_dir + "/com/output/dev/" + date_string + "/*_" + cycle + ".*.bqs3")

# jlog directory
dirs_to_remove.append(work_dir + "/com/logs/jlogfiles/jlogfile." + exp_id + date_string + cycle + "*")

for path in dirs_to_remove:
	# print(path)
	for f in glob.glob(path):
		print("Removing " + f)
		# Delete if it is a directory
		try:
			shutil.rmtree(f)
		except OSError:
			pass
		# Delete if it is a file
		try:
			os.remove(f)
		except OSError:
			pass
