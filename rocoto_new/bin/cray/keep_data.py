#! /usrx/local/prod/python/2.7.13/bin/python

##########################################################
# Copies specified directories of GEFS output from temporary location to another location.
#
# Inputs (via environment variables):
# 	WORKDIR         : The base GEFS output directory (usually in ptmp)
# 	KEEP_DIR        : The base directory to store GEFS output (usually in noscrub)
# 	DIRS_TO_KEEP    : A comma-separated list of directories to retain
# 	PDY             : Initialization date in YYYYMMDD form
# 	cyc             : Initialization hour in HH form
#
# 	GEFS output must be located in the WORKDIR com directory as such:
# 		<WORKDIR>/com/gens/dev/gefs.<PDY>/<cyc>/<directory>
#
# Outputs:
# 	For each directory in DIRS_TO_KEEP, that directory will be copied to the KEEP_DIR in
#	  the following location:
#		<KEEP_DIR>/gefs.<PDY>/<cyc>/<directory>
#
# Error Codes:
# 	-100 : Required environment variable not defined
# 	-102 : KEEP_DIR already exists and couldn't be deleted
#
##########################################################

import os, shutil, glob

# Read in environment variables and make sure they exist
work_dir = os.environ.get("WORKDIR")
if( work_dir == None ):
	print("FATAL: Environment variable WORKDIR not set")
	quit(-100)

keep_dir = os.environ.get("KEEP_DIR")
if( keep_dir == None ):
	print("FATAL: Environment variable KEEP_DIR not set")
	quit(-100)

dirs_to_keep_string = os.environ.get("DIRS_TO_KEEP")
if( dirs_to_keep_string == None ):
	print("FATAL: Environment variable DIRS_TO_KEEP not set")
	quit(-100)
# Convert to array using commas and removing whitespace
dirs_to_keep = [x.strip() for x in dirs_to_keep_string.split(',')]

date_string = os.environ.get("PDY")
if( date_string == None ):
	print("FATAL: Environment variable PDY not set")
	quit(-100)

cycle = os.environ.get("cyc")
if( cycle == None ):
	print("FATAL: Environment variable cyc not set")
	quit(-100)

print("Starting GEFS keep_data with the following settings:")
print("Source Directory      : " + work_dir)
print("Destination Directory : " + keep_dir)
print("Directories to Keep   : " + str(dirs_to_keep))
print("Date/Cycle            : " + date_string + "_" + cycle)

# keep_dir = "/scratch4/NCEPDEV/ensemble/noscrub/Walter.Kolczynski/GEFS"
clobber = True

destination_path = keep_dir + "/gefs." + date_string + "/" + cycle
output_path = work_dir + "/com/gens/dev/gefs." + date_string + "/" + cycle

# Output directories
for directory in dirs_to_keep:
	output_dir = output_path + "/" + directory
	if os.path.exists(output_dir):
		destination_dir = destination_path + "/" + directory
		if ( os.path.exists(destination_dir) ):
			if(clobber):
				shutil.rmtree(destination_dir)
			else:
				print("FATAL: Destination diretory " + destination_dir + " already exists and clobber is False")
				quit(-102)

		if ( not os.path.exists(destination_path) ):
			os.makedirs(destination_path)

		print("Copying files for " + directory)
		print("    From " + output_dir + " to " + destination_dir)
		shutil.copytree(output_dir, destination_dir)
