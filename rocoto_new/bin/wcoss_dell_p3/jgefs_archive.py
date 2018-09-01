#! /usrx/local/prod/python/2.7.13/bin/python

##########################################################
# Archives specified directories of GEFS output to HPSS for long-term storage.
#
# Inputs (via environment variables):
# 	WORKDIR         : The base GEFS output directory (usually in ptmp)
# 	HPSS_DIR        : The base HPSS directory in which to store tar files
# 	DIRS_TO_ARCHIVE : A comma-separated list of directories to archive
# 	PDY             : Initialization date in YYYYMMDD form
# 	cyc             : Initialization hour in HH form
#
# 	GEFS output must be located in the WORKDIR com directory as such:
# 		<WORKDIR>/com/gens/dev/gefs.<PDY>/<cyc>/<directory>
#
# Outputs:
# 	For each directory in DIRS_TO_ARCHIVE, a tar file will be created in the following
# 	  location on HPSS:
# 	  	<HPSS_DIR>/<YYYY>/<YYYY><MM>/<YYYY><MM><DD>/gefs.<YYYY><MM><DD>_<HH>.<directory>.tar
#
# Error Codes:
# 	-100 : Required environment variable not defined
# 	-101 : HPSS error
#
##########################################################

import os, shutil, glob, subprocess, sys

# Make sure print statements are flushed immediately, otherwise
#   print statments may be out-of-order with subprocess output
# print = partial(print, flush=True)

# Read in environment variables and make sure they exist
work_dir = os.environ.get("WORKDIR")
if( work_dir == None ):
	print("FATAL: Environment variable WORKDIR not set")
	quit(-100)

hpss_path = os.environ.get("HPSS_DIR")
if( hpss_path == None ):
	print("FATAL: Environment variable HPSS_DIR not set")
	quit(-100)

dirs_to_archive_string = os.environ.get("DIRS_TO_ARCHIVE")
if( dirs_to_archive_string == None ):
	print("FATAL: Environment variable HPSS_DIR not set")
	quit(-100)
# Convert to array using commas and removing whitespace
dirs_to_archive = [x.strip() for x in dirs_to_archive_string.split(',')]

date_string = os.environ.get("PDY")
if( date_string == None ):
	print("FATAL: Environment variable PDY not set")
	quit(-100)

cycle = os.environ.get("cyc")
if( cycle == None ):
	print("FATAL: Environment variable cyc not set")
	quit(-100)

print("Starting GEFS archive with the following settings:")
print("Source directory              : " + work_dir)
print("Destination Directory on HPSS : " + hpss_path)
print("Directories to Archive        : " + str(dirs_to_archive))
print("Date/Cycle                    : " + date_string + "_" + cycle)
sys.stdout.flush()

yyyy = date_string[0:4]
mm = date_string[4:6]
dd = date_string[6:8]

destination_path = hpss_path + "/" + yyyy + "/" + yyyy + mm + "/" + yyyy + mm + dd
output_path = work_dir + "/com/gens/dev/gefs." + date_string + "/" + cycle

# Create directory on HPSS
err_code = subprocess.call(["hsi", "mkdir", "-p", destination_path])
if(err_code != 0):
	print("FATAL: Could not create destination directory " + destination_path + " on HPSS, error code: " + str(err_code))
	quit(-101)

# Archive directories
os.chdir(output_path)
for directory in dirs_to_archive:
	tar_folder = destination_path + "/gefs." + date_string + "_" + cycle + "." + directory
	if os.path.exists(tar_folder):
		tar_file = tar_folder + ".tar"
		print("Creating tar on HPSS for " + directory)
		print("    From " + output_path + "/" + directory + " to " + tar_file)
		sys.stdout.flush()
		err_code = subprocess.call(["htar", "-cvf", tar_file, directory])
		if(err_code != 0):
			print("FATAL: Could not create " + tar_file + " on HPSS, error code: " + str(err_code))
			quit(-101)
