#! /usrx/local/prod/python/2.7.13/bin/python

##########################################################
# Copies specified directories of GEFS output from temporary location to another location.
#
# Inputs (via environment variables):
# 	WORKDIR         : The base GEFS working directory (usually in ptmp)
# 	INIT_DIR        : The base directory with the GEFS init files
# 	MEMBER    		: Ensemble member for this job
# 	PDY             : Initialization date in YYYYMMDD form
# 	cyc             : Initialization hour in HH form
#
# 	GEFS init files must be located in the INIT_DIR as such:
#		<INIT_DIR>/<PDY>/<cyc>/<MEMBER>
#
# Outputs:
# 	Contents of the init directory will be copied to the WORKDIR in the following location:
# 		<WORKDIR>/nwges/dev/gefs.<PDY>/<cyc>/<MEMBER>
#
# Error Codes:
# 	-100 : Required environment variable not defined
# 	-102 : Destination directory already exists and couldn't be deleted
#
##########################################################

import os, shutil, glob, sys

# Read in environment variables and make sure they exist
work_dir = os.environ.get("WORKDIR")
if( work_dir == None ):
	print("FATAL: Environment variable WORKDIR not set")
	quit(-100)

init_dir = os.environ.get("INIT_DIR")
if( init_dir == None ):
	print("FATAL: Environment variable INIT_DIR not set")
	quit(-100)

member = os.environ.get("MEMBER")
if( member == None ):
	print("FATAL: Environment variable MEMBER not set")
	quit(-100)

date_string = os.environ.get("PDY")
if( date_string == None ):
	print("FATAL: Environment variable PDY not set")
	quit(-100)

cycle = os.environ.get("cyc")
if( cycle == None ):
	print("FATAL: Environment variable cyc not set")
	quit(-100)

print("Starting GEFS copy_init with the following settings:")
print("Source Directory      : " + init_dir)
print("Destination Directory : " + work_dir)
print("Date/Cycle            : " + date_string + "_" + cycle)
sys.stdout.flush()

clobber = True

source_dir = init_dir + "/" + date_string + "/" + cycle + "/" + member
destination_base = work_dir + "/nwges/dev/gefs." + date_string + "/" + cycle
destination_dir = destination_base + "/" + member

if ( os.path.exists(destination_dir) ):
	if(clobber):
		shutil.rmtree(destination_dir)
	else:
		print(destination_dir + " already exists and clobber is False, skipping.")
		sys.stdout.flush()
		quit(0)

if ( not os.path.exists(destination_base) ):
	try:
		os.makedirs(destination_base)
	except FileExistsError:
		pass

print("Copying files for init")
print("    From " + source_dir + " to " + destination_dir)
sys.stdout.flush()
shutil.copytree(source_dir, destination_dir, symlinks=True)
