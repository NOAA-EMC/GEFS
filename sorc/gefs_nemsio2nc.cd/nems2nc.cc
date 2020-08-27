#include "nems2nc.h"
#include <iostream>
#include <string>
#include <cstdlib>
#include "gfsnc.h"
#include "nemsio.h"

namespace nems2nc {

 int Convert::init(int argc, char** argv) {
   // initialize the code
   // get command line arguments
   if ( argc < 3 ) {
     // need at least input/output filenames
     std::cout << "Wrong usage!" << std::endl;
     std::cout << "example: nemsio2nc nemsiofile ncfile <deflate> <nbits>" << std::endl;
     return 1;
   } else {
     nemsio_file = argv[1];
     netcdf_file = argv[2];
     std::cout << "nemsio2nc - convert GFS NEMSIO file to GFS netCDF file" << std::endl;
     std::cout << "Input file: " << nemsio_file << std::endl;
     std::cout << "Output file: " << netcdf_file << std::endl;
     deflate = 0;
     nbits_out = 0;
     // check if we are using ZLib compression to write out
     if ( argc > 3 ) {
       deflate = atoi(argv[3]);
     }
     // if using ZLib, what about lossy 'quantize_data'?
     if ( argc > 4 ) {
       nbits_out = atoi(argv[4]);
     }
     if ( deflate > 0 ) {
       std::cout << "Deflate level =" << deflate << ". ZLib compression will be used." << std::endl;
       if ( nbits_out > 0 ) {
         std::cout << "nbits =" << nbits_out << ". Lossy compression will also be used." << std::endl;
       }
     }
     return 0;
   }
 }

 int Convert::run() {
   nems2nc::gfsnc ncfile;
   nems2nc::nemsio nemsiofile;
   // open the NEMSIO file and get header info (time, lat, ak, etc.)
   nemsiofile.open(nemsio_file);
   // create netCDF file with proper dimensions / metadata
   ncfile.create(netcdf_file, nemsiofile);
   // define output variables in netCDF file
   ncfile.def_vars(nemsiofile, deflate);
   // write variables to output file
   ncfile.write_vars(nemsiofile, nbits_out);
   // close output file
   ncfile.close();
   // TODO add error handling with return codes?
   return 0;
 }

 int Convert::cleanup() {
   return 0;
 }

}
