#ifndef NEMSIO2NC_CONVERT
#define NEMSIO2NC_CONVERT
#include <string>

namespace nems2nc {
 class Convert {
   public:

     std::string nemsio_file;
     std::string netcdf_file;
     int deflate;
     int nbits_out;

     int init(int argc, char** argv);
     int run();
     int cleanup();
 };
}

#endif // NEMSIO2NC_CONVERT
