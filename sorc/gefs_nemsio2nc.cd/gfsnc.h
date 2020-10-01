#ifndef NEMSIO2NC_GFSNC
#define NEMSIO2NC_GFSNC
#include <string>
#include <vector>
#include "nemsio.h"

namespace nems2nc {
 class gfsnc {
   public:
     std::string filename;
     int ncid, xdimid, ydimid, zdimid, zidimid, tdimid;
     std::vector<std::string> vars2d, vars3d;

     int create(std::string filenamein, nems2nc::nemsio nemsfile);

     int def_vars(nems2nc::nemsio nemsfile, int deflate);

     int write_vars(nems2nc::nemsio nemsfile, int quantize);

     int close();

 };
}

#endif // NEMSIO2NC_GFSNC
