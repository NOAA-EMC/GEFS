#ifndef NEMSIO2NC_NEMSIO
#define NEMSIO2NC_NEMSIO
#include <string>
#include <map>
#include <vector>

namespace nems2nc {
 class nemsio {
   public:
     std::string filename;

     int nx, ny, nz, nrec, fhour;
     int idate[5];
     int ntrac;
     double* lat = NULL;
     double* lon = NULL;
     double* pfull = NULL;
     double* phalf = NULL;
     double* ak = NULL;
     double* bk = NULL;
     std::vector<std::string> recname;
     std::vector<std::string> reclevtype;
     std::vector<std::string> recfields;
     int* reclev = NULL;
     std::map<std::string, int> countRecs;

     int open(std::string filenamein);
     int read_rec(std::string recname,
                  std::string levtyp, int lev, double* data);

 };
}

#endif // NEMSIO2NC_NEMSIO
