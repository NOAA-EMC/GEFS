#ifndef __NEMSIO_INTERFACE_H__
#define __NEMSIO_INTERFACE_H__

#ifdef __cplusplus
extern "C" {
#endif

void nemsio_open_f90(int fname_len, char* fname);

void nemsio_get_header_f90(int idate[5], int & fhour,
                           int & nx, int & ny, int & nz, int & nrec);

void nemsio_get_akbk_latlon_f90(int & nx, int & ny, int & nz, double* ak, double* bk, double* lat,
                                double* lon, double* pfull, double* phalf, int & ntrac);

void nemsio_get_recinfo_f90(int & nrec, char* recname, char* reclevtyp, int* reclev);

void nemsio_readrec_f90(const char* recname, const char* levtyp, int & strlen1,
                        int & strlen2, int & lev, int & npts, double* data);

#ifdef __cplusplus
}
#endif

#endif
