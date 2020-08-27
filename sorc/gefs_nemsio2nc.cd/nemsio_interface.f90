module nemsio_interface_c
 use iso_c_binding
 use nemsio_module
 implicit none

 type(nemsio_gfile) :: nemsfile

contains
 subroutine nemsio_open_c(fname_len2, fname_in, fname_len) bind(c,name='nemsio_open_f90')
   integer(nemsio_intkind)         :: iret
   integer(c_int), intent(in   ), value :: fname_len, fname_len2
   character(len=1,kind=c_char), dimension(fname_len), intent(in    ) :: fname_in
   character(len=255) :: fname
   integer :: i
   ! convert str from c++ to f90
   do i=1,fname_len2
      fname(i:i) = fname_in(i)
   end do
   do i=fname_len2+1,255
      fname(i:i) = ' '
   end do
   call nemsio_init(iret)
   call nemsio_open(nemsfile, trim(fname), "read", iret=iret)
   if (iret /= 0) print *, 'NEMSIO F90 ERROR', iret
 end subroutine nemsio_open_c

 subroutine nemsio_get_header_c(idate_out, fhour, nx, ny, nz, nrec) bind(c,name='nemsio_get_header_f90')
   integer(c_int) :: fhour, nx, ny, nz, nrec
   integer(c_int), dimension(5), intent(out) :: idate_out
   integer(nemsio_intkind) :: iret
   integer(nemsio_intkind), dimension(7) :: idate
   integer(nemsio_intkind) :: nfday, nfhour, nfminute, dimx, dimy, dimz, nrecs
   call nemsio_getfilehead(nemsfile,iret=iret,idate=idate,nfday=nfday,nfhour=nfhour, &
                           nfminute=nfminute,dimx=dimx,dimy=dimy,dimz=dimz,nrec=nrecs)
   if (iret /= 0) print *, 'NEMSIO F90 ERROR', iret
   idate_out(1:5) = idate(1:5)
   nx = int(dimx,4)
   ny = int(dimy,4)
   nz = int(dimz,4)
   nrec = int(nrecs,4)
   fhour = int(nfhour,4)
 end subroutine nemsio_get_header_c

 subroutine nemsio_get_akbk_latlon_c(dimx, dimy, dimz, ak_out, bk_out, lat_out, lon_out,&
                                     pfull_out, phalf_out, ntrac_out) bind(c,name='nemsio_get_akbk_latlon_f90')
   integer(c_int) :: dimx, dimy, dimz
   real(c_double) :: lat_out(dimx*dimy), lon_out(dimx*dimy)
   real(c_double) :: ak_out(dimz+1), bk_out(dimz+1), pfull_out(dimz), phalf_out(dimz+1)
   integer(c_int) :: ntrac_out
   integer(nemsio_intkind) :: iret, ntrac
   integer :: k
   real(nemsio_realkind),dimension(:),allocatable :: lat, lon
   real(nemsio_realkind),dimension(:,:,:),allocatable :: vcoord
   real, allocatable, dimension(:) :: ak, bk, pfull, phalf
   allocate(lat(dimx*dimy),lon(dimx*dimy))
   call nemsio_getfilehead(nemsfile,iret=iret,lat=lat,lon=lon)
   if (iret /= 0) print *, 'NEMSIO F90 ERROR', iret
   lat_out = lat
   lon_out = lon
   allocate(vcoord(dimz+1,3,2))
   call nemsio_getfilehead(nemsfile,iret=iret,vcoord=vcoord, ntrac=ntrac)
   if (iret /= 0) print *, 'NEMSIO F90 ERROR', iret
   allocate(ak(dimz+1),bk(dimz+1),pfull(dimz),phalf(dimz+1))
   do k=1,dimz+1
     ak(dimz+2-k) = vcoord(k,1,1)
     bk(dimz+2-k) = vcoord(k,2,1)
   end do
   ak_out = ak
   bk_out = bk
   call get_eta_level(dimz, 100000., pfull, phalf, ak, bk, 0.01)
   pfull_out = pfull
   phalf_out = phalf
   ntrac_out = ntrac
 end subroutine nemsio_get_akbk_latlon_c

 subroutine nemsio_get_recinfo_c(nrecs, recname_out, reclevtyp_out, reclev_out) bind(c,name='nemsio_get_recinfo_f90')
   integer(c_int) :: nrecs
   character(10), allocatable, dimension(:) :: recname, reclevtyp
   integer, allocatable, dimension(:) :: reclev
   character(len=1,kind=c_char), dimension(nrecs*10) :: recname_out, reclevtyp_out
   character(len=10) :: tmprec, tmprec2
   integer(c_int), dimension(nrecs) :: reclev_out
   integer(nemsio_intkind) :: iret
   integer :: i,j,k
   allocate(recname(nrecs), reclevtyp(nrecs), reclev(nrecs))
   call nemsio_getfilehead(nemsfile,iret=iret,recname=recname,reclevtyp=reclevtyp,reclev=reclev)
   if (iret /= 0) print *, 'NEMSIO F90 ERROR', iret
   reclev_out = reclev
   ! convert str from c++ to f90
   j=1
   do i=1,nrecs
      tmprec = recname(i)
      tmprec2 = reclevtyp(i)
      do k=1,10
         recname_out(j) = tmprec(k:k)
         reclevtyp_out(j) = tmprec2(k:k)
         j = j+1
      end do
   end do
 end subroutine nemsio_get_recinfo_c

 subroutine nemsio_readrec_c(recname_in, reclevtyp_in, strlen1, strlen2, reclev_in, npts, values_out) bind(c,name='nemsio_readrec_f90')
   integer(c_int) :: reclev_in, npts, strlen1, strlen2
   real(c_double), dimension(npts) :: values_out
   character(len=1,kind=c_char), dimension(*) :: recname_in, reclevtyp_in
   character(len=10) :: recname, reclevtyp
   integer :: i, iret
   integer(nemsio_intkind) :: reclev
   real(nemsio_realkind), allocatable, dimension(:) :: tmp1d
   ! convert strings from C to F
   do i=1,strlen1
     recname(i:i) = recname_in(i)
   end do
   do i=strlen1+1, 10
     recname(i:i) = ' '
   end do
   do i=1,strlen2
     reclevtyp(i:i) = reclevtyp_in(i)
   end do
   do i=strlen2+1,10
     reclevtyp(i:i) = ' '
   end do
   reclev = reclev_in
   allocate(tmp1d(npts))
   !call nemsio_readrecvw34(nemsfile, trim(recname), trim(reclevtyp), lev=reclev, &
   !                        data=tmp1d(:), iret=iret)
   call nemsio_readrecvw34(nemsfile, trim(recname), trim(reclevtyp), lev=reclev, data=tmp1d(:), iret=iret)
   if (iret /= 0) print *, 'NEMSIO F90 ERROR', iret
   values_out(:) = tmp1d(:)
 end subroutine nemsio_readrec_c

 subroutine get_eta_level(npz, p_s, pf, ph, ak, bk, pscale)
  ! borrowed from FV3GFS
  integer, intent(in) :: npz
  real, intent(in)  :: p_s            !< unit: pascal
  real, intent(in)  :: ak(npz+1)
  real, intent(in)  :: bk(npz+1)
  real, intent(in), optional :: pscale
  real, intent(out) :: pf(npz)
  real, intent(out) :: ph(npz+1)
  integer k
  real, parameter :: kappa=287.05/1004.6
  ph(1) = ak(1)
  do k=2,npz+1
     ph(k) = ak(k) + bk(k)*p_s
  enddo
  if ( present(pscale) ) then
      do k=1,npz+1
         ph(k) = pscale*ph(k)
      enddo
  endif
  if( ak(1) > 1.E-8 ) then
     pf(1) = (ph(2) - ph(1)) / log(ph(2)/ph(1))
  else
     pf(1) = (ph(2) - ph(1)) * kappa/(kappa+1.)
  endif
  do k=2,npz
     pf(k) = (ph(k+1) - ph(k)) / log(ph(k+1)/ph(k))
  enddo
 end subroutine get_eta_level

end module nemsio_interface_c
