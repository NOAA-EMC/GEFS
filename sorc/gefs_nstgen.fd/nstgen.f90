  program rd_gfs_nc

!-----------------------------------------------------------------------------
! read GFS nc data.
! added: 2020/04/21, Wei Li
!
! compile: ifort -o rd_gfs_nc.exe -I${NETCDF}/include rd_gfs_nc.f90 -L${NETCDF}/lib -lnetcdff
! run:     rd_gfs_nc.exe filename_in
!-----------------------------------------------------------------------------
  use netcdf
  implicit none

!WWLL
  integer,     parameter :: iunit=51,ounit=61
  integer,     parameter :: maxgrd=4718592,grib_yt=1536,grib_xt=3072
  integer      index,mskp,kskp,j,jf,k,kg,kf,ihr,jhr,iret,kcent
  integer      kpds(25),kgds(22),jpds(25),jgds(22)
  real*4       tmpsfc_1D(maxgrd), var_save(grib_xt,grib_yt)

  character (len=2500) :: filename_in,filename_out
  integer              :: iargc, rcode, ncid, ndims, nvars, varid, n, i 
  character (len=150)  :: dimname
  character (len=10)  :: date

  integer              :: nx, ny, nz, nt
  real*8, allocatable,dimension(:,:) :: lat, lon
  real*8, allocatable,dimension(:) :: time
  real*8, allocatable,dimension(:,:,:,:) :: dat8d
  real*4, allocatable,dimension(:,:,:,:) :: dat4d
  logical(1)   lbms(maxgrd)

  type var_3d
       character (len=256)  :: longname, units, cell_methods, output_file
       real                 :: missing
       real, allocatable,dimension(:,:,:) :: dat
  endtype var_3d

  type(var_3d) :: tmpsfc

!------------------------------------------------------------
! --- 1.0 get argc 
  if (iargc() .lt. 1) then
     write(*,*)' usage: rd_gfs_nc.exe filename_in'
     stop
  else
     call getarg(1, filename_in)
  endif
  call getarg(2,filename_out)
  call getarg(3,date)
  write(*,'(a)')trim(filename_out)
  write(*,'(a)') date

!------------------------------------------------------------
! --- 2.0 get gfs data dimensions and initilization
  rcode=nf90_open(trim(filename_in), nf90_nowrite, ncid)
  if ( rcode /= nf90_noerr ) then
     write(*,'(a)')' !!! Failed to open '//trim(filename_in)
     stop
  endif

  nx=0; ny=0; nz=0; nt=0
  call check(nf90_inquire(ncid, ndims, nvars), 'nf90_inquire', 1)
  do n = 1, ndims
     call check(nf90_inquire_dimension(ncid,n,name=dimname, len=i), 'nf90_inquire_dimension', 1)
     if ( trim(dimname) == 'grid_xt') nx=i
     if ( trim(dimname) == 'grid_yt') ny=i
     if ( trim(dimname) == 'plevel')  nz=i
     if ( trim(dimname) == 'time')    nt=i
  enddo

!------------------------------------------------------------
! --- 3.0 get dimension's data
  !-----3.1, lat/lon
  allocate (dat8d(nx, ny,1,1))
  call get_var_data_dbl(filename_in, 'lat', nx, ny, 1, 1, dat8d)
  allocate (lat(nx,ny))
  lat = dat8d(:,:,1,1)
  call get_var_data_dbl(filename_in, 'lon', nx, ny, 1, 1, dat8d)
  allocate (lon(nx,ny))
  lon = dat8d(:,:,1,1)
  deallocate (dat8d)

  !-----3.2, time
  allocate (dat8d(nt,1,1,1))
  call get_var_data_dbl(filename_in, 'time', nt, 1,1,1, dat8d)
  allocate (time(nt))
  time = dat8d(:,1,1,1)
  deallocate (dat8d)

!------------------------------------------------------------
! --- 4.0 get var
  !-----4.1, get tmpsfc
  allocate (dat4d(nx,ny,nt,1))
  call get_var_data(filename_in, 'tmpsfc', nx, ny, nt, 1, dat4d)
  allocate(tmpsfc%dat(nx,ny,nt))
  tmpsfc%dat = dat4d(:,:,:,1)
  deallocate (dat4d)

  call check(nf90_inq_varid(ncid, 'tmpsfc', varid), 'tmpsfc nf90_inq_varid',0)
  call check(nf90_get_att(ncid, varid, 'long_name', tmpsfc%longname), 'tmpsfc long_name',0)
  call check(nf90_get_att(ncid, varid, 'units', tmpsfc%units), 'tmpsfc units',0)
  call check(nf90_get_att(ncid, varid, 'cell_methods', tmpsfc%cell_methods), 'tmpsfc cell_methods',0)
  call check(nf90_get_att(ncid, varid, 'output_file', tmpsfc%output_file), 'tmpsfc output_file',0)
  call check(nf90_get_att(ncid, varid, 'missing', tmpsfc%missing), 'tmpsfc missing',0)

  !---debug
  write(*,'(a,4i5)')'--- get '//trim(tmpsfc%longname)//' ('//trim(tmpsfc%units)//') at x,y,z,t dimensions:', nx, ny, nz, nt
  write(*,'(50f8.2)')tmpsfc%dat(100:104,100:104,1)
!  write(*,*)tmpsfc%dat !analysis
  var_save(:,:)=-99999.
  var_save(:,:)=tmpsfc%dat(:,:,1)

!------------------------------------------------------------
  call check(nf90_close(ncid), 'nf90_close', 0)
  deallocate (lat)
  deallocate (lon)
  deallocate (time)
  deallocate (tmpsfc%dat)

!wite to grib
   index = 0
   mskp =  0   !messages to skip
   jpds = -1
   jgds = -1

   iret = 0
   call baopenr(iunit,'/gpfs/hps3/emc/ensemble/noscrub/Wei.Li/FV3_SST/outdata/gfs.t00z.Tsfc.20170101.grb',iret)
   if(iret.ne.0) then; print *,'error opening input unit';endif

   call getgbh(iunit,index,mskp,jpds,jgds,kg,kf,k,kpds,kgds,iret)
   if(iret.ne.0) then; print *,'error get header=',iunit;stop;endif
   read(date(3:4),*)kpds(8) !2 digit year
   read(date(5:6),*)kpds(9) !month
   read(date(7:8),*)kpds(10) !day
   read(date(1:2),*) kcent !century
   kpds(21)=kcent+1
    
   print *, kpds
!
   iret = 0
   call baopenw(ounit,trim(filename_out),iret)
   if(iret.ne.0) then; print *,'error opening output unit';endif

   tmpsfc_1D=reshape(var_save,(/maxgrd/))
   call putgb(ounit,maxgrd,kpds,kgds,lbms,tmpsfc_1D,iret)

   call baclose(iunit,iret)
   if(iret.ne.0) then; print *,'error closing file=',iunit;stop;endif
   call baclose(ounit,iret)
   if(iret.ne.0) then; print *,'error closing file=',ounit;stop;endif

  end program

!========================================================================================
  subroutine check(istatus, printinfo, nstop)

  use netcdf
  implicit none
  integer, intent(in) :: istatus, nstop
  character (len=*), intent(in) :: printinfo

  if (istatus /= nf90_noerr) then 
     write(*,*) trim(adjustl(nf90_strerror(istatus)))//' --- '//trim(printinfo)
     if ( nstop > 0 ) stop
  endif   
  end subroutine check

!========================================================================================
  subroutine get_var_data (ncfile, var, ix, jx, kx, tx, data)

  use netcdf
  implicit none
  character(len=*), intent(in)    :: ncfile
  character(len=*), intent(in)    :: var
  integer, intent(in)             :: ix, jx, kx, tx
  real, dimension(ix, jx, kx, tx) :: data
  integer                         :: ncid, varid

  call check(nf90_open(trim(ncfile), nf90_nowrite, ncid), 'nf90_open'//trim(ncfile), 1)
  call check(nf90_inq_varid(ncid, trim(var), varid), 'nf90_inq_varid'//trim(var), 0)
  call check(nf90_get_var(ncid, varid, data), 'nf90_get_var'//trim(var), 0)

  return
  end subroutine get_var_data

!========================================================================================
  subroutine get_var_data_dbl (ncfile, var, ix, jx, kx, tx, data)

  use netcdf
  implicit none
  character(len=*), intent(in)    :: ncfile
  character(len=*), intent(in)    :: var
  integer, intent(in)             :: ix, jx, kx, tx
  real*8, dimension(ix, jx, kx, tx) :: data
  integer                         :: ncid, varid

  call check(nf90_open(trim(ncfile), nf90_nowrite, ncid), 'nf90_open'//trim(ncfile), 1)
  call check(nf90_inq_varid(ncid, trim(var), varid), 'nf90_inq_varid'//trim(var), 0)
  call check(nf90_get_var(ncid, varid, data), 'nf90_get_var'//trim(var), 0)

  return
  end subroutine get_var_data_dbl
!========================================================================================
