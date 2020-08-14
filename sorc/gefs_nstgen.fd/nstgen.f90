  program rd_gfs_nc

!-----------------------------------------------------------------------------
! read GFS nc data.
! added: 2020/04/21, Wei Li
!-----------------------------------------------------------------------------
  use netcdf
  implicit none

  integer,     parameter :: iunit=51,ounit=61
  integer,     parameter :: maxgrd=4718592,grib_yt=1536,grib_xt=3072
  integer      kskp,j,jf,k,kg,kf,ihr,jhr,iret
  integer      kpds(25),kgds(22)
  real*4       tref_1D(maxgrd), var_save(grib_xt,grib_yt)

  character (len=2500) :: filename_in,filename_out
  integer              :: iargc, rcode, ncid, ndims, nvars, varid, n, i 
  character (len=150)  :: dimname,time_unit
  character (len=4)  :: iyear
  character (len=2)  :: imon,iday,ihour,imin

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

  type(var_3d) :: tref

!------------------------------------------------------------
! --- 1.0 get argc 
  if (iargc() .lt. 1) then
     write(*,*)' usage: rd_gfs_nc.exe filename_in'
     stop
  else
     call getarg(1, filename_in)
  endif
  call getarg(2,filename_out)

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
  print *, '      kgds(4)     kgds(7)     kgds(8)    kgds(9)' 
  print *, NINT(lat(1,1)*1000), NINT(lat(1,ny)*1000), NINT(lon(2,1)*1000), -1*NINT(lon(2,1)*1000)

  !-----3.2, time
  allocate (dat8d(nt,1,1,1))
  call get_var_data_dbl(filename_in, 'time', nt, 1,1,1, dat8d)
  allocate (time(nt))
  time = dat8d(:,1,1,1)
  deallocate (dat8d)
  call check(nf90_inq_varid(ncid, 'time', varid), 'time nf90_inq_varid',0)
  call check(nf90_get_att(ncid, varid, 'units', time_unit), 'time units',0)
  iyear  = time_unit(13:16)
  imon   = time_unit(18:19)
  iday   = time_unit(21:22)
  ihour  = time_unit(24:25)
  imin   = time_unit(27:28)
  print *, 'time_unit:',trim(time_unit), '    date:',iyear,imon,iday,ihour,imin

!------------------------------------------------------------
! --- 4.0 get var
  !-----4.1, get tref
  allocate (dat4d(nx,ny,nt,1))
  call get_var_data(filename_in, 'tref', nx, ny, nt, 1, dat4d)
  allocate(tref%dat(nx,ny,nt))
  tref%dat = dat4d(:,:,:,1)
  deallocate (dat4d)

  call check(nf90_inq_varid(ncid, 'tref', varid), 'tref nf90_inq_varid',0)
  call check(nf90_get_att(ncid, varid, 'long_name', tref%longname), 'tref long_name',0)
  call check(nf90_get_att(ncid, varid, 'units', tref%units), 'tref units',0)
  call check(nf90_get_att(ncid, varid, 'cell_methods', tref%cell_methods), 'tref cell_methods',0)
  call check(nf90_get_att(ncid, varid, 'output_file', tref%output_file), 'tref output_file',0)
  call check(nf90_get_att(ncid, varid, 'missing', tref%missing), 'tref missing',0)

  !---debug
  write(*,'(a,4i5)')'--- get '//trim(tref%longname)//' ('//trim(tref%units)//') at x,y,z,t dimensions:', nx, ny, nz, nt
  write(*,'(50f8.2)')tref%dat(100:104,100:104,1)
  var_save(:,:)=-99999.
  var_save(:,:)=tref%dat(:,:,1)

!------------------------------------------------------------
  call check(nf90_close(ncid), 'nf90_close', 0)
  deallocate (lat)
  deallocate (lon)
  deallocate (time)
  deallocate (tref%dat)

! --- 5.0 wite to grib
  ! --- defind header
   kpds(1) = 7                 !ID OF CENTER
   kpds(2) = 82                !GENERATING PROCESS ID NUMBER
   kpds(3) = 255               !GRID DEFINITION
   kpds(4) = 128               !GDS/BMS FLAG (RIGHT ADJ COPY OF OCTET 8)
   kpds(5) = 11                !INDICATOR OF PARAMETER
   kpds(6) = 1                 !TYPE OF LEVEL
   kpds(7) = 0                 !HEIGHT/PRESSURE , ETC OF LEVEL
   read(iyear(3:4),*) kpds(8)  !2 digit year YEAR INCLUDING (CENTURY-1)
   read(imon,*) kpds(9)        !MONTH OF YEAR 
   read(iday,*) kpds(10)       !DAY OF MONTH
   read(ihour,*) kpds(11)      !HOUR OF DAY 
   read(imin,*) kpds(12)       !MINUTE OF HOUR 
!   kpds(11:12) = 0             !HOUR OF DAY; MINUTE OF HOUR
   kpds(13) = 1                !INDICATOR OF FORECAST TIME UNIT
   kpds(14:15) = 0             !TIME RANGE 1; TIME RANGE 2
   kpds(16) = 10               !TIME RANGE FLAG
   kpds(17:18) = 0             !NUMBER INCLUDED IN AVERAGE; VERSION NR OF GRIB SPECIFICATION
   kpds(19) = 2                !VERSION NR OF PARAMETER TABLE
   kpds(20) = 0                !NR MISSING FROM AVERAGE/ACCUMULATION
   read(iyear(1:2),*) kpds(21) !CENTURY OF REFERENCE TIME OF DATA
   kpds(21)=kpds(21)+1
   kpds(22) = 3                !UNITS DECIMAL SCALE FACTOR
   kpds(23:25) = 0             !SUBCENTER NUMBER;PDS BYTE 29, FOR NMC ENSEMBLE PRODUCTS; PDS BYTE 30, NOT USED

   kgds(1) = 4                 !DATA REPRESENTATION TYPE
   kgds(2) = nx                !N(I) NR POINTS ON LATITUDE CIRCLE
   kgds(3) = ny                !N(J) NR POINTS ON LONGITUDE MERIDIAN 
   kgds(4) = 89909             !LA(1) LATITUDE OF ORIGIN 
   kgds(5) = 0                 !LO(1) LONGITUDE OF ORIGIN
   kgds(6) = 128               !RESOLUTION FLAG (RIGHT ADJ COPY OF OCTET 17)
   kgds(7) = -89909            !LA(2) LATITUDE OF EXTREME POINT 
   kgds(8) = -117              !LO(2) LONGITUDE OF EXTREME POINT
   kgds(9) = 117               !DI LONGITUDINAL DIRECTION OF INCREMENT
   kgds(10) = 768              !DJ LATITUDINAL DIRECTION INCREMENT
   kgds(11:19) = 0             !SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28); gds19=NUMBER OF VERTICAL COORDINATE PARAMETERS
   kgds(20) = 255              !OCTET NUMBER OF THE LIST OF VERTICAL COORDINATE PARAMETERS or OCTET NUMBER OF THE LIST OF NUMBERS OF POINTS IN EACH ROW OR 255 IF NEITHER ARE PRESENT
   kgds(21:22) = 0             !FOR GRIDS WITH PL, NUMBER OF POINTS IN GRID; NUMBER OF WORDS IN EACH ROW
 
   iret = 0
   call baopenw(ounit,trim(filename_out),iret)
   if(iret.ne.0) then; print *,'error opening output unit';endif

   tref_1D=reshape(var_save,(/maxgrd/))
   call putgb(ounit,maxgrd,kpds,kgds,lbms,tref_1D,iret)

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
