module sig2p
!$$$   module documentation block
!                .      .    .                                       .
! module:    sig2p
!   prgmmr: ota              org: np23                date: 2012-10-23
!
! abstract: contains routines for reading sigma and surface files and
!           interpolating to global latitude-longitude fields on
!           pressure levels necessary for the TC tracking.
!
! program history log:
!   2012-10-23      ota, initial version.
!
! subroutines included:
! --- public routines ---
!   init_sig2p          - initialize spherical transformation matrix 
!                         and horizontal interpolation options
!   oper_sig2p          - read sigma and surface (optional) file and
!                         interpolate to pressure level surfaces
!   destroy_sig2p       - free arrays used in this module
! --- following subroutines are not accessible from the outside ---
!   init_kgds           - initalize grid information for the
!                         horizontal interpolation
!   hint                - perform horizontal interpolation of scalar
!                         variable using ip library (bi-linear)
!   hintsp              - perform horizontal interpolation of scalar
!                         variable using ip library (spectral)
!   hintv               - perform horizontal interpolation of 2D
!                         vector using ip library (bi-linear)
!
! variable definitions:
!   def nllon           - number of output grids on 1 latitudinal
!                         circle (integer)
!   def nllat           - number of output grids on 1 meridional
!                         circle (integer)
!   def fsig_init       - flag to show if init_sig2p is successfully
!                         operated (logical)
!
! attributes:
!   language: f90
!   machine: 
!
!$$$ end documentation block

  use trig_vals,only: dtr,rd_over_g
  use sigio_module
  use sfcio_module
  implicit none

  private
  public :: init_sig2p, oper_sig2p, destroy_sig2p, nllon, nllat, fsig_init

  ! structures containing the data of sigma and surface files
  type(sigio_head),save :: shead
  type(sigio_data),save :: sdata
  type(sfcio_head),save :: sfchead
  type(sfcio_data),save :: sfcdata
  ! arrays used for spectral transformation
  real,allocatable :: eps(:),epstop(:),enn1(:),elonn1(:),eon(:),eontop(:)
  ! information of input and output grids
  integer,save :: kgdsi(200),kgdso(200)
  integer,save :: nllon,nllat,maxslev
  ! variables on model grid
  real,allocatable :: vor(:,:),ug(:,:),vg(:,:),gh(:,:),tv(:,:),lnpm(:,:)
  real,allocatable :: lnps(:),hs(:),pmsl(:)
  ! cliteria of maximum surface pressure to reduce computational cost
  real,parameter :: psclim(1)=1500e+2
  ! parameters used for temperature, height and MSLP underground extrapolation
  real,parameter :: gamma=6.5e-3
  real,parameter :: zshul=75.0
  real,parameter :: tvshul=290.66
  ! flag for init_sig2p operation
  logical,save :: fsig_init=.false.

  contains

subroutine init_sig2p(iunit,ifile,ddeg,minplev,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_sig2p
!   prgmmr: ota              org: np23                date: 2012-10-23
!
! abstract: initialize spherical transformation matrix and horizontal
!           interpolation options. This reads header information of
!           input sigma file, allocates all arrays used for the
!           spherical transformations, and initialize input/output
!           grid information for horizontal interpolation.
!
! program history log:
!   2012-10-23      ota - initial version
!
! input argument list:
!   iunit               - unit number used for reading sigma file (integer)
!   ifile               - name of input sigma file (character)
!   ddeg                - horizontal resolution of output lat-lon grid in
!                         degrees (real)
!   minplev             - minimum pressure level generated in oper_sig2p
!                         (Pa,real)
!
! output argument list:
!   iret                - return code (integer)
!
! attributes:
!   language: f90
!   machine: 
!
!$$$

  implicit none
  integer,intent(in) :: iunit
  character(*),intent(in) :: ifile
  real(8),intent(in) :: ddeg
  real(8),intent(in) :: minplev
  integer,intent(out) :: iret
  real,allocatable :: pm(:)
  integer :: k

  ! Read header information
  call sigio_sropen(iunit,ifile,iret)
  if(iret /= 0) return
  call sigio_srhead(iunit,shead,iret)
  if(iret /= 0) return
  call sigio_sclose(iunit,iret)
  if(iret /= 0) return

  ! First, deallocate all allocatable arrays
  call destroy_sig2p

  ! Necessary vertical model levels computed from maximum surface pressure
  ! within physically reasonable range
  allocate(pm(shead%levs))
  call sigio_modpr(1,1,shead%levs,shead%nvcoord,shead%idvc,shead%idsl, &
       & shead%vcoord,iret,ps=psclim,pm=pm)
  maxslev = 0
  do k=1,shead%levs-1
     if(pm(k) < minplev) then
        maxslev = k+1
        exit
     end if
  end do
  if(maxslev == 0) maxslev = shead%levs
  deallocate(pm)

  ! Preparation for the transformation (spectrum -> grid)
  allocate(eps((shead%jcap+1)*(shead%jcap+2)/2),epstop(shead%jcap+1))
  allocate(enn1((shead%jcap+1)*(shead%jcap+2)/2))
  allocate(elonn1((shead%jcap+1)*(shead%jcap+2)/2))
  allocate(eon((shead%jcap+1)*(shead%jcap+2)/2),eontop(shead%jcap+1))
  allocate(vor(shead%latf*shead%lonf,maxslev))
  allocate(ug(shead%latf*shead%lonf,maxslev))
  allocate(vg(shead%latf*shead%lonf,maxslev))
  allocate(gh(shead%latf*shead%lonf,maxslev))
  allocate(tv(shead%latf*shead%lonf,maxslev))
  allocate(hs(shead%latf*shead%lonf))
  allocate(pmsl(shead%latf*shead%lonf))
  allocate(lnpm(shead%latf*shead%lonf,maxslev))
  allocate(lnps(shead%latf*shead%lonf))
  call spwget(0,shead%jcap,eps,epstop,enn1,elonn1,eon,eontop)

  ! Preparation for horizontal interpolation
  call init_kgds(real(ddeg))
  fsig_init = .true.

  return
end subroutine init_sig2p

subroutine oper_sig2p(iunit,ifile,plev,iret,isfluxfile)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    oper_sig2p
!   prgmmr: ota              org: np23                date: 2012-10-23
!
! abstract: read sigma and surface (optional) file and interpolate to
!           pressure level surfaces. Only variables used in the TC
!           trcaker are retrieved. Spectral transformation is performed
!           using sp library. Geopotential height and MSLP are computed
!           by the formula used in global_nceppost. Vertical
!           interpolation is performed based on logP linear
!           interpolation. Extrapolation under the ground is based on
!           the algorithm in global_nceppost. Horizontal interpolation
!           is performed using ip library. (MSLP is smoothed by the
!           spectral interpolation with the truncation on 80 total wave
!           number in the original version.) 10 m wind is read from the
!           surace flux file when it is available. Otherwise, wind on
!           the lowest model level is used as 10 m wind. This only
!           affects the wind information of the TC track file (not used
!           in tracking algorithm).
!
! program history log:
!   2012-10-23      ota - initial version
!
! input argument list:
!   iunit               - unit number used for reading sigma and surface
!                         file (integer)
!   ifile               - name of input sigma file (character)
!   plev                - pressure levels of output grids in log(kPa) (real)
!   isfluxfile          - name of input surface flux file (character,optional)
!
! output argument list:
!   iret                - return code (integer)
!
! attributes:
!   language: f90
!   machine: 
!
!$$$

  use tracked_parms

  implicit none

  integer,intent(in) :: iunit
  character(*),intent(in) :: ifile
  real(8),intent(in) :: plev(:)   ! logp
  integer,intent(out) :: iret
  character(*),intent(in),optional :: isfluxfile
  real,allocatable :: vorp(:,:),ghp(:,:),up(:,:),vp(:,:)
  real,allocatable :: f10(:),field(:,:,:),field2(:,:,:)
  real :: al,gammas,tvd,tvsfc,tau
  logical,allocatable :: lb(:)
  integer :: jpdst(200),jgdst(200),kpdst(200),kgdst(200)
  integer :: i,k,kf,l,lm,lp,pnum,levsp

  ! Read sigma file
  call sigio_srohdc(iunit,ifile,shead,sdata,iret)
  if(iret /= 0) return

  ! Divergence and relative vorticity -> 2D wind vector
  call sptezm(0,shead%jcap,4,shead%lonf,shead%latf,maxslev, &
       & sdata%z(:,1:maxslev),vor,1)
  call sptezmv(0,shead%jcap,4,shead%lonf,shead%latf,maxslev, &
       & sdata%d(:,1:maxslev),sdata%z(:,1:maxslev),ug,vg,1)

  ! Compute full level pressure
  call sptez(0,shead%jcap,4,shead%lonf,shead%latf,sdata%ps,lnps,1)
  lnps(:) = exp(lnps(:)) * 1.e3
  call sptezm(0,shead%jcap,4,shead%lonf,shead%latf,maxslev, &
       & sdata%t(:,1:maxslev),tv,1)
  call sigio_modpr(shead%lonf*shead%latf,shead%lonf*shead%latf,maxslev, &
       & shead%nvcoord,shead%idvc,shead%idsl,shead%vcoord(1:maxslev+1,:), &
       & iret,ps=lnps,pm=lnpm)
  lnps(:) = log(lnps(:)*1.e-3)
  lnpm(:,:) = log(lnpm(:,:)*1.e-3)

  ! Compute geopotential height
  call sptez(0,shead%jcap,4,shead%lonf,shead%latf,sdata%hs,hs,1)
  gh(:,1) = hs(:) - rd_over_g * tv(:,1) * (lnpm(:,1) - lnps(:))
  do k=2,maxslev
     gh(:,k) = gh(:,k-1) - rd_over_g * 0.5 * (tv(:,k-1) + tv(:,k)) &
          & * (lnpm(:,k) - lnpm(:,k-1))
  end do

  ! Vertical interpolation (model -> p-surface)
  pnum = size(plev,1)
  levsp = maxslev+1
  allocate(vorp(shead%latf*shead%lonf,pnum))
  allocate(ghp(shead%latf*shead%lonf,pnum))
  allocate(up(shead%latf*shead%lonf,pnum+1))
  allocate(vp(shead%latf*shead%lonf,pnum+1))
  do k=1,pnum
!$omp parallel private(i,l,lm,lp,al,tvd,gammas)
!$omp do
     do i=1,shead%latf*shead%lonf
        ! Find model layer just below the pressure level
        lm = levsp
        do l=maxslev-1,1,-1
           if(lm==levsp .and. lnpm(i,l)>plev(k)) lm=l
        end do
        ! In case between surface and the lowest model layer
        if(lm==levsp .and. lnps(i)>plev(k)) lm=1
        ! Interpolation above ground
        if(lm/=levsp) then
           lp = lm+1
           al = (plev(k) - lnpm(i,lp)) / (lnpm(i,lm) - lnpm(i,lp))
           vorp(i,k) = al * vor(i,lm) + (1.0 - al) * vor(i,lp)
           ghp(i,k) = al * gh(i,lm) + (1.0 - al) * gh(i,lp)
           up(i,k) = al * ug(i,lm) + (1.0 - al) * ug(i,lp)
           vp(i,k) = al * vg(i,lm) + (1.0 - al) * vg(i,lp)
        else ! Underground
           ! Same as the lowest level for wind variables
           vorp(i,k) = vor(i,1)
           up(i,k) = ug(i,1)
           vp(i,k) = vg(i,1)
           ! Extrapolation for geopotential height by Shuell
           al = rd_over_g * (plev(k) - lnpm(i,1))
           if(gh(i,1) > zshul) then
              tvd = tv(i,1) + gamma * gh(i,1)
              if(tvd > tvshul) then
                 if(tv(i,1) > tvshul) then
                    tvd = tvshul - 5.e-3 * (tv(i,1) - tvshul) ** 2
                 else
                    tvd = tvshul
                 end if
              end if
              gammas = (tv(i,1) - tvd) / gh(i,1)
           else
              gammas = 0.0
           end if
           ghp(i,k) = gh(i,1) - tv(i,1) * al / (1.0 + 0.5*gammas*al)
        end if
     end do
!$omp end do
!$omp end parallel
  end do

  ! MSLP
!$omp parallel private(i,tvd,tvsfc,tau)
!$omp do
  do i=1,shead%latf*shead%lonf
     tvd = tv(i,1) + gamma * gh(i,1)
     tvsfc = tv(i,1) + gamma * (gh(i,1) - hs(i))
     if(tvd > tvshul) then
        if(tv(i,1) > tvshul) then
           tvd = tvshul - 5.e-3 * (tv(i,1) - tvshul) ** 2
        else
           tvd = tvshul
        end if
     end if
     tau = 0.5 * rd_over_g * (tvd + tvsfc)
     pmsl(i) = 1.e3 * exp(lnps(i) + hs(i) / tau)
  end do
!$omp end do
!$omp end parallel

  ! 10-m wind speed (for max wind speed)
  if(present(isfluxfile)) then ! Use sflux file if available
     call baopenr(iunit,isfluxfile,iret)
     if(iret /= 0) then
        up(:,pnum+1) = ug(:,1)
        vp(:,pnum+1) = vg(:,1)
     else
        allocate(f10(shead%lonf*shead%latf))
        allocate(lb(shead%lonf*shead%latf))
        jgdst(:)=-1
        jpdst(:)=-1
        jpdst(6)=105
        jpdst(7)=10
        jpdst(5)=33
        call getgb (iunit,0,shead%lonf*shead%latf,-1,jpdst,jgdst, &
             &      kf,k,kpdst,kgdst,lb,f10,iret)
        up(:,pnum+1) = f10(:)
        jpdst(5)=34
        call getgb (iunit,0,shead%lonf*shead%latf,-1,jpdst,jgdst, &
             &      kf,k,kpdst,kgdst,lb,f10,iret)
        vp(:,pnum+1) = f10(:)
        deallocate(f10,lb)
        call baclose(iunit,iret)
     end if
  else ! Use lowest level wind speed directly
     up(:,pnum+1) = ug(:,1)
     vp(:,pnum+1) = vg(:,1)
  end if

  ! Horizontal interpolation (Gaussian grid -> lat-lon grid)
  allocate(field(nllon,nllat,pnum+1))
  allocate(field2(nllon,nllat,pnum+1))
  call hint(pnum-1,vorp(1,1),field(:,:,1:pnum-1),iret)
  if(iret /= 0) then
     deallocate(vorp,ghp,up,vp,field,field2)
     return
  end if
  zeta = field(:,:,1:pnum-1)
  call hintv(pnum+1,up,vp,field,field2,iret)
  if(iret /= 0) then
     deallocate(vorp,ghp,up,vp,field,field2)
     return
  end if
  u = field
  v = field2
  call hint(pnum-1,ghp(1,1),field(:,:,1:pnum-1),iret)
  if(iret /= 0) then
     deallocate(vorp,ghp,up,vp,field,field2)
     return
  end if
  hgt = field(:,:,1:pnum-1)
!  call hintsp(1,80,pmsl,field(:,:,1),iret)   ! spectral smoothing for MSLP
  call hint(1,pmsl,field(:,:,1),iret)   ! no smoothing
  slp = field(:,:,1)

  deallocate(vorp,ghp,up,vp,field,field2)

  return
end subroutine oper_sig2p

subroutine init_kgds(ddeg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_kgds
!   prgmmr: ota              org: np23                date: 2012-10-23
!
! abstract: initialize grid information of input and output grids used
!           in horizontal interpolation routines.
!
! program history log:
!   2012-10-23      ota - initial version
!
! input argument list:
!   ddeg                - horizontal resolution of output lat-lon grid in
!                         degrees (real)
!
! output argument list:
!
! attributes:
!   language: f90
!   machine: 
!
!$$$

  implicit none
  real,intent(in) :: ddeg
  real,allocatable :: gaulat(:), wlat(:)

  nllon = nint(360.0 / ddeg)
  nllat = nint(180.0 / ddeg) + 1
  allocate(gaulat(shead%latf))
  allocate(wlat(shead%latf))
  call splat(4,shead%latf,gaulat,wlat)
  deallocate(wlat)
  gaulat(:) = asin(gaulat(:)) / dtr

  ! Input grid (Gaussian grid)
  kgdsi(1)=4                            ! 4:Gaussian grid
  kgdsi(2)=shead%lonf                   ! Points on latitude circle
  kgdsi(3)=shead%latf                   ! Points on longitude circle
  kgdsi(4)=nint(gaulat(1)*1000)         ! Latitude of origin (x1000)
  kgdsi(5)=0                            ! Longitude of origin (x1000)
  kgdsi(6)=0                            ! Resolution flag (octet 17)
  kgdsi(7)=nint(gaulat(shead%latf)*1000) ! Latitude of extreme (x1000)
  kgdsi(8)=nint(360.0*real(shead%lonf-1)/real(shead%lonf)*1000) ! Longitude of extreme (x1000)
  kgdsi(9)=0                            ! Latitudinal direction of increment
  kgdsi(10)=shead%latf/2                ! Number of circles pole to equator
  kgdsi(11)=0                           ! Scanning mode (octet 28)
  kgdsi(12)=0                           ! Number of vertical coordinate params
  kgdsi(13)=255                         ! 255: No PV nor PL
  kgdsi(20)=255                         

  ! Output grid (lat-lon grid)
  kgdso(1)=0                            ! 0:Lat-lon grid
  kgdso(2)=nllon                        ! Points on latitude circle
  kgdso(3)=nllat                        ! Points on longitude circle
  kgdso(4)=90000                        ! Latitude of origin (x1000)
  kgdso(5)=0                            ! Longitude of origin (x1000)
  kgdso(6)=0                            ! Resolution flag (octet 17)
  kgdso(7)=-90000                       ! Latitude of extreme (x1000)
  kgdso(8)=360000-nint(ddeg*1000)       ! Longitude of extreme (x1000)
  kgdso(9)=nint(ddeg*1000)              ! Latitudinal direction of increment
  kgdso(10)=nint(ddeg*1000)             ! Longitudinal direction of increment
  kgdso(11)=0                           ! Scanning mode (octet 28)
  kgdso(20)=255

  deallocate(gaulat)

  return
end subroutine init_kgds

subroutine hint(lnum,gdata,ldata,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    hint
!   prgmmr: ota              org: np23                date: 2012-10-23
!
! abstract: perform horizontal interpolation of scalar variables
!           from Gaussian grids to global latitude-longitude grids
!           using ip library. Bi-linear interpolation is used.
!
! program history log:
!   2012-10-23      ota - initial version
!
! input argument list:
!   lnum                - number of fields to be interpolated (integer)
!   gdata               - input fields on Gaussian grid (real)
!
! output argument list:
!   ldata               - output fields on lat-lon grid (real)
!   iret                - return code (integer)
!
! attributes:
!   language: f90
!   machine: 
!
!$$$

  implicit none
  integer,intent(in) :: lnum
  real,intent(in) :: gdata(shead%lonf*shead%latf,lnum)
  real,intent(out) :: ldata(nllon*nllat,lnum)
  integer,intent(out) :: iret
  integer :: ip,ipopt(20),mi,mo,ibi(lnum),ibo(lnum),no
  logical,allocatable :: li(:,:),lo(:,:)
  real,allocatable :: rlat(:),rlon(:)

  ip=0       ! bi-linear interpolation
  ibi(:)=0
  mi=shead%lonf*shead%latf
  mo=nllon*nllat
  allocate(li(mi,lnum))
  allocate(lo(mo,lnum))
  allocate(rlat(mo),rlon(mo))
  call ipolates(ip,ipopt,kgdsi,kgdso,mi,mo,lnum,ibi,li,gdata, &
       & no,rlat,rlon,ibo,lo,ldata,iret)
  deallocate(li,lo,rlat,rlon)

  return
end subroutine hint

subroutine hintsp(lnum,sptrunc,gdata,ldata,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    hintsp
!   prgmmr: ota              org: np23                date: 2012-10-23
!
! abstract: perform horizontal interpolation of scalar variables
!           from Gaussian grids to global latitude-longitude grids
!           using ip library. The field is spectraly smoohted first,
!           then interpolated with bilinear interpolation.
!
! program history log:
!   2012-10-23      ota - initial version
!
! input argument list:
!   lnum                - number of fields to be interpolated (integer)
!   sptrunc             - truncation wave number for interpolation
!                         (integer)
!   gdata               - input fields on Gaussian grid (real)
!
! output argument list:
!   ldata               - output fields on lat-lon grid (real)
!   iret                - return code (integer)
!
! attributes:
!   language: f90
!   machine: 
!
!$$$

  implicit none
  integer,intent(in) :: lnum
  integer,intent(in) :: sptrunc
  real,intent(in) :: gdata(shead%lonf*shead%latf,lnum)
  real,intent(out) :: ldata(nllon*nllat,lnum)
  integer,intent(out) :: iret
  integer :: ip,ipopt(20),mi,mo,ibi(lnum),ibo(lnum),no
  real :: gdata2(shead%lonf*shead%latf,lnum)
  logical,allocatable :: li(:,:),lo(:,:)
  real,allocatable :: rlat(:),rlon(:)

  ip=4       ! spectral interpolation
  ipopt(1)=0
  ipopt(2)=sptrunc
  ibi(:)=0
  mi=shead%lonf*shead%latf
  mo=shead%lonf*shead%latf
  allocate(li(mi,lnum))
  allocate(lo(mo,lnum))
  allocate(rlat(mo),rlon(mo))
  call ipolates(ip,ipopt,kgdsi,kgdsi,mi,mo,lnum,ibi,li,gdata, &
       & no,rlat,rlon,ibo,lo,gdata2,iret)
  deallocate(li,lo,rlat,rlon)
  call hint(lnum,gdata2,ldata,iret)

  return
end subroutine hintsp

subroutine hintv(lnum,udata,vdata,ludata,lvdata,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    hintv
!   prgmmr: ota              org: np23                date: 2012-10-23
!
! abstract: perform horizontal interpolation of 2D vectors
!           from Gaussian grids to global latitude-longitude grids
!           using ip library. Bi-linear interpolation is used.
!
! program history log:
!   2012-10-23      ota - initial version
!
! input argument list:
!   lnum                - number of fields to be interpolated (integer)
!   udata               - input U-components on Gaussian grid (real)
!   vdata               - input V-components on Gaussian grid (real)
!
! output argument list:
!   ludata              - output U-components on lat-lon grid (real)
!   lvdata              - output V-components on lat-lon grid (real)
!   iret                - return code (integer)
!
! attributes:
!   language: f90
!   machine: 
!
!$$$

  implicit none
  integer,intent(in) :: lnum
  real,intent(in) :: udata(shead%lonf*shead%latf,lnum)
  real,intent(in) :: vdata(shead%lonf*shead%latf,lnum)
  real,intent(out) :: ludata(nllon*nllat,lnum)
  real,intent(out) :: lvdata(nllon*nllat,lnum)
  integer,intent(out) :: iret
  integer :: ip,ipopt(20),mi,mo,ibi(lnum),ibo(lnum),no
  logical,allocatable :: li(:,:),lo(:,:)
  real,allocatable :: rlat(:),rlon(:),crot(:),srot(:)

  ip=0       ! bi-linear interpolation
  ibi(:)=0
  mi=shead%lonf*shead%latf
  mo=nllon*nllat
  allocate(li(mi,lnum))
  allocate(lo(mo,lnum))
  allocate(rlat(mo),rlon(mo))
  allocate(crot(mo),srot(mo))
  call ipolatev(ip,ipopt,kgdsi,kgdso,mi,mo,lnum,ibi,li,udata,vdata, &
       & no,rlat,rlon,crot,srot,ibo,lo,ludata,lvdata,iret)
  deallocate(li,lo,rlat,rlon,crot,srot)

  return
end subroutine hintv

subroutine destroy_sig2p
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_sig2p
!   prgmmr: ota              org: np23                date: 2012-10-23
!
! abstract: free memories for all allocatable variables used in this
!           module.
!
! program history log:
!   2012-10-23      ota - initial version
!
! input argument list:
!
! output argument list:
!
! attributes:
!   language: f90
!   machine: 
!
!$$$

  implicit none

  if(allocated(eps)) deallocate(eps)
  if(allocated(epstop)) deallocate(epstop)
  if(allocated(enn1)) deallocate(enn1)
  if(allocated(elonn1)) deallocate(elonn1)
  if(allocated(eon)) deallocate(eon)
  if(allocated(eontop)) deallocate(eontop)
  if(allocated(vor)) deallocate(vor)
  if(allocated(ug)) deallocate(ug)
  if(allocated(vg)) deallocate(vg)
  if(allocated(gh)) deallocate(gh)
  if(allocated(tv)) deallocate(tv)
  if(allocated(hs)) deallocate(hs)
  if(allocated(pmsl)) deallocate(pmsl)
  if(allocated(lnpm)) deallocate(lnpm)
  if(allocated(lnps)) deallocate(lnps)

  return
end subroutine destroy_sig2p

end module sig2p
