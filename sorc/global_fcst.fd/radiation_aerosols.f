!!!!!  ==========================================================  !!!!!
!!!!!            'module_radiation_aerosols' description           !!!!!
!!!!!  ==========================================================  !!!!!
!                                                                      !
!   this module contains different atmospheric aerosols schemes for    !
!   radiation computations.                                            !
!                                                                      !
!   in the module, the externally callable subroutines are :           !
!                                                                      !
!      'aerinit'    -- initialization, input aerosol data, etc.        !
!         inputs:                                                      !
!           (iyear, imon, IAER, me)                                    !
!         outputs:                                                     !
!           (none)                                                     !
!                                                                      !
!      'setaer'     -- mapping aeros profile, compute aeros opticals   !
!         inputs:                                                      !
!           (xlon,xlat,prsi,prsl,tlay,qlay,rhlay,                      !
!            IMAX,NLAY,NLP1,iflip,lsswr,lslwr)                         !
!         outputs:                                                     !
!           (aerosw,aerolw)                                            !
!                                                                      !
!                                                                      !
!   internal subroutine called:                                        !
!       clim_aerinit, setclimaer   - for opac climatological aerosols  !
!                                                                      !
!       gocart_init,  setgocart    - for gocart aerosols               !
!                                                                      !
!                                                                      !
!   external modules referenced:                                       !
!                                                                      !
!       'module machine'                 in 'machine.f'                !
!       'module physcons'                in 'physcons.f'               !
!       'module module_radsw_parameters' in 'radsw_xxxx#_param.f'      !
!       'module module_radlw_parameters' in 'radlw_xxxx#_param.f'      !
!       'module module_radlw_cntr_para'  in 'radsw_xxxx#_param.f'      !
!                                                                      !
!   output variable definitions:                                       !
!       aerosw(IMAX,NLAY,NBDSW,1) - aerosols optical depth for sw      !
!       aerosw(IMAX,NLAY,NBDSW,2) - aerosols single scat albedo for sw !
!       aerosw(IMAX,NLAY,NBDSW,3) - aerosols asymmetry parameter for sw!
!                                                                      !
!       aerolw(IMAX,NLAY,NBDLW,1) - aerosols optical depth for lw      !
!       aerolw(IMAX,NLAY,NBDLW,2) - aerosols single scattering albedo  !
!       aerolw(IMAX,NLAY,NBDLW,3) - aerosols asymetry parameter        !
!                                                                      !
!                                                                      !
!   program history:                                                   !
!     apr     2003  ---  y.-t. hou     created                         !
!     nov 04, 2003  ---  y.-t. hou     modified version                !
!     apr 15, 2005  ---  y.-t. hou     modified module structure       !
!     jul     2006  ---  y.-t. hou     add volcanic forcing            !
!     feb     2007  ---  y.-t. hou     add generalized spectral band   !
!                   interpolation for sw aerosol optical properties    !
!     mar     2007  ---  y.-t. hou     add generalized spectral band   !
!                   interpolation for lw aerosol optical properties    !
!                                                                      !
!                                                                      !
!   references for opac climatological aerosols:                       !
!     hou et al. 2002  (ncep office note 441)                          !
!     hess et al. 1998 - bams v79 831-844                              !
!                                                                      !
!   references for glcart interactive aerosols:                        !
!                                                                      !
!                                                                      !
!   references for stratosperic volcanical aerosols:                   !
!     sato et al. 1993 - jgr, v98, d12, 22987-22994                    !
!                                                                      !
!                                                                      !
!!!!!  ==========================================================  !!!!!
!!!!!                       end descriptions                       !!!!!
!!!!!  ==========================================================  !!!!!



!========================================!
      module module_radiation_aerosols   !
!........................................!
!
      use machine,      only : kind_io8, kind_phys
      use physcons,     only : con_pi, con_rd, con_fvirt, con_g,        &
     &                         con_t0c, con_c, con_boltz, con_plnk

      use module_iounitdef,        only : NIAERCM
      use module_radsw_parameters, only : NBDSW, NSWSTR, wvnum1, wvnum2
      use module_radlw_parameters, only : NBDLW, wvnlw1, wvnlw2
      use module_radlw_cntr_para,  only : iaerlw
!
      implicit   none
!
      private

!  ---  general use parameter constants:
      integer, parameter, public :: NF_AESW = 3     ! num of output fields for sw rad
      integer, parameter, public :: NF_AELW = 3     ! num of output fields for lw rad

      real (kind=kind_phys), parameter :: f_zero = 0.0
      real (kind=kind_phys), parameter :: f_one  = 1.0

!  ---  module control parameters set in subroutine "aerinit"
      integer, save :: iaerflg = 1       ! choice of tropospheric aerosol schemes
                                         ! =0 no aer; =1 opac; =2 gocart
      logical, save :: lalwflg = .false. ! lw aerosols effect
                                         ! =t compute lw aerosol optical prop
      logical, save :: laswflg = .false. ! sw aerosols effect
                                         ! =t compute sw aerosol optical prop
      integer, save :: NBDIR   = NBDLW   ! num of actual bands for lw aerosols
                                         ! calculated according to iaerlw setting
      integer, save :: NBDSWLW = NBDSW+NBDLW
                                         ! total num of bands for sw+lw aerosols
      logical, save :: lvolflg = .false. ! volcanic forcing
                                         ! =t include stratos volcanic forcing

! --------------------------------------------------------------------- !
!   section-1 : module variables for spectral band interpolation        !
!               similar to gfdl-sw treatment (2000 version)             !
! --------------------------------------------------------------------- !

!  ---  parameter constants:
      integer, parameter, public :: NWVSOL  = 151   ! num of wvnum regions where solar
                                                    ! flux is constant
      integer, parameter, public :: NWVTOT  = 57600 ! total num of wvnum included
      integer, parameter, public :: NWVTIR  = 4000  ! total num of wvnum in ir range

!  ---  number of wavenumbers in each region where the solar flux is constant
      integer, dimension(NWVSOL) :: nwvns0

      data nwvns0   / 100,  11,  14,  18,  24,  33,  50,  83,  12,  12, &
     &  13,  15,  15,  17,  18,  20,  21,  24,  26,  30,  32,  37,  42, &
     &  47,  55,  64,  76,  91, 111, 139, 179, 238, 333,  41,  42,  45, &
     &  46,  48,  51,  53,  55,  58,  61,  64,  68,  71,  75,  79,  84, &
     &  89,  95, 101, 107, 115, 123, 133, 142, 154, 167, 181, 197, 217, &
     & 238, 263, 293, 326, 368, 417, 476, 549, 641, 758, 909, 101, 103, &
     & 105, 108, 109, 112, 115, 117, 119, 122, 125, 128, 130, 134, 137, &
     & 140, 143, 147, 151, 154, 158, 163, 166, 171, 175, 181, 185, 190, &
     & 196, 201, 207, 213, 219, 227, 233, 240, 248, 256, 264, 274, 282, &
     & 292, 303, 313, 325, 337, 349, 363, 377, 392, 408, 425, 444, 462, &
     & 483, 505, 529, 554, 580, 610, 641, 675, 711, 751, 793, 841, 891, &
     & 947,1008,1075,1150,1231,1323,1425,1538,1667,1633,14300 /

!  ---  solar flux (w/m**2) in each wvnumb region where it is constant
      real (kind=kind_phys), dimension(NWVSOL) :: s0intv

      data  s0intv(  1: 50)       /                                     &
     &     1.60000E-6, 2.88000E-5, 3.60000E-5, 4.59200E-5, 6.13200E-5,  &
     &     8.55000E-5, 1.28600E-4, 2.16000E-4, 2.90580E-4, 3.10184E-4,  &
     &     3.34152E-4, 3.58722E-4, 3.88050E-4, 4.20000E-4, 4.57056E-4,  &
     &     4.96892E-4, 5.45160E-4, 6.00600E-4, 6.53600E-4, 7.25040E-4,  &
     &     7.98660E-4, 9.11200E-4, 1.03680E-3, 1.18440E-3, 1.36682E-3,  &
     &     1.57560E-3, 1.87440E-3, 2.25500E-3, 2.74500E-3, 3.39840E-3,  &
     &     4.34000E-3, 5.75400E-3, 7.74000E-3, 9.53050E-3, 9.90192E-3,  &
     &     1.02874E-2, 1.06803E-2, 1.11366E-2, 1.15830E-2, 1.21088E-2,  &
     &     1.26420E-2, 1.32250E-2, 1.38088E-2, 1.44612E-2, 1.51164E-2,  &
     &     1.58878E-2, 1.66500E-2, 1.75140E-2, 1.84450E-2, 1.94106E-2 /
      data  s0intv( 51:100)       /                                     &
     &     2.04864E-2, 2.17248E-2, 2.30640E-2, 2.44470E-2, 2.59840E-2,  &
     &     2.75940E-2, 2.94138E-2, 3.13950E-2, 3.34800E-2, 3.57696E-2,  &
     &     3.84054E-2, 4.13490E-2, 4.46880E-2, 4.82220E-2, 5.22918E-2,  &
     &     5.70078E-2, 6.19888E-2, 6.54720E-2, 6.69060E-2, 6.81226E-2,  &
     &     6.97788E-2, 7.12668E-2, 7.27100E-2, 7.31610E-2, 7.33471E-2,  &
     &     7.34814E-2, 7.34717E-2, 7.35072E-2, 7.34939E-2, 7.35202E-2,  &
     &     7.33249E-2, 7.31713E-2, 7.35462E-2, 7.36920E-2, 7.23677E-2,  &
     &     7.25023E-2, 7.24258E-2, 7.20766E-2, 7.18284E-2, 7.32757E-2,  &
     &     7.31645E-2, 7.33277E-2, 7.36128E-2, 7.33752E-2, 7.28965E-2,  &
     &     7.24924E-2, 7.23307E-2, 7.21050E-2, 7.12620E-2, 7.10903E-2 /
      data  s0intv(101:151)       /                        7.12714E-2,  &
     &     7.08012E-2, 7.03752E-2, 7.00350E-2, 6.98639E-2, 6.90690E-2,  &
     &     6.87621E-2, 6.52080E-2, 6.65184E-2, 6.60038E-2, 6.47615E-2,  &
     &     6.44831E-2, 6.37206E-2, 6.24102E-2, 6.18698E-2, 6.06320E-2,  &
     &     5.83498E-2, 5.67028E-2, 5.51232E-2, 5.48645E-2, 5.12340E-2,  &
     &     4.85581E-2, 4.85010E-2, 4.79220E-2, 4.44058E-2, 4.48718E-2,  &
     &     4.29373E-2, 4.15242E-2, 3.81744E-2, 3.16342E-2, 2.99615E-2,  &
     &     2.92740E-2, 2.67484E-2, 1.76904E-2, 1.40049E-2, 1.46224E-2,  &
     &     1.39993E-2, 1.19574E-2, 1.06386E-2, 1.00980E-2, 8.63808E-3,  &
     &     6.52736E-3, 4.99410E-3, 4.39350E-3, 2.21676E-3, 1.33812E-3,  &
     &     1.12320E-3, 5.59000E-4, 3.60000E-4, 2.98080E-4, 7.46294E-5  /

! --------------------------------------------------------------------- !
!   section-2 : module variables for stratospheric volcanic aerosols    !
!               from historical data (sato et al. 1993)                 !
! --------------------------------------------------------------------- !

!  ---  parameter constants:
      integer, parameter :: MINVYR = 1850    ! lower lim (year) data available
      integer, parameter :: MAXVYR = 1999    ! upper lim (year) data available

!  ---  monthly, 45-deg lat-zone aerosols data set in subroutine 'aerinit'
      integer, allocatable :: ivolae(:,:,:)

!  ---  static control variables:
      integer, save :: kyrstr  = 0
      integer, save :: kyrend  = 0
      integer, save :: kyrsav  = 0
      integer, save :: kmonsav = 0

! --------------------------------------------------------------------- !
!   section-3 : module variables for opac climatological aerosols       !
!               optical properties (hess et al. 1989)                   !
! --------------------------------------------------------------------- !

!  ---  parameters and constants:
      integer, parameter :: NXC = 5    ! num of max componets in a profile
      integer, parameter :: NAE = 7    ! num of aerosols profile structures
      integer, parameter :: NDM = 5    ! num of atmos aerosols domains
      integer, parameter :: IMXAE = 72 ! num of lon-points in glb aeros data set
      integer, parameter :: JMXAE = 37 ! num of lat-points in glb aeros data set
      integer, parameter :: NAERBND=61 ! num of bands for clim aer data (opac)
      integer, parameter :: NRHLEV =8  ! num of rh levels for rh-dep components
      integer, parameter :: NCM1 = 6   ! num of rh independent aeros species
      integer, parameter :: NCM2 = 4   ! num of rh dependent aeros species
      integer, parameter :: NCM  = NCM1+NCM2

      real (kind=kind_phys), dimension(NRHLEV) :: rhlev
      data  rhlev (:) / 0.0, 0.5, 0.7, 0.8, 0.9, 0.95, 0.98, 0.99 /

!  ---  the following arrays are for climatological data that are
!           allocated and read in subroutine 'clim_aerinit'.
!   - spectral band structure:
!      iendwv(NAERBND)      - ending wavenumber (cm**-1) for each band
!   - relativ humidity independent aerosol optical properties:
!      species : insoluble        (inso); soot             (soot);
!                mineral nuc mode (minm); mineral acc mode (miam);
!                mineral coa mode (micm); mineral transport(mitr).
!      rhidext0(NAERBND,NCM1) - extinction coefficient
!      rhidsca0(NAERBND,NCM1) - scattering coefficient
!      rhidssa0(NAERBND,NCM1) - single scattering albedo
!      rhidasy0(NAERBND,NCM1) - asymmetry parameter
!   - relative humidity dependent aerosol optical properties:
!      species : water soluble    (waso); sea salt acc mode(ssam);
!                sea salt coa mode(sscm); sulfate droplets (suso).
!      rh level: 00%, 50%, 70%, 80%, 90%, 95%, 98%, 99%
!      rhdpext0(NAERBND,NRHLEV,NCM2) - extinction coefficient
!      rhdpsca0(NAERBND,NRHLEV,NCM2) - scattering coefficient
!      rhdpssa0(NAERBND,NRHLEV,NCM2) - single scattering albedo
!      rhdpasy0(NAERBND,NRHLEV,NCM2) - asymmetry parameter
!   - stratospheric background aerosol optical properties:
!      straext0(NAERBND)             - extingction coefficients
!   - global aerosol distribution:
!      haer(NDM,NAE)    - scale height of aerosols (km)
!      prsref(NDM,NAE)  - ref pressure lev (sfc to toa) in mb (100Pa)

      integer,               allocatable, dimension(:)    :: iendwv
      real (kind=kind_phys), allocatable, dimension(:,:)  :: rhidext0,  &
     &       rhidsca0, rhidssa0, rhidasy0
      real (kind=kind_phys), allocatable, dimension(:,:,:):: rhdpext0,  &
     &       rhdpsca0, rhdpssa0, rhdpasy0
      real (kind=kind_phys), allocatable, dimension(:)    :: straext0
      real (kind=kind_phys), allocatable, save, dimension(:,:) :: haer
      real (kind=kind_phys), allocatable, save, dimension(:,:) :: prsref

!  ---  the following arrays are allocate and setup in subr 'clim_aerinit'
!   - for relative humidity independent aerosol optical properties:
!      species : insoluble        (inso); soot             (soot);
!                mineral nuc mode (minm); mineral acc mode (miam);
!                mineral coa mode (micm); mineral transport(mitr).
!      extrhi(NCM1,NBDSWLW) - extinction coefficient for sw+lw spectral band
!      scarhi(NCM1,NBDSWLW) - scattering coefficient for sw+lw spectral band
!      ssarhi(NCM1,NBDSWLW) - single scattering albedo for sw+lw spectral band
!      asyrhi(NCM1,NBDSWLW) - asymmetry parameter for sw+lw spectral band
!   - for relative humidity dependent aerosol optical properties:
!      species : water soluble    (waso); sea salt acc mode(ssam);
!                sea salt coa mode(sscm); sulfate droplets (suso).
!      rh level: 00%, 50%, 70%, 80%, 90%, 95%, 98%, 99%
!      extrhd(NRHLEV,NCM2,NBDSWLW) - extinction coefficient for sw+lw band
!      scarhd(NRHLEV,NCM2,NBDSWLW) - scattering coefficient for sw+lw band
!      ssarhd(NRHLEV,NCM2,NBDSWLW) - single scattering albedo for sw+lw band
!      asyrhd(NRHLEV,NCM2,NBDSWLW) - asymmetry parameter for sw+lw band
!   - for stratospheric aerosols optical properties:
!      extstra(NBDSWLW)            - extinction coefficient for sw+lw band
!   - for topospheric aerosol profile distibution:
!      kprfg (    IMXAE*JMXAE)   - aeros profile index
!      idxcg (NXC*IMXAE*JMXAE)   - aeros component index
!      cmixg (NXC*IMXAE*JMXAE)   - aeros component mixing ratio
!      denng (NXC*IMXAE*JMXAE)   - aerosols number density

      real (kind=kind_phys), allocatable, save, dimension(:,:)   ::     &
     &       extrhi, scarhi, ssarhi, asyrhi
      real (kind=kind_phys), allocatable, save, dimension(:,:,:) ::     &
     &       extrhd, scarhd, ssarhd, asyrhd
      real (kind=kind_phys), allocatable, save, dimension(:)     ::     &
     &       extstra

      real (kind=kind_phys),allocatable,save:: cmixg(:,:,:),denng(:,:,:)
      integer,              allocatable,save:: kprfg(:,:),  idxcg(:,:,:)

!  ---  logical parameter for clim opac optic prop input control
      logical, save :: lclmin = .true.



!  ---  public interfaces

      public aerinit, setaer


! =================
      contains
! =================

!-----------------------------------
      subroutine aerinit                                                &
!...................................

!  ---  inputs:
     &     ( iyear, imon, IAER, me )
!  ---  outputs: ( none )

!  ==================================================================  !
!                                                                      !
!  aerinit does invoke aerosol initialization programs based on the    !
!  selection of schemes.                                               !
!                                                                      !
!  inputs:                                                             !
!     iyear   - 4-digit calender year                 1                !
!     imon    - month of the year                     1                !
!     IAER    - 3-digit aerosol flag (volc,lw,sw)     1                !
!               =  0: turn all aeros effects off (sw,lw,volc)          !
!               =  1: use clim tropspheric aerosol for sw only         !
!               = 10: use clim tropspheric aerosol for lw only         !
!               = 11: use clim tropspheric aerosol for both sw and lw  !
!               =100: volc aerosol only for both sw and lw             !
!               =101: volc and clim trops aerosol for sw only          !
!               =110: volc and clim trops aerosol for lw only          !
!               =111: volc and clim trops aerosol for both sw and lw   !
!not-yet        =  2: gocart tropspheric aerosol for sw only           !
!  ''           = 20: gocart tropspheric aerosol for lw only           !
!  ''           = 22: gocart tropspheric aerosol for both sw and lw    !
!  ''           =102: volc and gocart trops aerosol for sw only        !
!  ''           =120: volc and gocart trops aerosol for lw only        !
!not-yet        =122: volc and gocart trops aerosol for both sw and lw !
!     me      - print message control flag            1                !
!                                                                      !
!  outputs: (to the module variables)                                  !
!    ( none )                                                          !
!                                                                      !
!  module variables:                                                   !
!     kprfg   - aerosols profile index                IMXAE*JMXAE      !
!     idxcg   - aerosols component index              NXC*IMXAE*JMXAE  !
!     cmixg   - aerosols component mixing ratio       NXC*IMXAE*JMXAE  !
!     denng   - aerosols number density               NXC*IMXAE*JMXAE  !
!                                                                      !
!     ivolae  - stratosphere volcanic aerosol optical depth (fac 1.e4) !
!                                                     12*4*10          !
!                                                                      !
!  usage:    call aerinit                                              !
!                                                                      !
!  subprograms called:  clim_aerinit                                   !
!                                                                      !
!  ==================================================================  !
!
      implicit none

!  ---  inputs:
      integer,  intent(in) :: iyear, imon, IAER, me

!  ---  output: ( none )

!  ---  locals:
      real (kind=kind_phys), dimension(NWVTOT) :: solfwv        ! one wvn sol flux
      real (kind=kind_phys), dimension(NWVTIR) :: eirfwv        ! one wvn ir flux
      real (kind=kind_phys) :: soltot, tmp1, tmp2, tmp3

      integer :: nb, ni, nw, nw1, nw2, nmax, nmin

      character            :: cline*80, ctyp*3, volcano_file*32

      integer :: i, j, k, nc, iy, id, ilw, isw
      logical :: file_exist

      data volcano_file / 'volcanic_aerosols_1850-1859.txt ' /

!===>  ...  begin here

      isw = mod(IAER,10)                ! trop-aer scheme for sw
      iy  = IAER / 10
      ilw = mod(iy , 10)                ! trop-aer scheme for lw

      iaerflg = max( isw, ilw )         ! flag for trop-aer scheme selection
      lalwflg = ilw  > 0                ! flag for lw trop-aer properties
      laswflg = isw  > 0                ! flag for sw trop-aer properties
      lvolflg = IAER >= 100             ! flag for stratospheric volcanic aer

!  --- ...  in sw, aerosols optical properties are computed for each radiation
!           spectral band; while in lw, optical properties can be calculated
!           for either only one broad band or for each of the lw radiation bands

      if ( iaerlw == 1 ) then
        NBDIR = NBDLW
      else
        NBDIR = 1
      endif
      NBDSWLW = NBDSW + NBDIR

!  --- ...  define the one wavenumber solar fluxes based on toa solar
!           spectral distribution

      nmax = min( NWVTOT, nint( maxval(wvnum2) ))
      nmin = max( 1,      nint( minval(wvnum1) ))

!     print *,' MINWVN, MAXWVN = ',nmin, nmax

!     soltot1 = f_zero
      soltot  = f_zero
      do nb = 1, NWVSOL
        if ( nb == 1 ) then
          nw1 = 1
        else
          nw1 = nw1 + nwvns0(nb-1)
        endif

        nw2 = nw1 + nwvns0(nb) - 1

        do nw = nw1, nw2
          solfwv(nw) = s0intv(nb)
!         soltot1 = soltot1 + s0intv(nb)
          if ( nw >= nmin .and. nw <= nmax ) then
            soltot = soltot + s0intv(nb)
          endif
        enddo
      enddo

!  --- ...  define the one wavenumber ir fluxes based on black-body
!           emission distribution at a predefined temperature

      tmp1 = 2.0 * con_pi * con_plnk * (con_c**2)
      tmp2 = con_plnk * con_c / (con_boltz * con_t0c)

      do ni = 1, NWVTIR
        tmp3 = 100.0 * ni
        eirfwv(ni) = (tmp1 * tmp3**3) / (exp(tmp2*tmp3) - 1.0)
      enddo

!  --- ...  write aerosol parameter configuration to output logs

      if ( me == 0 ) then

        print *,'   IAER=',IAER,'  iaerflg=',iaerflg,'  LW-trop-aer='   &
     &         ,lalwflg,'  SW-trop-aer=',laswflg,'  Volc-aer=',lvolflg

        if ( IAER <= 0 ) then        ! turn off all aerosol effects

          print *,' - No tropospheric/volcanic aerosol effect included'
          print *,'      Input values of aerosol optical properties to' &
     &           ,' both SW and LW radiations are set to zeros'

        elseif ( IAER == 100 ) then  ! only stratospheric volcanic aerosols

          print *,' - Include onle volcanic aerosols in both SW and LW' &
     &           ,' for year, month =', iyear, imon

        else

          if ( IAER < 100 ) then       ! no stratospheric volcanic aerosols
            print *,' - No stratospheric volcanic aerosol effect'
          else                         ! include stratospheric volcanic aerosols
            print *,' - Include stratospheric volcanic aerosol effect'  &
     &             ,' for year, month =', iyear, imon
          endif

          if ( iaerflg == 1 ) then      ! opac tropospheric climatological

            print *,' - Using OPAC climatology for tropospheric aerosol'

!gocart!  elseif ( iaerflg == 2 ) then  ! opac tropospheric climatological

!gocart!    print *,' - Using GOCART scheme for tropospheric aerosol'

          endif                         ! end if_iaerflg_block
        
          if ( laswflg ) then           ! shcek for sw effect
            print *,'      Compute aerosol optical properties for SW'   &
     &             ,' input parameters'
          else
            print *,'      No SW radiation aerosol effect, values of'   &
     &             ,' aerosol properties to SW input are set to zeros'
          endif                         ! end if_laswflg_block

          if ( lalwflg ) then           ! check for lw effect
            print *,'      Compute aerosol optical properties for LW'   &
     &             ,' input parameters'
          else
            print *,'      No LW radiation aerosol effect, values of'   &
     &             ,' aerosol properties to LW input are set to zeros'
          endif                         ! end if_lalwflg_block

        endif     ! end if_IAER_block

      endif       ! end if_me_block

!  --- ...  tropospheric aerosol initialization

      if ( IAER == 0 ) then 

        return

      elseif ( IAER /= 100 ) then

        if ( iaerflg == 1 ) then      ! opac tropospheric climatology

          if ( imon < 1 .or. imon > 12 ) then
            print *,' ***** ERROR in specifying requested month!!! ',   &
     &              'imon=', imon
            print *,' ***** STOPPED in subroutinte AERINIT!!!'
            stop
          endif

          call clim_aerinit                                             &
!  ---  inputs:
     &     ( NWVTOT,solfwv,soltot,NWVTIR,eirfwv,                        &
     &       NBDSW,NBDIR,NBDSWLW, imon, me                              &
!  ---  outputs:  (none)
     &     )

!!!     elseif ( iaerflg == 2 ) then  ! gocart prognostic aerosols

!!!       call gocart aerosol initialization routine here   !!!
!!!
!!!       call gocart_aerinit

        else
          print *,'   ERROR in aerosols specification! IAER =',iaer
          print *,'     iaerflg, lvolflg =',iaerflg,lvolflg
          print *,'   *** Stopped in subroutine AERINIT !!'
          stop
        endif                          ! end if_iaerflg_block

      endif    ! end if_IAER_block

!  --- ...  stratosperic volcanic aerosol initialization

      if ( lvolflg ) then

!  ---  allocate data space

        if ( .not. allocated(ivolae) ) then
          allocate ( ivolae(12,4,10) )   ! for 12-mon,4-lat_zone,10-year
        endif

        kmonsav = imon

        if ( kyrstr<=iyear .and. iyear<=kyrend ) then   ! use previously input data
          kyrsav = iyear
          return
        else                                            ! need to input new data
          kyrsav = iyear
          kyrstr = iyear - mod(iyear,10)
          kyrend = kyrstr + 9
!check    print *,'  kyrstr, kyrend, kyrsav, kmonsav =',                &
!    &            kyrstr,kyrend,kyrsav,kmonsav

          if ( iyear < MINVYR .or. iyear > MAXVYR ) then
            ivolae(:,:,:) = 1            ! set as lowest value
            if ( me == 0 ) then
              print *,'   Request volcanic date out of range,',         &
     &                ' optical depth set to lowest value'
            endif
          else
            write(volcano_file(19:27),60) kyrstr,kyrend
  60        format(i4.4,'-',i4.4)

            inquire (file=volcano_file, exist=file_exist)
            if ( file_exist ) then
              open (unit=NIAERCM,file=volcano_file,status='OLD',        &
     &              form='FORMATTED')

              read(NIAERCM,62) cline
  62          format(a80)

!  ---  check print
              if ( me == 0 ) then
                print *,'   Opened volcanic data file: ',volcano_file
                print *, cline
              endif

              do k = 1, 10
                do j = 1, 4
                  read(NIAERCM,64) ivolae(:,j,k)
!                 read(NIAERCM,64) (ivolae(i,j,k),i=1,12)
  64              format(12i5)
                enddo
              enddo

              close (NIAERCM)
            else
              print *,'   Requested volcanic data file "',              &
     &                volcano_file,'" not found!'
              print *,'   *** Stopped in subroutine AERINIT !!'
              stop
            endif              ! end if_file_exist_block
          endif                ! end if_iyear_block

        endif              ! end if_kyrstr_block

        if ( me == 0 ) then
          iy = mod(kyrsav,10) + 1
          print *,' CHECK: Sample Volcanic data used for month, year:', &
     &             imon, iyear
          print *,  ivolae(kmonsav,:,iy)
        endif
      endif                ! end if_lvolflg_block
!
      return
!...................................
      end subroutine aerinit
!-----------------------------------



!-----------------------------------
      subroutine setaer                                                 &
!...................................

!  ---  inputs:
     &     ( xlon,xlat,prsi,prsl,tlay,qlay,rhlay,                       &
     &       IMAX,NLAY,NLP1, iflip, lsswr,lslwr,                        &
!  ---  outputs:
     &       aerosw,aerolw                                              &
     &     )

!  ==================================================================  !
!                                                                      !
!  setaer computes aerosols optical properties from different global   !
!  aerosols data sets based on scheme selection.                       !
!                                                                      !
!  inputs:                                                             !
!     xlon, xlat                                             IMAX      !
!             - longitude and latitude of given points in radiance     !
!     prsi    - pressure at interface              mb   IMAX*NLP1      !
!     prsl    - layer mean pressure                mb   IMAX*NLAY      !
!     tlay    - layer mean temperature             k    IMAX*NLAY      !
!     qlay    - layer mean specific humidity       g/g  IMAX*NLAY      !
!     rhlay   - layer mean relative humidity            IMAX*NLAY      !
!     IMAX    - horizontal dimension of arrays                  1      !
!     NLAY,NLP1-vertical dimensions of arrays                   1      !
!     iflip   - control flag for direction of vertical index    1      !
!               =0: index from toa to surface                          !
!               =1: index from surface to toa                          !
!     lsswr,lslwr                                                      !
!             - logical flags for sw/lw radiation calls         1      !
!                                                                      !
!  outputs:                                                            !
!     aerosw - aeros opt properties for sw      IMAX*NLAY*NBDSW*NF_AESW!
!               (:,:,:,1): optical depth                               !
!               (:,:,:,2): single scattering albedo                    !
!               (:,:,:,3): asymmetry parameter                         !
!     aerolw - aeros opt properties for lw      IMAX*NLAY*NBDLW*NF_AELW!
!               (:,:,:,1): optical depth                               !
!               (:,:,:,2): single scattering albedo                    !
!               (:,:,:,3): asymmetry parameter                         !
!                                                                      !
!                                                                      !
!                                                                      !
!  module variable: (set by subroutine aerinit)                        !
!     iaerflg - control flag for tropospheric aerosols selection       !
!               =0: do not calc tropospheric aerosol optical properties!
!               =1: use opac tropospheric aerosol climatology          !
!               =2: use gocart tropospheric interactive aerosol        !
!     lalwflg - control flag for lw radiation aerosols effect          !
!               =f: do not calc lw aerosol optical properties          !
!               =t: calculate lw aerosol optical properties            !
!     laswflg - control flag for sw radiation aerosols effect          !
!               =f: do not calc sw aerosol optical properties          !
!               =t: calculate sw aerosol optical properties            !
!     lvolflg - control flag for stratospheric vocanic aerosols        !
!               =t: add volcanic aerosols to the background aerosols   !
!               =f: do not add volcanic aerosols                       !
!                                                                      !
!     ivolae  - stratosphere volcanic aerosol optical depth (fac 1.e4) !
!                                                     12*4*10          !
!                                                                      !
!  usage:    call setaer                                               !
!                                                                      !
!  subprograms called:  setclimaer                                     !
!                                                                      !
!  ==================================================================  !
!
      implicit none

!  ---  inputs:
      integer, intent(in) :: IMAX,NLAY,NLP1, iflip

      real (kind=kind_phys), dimension(:,:), intent(in) :: prsi, prsl,  &
     &       tlay, qlay, rhlay
      real (kind=kind_phys), dimension(:),   intent(in) :: xlon, xlat
      logical, intent(in) :: lsswr, lslwr

!  ---  outputs:
      real (kind=kind_phys), dimension(:,:,:,:), intent(out) ::         &
     &       aerosw, aerolw

!  ---  locals:
      real (kind=kind_phys), dimension(IMAX) :: alon, alat, volcae, delp
      real (kind=kind_phys) :: prsln(NLP1),hz(IMAX,NLP1),dz(IMAX,NLAY)
      real (kind=kind_phys) :: tmp1, tmp2, psrfh, psrfl

      integer               :: kcutl(IMAX), kcuth(IMAX)
      integer               :: i, i1, k, m, mb, kp, kh, kl

      logical               :: laddsw = .false.
      logical               :: laddlw = .false.

!  ---  conversion constants
      real (kind=kind_phys), parameter :: rdg  = 180.0 / con_pi
      real (kind=kind_phys), parameter :: rovg = 0.001 * con_rd / con_g


!===>  ...  begin here

      if ( .not. (lsswr .or. lslwr) ) then
        return
      endif

      if ( .not. laswflg ) then
        aerosw = f_zero
      endif

      if ( .not. lalwflg ) then
        aerolw = f_zero
      endif

      if ( (.not.lvolflg) .and. (iaerflg==0) ) then
        return
      endif

!  ---  ...  convert lat/lon from radiance to degree

      do i = 1, IMAX
        alon(i) = xlon(i) * rdg
        if (alon(i) < f_zero) alon(i) = alon(i) + 360.0
        alat(i) = xlat(i) * rdg
      enddo

!  ---  ...  compute level height and layer thickness

      lab_do_IMAX : do i = 1, IMAX

        lab_if_flip : if (iflip == 1) then        ! input from sfc to toa

          do k = 1, NLAY
            prsln(k) = log(prsi(i,k))
          enddo
          prsln(NLP1)= log(prsl(i,NLAY))

          do k = NLAY, 1, -1
            dz(i,k) = rovg * (prsln(k) - prsln(k+1))                    &
     &              * tlay(i,k) * (f_one + con_fvirt*qlay(i,k))
          enddo
          dz(i,NLAY)  = 2.0 * dz(i,NLAY)

          hz(i,1) = f_zero
          do k = 1, NLAY
            hz(i,k+1) = hz(i,k) + dz(i,k)
          enddo

        else  lab_if_flip                         ! input from toa to sfc

          prsln(1) = log(prsl(i,1))
          do k = 2, NLP1
            prsln(k) = log(prsi(i,k))
          enddo

          do k = 1, NLAY
            dz(i,k) = rovg * (prsln(k+1) - prsln(k))                    &
     &              * tlay(i,k) * (f_one + con_fvirt*qlay(i,k))
          enddo
          dz(i,1) = 2.0 * dz(i,1)

          hz(i,NLP1) = f_zero
          do k = NLAY, 1, -1
            hz(i,k) = hz(i,k+1) + dz(i,k)
          enddo

        endif  lab_if_flip

      enddo  lab_do_IMAX

!  ---  ...  calculate sw aerosol optical properties for the corresponding
!            frequency bands based on scheme selection

      if ( iaerflg == 1 ) then      ! use opac aerosol climatology

        call setclimaer                                                 &
!  ---  inputs:
     &     ( alon,alat,prsi,rhlay,dz,hz,NBDSWLW,                        &
     &       IMAX,NLAY,NLP1, iflip, lsswr,lslwr,                        &
!  ---  outputs:
     &       aerosw,aerolw                                              &
     &     )

!!!   elseif ( iaerflg == 2 )       ! use gocart aerosol scheme


      endif     ! end if_iaerflg_block

!  ---  check print
!     do m = 1, NBDSW
!       print *,'  ***  CHECK AEROSOLS PROPERTIES FOR SW BAND =',m,     &
!    &          ' ***'
!       do k = 1, 10
!         print *,'  LEVEL :',k
!         print *,'  TAUAER:',aerosw(:,k,m,1)
!         print *,'  SSAAER:',aerosw(:,k,m,2)
!         print *,'  ASYAER:',aerosw(:,k,m,3)
!       enddo
!     enddo
!     do m = 1, NBDIR
!       print *,'  ***  CHECK AEROSOLS PROPERTIES FOR LW BAND =',m,     &
!    &          ' ***'
!       do k = 1, 10
!         print *,'  LEVEL :',k
!         print *,'  TAUAER:',aerolw(:,k,m,1)
!         print *,'  SSAAER:',aerolw(:,k,m,2)
!         print *,'  ASYAER:',aerolw(:,k,m,3)
!       enddo
!     enddo


!  ---  ...  stratosphere volcanic forcing

      if ( lvolflg ) then

        laddsw = lsswr .and. (laswflg .or. iaerflg==0)
        laddlw = lslwr .and. (lalwflg .or. iaerflg==0)

        i1 = mod(kyrsav, 10) + 1

!  ---  select data in 4 lat bands, interpolation at the boundaires

        do i = 1, IMAX
          if      ( alat(i) > 46.0 ) then
            volcae(i) = 1.0e-4 * ivolae(kmonsav,1,i1)
          else if ( alat(i) > 44.0 ) then
            volcae(i) = 5.0e-5                                          &
     &                * (ivolae(kmonsav,1,i1) + ivolae(kmonsav,2,i1))
          else if ( alat(i) >  1.0 ) then
            volcae(i) = 1.0e-4 * ivolae(kmonsav,2,i1)
          else if ( alat(i) > -1.0 ) then
            volcae(i) = 5.0e-5                                          &
     &                * (ivolae(kmonsav,2,i1) + ivolae(kmonsav,3,i1))
          else if ( alat(i) >-44.0 ) then
            volcae(i) = 1.0e-4 * ivolae(kmonsav,3,i1)
          else if ( alat(i) >-46.0 ) then
            volcae(i) = 5.0e-5                                          &
     &                * (ivolae(kmonsav,3,i1) + ivolae(kmonsav,4,i1))
          else
            volcae(i) = 1.0e-4 * ivolae(kmonsav,4,i1)
          endif
        enddo

        if ( iflip == 0 ) then          ! input data from toa to sfc

          psrfh = 5.0                        ! ref press for upper bound

!  ---  find lower boundary of stratosphere

          do i = 1, IMAX

            tmp1 = abs( alat(i) )
            if ( tmp1 > 70.0 ) then          ! polar, fixed at 250mb
              psrfl = 250.0
            elseif ( tmp1 < 20.0 ) then      ! tropic, fixed at 150mb
              psrfl = 150.0
            else                             ! mid-lat, interp
              psrfl = 110.0 + 2.0*tmp1
            endif

            kcuth(i) = NLAY - 1
            kcutl(i) = 2
            delp(i) = prsi(i,2)

            lab_do_kcuth0 : do k = 2, NLAY-2
              if ( prsi(i,k) >= psrfh ) then
                kcuth(i) = k - 1
                exit lab_do_kcuth0
              endif
            enddo  lab_do_kcuth0

            lab_do_kcutl0 : do k = 2, NLAY-2
              if ( prsi(i,k) >= psrfl ) then
                kcutl(i) = k - 1
                delp(i) = prsi(i,k) - prsi(i,kcuth(i))
                exit lab_do_kcutl0
              endif
            enddo  lab_do_kcutl0
          enddo

!  ---  sw: add volcanic aerosol optical depth to the background value

          if ( laddsw ) then
            do m = 1, NBDSW
              mb = NSWSTR + m - 1

              if     ( wvnum1(mb) > 20000 ) then   ! range of wvlth < 0.5mu
                tmp2 = 0.74
              elseif ( wvnum2(mb) < 20000 ) then   ! range of wvlth > 0.5mu
                tmp2 = 1.14
              else                                 ! range of wvlth in btwn
                tmp2 = 0.94
              endif
              tmp1 = (0.275e-4 * (wvnum2(mb)+wvnum1(mb))) ** tmp2

              do i = 1, IMAX
                kh = kcuth(i)
                kl = kcutl(i)
                do k = kh, kl
                  tmp2 = tmp1 * ((prsi(i,k+1) - prsi(i,k)) / delp(i))
                  aerosw(i,k,m,1) = aerosw(i,k,m,1) + tmp2*volcae(i)
                enddo

!  ---  smoothing profile at boundary if needed

                if ( aerosw(i,kl,m,1) > 10.*aerosw(i,kl+1,m,1) ) then
                  tmp2 = aerosw(i,kl,m,1) + aerosw(i,kl+1,m,1)
                  aerosw(i,kl  ,m,1) = 0.8 * tmp2
                  aerosw(i,kl+1,m,1) = 0.2 * tmp2
                endif
              enddo    ! end do_i_block
            enddo      ! end do_m_block

!  ---  check print

!           do i = 1, IMAX
!             print *,' LEV  PRESS    TAUSAV    NEWTAU    FOR PROFILE:',&
!    &                i,'  KCUTH, KCUTL =',kcuth(i),kcutl(i)
!             kh = kcuth(i) - 1
!             kl = kcutl(i) + 10
!             do k = kh, kl
!               write(6,71) k, prsl(i,k), aersav(i,k), aerosw(i,k,1,1)
! 71            format(i3,f9.3,2e11.4)
!             enddo
!           enddo
          endif        ! end if_laddsw_block

!  ---  lw: add volcanic aerosol optical depth to the background value

          if ( laddlw ) then
            if ( NBDIR == 1 ) then

              tmp1 = (0.55 / 11.0) ** 1.2
              do i = 1, IMAX
                kh = kcuth(i)
                kl = kcutl(i)
                do k = kh, kl
                  tmp2 = tmp1 * ((prsi(i,k+1) - prsi(i,k)) / delp(i))
                  aerolw(i,k,1,1) = aerolw(i,k,1,1) + tmp2*volcae(i)
                enddo
              enddo    ! end do_i_block

            else

              do m = 1, NBDIR
                tmp1 = (0.275e-4 * (wvnlw2(m) + wvnlw1(m))) ** 1.2

                do i = 1, IMAX
                  kh = kcuth(i)
                  kl = kcutl(i)
                  do k = kh, kl
                    tmp2 = tmp1 * ((prsi(i,k+1)-prsi(i,k)) / delp(i))
                    aerolw(i,k,m,1) = aerolw(i,k,m,1) + tmp2*volcae(i)
                  enddo
                enddo    ! end do_i_block
              enddo      ! end do_m_block

            endif      ! end if_NBDIR_block
          endif        ! end if_laddlw_block

        else                            ! input data from sfc to toa

          psrfh = 5.0                        ! ref press for upper bound

!  ---  find lower boundary of stratosphere

          do i = 1, IMAX

            tmp1 = abs( alat(i) )
            if ( tmp1 > 70.0 ) then          ! polar, fixed at 250mb
              psrfl = 250.0
            elseif ( tmp1 < 20.0 ) then      ! tropic, fixed at 150mb
              psrfl = 150.0
            else                             ! mid-lat, interp
              psrfl = 110.0 + 2.0*tmp1
            endif

            kcuth(i) = 2
            kcutl(i) = NLAY - 1
            delp(i) = prsi(i,NLAY-1)

            lab_do_kcuth1 : do k = NLAY-1, 2, -1
              if ( prsi(i,k) >= psrfh ) then
                kcuth(i) = k
                exit lab_do_kcuth1
              endif
            enddo  lab_do_kcuth1

            lab_do_kcutl1 : do k = NLAY, 2, -1
              if ( prsi(i,k) >= psrfl ) then
                kcutl(i) = k
                delp(i) = prsi(i,k) - prsi(i,kcuth(i)+1)
                exit lab_do_kcutl1
              endif
            enddo  lab_do_kcutl1
          enddo

!  ---  sw: add volcanic aerosol optical depth to the background value

          if ( laddsw ) then
            do m = 1, NBDSW
              mb = NSWSTR + m - 1

              if     ( wvnum1(mb) > 20000 ) then   ! range of wvlth < 0.5mu
                tmp2 = 0.74
              elseif ( wvnum2(mb) < 20000 ) then   ! range of wvlth > 0.5mu
                tmp2 = 1.14
              else                                 ! range of wvlth in btwn
                tmp2 = 0.94
              endif
              tmp1 = (0.275e-4 * (wvnum2(mb)+wvnum1(mb))) ** tmp2

              do i = 1, IMAX
                kh = kcuth(i)
                kl = kcutl(i)
                do k = kl, kh
                  tmp2 = tmp1 * ((prsi(i,k) - prsi(i,k+1)) / delp(i))
                  aerosw(i,k,m,1) = aerosw(i,k,m,1) + tmp2*volcae(i)
                enddo

!  ---  smoothing profile at boundary if needed

                if ( aerosw(i,kl,m,1) > 10.*aerosw(i,kl-1,m,1) ) then
                  tmp2 = aerosw(i,kl,m,1) + aerosw(i,kl-1,m,1)
                  aerosw(i,kl  ,m,1) = 0.8 * tmp2
                  aerosw(i,kl-1,m,1) = 0.2 * tmp2
                endif
              enddo    ! end do_i_block
            enddo      ! end do_m_block

!  ---  check print

!           do i = 1, IMAX
!             print *,' LEV  PRESS    TAUSAV    NEWTAU    FOR PROFILE:',&
!    &                i,'  KCUTH, KCUTL =',kcuth(i),kcutl(i)
!             kh = kcuth(i) + 1
!             kl = kcutl(i) - 10
!             do k = kh, kl, -1
!               write(6,71) NLP1-k,prsl(i,k),aersav(i,k),aerosw(i,k,1,1)
!             enddo
!           enddo
          endif        ! end if_laddsw_block

!  ---  lw: add volcanic aerosol optical depth to the background value

          if ( laddlw ) then
            if ( NBDIR == 1 ) then

              tmp1 = (0.55 / 11.0) ** 1.2
              do i = 1, IMAX
                kh = kcuth(i)
                kl = kcutl(i)
                do k = kl, kh
                  tmp2 = tmp1 * ((prsi(i,k) - prsi(i,k+1)) / delp(i))
                  aerolw(i,k,1,1) = aerolw(i,k,1,1) + tmp2*volcae(i)
                enddo
              enddo    ! end do_i_block

            else

              do m = 1, NBDIR
                tmp1 = (0.275e-4 * (wvnlw2(m) + wvnlw1(m))) ** 1.2

                do i = 1, IMAX
                  kh = kcuth(i)
                  kl = kcutl(i)
                  do k = kl, kh
                    tmp2 = tmp1 * ((prsi(i,k)-prsi(i,k+1)) / delp(i))
                    aerolw(i,k,m,1) = aerolw(i,k,m,1) + tmp2*volcae(i)
                  enddo
                enddo    ! end do_i_block
              enddo      ! end do_m_block

            endif      ! end if_NBDIR_block
          endif        ! end if_laddlw_block

        endif                           ! end if_iflip_block

!  ---  adding volcanic optical depth to stratospheric layers


      endif   ! end if_lvolflg_block

!
      return
!...................................
      end subroutine setaer
!-----------------------------------



!-----------------------------------
      subroutine clim_aerinit                                           &
!...................................
!  ---  inputs:
     &     ( NWVTOT,solfwv,soltot,NWVTIR,eirfwv,                        &
     &       NBDSW,NBDIR,NBDSWLW, imon, me                              &
!  ---  outputs: ( none )
     &     )

!  ==================================================================  !
!                                                                      !
!  subprogram : clim_aerinit                                           !
!                                                                      !
!    this is the initialization progrmam for climatological aerosols   !
!                                                                      !
!    it reads in monthly global distribution of aerosol profiles in    !
!    five degree horizontal resolution. Then, it reads and maps the    !
!    tabulated aerosol optical spectral data onto corresponding sw     !
!    radiation spectral bands.                                         !
!                                                                      !
!  ====================  defination of variables  ===================  !
!                                                                      !
!  inputs:                                                             !
!   NWVTOT           - total num of wave numbers used in sw spectrum   !
!   solfwv(NWVTOT)   - solar flux for each individual wavenumber (w/m2)!
!   soltot           - total solar flux for the spectrual range  (w/m2)!
!   NWVTIR           - total num of wave numbers used in the ir region !
!   eirfwv(NWVTIR)   - ir flux(273k) for each individual wavenum (w/m2)!
!   NBDSW            - num of bands calculated for sw aeros opt prop   !
!   NBDIR            - num of bands calculated for lw aeros opt prop   !
!   NBDSWLW          - total num of bands calc for sw+lw aeros opt prop!
!   imon             - month of the year                               !
!   me               - print message control flag                      !
!                                                                      !
!  outputs: (to the module variables)                                  !
!                                                                      !
!  module variables:                                                   !
!     NBDSW   - total number of sw spectral bands                      !
!     wvnum1,wvnum2 (NSWSTR:NSWEND)                                    !
!             - start/end wavenumbers for each of sw bands             !
!     NBDLW   - total number of lw spectral bands                      !
!     wvnlw1,wvnlw2 (NBDLW)                                            !
!             - start/end wavenumbers for each of lw bands             !
!     NBDSWLW - total number of sw+lw bands used in this version       !
!     extrhi  - extinction coef for rh-indep aeros         NCM1*NBDSWLW!
!     scarhi  - scattering coef for rh-indep aeros         NCM1*NBDSWLW!
!     ssarhi  - single-scat-alb for rh-indep aeros         NCM1*NBDSWLW!
!     asyrhi  - asymmetry factor for rh-indep aeros        NCM1*NBDSWLW!
!     extrhd  - extinction coef for rh-dep aeros    NRHLEV*NCM2*NBDSWLW!
!     scarhd  - scattering coef for rh-dep aeros    NRHLEV*NCM2*NBDSWLW!
!     ssarhd  - single-scat-alb for rh-dep aeros    NRHLEV*NCM2*NBDSWLW!
!     asyrhd  - asymmetry factor for rh-dep aeros   NRHLEV*NCM2*NBDSWLW!
!                                                                      !
!     kprfg   - aerosols profile index                IMXAE*JMXAE      !
!     idxcg   - aerosols component index              NXC*IMXAE*JMXAE  !
!     cmixg   - aerosols component mixing ratio       NXC*IMXAE*JMXAE  !
!     denng   - aerosols number density               NXC*IMXAE*JMXAE  !
!                                                                      !
!  usage:    call aerinit                                              !
!                                                                      !
!  subprograms called:  optavg                                         !
!                                                                      !
!  ==================================================================  !
!
      implicit none

!  ---  inputs:
      integer, intent(in) :: NWVTOT,NWVTIR,NBDSW,NBDIR,NBDSWLW,imon,me

      real (kind=kind_phys), intent(in) :: solfwv(:),soltot, eirfwv(:)

!  ---  output: ( none )

!  ---  locals:
      real (kind=kind_io8) :: cmix(NXC), denn, tem
      integer              :: idxc(NXC), kprf

      real (kind=kind_phys), dimension(NBDSW,NAERBND) :: solwaer
      real (kind=kind_phys), dimension(NBDSW)         :: solbnd
      real (kind=kind_phys), dimension(NBDIR,NAERBND) :: eirwaer
      real (kind=kind_phys), dimension(NBDIR)         :: eirbnd
      real (kind=kind_phys) :: sumsol, sumir

      integer, dimension(NBDSW) :: nv1, nv2
      integer, dimension(NBDIR) :: nr1, nr2

      integer :: i, j, k, m, mb, nc, iy, ib, ii, id, iw, iw1, iw2
      logical :: file_exist

      character :: cline*80, ctyp*3, aerosol_file*24

!     data aerosol_file / 'climaeropac_global.txt  ' /
      data aerosol_file / 'aerosol.dat  ' /

!===>  ...  begin here

      if ( .not. allocated(kprfg) ) then
        allocate ( kprfg (    IMXAE,JMXAE) )
        allocate ( cmixg (NXC,IMXAE,JMXAE) )
        allocate ( denng (NXC,IMXAE,JMXAE) )
        allocate ( idxcg (NXC,IMXAE,JMXAE) )
      endif

!  --- ...  reading climatological aerosols data

      inquire (file=aerosol_file, exist=file_exist)

      if ( file_exist ) then
        open (unit=NIAERCM,file=aerosol_file,status='OLD',              &
     &        form='FORMATTED')
        rewind (NIAERCM)

        if ( me == 0 ) then
          print *,'   Opened aerosol data file: ',aerosol_file
        endif
      else
        print *,'    Requested aerosol data file "',aerosol_file,       &
     &          '" not found!'
        print *,'    *** Stopped in subroutine AERINIT !!'
        stop
      endif              ! end if_file_exist_block

      cmixg = f_zero
      denng = f_zero
      idxcg = 0

!  --- ...  loop over 12 month global distribution

      Lab_do_12mon : do m = 1, 12

        read(NIAERCM,12) cline
  12    format(a80/)

        if ( m /= imon ) then
!         if ( me == 0 ) print *,'  *** Skipped ',cline

          do j = 1, JMXAE
          do i = 1, IMXAE
            read(NIAERCM,*) id
          enddo
          enddo
        else
          if ( me == 0 ) print *,'  --- Reading ',cline

          do j = 1, JMXAE
          do i = 1, IMXAE
            read(NIAERCM,14) (idxc(k),cmix(k),k=1,NXC),kprf,denn,nc,ctyp
  14        format(5(i2,e11.4),i2,f8.2,i3,1x,a3)

            kprfg(i,j)     = kprf
            denng(1,i,j)   = denn       ! num density of 1st layer
            if ( kprf >= 6 ) then
              denng(2,i,j) = cmix(NXC)  ! num density of 2dn layer
            else
              denng(2,i,j) = f_zero
            endif

            tem = f_one
            do k = 1, NXC-1
              idxcg(k,i,j) = idxc(k)    ! component index
              cmixg(k,i,j) = cmix(k)    ! component mixing ratio
              tem          = tem - cmix(k)
            enddo
            idxcg(NXC,i,j) = idxc(NXC)
            cmixg(NXC,i,j) = tem        ! to make sure all add to 1.
          enddo
          enddo

          if ( .not. lclmin ) then
            close (NIAERCM)
            exit  Lab_do_12mon
          endif
        endif     ! end if_m_block

      enddo  Lab_do_12mon

!  --  check print

!     print *,'  IDXCG :'
!     print 16,idxcg
! 16  format(40i3)
!     print *,'  CMIXG :'
!     print 17,cmixg
!     print *,'  DENNG :'
!     print 17,denng
!     print *,'  KPRFG :'
!     print 17,kprfg
! 17  format(8e16.9)

      if ( .not. lclmin ) then

!  --- ...  already done optical property interpolation, exit

        return

      else

!  --- ...  aloocate and input aerosol optical data

        if ( .not. allocated( rhidext0 ) ) then
          allocate ( rhidext0(NAERBND,NCM1) )
          allocate ( rhidsca0(NAERBND,NCM1) )
          allocate ( rhidssa0(NAERBND,NCM1) )
          allocate ( rhidasy0(NAERBND,NCM1) )
          allocate ( rhdpext0(NAERBND,NRHLEV,NCM2) )
          allocate ( rhdpsca0(NAERBND,NRHLEV,NCM2) )
          allocate ( rhdpssa0(NAERBND,NRHLEV,NCM2) )
          allocate ( rhdpasy0(NAERBND,NRHLEV,NCM2) )
          allocate ( straext0(NAERBND) )
        endif

        if ( .not. allocated( iendwv ) ) then
          allocate ( iendwv (NAERBND) )
          allocate ( haer   (NDM,NAE) )
          allocate ( prsref (NDM,NAE) )
        endif

        if ( .not. allocated( extrhi ) ) then
          allocate ( extrhi (       NCM1,NBDSWLW) )
          allocate ( scarhi (       NCM1,NBDSWLW) )
          allocate ( ssarhi (       NCM1,NBDSWLW) )
          allocate ( asyrhi (       NCM1,NBDSWLW) )
          allocate ( extrhd (NRHLEV,NCM2,NBDSWLW) )
          allocate ( scarhd (NRHLEV,NCM2,NBDSWLW) )
          allocate ( ssarhd (NRHLEV,NCM2,NBDSWLW) )
          allocate ( asyrhd (NRHLEV,NCM2,NBDSWLW) )
          allocate ( extstra(            NBDSWLW) )
        endif

        read(NIAERCM,21) cline   ! ending wave num for 61 aeros spectral bands
  21    format(a80)
        read(NIAERCM,22) iendwv(:)
  22    format(13i6)

        read(NIAERCM,21) cline   ! atmos scale height for 5 domains, 7 profs
        read(NIAERCM,24) haer(:,:)
  24    format(20f4.1)

        read(NIAERCM,21) cline   ! reference pressure for 5 domains, 7 profs
        read(NIAERCM,26) prsref(:,:)
  26    format(10f7.2)

        read(NIAERCM,21) cline   ! rh indep ext coef for 61 bands, 6 species
        read(NIAERCM,28) rhidext0(:,:)
  28    format(8e10.3)

        read(NIAERCM,21) cline   ! rh indep sca coef for 61 bands, 6 species
        read(NIAERCM,28) rhidsca0(:,:)

        read(NIAERCM,21) cline   ! rh indep ssa coef for 61 bands, 6 species
        read(NIAERCM,28) rhidssa0(:,:)

        read(NIAERCM,21) cline   ! rh indep asy coef for 61 bands, 6 species
        read(NIAERCM,28) rhidasy0(:,:)

        read(NIAERCM,21) cline   ! rh dep ext coef for 61 bands, 8 rh lev, 4 species
        read(NIAERCM,28) rhdpext0(:,:,:)

        read(NIAERCM,21) cline   ! rh dep sca coef for 61 bands, 8 rh lev, 4 species
        read(NIAERCM,28) rhdpsca0(:,:,:)

        read(NIAERCM,21) cline   ! rh dep ssa coef for 61 bands, 8 rh lev, 4 species
        read(NIAERCM,28) rhdpssa0(:,:,:)

        read(NIAERCM,21) cline   ! rh dep asy coef for 61 bands, 8 rh lev, 4 species
        read(NIAERCM,28) rhdpasy0(:,:,:)

        read(NIAERCM,21) cline   ! stratospheric background aeros for 61 bands
        read(NIAERCM,28) straext0(:)

        lclmin = .false.

!  --- ...  compute solar flux weights and interval indices for mapping
!           spectral bands between sw radiation and aerosol data

        solbnd (:)   = f_zero
        solwaer(:,:) = f_zero

        do ib = 1, NBDSW
          mb = ib + NSWSTR - 1
          ii = 1
          iw1 = nint(wvnum1(mb))
          iw2 = nint(wvnum2(mb))

          Lab_swdowhile : do while ( iw1 > iendwv(ii) )
            if ( ii == NAERBND ) exit Lab_swdowhile
            ii = ii + 1
          enddo  Lab_swdowhile

          sumsol = f_zero
          nv1(ib) = ii

          do iw = iw1, iw2
            solbnd(ib) = solbnd(ib) + solfwv(iw)
            sumsol = sumsol + solfwv(iw)

            if ( iw == iendwv(ii) ) then
              solwaer(ib,ii) = sumsol

              if ( ii < NAERBND ) then
                sumsol = f_zero
                ii = ii + 1
              endif
            endif
          enddo

          if ( iw2 /= iendwv(ii) ) then
            solwaer(ib,ii) = sumsol
          endif

          nv2(ib) = ii
!         frcbnd(ib) = solbnd(ib) / soltot
        enddo     ! end do_ib_block for sw

!  --- ...  compute ir flux weights and interval indices for mapping
!           spectral bands between lw radiation and aerosol data

        eirbnd (:)   = f_zero
        eirwaer(:,:) = f_zero

        do ib = 1, NBDIR
          ii = 1
          if ( NBDIR == 1 ) then
!           iw1 = 250                   ! corresponding 40 mu
            iw1 = 400                   ! corresponding 25 mu
            iw2 = 2500                  ! corresponding 4  mu
          else
            iw1 = nint(wvnlw1(ib))
            iw2 = nint(wvnlw2(ib))
          endif

          Lab_lwdowhile : do while ( iw1 > iendwv(ii) )
            if ( ii == NAERBND ) exit Lab_lwdowhile
            ii = ii + 1
          enddo  Lab_lwdowhile

          sumir = f_zero
          nr1(ib) = ii

          do iw = iw1, iw2
            eirbnd(ib) = eirbnd(ib) + eirfwv(iw)
            sumir  = sumir  + eirfwv(iw)

            if ( iw == iendwv(ii) ) then
              eirwaer(ib,ii) = sumir

              if ( ii < NAERBND ) then
                sumir = f_zero
                ii = ii + 1
              endif
            endif
          enddo

          if ( iw2 /= iendwv(ii) ) then
            eirwaer(ib,ii) = sumir
          endif

          nr2(ib) = ii
        enddo     ! end do_ib_block for lw

!  ---  compute spectral band mean properties for each species

        call optavg
!  ---  inputs:  (in scope variables)
!  ---  outputs: (in scope variables)

!  ---  check print

!       do ib = 1, NBDSW
!         print *,' After optavg, for sw band:',ib
!         print *,'  extrhi:', extrhi(:,ib)
!         print *,'  scarhi:', scarhi(:,ib)
!         print *,'  ssarhi:', ssarhi(:,ib)
!         print *,'  asyrhi:', asyrhi(:,ib)
!         mb = ib + NSWSTR - 1
!         print *,'  wvnum1,wvnum2 :',wvnum1(mb),wvnum2(mb)
!         do i = 1, NRHLEV
!           print *,'  extrhd for rhlev:',i
!           print *,extrhd(i,:,ib)
!           print *,'  scarhd for rhlev:',i
!           print *,scarhd(i,:,ib)
!           print *,'  ssarhd for rhlev:',i
!           print *,ssarhd(i,:,ib)
!           print *,'  asyrhd for rhlev:',i
!           print *,asyrhd(i,:,ib)
!         enddo
!         print *,' extstra:', extstra(ib)
!       enddo
!       print *,'  wvnlw1 :',wvnlw1
!       print *,'  wvnlw2 :',wvnlw2
!       do ib = 1, NBDIR
!         ii = NBDSW + ib
!         print *,' After optavg, for lw band:',ib
!         print *,'  extrhi:', extrhi(:,ii)
!         print *,'  scarhi:', scarhi(:,ii)
!         print *,'  ssarhi:', ssarhi(:,ii)
!         print *,'  asyrhi:', asyrhi(:,ii)
!         do i = 1, NRHLEV
!           print *,'  extrhd for rhlev:',i
!           print *,extrhd(i,:,ii)
!           print *,'  scarhd for rhlev:',i
!           print *,scarhd(i,:,ii)
!           print *,'  ssarhd for rhlev:',i
!           print *,ssarhd(i,:,ii)
!           print *,'  asyrhd for rhlev:',i
!           print *,asyrhd(i,:,ii)
!         enddo
!         print *,' extstra:', extstra(ii)
!       enddo

!  --- ...  dealoocate input data arrays no longer needed

        deallocate ( rhidext0 )
        deallocate ( rhidsca0 )
        deallocate ( rhidssa0 )
        deallocate ( rhidasy0 )
        deallocate ( rhdpext0 )
        deallocate ( rhdpsca0 )
        deallocate ( rhdpssa0 )
        deallocate ( rhdpasy0 )
        deallocate ( straext0 )
        deallocate ( iendwv   )

      endif  ! end if_lclmin_block

! =================
      contains
! =================

!-----------------------------
      subroutine optavg
!.............................
!  ---  inputs:  (in scope variables)
!  ---  outputs: (in scope variables)

! ==================================================================== !
!                                                                      !
! subprogram: optavg                                                   !
!                                                                      !
!   compute mean aerosols optical properties over each sw radiation    !
!   spectral band for each of the species components.  This program    !
!   follows gfdl's approach for thick cloud opertical property in      !
!   sw radiation scheme (2000).                                        !
!                                                                      !
!  ====================  defination of variables  ===================  !
!                                                                      !
! input arguments:                                                     !
!   nv1,nv2 (NBDSW)  - start/end spectral band indices of aerosol data !
!                      for each sw radiation spectral band             !
!   nr1,nr2 (NBDIR)  - start/end spectral band indices of aerosol data !
!                      for each ir radiation spectral band             !
!   solwaer (NBDSW,NAERBND)                                            !
!                    - solar flux weight over each sw radiation band   !
!                      vs each aerosol data spectral band              !
!   eirwaer (NBDIR,NAERBND)                                            !
!                    - ir flux weight over each lw radiation band      !
!                      vs each aerosol data spectral band              !
!   solbnd  (NBDSW)  - solar flux weight over each sw radiation band   !
!   eirbnd  (NBDIR)  - ir flux weight over each lw radiation band      !
!   NBDSW            - total number of sw spectral bands               !
!   NBDIR            - total number of lw spectral bands               !
!   NBDSWLW          - total number of sw+lw spectral bands            !
!                                                                      !
! output arguments: (to module variables)                              !
!                                                                      !
!  ==================================================================  !
!
      implicit none

!  ---  inputs:
!  ---  output:

!  ---  locals:
      real (kind=kind_phys) :: sumk, sums, sumok, sumokg, sumreft,      &
     &       sp, refb, reft, rsolbd, rirbd

      integer :: ib, nb, ni, nh, nc
!
!===> ...  begin here
!
!  --- ...  loop for each sw radiation spectral band

      do nb = 1, NBDSW
        rsolbd = f_one / solbnd(nb)

!  ---  for rh independent aerosol species

        do nc = 1, NCM1
          sumk    = f_zero
          sums    = f_zero
          sumok   = f_zero
          sumokg  = f_zero
          sumreft = f_zero

          do ni = nv1(nb), nv2(nb)
            sp   = sqrt( (f_one - rhidssa0(ni,nc))                      &
     &           / (f_one - rhidssa0(ni,nc)*rhidasy0(ni,nc)) )
            reft = (f_one - sp) / (f_one + sp)
            sumreft = sumreft + reft*solwaer(nb,ni)

            sumk    = sumk    + rhidext0(ni,nc)*solwaer(nb,ni)
            sums    = sums    + rhidsca0(ni,nc)*solwaer(nb,ni)
            sumok   = sumok   + rhidssa0(ni,nc)*solwaer(nb,ni)          &
     &              * rhidext0(ni,nc)
            sumokg  = sumokg  + rhidssa0(ni,nc)*solwaer(nb,ni)          &
     &              * rhidext0(ni,nc)*rhidasy0(ni,nc)
          enddo

          refb = sumreft * rsolbd

          extrhi(nc,nb) = sumk   * rsolbd
          scarhi(nc,nb) = sums   * rsolbd
          asyrhi(nc,nb) = sumokg / (sumok + 1.0e-10)
          ssarhi(nc,nb) = 4.0*refb                                      &
     &         / ( (f_one+refb)**2 - asyrhi(nc,nb)*(f_one-refb)**2 )
        enddo   ! end do_nc_block for rh-ind aeros

!  ---  for rh dependent aerosols species

        do nc = 1, NCM2
          do nh = 1, NRHLEV
            sumk    = f_zero
            sums    = f_zero
            sumok   = f_zero
            sumokg  = f_zero
            sumreft = f_zero

            do ni = nv1(nb), nv2(nb)
              sp   = sqrt( (f_one - rhdpssa0(ni,nh,nc))                 &
     &             / (f_one - rhdpssa0(ni,nh,nc)*rhdpasy0(ni,nh,nc)) )
              reft = (f_one - sp) / (f_one + sp)
              sumreft = sumreft + reft*solwaer(nb,ni)

              sumk    = sumk    + rhdpext0(ni,nh,nc)*solwaer(nb,ni)
              sums    = sums    + rhdpsca0(ni,nh,nc)*solwaer(nb,ni)
              sumok   = sumok   + rhdpssa0(ni,nh,nc)*solwaer(nb,ni)     &
     &                * rhdpext0(ni,nh,nc)
              sumokg  = sumokg  + rhdpssa0(ni,nh,nc)*solwaer(nb,ni)     &
     &                * rhdpext0(ni,nh,nc)*rhdpasy0(ni,nh,nc)
            enddo

            refb = sumreft * rsolbd

            extrhd(nh,nc,nb) = sumk   * rsolbd
            scarhd(nh,nc,nb) = sums   * rsolbd
            asyrhd(nh,nc,nb) = sumokg / (sumok + 1.0e-10)
            ssarhd(nh,nc,nb) = 4.0*refb                                 &
     &         / ( (f_one+refb)**2 - asyrhd(nh,nc,nb)*(f_one-refb)**2 )
          enddo   ! end do_nh_block
        enddo   ! end do_nc_block for rh-dep aeros

!  ---  for stratospheric background aerosols

        sumk = f_zero
        do ni = nv1(nb), nv2(nb)
          sumk = sumk + straext0(ni)*solwaer(nb,ni)
        enddo

        extstra(nb) = sumk * rsolbd

!  ---  check print
!       if ( nb > 6 .and. nb < 10) then
!         print *,' in optavg for sw band',nb
!         print *,'  nv1, nv2:',nv1(nb),nv2(nb)
!         print *,'  solwaer:',solwaer(nb,nv1(nb):nv2(nb))
!         print *,'  extrhi:', extrhi(:,nb)
!         do i = 1, NRHLEV
!           print *,'  extrhd for rhlev:',i
!           print *,extrhd(i,:,nb)
!         enddo
!         print *,'  sumk, rsolbd, extstra:',sumk,rsolbd,extstra(nb)
!       endif

      enddo   !  end do_nb_block for sw

!  --- ...  loop for each lw radiation spectral band

      do nb = 1, NBDIR

        ib = NBDSW + nb
        rirbd = f_one / eirbnd(nb)

!  ---  for rh independent aerosol species

        do nc = 1, NCM1
          sumk    = f_zero
          sums    = f_zero
          sumok   = f_zero
          sumokg  = f_zero
          sumreft = f_zero

          do ni = nr1(nb), nr2(nb)
            sp   = sqrt( (f_one - rhidssa0(ni,nc))                      &
     &           / (f_one - rhidssa0(ni,nc)*rhidasy0(ni,nc)) )
            reft = (f_one - sp) / (f_one + sp)
            sumreft = sumreft + reft*eirwaer(nb,ni)

            sumk    = sumk    + rhidext0(ni,nc)*eirwaer(nb,ni)
            sums    = sums    + rhidsca0(ni,nc)*eirwaer(nb,ni)
            sumok   = sumok   + rhidssa0(ni,nc)*eirwaer(nb,ni)          &
     &              * rhidext0(ni,nc)
            sumokg  = sumokg  + rhidssa0(ni,nc)*eirwaer(nb,ni)          &
     &              * rhidext0(ni,nc)*rhidasy0(ni,nc)
          enddo

          refb = sumreft * rirbd

          extrhi(nc,ib) = sumk   * rirbd
          scarhi(nc,ib) = sums   * rirbd
          asyrhi(nc,ib) = sumokg / (sumok + 1.0e-10)
          ssarhi(nc,ib) = 4.0*refb                                         &
     &         / ( (f_one+refb)**2 - asyrhi(nc,ib)*(f_one-refb)**2 )
        enddo   ! end do_nc_block for rh-ind aeros

!  ---  for rh dependent aerosols species

        do nc = 1, NCM2
          do nh = 1, NRHLEV
            sumk    = f_zero
            sums    = f_zero
            sumok   = f_zero
            sumokg  = f_zero
            sumreft = f_zero

            do ni = nr1(nb), nr2(nb)
              sp   = sqrt( (f_one - rhdpssa0(ni,nh,nc))                 &
     &           / (f_one - rhdpssa0(ni,nh,nc)*rhdpasy0(ni,nh,nc)) )
              reft = (f_one - sp) / (f_one + sp)
              sumreft = sumreft + reft*eirwaer(nb,ni)

              sumk    = sumk    + rhdpext0(ni,nh,nc)*eirwaer(nb,ni)
              sums    = sums    + rhdpsca0(ni,nh,nc)*eirwaer(nb,ni)
              sumok   = sumok   + rhdpssa0(ni,nh,nc)*eirwaer(nb,ni)     &
     &                * rhdpext0(ni,nh,nc)
              sumokg  = sumokg  + rhdpssa0(ni,nh,nc)*eirwaer(nb,ni)     &
     &                * rhdpext0(ni,nh,nc)*rhdpasy0(ni,nh,nc)
            enddo

            refb = sumreft * rirbd

            extrhd(nh,nc,ib) = sumk   * rirbd
            scarhd(nh,nc,ib) = sums   * rirbd
            asyrhd(nh,nc,ib) = sumokg / (sumok + 1.0e-10)
            ssarhd(nh,nc,ib) = 4.0*refb                                    &
     &         / ( (f_one+refb)**2 - asyrhd(nh,nc,ib)*(f_one-refb)**2 )
          enddo   ! end do_nh_block
        enddo   ! end do_nc_block for rh-dep aeros

!  ---  for stratospheric background aerosols

        sumk = f_zero
        do ni = nr1(nb), nr2(nb)
          sumk = sumk + straext0(ni)*eirwaer(nb,ni)
        enddo

        extstra(ib) = sumk * rirbd

!  ---  check print
!       if ( nb >= 1 .and. nb < 5) then
!         print *,' in optavg for ir band:',nb
!         print *,'  nr1, nr2:',nr1(nb),nr2(nb)
!         print *,'  eirwaer:',eirwaer(nb,nr1(nb):nr2(nb))
!         print *,'  extrhi:', extrhi(:,ib)
!         do i = 1, NRHLEV
!           print *,'  extrhd for rhlev:',i
!           print *,extrhd(i,:,ib)
!         enddo
!         print *,'  sumk, rirbd, extstra:',sumk,rirbd,extstra(ib)
!       endif

      enddo   !  end do_nb_block for lw

!
      return
!................................
      end subroutine optavg
!--------------------------------
!
!...................................
      end subroutine clim_aerinit
!-----------------------------------


!-----------------------------------
      subroutine setclimaer                                             &
!...................................

!  ---  inputs:
     &     ( alon,alat,prsi,rhlay,dz,hz,NBDSWLW,                        &
     &       IMAX,NLAY,NLP1, iflip, lsswr,lslwr,                        &
!  ---  outputs:
     &       aerosw,aerolw                                              &
     &     )

!  ==================================================================  !
!                                                                      !
!  setaer maps the 5 degree global climatological aerosol data set     !
!  onto model grids                                                    !
!                                                                      !
!  inputs:                                                             !
!     NBDSW   - total number of sw spectral bands               1      !
!     alon, alat                                             IMAX      !
!             - longitude and latitude of given points in degree       !
!     prsi    - pressure at interface              mb   IMAX*NLP1      !
!     rhlay   - layer mean relative humidity            IMAX*NLAY      !
!     dz      - layer thickness                    m    IMAX*NLAY      !
!     hz      - level high                         m    IMAX*NLP1      !
!     NBDSWLW - total number of sw+ir bands for aeros opt prop  1      !
!     IMAX    - horizontal dimension of arrays                  1      !
!     NLAY,NLP1-vertical dimensions of arrays                   1      !
!     iflip   - control flag for direction of vertical index    1      !
!               =0: index from toa to surface                          !
!               =1: index from surface to toa                          !
!     lsswr,lslwr                                                      !
!             - logical flag for sw/lw radiation calls          1      !
!                                                                      !
!  outputs:                                                            !
!     aerosw - aeros opt properties for sw      IMAX*NLAY*NBDSW*NF_AESW!
!               (:,:,:,1): optical depth                               !
!               (:,:,:,2): single scattering albedo                    !
!               (:,:,:,3): asymmetry parameter                         !
!     aerolw - aeros opt properties for lw      IMAX*NLAY*NBDLW*NF_AELW!
!               (:,:,:,1): optical depth                               !
!               (:,:,:,2): single scattering albedo                    !
!               (:,:,:,3): asymmetry parameter                         !
!                                                                      !
!  module parameters and constants:                                    !
!     NBDSW   - total number of sw bands for aeros opt prop     1      !
!     NBDIR   - total number of ir bands for aeros opt prop     1      !
!                                                                      !
!  module variable: (set by subroutine clim_aerinit)                   !
!     kprfg   - aerosols profile index                IMXAE*JMXAE      !
!     idxcg   - aerosols component index              NXC*IMXAE*JMXAE  !
!     cmixg   - aerosols component mixing ratio       NXC*IMXAE*JMXAE  !
!     denng   - aerosols number density               NXC*IMXAE*JMXAE  !
!                                                                      !
!  usage:    call setclimaer                                           !
!                                                                      !
!  subprograms called:  radclimaer                                     !
!                                                                      !
!  ==================================================================  !
!
      implicit none

!  ---  inputs:
      integer, intent(in) :: IMAX,NLAY,NLP1,iflip,NBDSWLW
      logical, intent(in) :: lsswr, lslwr

      real (kind=kind_phys), dimension(:,:), intent(in) :: prsi,        &
     &       rhlay, dz, hz
      real (kind=kind_phys), dimension(:),   intent(in) :: alon, alat

!  ---  outputs:
      real (kind=kind_phys), dimension(:,:,:,:), intent(out) ::         &
     &       aerosw, aerolw

!  ---  locals:
      real (kind=kind_phys), dimension(NXC)  :: cmix, denn
      integer,               dimension(NXC)  :: idxc

      real (kind=kind_phys), dimension(NLAY) :: delz, rh1, dz1
      integer,               dimension(NLAY) :: idmaer

      real (kind=kind_phys), dimension(NLAY,NBDSWLW):: tauae,ssaae,asyae
!test real (kind=kind_phys), dimension(IMAX,NLAY) :: aersav

      real (kind=kind_phys) :: tmp1, tmp2

      integer               :: i, i1, i2, j1, j2, k, m, m1, kp

!  ---  conversion constants
      real (kind=kind_phys), parameter :: dltg = 360.0 / float(IMXAE)
      real (kind=kind_phys), parameter :: hdlt = 0.5 * dltg

!
!===>  ...  begin here
!
!  ---  map grid in longitude direction

      lab_do_IMAX : do i = 1, IMAX
        i2 = 1
        j2 = 1

        lab_do_IMXAE : do i1 = 1, IMXAE
          tmp1 = dltg * (i1 - 1) + hdlt

          if (abs(alon(i)-tmp1) <= hdlt) then
            i2 = i1
            exit lab_do_IMXAE
          endif
        enddo  lab_do_IMXAE

!  ---  map grid in latitude direction

        lab_do_JMXAE : do j1 = 1, JMXAE
          tmp2 = 90.0 - dltg * (j1 - 1)

          if (abs(alat(i)-tmp2) <= hdlt) then
            j2 = j1
            exit lab_do_JMXAE
          endif
        enddo  lab_do_JMXAE

        do m = 1, NXC
          idxc(m) = idxcg(m,i2,j2)
          cmix(m) = cmixg(m,i2,j2)
          denn(m) = denng(m,i2,j2)
        enddo

        kp = kprfg(i2,j2)

        do k = 1, NLAY
          rh1(k) = rhlay(i,k)
          dz1(k) = dz   (i,k)
        enddo

!  ---  compute vertical domain indices

        lab_if_flip : if (iflip == 1) then        ! input from sfc to toa

!  ---  setup domain index array and effective layer thickness

          i1 = 1
          do k = 1, NLAY
            if (prsi(i,k+1) < prsref(i1,kp)) then
              i1 = i1 + 1
              if (i1 == 2 .and. prsref(2,kp) == prsref(3,kp)) then
                i1 = 3
              endif
            endif
            idmaer(k) = i1

            tmp1 = haer(i1,kp)
            if (tmp1 > f_zero) then
              tmp2 = f_one / tmp1
              delz(k) = tmp1 * (exp(-hz(i,k)*tmp2)-exp(-hz(i,k+1)*tmp2))
            else
              delz(k) = dz1(k)
            endif
          enddo

        else  lab_if_flip                         ! input from toa to sfc

!  ---  setup domain index array and modified layer thickness

          i1 = 1
          do k = NLAY, 1, -1
            if (prsi(i,k) < prsref(i1,kp)) then
              i1 = i1 + 1
              if (i1 == 2 .and. prsref(2,kp) == prsref(3,kp)) then
                i1 = 3
              endif
            endif
            idmaer(k) = i1

            tmp1 = haer(i1,kp)
            if (tmp1 > f_zero) then
              tmp2   = f_one / tmp1
              delz(k) = tmp1 * (exp(-hz(i,k+1)*tmp2)-exp(-hz(i,k)*tmp2))
            else
              delz(k) = dz1(k)
            endif
          enddo

        endif  lab_if_flip

!  ---  check print

!       print *,' in setclimaer, profile:',i
!       print *,'  rh   :',rh1
!       print *,'  dz   :',dz1
!       print *,'  delz :',delz
!       print *,'  idmaer:',idmaer

!  ---  calculate sw/lw aerosol optical properties for the
!       corresponding frequency bands

        call radclimaer
!  ---  inputs:  (in scope variables)
!  ---  outputs: (in scope variables)

        if ( lsswr ) then

          if ( laswflg ) then

            do m = 1, NBDSW
              do k = 1, NLAY
                aerosw(i,k,m,1) = tauae(k,m)
                aerosw(i,k,m,2) = ssaae(k,m)
                aerosw(i,k,m,3) = asyae(k,m)
              enddo
            enddo

          else

            aerosw(:,:,:,:) = f_zero

          endif

!  ---  testing use
!         do k = 1, NLAY
!           aersav(i,k) = tauae(k,1)
!test     enddo
        endif     ! end if_lsswr_block

        if ( lslwr ) then

          if ( lalwflg ) then

            if ( NBDIR == 1 ) then
              m1 = NBDSW + 1
              do m = 1, NBDLW
                do k = 1, NLAY
                  aerolw(i,k,m,1) = tauae(k,m1)
                  aerolw(i,k,m,2) = ssaae(k,m1)
                  aerolw(i,k,m,3) = asyae(k,m1)
                enddo
              enddo
            else
              do m = 1, NBDLW
                m1 = NBDSW + m
                do k = 1, NLAY
                  aerolw(i,k,m,1) = tauae(k,m1)
                  aerolw(i,k,m,2) = ssaae(k,m1)
                  aerolw(i,k,m,3) = asyae(k,m1)
                enddo
              enddo
            endif

          else

            aerolw(:,:,:,:) = f_zero

          endif
        endif     ! end if_lslwr_block

      enddo  lab_do_IMAX

! =================
      contains
! =================

!--------------------------------
      subroutine radclimaer
!................................

!  ---  inputs:  (in scope variables)
!  ---  outputs: (in scope variables)

!  ==================================================================  !
!                                                                      !
!  compute aerosols optical properties in NBDSW sw bands. there are    !
!  seven different vertical profile structures. in the troposphere,    !
!  aerosol distribution at each grid point is composed from up to      !
!  six components out of a total of ten different substances.          !
!                                                                      !
!  ref: wmo report wcp-112 (1986)                                      !
!                                                                      !
!  input variables:                                                    !
!     idxc   - indices of aerosol components         -     NXC         !
!     cmix   - mixing ratioes of aerosol components  -     NXC         !
!     denn   - aerosol number densities              -     NXC         !
!     rh1    - relative humidity                     -     NLAY        !
!     delz   - effective layer thickness             km    NLAY        !
!     idmaer - aerosol domain index                  -     NLAY        !
!     NXC    - number of different aerosol components-     1           !
!     NLAY   - vertical dimensions                   -     1           !
!     iflip  - control flag for direction of vertical index            !
!               =0: index from toa to surface                          !
!               =1: index from surface to toa                          !
!                                                                      !
!  output variables:                                                   !
!     tauae  - optical depth                         -     NLAY*NBDSW  !
!     ssaae  - single scattering albedo              -     NLAY*NBDSW  !
!     asyae  - asymmetry parameter                   -     NLAY*NBDSW  !
!                                                                      !
!  ==================================================================  !
!
      implicit none

!
      real (kind=kind_phys) :: crt1, crt2
      parameter (crt1=30.0, crt2=0.03333)

!  ---  inputs:
!  ---  outputs:

!  ---  locals:
      real (kind=kind_phys) :: cm, hd, hdi, sig0u, sig0l, ratio, tt0,   &
     &      ex00, sc00, ss00, as00, ex01, sc01, ss01, as01,     tt1,    &
     &      ex02, sc02, ss02, as02, ex03, sc03, ss03, as03,     tt2,    &
     &      ext1, sca1, ssa1, asy1, drh0, drh1, rdrh

      integer :: ih1, ih2, kk, idom, icmp, ib, ii, ic, ic1

!
!===> ... loop over vertical layers from top to surface
!
      lab_do_layer : do kk = 1, NLAY

! --- linear interp coeffs for rh-dep species

        ih2 = 1
        do while ( rh1(kk) > rhlev(ih2) )
          ih2 = ih2 + 1
          if ( ih2 > NRHLEV ) exit
        enddo
        ih1 = max( 1, ih2-1 )
        ih2 = min( NRHLEV, ih2 )

        drh0 = rhlev(ih2) - rhlev(ih1)
        drh1 = rh1(kk) - rhlev(ih1)
        if ( ih1 == ih2 ) then
          rdrh = f_zero
        else
          rdrh = drh1 / drh0
        endif

! --- assign optical properties in each domain

        idom = idmaer(kk)

        lab_if_idom : if (idom == 5) then
! --- 5th domain - upper stratosphere assume no aerosol

          do ib = 1, NBDSWLW
            tauae(kk,ib) = f_zero
            if ( ib <= NBDSW ) then
              ssaae(kk,ib) = 0.99
              asyae(kk,ib) = 0.696
            else
              ssaae(kk,ib) = 0.5
              asyae(kk,ib) = 0.3
            endif
          enddo

        elseif (idom == 4) then    lab_if_idom
! --- 4th domain - stratospheric layers

          do ib = 1, NBDSWLW
            tauae(kk,ib) = extstra(ib) * delz(kk)
            if ( ib <= NBDSW ) then
              ssaae(kk,ib) = 0.99
              asyae(kk,ib) = 0.696
            else
              ssaae(kk,ib) = 0.5
              asyae(kk,ib) = 0.3
            endif
          enddo

        elseif (idom == 3) then    lab_if_idom
! --- 3rd domain - free tropospheric layers
!   1:inso 0.17e-3; 2:soot 0.4; 7:waso 0.59983; n:730

          do ib = 1, NBDSWLW
            ex01 = extrhi(1,ib)
            sc01 = scarhi(1,ib)
            ss01 = ssarhi(1,ib)
            as01 = asyrhi(1,ib)

            ex02 = extrhi(2,ib)
            sc02 = scarhi(2,ib)
            ss02 = ssarhi(2,ib)
            as02 = asyrhi(2,ib)

            ex03 = extrhd(ih1,1,ib)                                     &
     &           + rdrh * (extrhd(ih2,1,ib) - extrhd(ih1,1,ib))
            sc03 = scarhd(ih1,1,ib)                                     &
     &           + rdrh * (scarhd(ih2,1,ib) - scarhd(ih1,1,ib))
            ss03 = ssarhd(ih1,1,ib)                                     &
     &           + rdrh * (ssarhd(ih2,1,ib) - ssarhd(ih1,1,ib))
            as03 = asyrhd(ih1,1,ib)                                     &
     &           + rdrh * (asyrhd(ih2,1,ib) - asyrhd(ih1,1,ib))

            ext1 = 0.17e-3*ex01 + 0.4*ex02 + 0.59983*ex03
            sca1 = 0.17e-3*sc01 + 0.4*sc02 + 0.59983*sc03
            ssa1 = 0.17e-3*ss01*ex01 + 0.4*ss02*ex02 + 0.59983*ss03*ex03
            asy1 = 0.17e-3*as01*sc01 + 0.4*as02*sc02 + 0.59983*as03*sc03

            tauae(kk,ib) = ext1 * 730.0 * delz(kk)
            ssaae(kk,ib) = min(f_one, ssa1/ext1)
            asyae(kk,ib) = min(f_one, asy1/sca1)
          enddo

        elseif (idom == 1) then    lab_if_idom
! --- 1st domain - mixing layer

          lab_do_ib : do ib = 1, NBDSWLW
            ext1 = f_zero
            sca1 = f_zero
            ssa1 = f_zero
            asy1 = f_zero

            lab_do_icmp : do icmp = 1, NXC
              ic = idxc(icmp)
              cm = cmix(icmp)

              lab_if_ic : if (ic > NCM1) then
                ic1 = ic - NCM1

                ex00 = extrhd(ih1,ic1,ib)                               &
     &               + rdrh * (extrhd(ih2,ic1,ib) - extrhd(ih1,ic1,ib))
                sc00 = scarhd(ih1,ic1,ib)                               &
     &               + rdrh * (scarhd(ih2,ic1,ib) - scarhd(ih1,ic1,ib))
                ss00 = ssarhd(ih1,ic1,ib)                               &
     &               + rdrh * (ssarhd(ih2,ic1,ib) - ssarhd(ih1,ic1,ib))
                as00 = asyrhd(ih1,ic1,ib)                               &
     &               + rdrh * (asyrhd(ih2,ic1,ib) - asyrhd(ih1,ic1,ib))

                ext1 = ext1 + cm * ex00
                sca1 = sca1 + cm * sc00
                ssa1 = ssa1 + cm * ss00 * ex00
                asy1 = asy1 + cm * as00 * sc00
              else if (ic > 0) then     lab_if_ic
                ext1 = ext1 + cm * extrhi(ic,ib)
                sca1 = sca1 + cm * scarhi(ic,ib)
                ssa1 = ssa1 + cm * ssarhi(ic,ib) * extrhi(ic,ib)
                asy1 = asy1 + cm * asyrhi(ic,ib) * scarhi(ic,ib)
              endif  lab_if_ic

            enddo  lab_do_icmp

            tauae(kk,ib) = ext1 * denn(1) * delz(kk)
            ssaae(kk,ib) = min(f_one, ssa1/ext1)
            asyae(kk,ib) = min(f_one, asy1/sca1)
          enddo  lab_do_ib

        elseif (idom == 2) then    lab_if_idom
! --- 2nd domain - mineral transport layers

          do ib = 1, NBDSWLW
            tauae(kk,ib) = extrhi(6,ib) * denn(2) * delz(kk)
            ssaae(kk,ib) = ssarhi(6,ib)
            asyae(kk,ib) = asyrhi(6,ib)
          enddo

        else  lab_if_idom
! --- domain index out off range, assume no aerosol

          do ib = 1, NBDSWLW
            tauae(kk,ib) = f_zero
            ssaae(kk,ib) = f_one
            asyae(kk,ib) = f_zero
          enddo

!         write(6,19) kk,idom
! 19      format(/'  ***  ERROR in sub AEROS: domain index out'         &
!    &,            ' of range!  K, IDOM =',3i5,' ***')
!         stop 19

        endif  lab_if_idom

      enddo  lab_do_layer
!
!===> ... smooth profile at domain boundaries
!
      if ( iflip == 0 ) then      ! input from toa to sfc

        do ib = 1, NBDSWLW
        do kk = 2, NLAY
          if ( tauae(kk,ib) > f_zero ) then
            ratio = tauae(kk-1,ib) / tauae(kk,ib)
          else
            ratio = f_one
          endif

          tt0 = tauae(kk,ib) + tauae(kk-1,ib)
          tt1 = 0.2 * tt0
          tt2 = tt0 - tt1

          if ( ratio > crt1 ) then
            tauae(kk,ib)   = tt1
            tauae(kk-1,ib) = tt2
          endif

          if ( ratio < crt2 ) then
            tauae(kk,ib)   = tt2
            tauae(kk-1,ib) = tt1
          endif
        enddo   ! do_kk_loop
        enddo   ! do_ib_loop

      else                      ! input from sfc to toa

        do ib = 1, NBDSWLW
        do kk = NLAY-1, 1, -1
          if ( tauae(kk,ib) > f_zero ) then
            ratio = tauae(kk+1,ib) / tauae(kk,ib)
          else
            ratio = f_one
          endif

          tt0 = tauae(kk,ib) + tauae(kk+1,ib)
          tt1 = 0.2 * tt0
          tt2 = tt0 - tt1

          if ( ratio > crt1 ) then
            tauae(kk,ib)   = tt1
            tauae(kk+1,ib) = tt2
          endif

          if ( ratio < crt2 ) then
            tauae(kk,ib)   = tt2
            tauae(kk+1,ib) = tt1
          endif
        enddo   ! do_kk_loop
        enddo   ! do_ib_loop

      endif

!
      return
!................................
      end subroutine radclimaer
!--------------------------------
!
!...................................
      end subroutine setclimaer
!-----------------------------------


!..........................................!
      end module module_radiation_aerosols !
!==========================================!
