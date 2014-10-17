!!!!!  ==========================================================  !!!!!
!!!!!              rrtm1 radiation package description             !!!!!
!!!!!  ==========================================================  !!!!!
!                                                                      !
!    the rrtm1 package includes these parts:                           !
!                                                                      !
!       'radlw_rrtm1_param.f'                                          !
!       'radlw_rrtm1_datatb.f'                                         !
!       'radlw_rrtm1_main.f'                                           !
!                                                                      !
!    the 'radlw_rrtm1_param.f' contains:                               !
!                                                                      !
!       'module_radlw_cntr_para'   -- control parameters set up        !
!       'module_radlw_parameters'  -- band parameters set up           !
!                                                                      !
!    the 'radlw_rrtm1_datatb.f' contains:                              !
!                                                                      !
!       'module_radlw_avplank'     -- plank flux data                  !
!       'module_radlw_cldprlw'     -- cloud property coefficients      !
!       'module_radlw_kgbnn'       -- absorption coeffients for 16     !
!                                     bands, where nn = 01-16          !
!                                                                      !
!    the 'radlw_rrtm1_main.f' contains:                                !
!                                                                      !
!       'module_radlw_main'        -- main lw radiation transfer       !
!                                                                      !
!    in the main module 'module_radlw_main' there are only two         !
!    externally callable subroutines:                                  !
!                                                                      !
!                                                                      !
!       'lwrad'     -- main rrtm1 lw radiation routine                 !
!          inputs:                                                     !
!           (pmid,pint,tmid,tint,qnm,o3mr,gasvmr,                      !
!            clouds,iovr,aerosols,sfemis,                              !
!            NPTS, NLAY, NLP1, iflip, lprnt,                           !
!          outputs:                                                    !
!            hlwc,topflx,sfcflx,                                       !
!!         optional outputs:                                           !
!            HLW0,HLWB,FLXPRF)                                         !
!                                                                      !
!       'rlwinit'   -- initialization routine                          !
!          inputs:                                                     !
!           ( icwp, me, NLAY )                                         !
!          outputs:                                                    !
!           (none)                                                     !
!                                                                      !
!    all the lw radiation subprograms become contained subprograms     !
!    in module 'module_radlw_main' and many of them are not directly   !
!    accessable from places outside the module.                        !
!                                                                      !
!                                                                      !
!    derived data type constructs used:                                !
!                                                                      !
!     1. radiation flux at toa: (from module 'module_radlw_parameters')!
!          topflw_type   -  derived data type for toa rad fluxes       !
!            upfxc              total sky upward flux at toa           !
!            upfx0              clear sky upward flux at toa           !
!                                                                      !
!     2. radiation flux at sfc: (from module 'module_radlw_parameters')!
!          sfcflw_type   -  derived data type for sfc rad fluxes       !
!            upfxc              total sky upward flux at sfc           !
!            upfx0              clear sky upward flux at sfc           !
!            dnfxc              total sky downward flux at sfc         !
!            dnfx0              clear sky downward flux at sfc         !
!                                                                      !
!     3. radiation flux profiles(from module 'module_radlw_parameters')!
!          proflw_type    -  derived data type for rad vertical prof   !
!            upfxc              level upward flux for total sky        !
!            dnfxc              level downward flux for total sky      !
!            upfx0              level upward flux for clear sky        !
!            dnfx0              level downward flux for clear sky      !
!                                                                      !
!    external modules referenced:                                      !
!                                                                      !
!       'module machine'                                               !
!       'module physcons'                                              !
!                                                                      !
!    compilation sequence is:                                          !
!                                                                      !
!       'radlw_rrtm1_param.f'                                          !
!       'radlw_rrtm1_datatb.f'                                         !
!       'radlw_rrtm1_main.f'                                           !
!                                                                      !
!    and all should be put in front of routines that use lw modules    !
!                                                                      !
!                                                                      !
!                                                                      !
!    the original program declarations:                                !
!                                                                      !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                      !
! Copyright 2002, 2003, Atmospheric & Environmental Research, Inc.(AER)!
! This software may be used, copied, or redistributed as long as it is !
! not sold and this copyright notice is reproduced on each copy made.  !
! This model is provided as is without any express or implied warranties
!                      (http://www.rtweb.aer.com/)                     !
!                                                                      !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                      !
!                               rrtm                                   !
!                                                                      !
!                   rapid radiative transfer model                     !
!                                                                      !
!            atmospheric and environmental research, inc.              !
!                        840 memorial drive                            !
!                        cambridge, ma 02139                           !
!                                                                      !
!                           eli j. mlawer                              !
!                         steven j. taubman~                           !
!                         shepard a. clough                            !
!                                                                      !
!                         ~currently at gfdl                           !
!                                                                      !
!                       email:  mlawer@aer.com                         !
!                                                                      !
!        the authors wish to acknowledge the contributions of the      !
!        following people:  patrick d. brown, michael j. iacono,       !
!        ronald e. farren, luke chen, robert bergstrom.                !
!                                                                      !
!                                                                      !
!                                                                      !
!    ncep modifications history log:                                   !
!                                                                      !
!       nov 1999,  ken campana  -- received the original code from aer !
!                  updated to link up with ncep mrf model              !
!       jun 2000,  ken campana                                         !
!                  added option to call aer max/ran overlap            !
!           2001,  shrinivas moorthi                                   !
!                  further updates for mrf model                       !
!       may 2001,  yu-tai hou                                          !
!                  updated on trace gases and cloud property based on  !
!                  rrtm_v3.0 codes                                     !
!       dec 2001,  yu-tai hou                                          !
!                  rewritten code into fortran 90                      !
!       jun 2004,  yu-tai hou                                          !
!                  add mike iacono's apr 2004 modification of variable !
!                  diffusivity angle                                   !
!       apr 2005,  yu-tai hou                                          !
!                  minor modifications on module structures            !
!       mar 2007,  yu-tai hou                                          !
!                  add aerosol effect for lw radiation                 !
!       apr 2007,  yu-tai hou                                          !
!                  add spectral band heating as optional output        !
!                                                                      !
!                                                                      !
!                                                                      !
!!!!!  ==========================================================  !!!!!
!!!!!                       end descriptions                       !!!!!
!!!!!  ==========================================================  !!!!!



!========================================!
      module module_radlw_main           !
!........................................!
!
      use machine,           only : kind_phys
      use physcons,          only : con_g, con_cp, con_avgd, con_amd,   &
     &                              con_amw, con_amo3

      use module_radlw_parameters
      use module_radlw_cntr_para
!
      implicit none
!
      private
!
!  ...  version tag and last revision date
!     character(24), parameter :: VTAGLW='RRTM-LW v2.3g   Apr 2004'
!     character(24), parameter :: VTAGLW='RRTM-LW v2.3g   Mar 2007'
      character(24), parameter :: VTAGLW='RRTM-LW v2.3g   Apr 2007'

!  ---  constant values
      real (kind=kind_phys) :: eps, oneminus, bpade, stpfac, wtnum      &
     &,     co2fac, f_zero

      parameter (eps=1.0e-6,  oneminus=1.0-eps)
      parameter (bpade=1.0/0.278)      ! pade approximation constant
      parameter (stpfac=296./1013.)
      parameter (wtnum=0.5)
!     parameter (avgdro=6.022e23)      ! avogadro constant  (1/mol)
!mji  parameter (secang=1.66)          ! diffusivity angle
      parameter (co2fac=3.55e-4)       ! factor for cal. of co2mult
      parameter (f_zero=0.0)

!  ...  atomic weights for conversion from mass to volume mixing ratios
      real (kind=kind_phys) :: amdw, amdo3

      parameter (amdw =con_amd/con_amw)
      parameter (amdo3=con_amd/con_amo3)

!  ...  band indices
      integer :: nspa(NBANDS), nspb(NBANDS), ngb(NGPT)

      data nspa / 1, 1,10, 9, 9, 1, 9, 1,11, 1, 1, 9, 9, 1, 9, 9 /
      data nspb / 1, 1, 5, 6, 5, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0 /
      data ngb  /  8*1, 14*2, 16*3, 14*4, 16*5,  8*6, 12*7,  8*8,       &
     &            12*9, 6*10, 8*11, 8*12, 4*13, 2*14, 2*15, 2*16 /

!  ...  band wavenumber intervals
!     real (kind=kind_phys) :: wavenum1(NBANDS), wavenum2(NBANDS)
!     data wavenum1/                                                    &
!    &         10.,  250.,  500.,  630.,  700.,  820.,  980., 1080.,    &
!    &       1180., 1390., 1480., 1800., 2080., 2250., 2380., 2600. /
!     data wavenum2/                                                    &
!    &        250.,  500.,  630.,  700.,  820.,  980., 1080., 1180.,    &
!    &       1390., 1480., 1800., 2080., 2250., 2380., 2600., 3000. /
      real (kind=kind_phys) :: delwave(NBANDS)
      data delwave / 240., 250., 130.,  70., 120., 160., 100., 100.,    &
     &               210.,  90., 320., 280., 170., 130., 220., 400. /

!mji ... coefficients for variable diffusivity angle
      real (kind=kind_phys), dimension(NBANDS) :: a0, a1, a2
      data a0 /  1.66, 1.55, 1.58, 1.66, 1.54,1.454, 1.89, 1.33,        &
     &          1.668, 1.66, 1.66, 1.66, 1.66, 1.66, 1.66, 1.66  /
      data a1 /  0.00, 0.25, 0.22, 0.00, 0.13,0.446,-0.10, 0.40,        &
     &         -0.006, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00  /
      data a2 /  0.00,-12.0,-11.7, 0.00,-0.72,-0.243,0.19,-0.062,       &
     &          0.414, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00  /

!  ---  reference pressure and temperature
      real (kind=kind_phys), dimension(59) :: pref, preflog, tref

!  ...  these pressures are chosen such that the ln of the first one
!       has only a few non-zero digits (i.e. ln(pref(1)) = 6.96000) and
!       each subsequent ln(pref) differs from the previous one by 0.2.
      data pref /                                                       &
     &    1.05363e+03,8.62642e+02,7.06272e+02,5.78246e+02,4.73428e+02,  &
     &    3.87610e+02,3.17348e+02,2.59823e+02,2.12725e+02,1.74164e+02,  &
     &    1.42594e+02,1.16746e+02,9.55835e+01,7.82571e+01,6.40715e+01,  &
     &    5.24573e+01,4.29484e+01,3.51632e+01,2.87892e+01,2.35706e+01,  &
     &    1.92980e+01,1.57998e+01,1.29358e+01,1.05910e+01,8.67114e+00,  &
     &    7.09933e+00,5.81244e+00,4.75882e+00,3.89619e+00,3.18993e+00,  &
     &    2.61170e+00,2.13828e+00,1.75067e+00,1.43333e+00,1.17351e+00,  &
     &    9.60789e-01,7.86628e-01,6.44036e-01,5.27292e-01,4.31710e-01,  &
     &    3.53455e-01,2.89384e-01,2.36928e-01,1.93980e-01,1.58817e-01,  &
     &    1.30029e-01,1.06458e-01,8.71608e-02,7.13612e-02,5.84256e-02,  &
     &    4.78349e-02,3.91639e-02,3.20647e-02,2.62523e-02,2.14936e-02,  &
     &    1.75975e-02,1.44076e-02,1.17959e-02,9.65769e-03 /
      data preflog /                                                    &
     &     6.9600e+00, 6.7600e+00, 6.5600e+00, 6.3600e+00, 6.1600e+00,  &
     &     5.9600e+00, 5.7600e+00, 5.5600e+00, 5.3600e+00, 5.1600e+00,  &
     &     4.9600e+00, 4.7600e+00, 4.5600e+00, 4.3600e+00, 4.1600e+00,  &
     &     3.9600e+00, 3.7600e+00, 3.5600e+00, 3.3600e+00, 3.1600e+00,  &
     &     2.9600e+00, 2.7600e+00, 2.5600e+00, 2.3600e+00, 2.1600e+00,  &
     &     1.9600e+00, 1.7600e+00, 1.5600e+00, 1.3600e+00, 1.1600e+00,  &
     &     9.6000e-01, 7.6000e-01, 5.6000e-01, 3.6000e-01, 1.6000e-01,  &
     &    -4.0000e-02,-2.4000e-01,-4.4000e-01,-6.4000e-01,-8.4000e-01,  &
     &    -1.0400e+00,-1.2400e+00,-1.4400e+00,-1.6400e+00,-1.8400e+00,  &
     &    -2.0400e+00,-2.2400e+00,-2.4400e+00,-2.6400e+00,-2.8400e+00,  &
     &    -3.0400e+00,-3.2400e+00,-3.4400e+00,-3.6400e+00,-3.8400e+00,  &
     &    -4.0400e+00,-4.2400e+00,-4.4400e+00,-4.6400e+00 /
!  ...  these are the temperatures associated with the respective
!       pressures for the MLS standard atmosphere.
      data tref /                                                       &
     &     2.9420E+02, 2.8799E+02, 2.7894E+02, 2.6925E+02, 2.5983E+02,  &
     &     2.5017E+02, 2.4077E+02, 2.3179E+02, 2.2306E+02, 2.1578E+02,  &
     &     2.1570E+02, 2.1570E+02, 2.1570E+02, 2.1706E+02, 2.1858E+02,  &
     &     2.2018E+02, 2.2174E+02, 2.2328E+02, 2.2479E+02, 2.2655E+02,  &
     &     2.2834E+02, 2.3113E+02, 2.3401E+02, 2.3703E+02, 2.4022E+02,  &
     &     2.4371E+02, 2.4726E+02, 2.5085E+02, 2.5457E+02, 2.5832E+02,  &
     &     2.6216E+02, 2.6606E+02, 2.6999E+02, 2.7340E+02, 2.7536E+02,  &
     &     2.7568E+02, 2.7372E+02, 2.7163E+02, 2.6955E+02, 2.6593E+02,  &
     &     2.6211E+02, 2.5828E+02, 2.5360E+02, 2.4854E+02, 2.4348E+02,  &
     &     2.3809E+02, 2.3206E+02, 2.2603E+02, 2.2000E+02, 2.1435E+02,  &
     &     2.0887E+02, 2.0340E+02, 1.9792E+02, 1.9290E+02, 1.8809E+02,  &
     &     1.8329E+02, 1.7849E+02, 1.7394E+02, 1.7212E+02 /

!! ---  logical flags for optional output fields

      logical :: lhlwb  = .false.
      logical :: lhlw0  = .false.
      logical :: lflxprf= .false.

!  ---  those data will be set up only once by "rlwinit"

!  ...  fluxfac, heatfac are factors for fluxes (in w/m**2) and heating
!       rates (in k/day, or k/sec set by subroutine 'rlwinit')
!       semiss0 are default surface emissivity for each bands

      real (kind=kind_phys) :: fluxfac, heatfac, semiss0(NBANDS)

      real (kind=kind_phys), dimension(0:N5000) :: tau, tf, trans
      real (kind=kind_phys), dimension(0:N200 ) :: corr1, corr2

      public lwrad, rlwinit


! =================
      contains
! =================

!-----------------------------------
      subroutine lwrad                                                  &
!...................................

!  ---  inputs:
     &     ( pmid,pint,tmid,tint,qnm,o3mr,gasvmr,                       &
     &       clouds,iovr,aerosols,sfemis,                               &
     &       NPTS, NLAY, NLP1, iflip, lprnt,                            &
!  ---  outputs:
     &       hlwc,topflx,sfcflx                                         &
!! ---  optional:
     &,      HLW0,HLWB,FLXPRF                                           &
     &     )

!  ====================  defination of variables  ===================  !
!                                                                      !
!  input variables:                                                    !
!     pmid   (NPTS,NLAY)    - layer pressures (mb)                     !
!     pint   (NPTS,NLP1)    - interface pressures (mb)                 !
!     tmid   (NPTS,NLAY)    - layer temperature (k)                    !
!     tint   (NPTS,NLP1)    - interface temperatures (k)               !
!     qnm    (NPTS,NLAY)    - layer h2o mixing ratio (gm/gm)*see inside!
!     o3mr   (NPTS,NLAY)    - layer o3 mixing ratio (gm/gm) *see inside!
!     gasvmr (NPTS,NLAY,:)  - atmospheric gases amount:                !
!                       (check module_radiation_gases for definition)  !
!       gasvmr(:,:,1)   -      co2 volume mixing ratio                 !
!       gasvmr(:,:,2)   -      n2o volume mixing ratio                 !
!       gasvmr(:,:,3)   -      ch4 volume mixing ratio                 !
!       gasvmr(:,:,4)   -      o2  volume mixing ratio                 !
!       gasvmr(:,:,5)   -      co  volume mixing ratio                 !
!       gasvmr(:,:,6)   -      cfc11 volume mixing ratio               !
!       gasvmr(:,:,7)   -      cfc12 volume mixing ratio               !
!       gasvmr(:,:,8)   -      cfc22 volume mixing ratio               !
!       gasvmr(:,:,9)   -      ccl4  volume mixing ratio               !
!     clouds (NPTS,NLAY,:)  - layer cloud profiles:                    !
!                       (check module_radiation_clouds for definition) !
!                ---  for  iflagliq > 0  ---                           !
!       clouds(:,:,1)  -   layer total cloud fraction                  !
!       clouds(:,:,2)  -   layer cloud liq water path      (g/m**2)    !
!       clouds(:,:,3)  -   mean eff radius for liq cloud   (micron)    !
!       clouds(:,:,4)  -   layer cloud ice water path      (g/m**2)    !
!       clouds(:,:,5)  -   mean eff radius for ice cloud   (micron)    !
!       clouds(:,:,6)  -   layer rain drop water path      (g/m**2)    !
!       clouds(:,:,7)  -   mean eff radius for rain drop   (micron)    !
!       clouds(:,:,8)  -   layer snow flake water path     (g/m**2)    !
!   ** fu's scheme need to be normalized by snow density (g/m**3/1.0e6)!
!       clouds(:,:,9)  -   mean eff radius for snow flake  (micron)    !
!                ---  for  iflagliq = 0  ---                           !
!       clouds(:,:,1)  -   layer total cloud fraction                  !
!       clouds(:,:,2)  -   layer cloud optical depth                   !
!       clouds(:,:,3)  -   layer cloud single scattering albedo        !
!       clouds(:,:,4)  -   layer cloud asymmetry factor                !
!     iovr                  - control flag for cloud overlapping       !
!                             =0: random overlapping clouds            !
!                             =1: max/ran overlapping clouds           !
!     aerosols(NPTS,NLAY,NBANDS,:) - aerosol optical properties        !
!                       (check module_radiation_aerosols for definition!
!        (:,:,:,1)          - optical depth                            !
!        (:,:,:,2)          - single scattering albedo                 !
!        (:,:,:,3)          - asymmetry parameter                      !
!     sfemis (NPTS)         - surface emissivity                       !
!     NPTS                  - total number of horizontal points        !
!     NLAY,NLP1             - total number of vertical layers, levels  !
!     iflip                 - control flag for in/out vertical index   !
!                             =0: index from toa to surface            !
!                             =1: index from surface to toa            !
!     lprnt                 - cntl flag for diagnostic print out       !
!                                                                      !
!  control parameters in module "module_radlw_cntr_para":              !
!     ilwrate               - heating rate unit selections             !
!                             =1: output in k/day                      !
!                             =2: output in k/second                   !
!     iaerlw                - control flag for aerosols                !
!                             =0: do not include aerosol effect        !
!                             >0: include aerosol effect               !
!     irgaslw               - control flag for rare gases              !
!                             (ch4,n2o,o2, etc.)                       !
!                             =0: do not include rare gases            !
!                             =1: include all rare gases               !
!     icfclw                - control flag for cfc gases               !
!                             =0: do not include cfc gases             !
!                             =1: include all cfc gases                !
!     iflagliq              - liq-cloud optical properties contrl flag !
!                             =0: input cld opt dep, ignor iflagice    !
!                             =1: input cwp,cip, (ccm2) ignor iflagice !
!                             =2: input cwp rew, (ccm3 method)         !
!                             =3: input cwp rew, hu and stamnes (1993) !
!     iflagice              - ice-cloud optical properties contrl flag !
!                       * * * if iflagliq .lt. 2, iflafice is ignored  !
!                             =0: input cip rei, (ccm3 method)         !
!                             =1: input cip rei, ebert and curry (1997)!
!                             =2: input cip rei, streamer (1996)       !
!                                                                      !
!  output variables:                                                   !
!     hlwc   (NPTS,NLAY)    - total sky heating rate (k/day or k/sec)  !
!     topflx (NPTS)         - radiation fluxes at top, component:      !
!                        (check module_radlw_paramters for definition) !
!        upfxc                 total sky upward flux at top (w/m2)     !
!        upfx0                 clear sky upward flux at top (w/m2)     !
!     sfcflx (NPTS)         - radiation fluxes at sfc, component:      !
!                        (check module_radlw_paramters for definition) !
!        upfxc                 total sky upward flux at sfc (w/m2)     !
!        upfx0                 clear sky upward flux at sfc (w/m2)     !
!        dnfxc                 total sky downward flux at sfc (w/m2)   !
!        dnfx0                 clear sky downward flux at sfc (w/m2)   !
!                                                                      !
!! optional output variables:                                          !
!     hlwb(NPTS,NLAY,NBANDS)- spectral band total sky heating rates    !
!     hlw0   (NPTS,NLAY)    - clear sky heating rate (k/day or k/sec)  !
!     flxprf (NPTS,NLP1)    - level radiative fluxes (w/m2), components!
!                        (check module_radlw_paramters for definition) !
!        upfxc                 total sky upward flux                   !
!        dnfxc                 total sky dnward flux                   !
!        upfx0                 clear sky upward flux                   !
!        dnfx0                 clear sky dnward flux                   !
!                                                                      !
!  module parameters, control and local variables:                     !
!     NBANDS                - number of longwave spectral bands        !
!     MAXGAS                - maximum number of absorbing gaseous      !
!     MAXXSEC               - maximum number of cross-sections         !
!     NGPT                  - total number of g-point subintervals     !
!     NGnn   (nn=1-16)      - number of g-points in band nn            !
!     nspa,nspb(NBANDS)     - number of lower/upper ref atm's per band !
!     delwave(NBANDS)       - longwave band width (wavenumbers)        !
!     bpade                 - pade approximation constant (1/0.278)    !
!     pavel  (NLAY)         - layer pressures (mb)                     !
!     delp   (NLAY)         - layer pressure thickness (mb)            !
!     tavel  (NLAY)         - layer temperatures (k)                   !
!     tz     (0:NLAY)       - level (interface) temperatures (k)       !
!     semiss (NBANDS)       - surface emissivity for each band         !
!     wx     (NLAY,MAXXSEC) - cross-section molecules concentration    !
!     coldry (NLAY)         - dry air column amount                    !
!                                   (1.e-20*molecules/cm**2)           !
!     cldfrac(0:NLP1)       - layer cloud fraction                     !
!     taucloud(NBANDS,NLAY) - layer cloud optical depth for each band  !
!     taug   (NGPT,NLAY)    - gaseous optical depths                   !
!     pfrac  (NGPT,NLAY)    - planck fractions                         !
!     itr    (NGPT,NLAY)    - integer look-up table index              !
!     colamt (NLAY,MAXGAS)  - column amounts of absorbing gases        !
!                             1-MAXGAS are for watervapor, carbon      !
!                             dioxide, ozone, nitrous oxide, methane,  !
!                             oxigen, carbon monoxide, respectively    !
!                             (molecules/cm**2)                        !
!     pwvcm                 - column precipitable water vapor (cm)     !
!     secdiff(NBANDS)       - variable diffusivity angle defined as    !
!                             an exponential function of the column    !
!                             water amount in bands 2-3 and 5-9.       !
!                             this reduces the bias of several w/m2 in !
!                             downward surface flux in high water      !
!                             profiles caused by using the constant    !
!                             diffusivity angle of 1.66.         (mji) !
!     co2mult(NLAY)         - the factor used to multiply the ave co2  !
!                             abs coeff to get the added contribution  !
!                             to the optical depth relative to 355 ppm.!
!     facij  (NLAY)         - indicator of interpolation factors       !
!                             =0/1: indicate lower/higher temp & height!
!     selffac(NLAY)         - scale factor for self-continuum, equals  !
!                          (w.v. density)/(atm density at 296K,1013 mb)!
!     selffrac(NLAY)        - factor for temp interpolation of ref     !
!                             self-continuum data                      !
!     indself(NLAY)         - index of the lower two appropriate ref   !
!                             temp for the self-continuum interpolation!
!     laytrop,layswtch,laylow                                          !
!                           - layer at which switch is made from one   !
!                             combination of key species to another    !
!     totuflux(0:NLAY)      - upward longwave flux (w/m2)              !
!     totdflux(0:NLAY)      - downward longwave flux (w/m2)            !
!     totuclfl(0:NLAY)      - clear-sky upward longwave flux (w/m2)    !
!     totdclfl(0:NLAY)      - clear-sky downward longwave flux (w/m2)  !
!     fnet    (0:NLAY)      - net longwave flux (w/m2)                 !
!     fnetc   (0:NLAY)      - clear-sky net longwave flux (w/m2)       !
!                                                                      !
!                                                                      !
!  =====================    end of definitions    ===================  !
!
      implicit none

!  ---  inputs:
      integer,  intent(in) :: NPTS, NLAY, NLP1, iovr, iflip

      logical,  intent(in) :: lprnt

      real (kind=kind_phys), dimension(:,:),  intent(in) :: pint, tint, &
     &       pmid, tmid, qnm, o3mr

      real (kind=kind_phys), dimension(:,:,:),intent(in) :: gasvmr,     &
     &       clouds

      real (kind=kind_phys), dimension(:),    intent(in) :: sfemis

      real (kind=kind_phys), dimension(:,:,:,:),intent(in) :: aerosols

!  ---  outputs:
      real (kind=kind_phys), dimension(:,:), intent(out) :: hlwc

      type (topflw_type),    dimension(:),   intent(out) :: topflx
      type (sfcflw_type),    dimension(:),   intent(out) :: sfcflx

!! ---  optional outputs:
      real (kind=kind_phys),dimension(:,:,:),optional,intent(out):: hlwb
      real (kind=kind_phys),dimension(:,:),optional,intent(out):: hlw0
      type (proflw_type),   dimension(:,:),optional,intent(out):: flxprf

!  ---  locals:
      real (kind=kind_phys), dimension(0:NLP1) :: cldfrac

      real (kind=kind_phys), dimension(0:NLAY) :: totuflux, totdflux,   &
     &       totuclfl, totdclfl, tz

      real (kind=kind_phys), dimension(NLAY)   :: htr, htrcl

      real (kind=kind_phys), dimension(NLAY)   :: pavel, tavel, delp,   &
     &       taucl, cwp1, cip1, rew1, rei1, cda1, cda2, cda3, cda4,     &
     &       coldry, co2mult, h2ovmr, o3vmr, fac00, fac01, fac10,       &
     &       fac11, forfac, plog, selffac, selffrac, temcol

      real (kind=kind_phys) :: colamt(NLAY,MAXGAS), wx(NLAY,MAXXSEC),   &
     &       taucloud(NBANDS,NLAY), pfrac(NGPT,NLAY), semiss(NBANDS),   &
     &       secdiff(NBANDS), tauaer(NBANDS,NLAY), htrb(NLAY,NBANDS)

      real (kind=kind_phys) :: fp, ft, ft1, tem0, tem1, tem2, pwvcm

      integer, dimension(NLAY) :: jp, jt, jt1, indself
      integer                  :: itr(NGPT,NLAY), laytrop, layswtch,    &
     &                            laylow, jp1, j, k, k1, iplon
!
!===> ... begin here
!

      lhlwb  = present ( hlwb )
      lhlw0  = present ( hlw0 )
      lflxprf= present ( flxprf )

!  ---  loop over horizontal NPTS profiles

      lab_do_iplon : do iplon = 1, NPTS

        if (sfemis(iplon) > eps .and. sfemis(iplon) <= 1.0) then  ! input surface emissivity
          do j = 1, NBANDS
            semiss(j) = sfemis(iplon)
          enddo
        else                                                      ! use default values
          do j = 1, NBANDS
            semiss(j) = semiss0(j)
          enddo
        endif

!  ---  prepare atmospheric profile for use in rrtm
!       the vertical index of internal array is from surface to top

        if (iflip == 0) then        ! input from toa to sfc

          tem1 = 100.0 * con_g
          tem2 = 1.0e-20 * 1.0e3 * con_avgd
          tz(0) = tint(iplon,NLP1)

          do k = 1, NLAY
            k1 = NLP1 - k
            pavel(k)= pmid(iplon,k1)
            delp(k) = pint(iplon,k1+1) - pint(iplon,k1)
            tavel(k)= tmid(iplon,k1)
            tz(k)   = tint(iplon,k1)

!  ---  set absorber amount
!test use
!           h2ovmr(k)= max(f_zero,qnm(iplon,k1)*amdw)                   ! input mass mixing ratio
!           h2ovmr(k)= max(f_zero,qnm(iplon,k1))                        ! input vol mixing ratio
!ncep model use
            h2ovmr(k)= max(f_zero,                                      &
     &                     qnm(iplon,k1)*amdw/(1.0-qnm(iplon,k1)))      ! input specific humidity
            o3vmr (k)= max(f_zero,o3mr(iplon,k1)*amdo3)                 ! input mass mixing ratio
!test use   o3vmr (k)= max(f_zero,o3mr(iplon,k1))                       ! input vol mixing ratio

            tem0 = (1.0 - h2ovmr(k))*con_amd + h2ovmr(k)*con_amw
            coldry(k) = tem2 * delp(k) / (tem1*tem0*(1.0 + h2ovmr(k)))
            temcol(k) = 1.0e-12 * coldry(k)

            colamt(k,1) =                coldry(k)*h2ovmr(k)            ! h2o
            colamt(k,2) = max(temcol(k), coldry(k)*gasvmr(iplon,k1,1))  ! co2
            colamt(k,3) =                coldry(k)*o3vmr(k)             ! o3
          enddo

!  ---  set aerosol optical properties

          if (iaerlw > 0) then
            do k = 1, NLAY
              k1 = NLP1 - k
              do j = 1, NBANDS
                tauaer(j,k) = aerosols(iplon,k1,j,1)                    &
     &                      * (1.0 - aerosols(iplon,k1,j,2))
              enddo
            enddo
          else
            tauaer(:,:) = f_zero
          endif

          if (iflagliq > 0) then   ! use prognostic cloud method
            do k = 1, NLAY
              k1 = NLP1 - k
              cldfrac(k)= clouds(iplon,k1,1)
              cwp1 (k)  = clouds(iplon,k1,2)
              rew1 (k)  = clouds(iplon,k1,3)
              cip1 (k)  = clouds(iplon,k1,4)
              rei1 (k)  = clouds(iplon,k1,5)
              cda1 (k)  = clouds(iplon,k1,6)
              cda2 (k)  = clouds(iplon,k1,7)
              cda3 (k)  = clouds(iplon,k1,8)
              cda4 (k)  = clouds(iplon,k1,9)
            enddo
          else                        ! use diagnostic cloud method
            do k = 1, NLAY
              k1 = NLP1 - k
              cldfrac(k)= clouds(iplon,k1,1)
              cda1(k)   = clouds(iplon,k1,2)
            enddo
          endif                       ! end if_iflagliq

          cldfrac(0)    = 1.0         ! padding value only
          cldfrac(NLP1) = f_zero      ! padding value only

        else                        ! input from sfc to toa

          tem1 = 100.0 * con_g
          tem2 = 1.0e-20 * 1.0e3 * con_avgd
          tz(0) = tint(iplon,1)

          do k = 1, NLAY
            pavel(k)= pmid(iplon,k)
            delp(k) = pint(iplon,k) - pint(iplon,k+1)
            tavel(k)= tmid(iplon,k)
            tz(k)   = tint(iplon,k+1)

!  ---  set absorber amount
!test use
!           h2ovmr(k)= max(f_zero,qnm(iplon,k)*amdw)                    ! input mass mixing ratio
!           h2ovmr(k)= max(f_zero,qnm(iplon,k))                         ! input vol mixing ratio
!ncep model use
            h2ovmr(k)= max(f_zero,qnm(iplon,k)*amdw/(1.0-qnm(iplon,k))) ! input specific humidity
            o3vmr (k)= max(f_zero,o3mr(iplon,k)*amdo3)                  ! input mass mixing ratio
!test use   o3vmr (k)= max(f_zero,o3mr(iplon,k))                        ! input vol mixing ratio

            tem0 = (1.0 - h2ovmr(k))*con_amd + h2ovmr(k)*con_amw
            coldry(k) = tem2 * delp(k) / (tem1*tem0*(1.0 + h2ovmr(k)))
            temcol(k) = 1.0e-12 * coldry(k)

            colamt(k,1) =                coldry(k)*h2ovmr(k)           ! h2o
            colamt(k,2) = max(temcol(k), coldry(k)*gasvmr(iplon,k,1))  ! co2
            colamt(k,3) =                coldry(k)*o3vmr(k)            ! o3
          enddo

!  ---  set aerosol optical properties

          if (iaerlw > 0) then
            do k = 1, NLAY
              do j = 1, NBANDS
                tauaer(j,k) = aerosols(iplon,k,j,1)                     &
     &                      * (1.0 - aerosols(iplon,k,j,2))
              enddo
            enddo
          else
            tauaer(:,:) = f_zero
          endif

          if (iflagliq > 0) then   ! use prognostic cloud method
            do k = 1, NLAY
              cldfrac(k)= clouds(iplon,k,1)
              cwp1 (k)  = clouds(iplon,k,2)
              rew1 (k)  = clouds(iplon,k,3)
              cip1 (k)  = clouds(iplon,k,4)
              rei1 (k)  = clouds(iplon,k,5)
              cda1 (k)  = clouds(iplon,k,6)
              cda2 (k)  = clouds(iplon,k,7)
              cda3 (k)  = clouds(iplon,k,8)
              cda4 (k)  = clouds(iplon,k,9)
            enddo
          else                        ! use diagnostic cloud method
            do k = 1, NLAY
              cldfrac(k)= clouds(iplon,k,1)
              cda1(k)   = clouds(iplon,k,2)
            enddo
          endif

          cldfrac(0)    = 1.0         ! padding value only
          cldfrac(NLP1) = f_zero      ! padding value only

        endif                       ! if_iflip

!  ---  set up col amount for rare gases, convert from volume mixing ratio to
!       molec/cm2 based on coldry (scaled to 1.0e-20) for use in rrtm

        if (iflip == 0) then        ! input from toa to sfc

          if (irgaslw == 1) then
            do k = 1, NLAY
              k1 = NLP1 - k
              colamt(k,4)=max(temcol(k), coldry(k)*gasvmr(iplon,k1,2))  ! n2o
              colamt(k,5)=max(temcol(k), coldry(k)*gasvmr(iplon,k1,3))  ! ch4
!             colamt(k,6)=max(f_zero,    coldry(k)*gasvmr(iplon,k1,4))  ! o2 - not used
!             colamt(k,7)=max(f_zero,    coldry(k)*gasvmr(iplon,k1,5))  ! co - not used
            enddo
          else
            do k = 1, NLAY
              colamt(k,4) = f_zero     ! n2o
              colamt(k,5) = f_zero     ! ch4
!             colamt(k,6) = f_zero     ! o2 - not used
!             colamt(k,7) = f_zero     ! co - not used
            enddo
          endif

          if (icfclw == 1) then
            do k = 1, NLAY
              k1 = NLP1 - k
              wx(k,1) = max( f_zero, coldry(k)*gasvmr(iplon,k1,9) )   ! ccl4
              wx(k,2) = max( f_zero, coldry(k)*gasvmr(iplon,k1,6) )   ! cf11
              wx(k,3) = max( f_zero, coldry(k)*gasvmr(iplon,k1,7) )   ! cf12
              wx(k,4) = max( f_zero, coldry(k)*gasvmr(iplon,k1,8) )   ! cf22
            enddo
          else
            wx(:,:) = f_zero
          endif

! mji - for variable diffusivity angle, sum moist atmos and water over column
          tem1 = f_zero
          tem2 = f_zero
          do k = 1, NLAY
            tem1 = tem1 + coldry(k) + colamt(k,1)
            tem2 = tem2 + colamt(k,1)
          enddo
! mji - calculate column precipitable water and variable diffusivity angle
!         tem3 = tem2 / (amdw * tem1)
!         pwvcm = tem3 * (1.0e3 * pint(iplon,NLP1)) / (1.0e2 * con_g)
          pwvcm = 10.0 * pint(iplon,NLP1) * tem2 /(amdw * tem1* con_g)

        else                        ! input from sfc to toa

          if (irgaslw == 1) then
            do k = 1, NLAY
              colamt(k,4)=max(temcol(k), coldry(k)*gasvmr(iplon,k,2))  ! n2o
              colamt(k,5)=max(temcol(k), coldry(k)*gasvmr(iplon,k,3))  ! ch4
!             colamt(k,6)=max(f_zero,    coldry(k)*gasvmr(iplon,k,4))  ! o2 - not used
!             colamt(k,7)=max(f_zero,    coldry(k)*gasvmr(iplon,k,5))  ! co - not used
            enddo
          else
            do k = 1, NLAY
              colamt(k,4) = f_zero     ! n2o
              colamt(k,5) = f_zero     ! ch4
!             colamt(k,6) = f_zero     ! o2 - not used
!             colamt(k,7) = f_zero     ! co - not used
            enddo
          endif

          if (icfclw == 1) then
            do k = 1, NLAY
              wx(k,1) = max( f_zero, coldry(k)*gasvmr(iplon,k,9) )   ! ccl4
              wx(k,2) = max( f_zero, coldry(k)*gasvmr(iplon,k,6) )   ! cf11
              wx(k,3) = max( f_zero, coldry(k)*gasvmr(iplon,k,7) )   ! cf12
              wx(k,4) = max( f_zero, coldry(k)*gasvmr(iplon,k,8) )   ! cf22
            enddo
          else
            wx(:,:) = f_zero
          endif

! mji - for variable diffusivity angle, sum moist atmos and water over column
          tem1 = f_zero
          tem2 = f_zero
          do k = 1, NLAY
            tem1 = tem1 + coldry(k) + colamt(k,1)
            tem2 = tem2 + colamt(k,1)
          enddo
! mji - calculate column precipitable water and variable diffusivity angle
!         tem3 = tem2 / (amdw * tem1)
!         pwvcm = tem3 * (1.0e3 * pint(iplon,1)) / (1.0e2 * con_g)
          pwvcm = 10.0 * pint(iplon,1) * tem2 /(amdw * tem1* con_g)

        endif                       ! if_iflip

        tem1 = 1.80
        tem2 = 1.50
        do j = 1, NBANDS
          if (j==1 .or. j==4 .or. j==10) then
            secdiff(j) = 1.66
          else
!            secdiff(j) = a0(j) + a1(j) * exp( a2(j)*pwvcm )
            secdiff(j) = min( tem1, max( tem2,
     &                   a0(j)+a1(j)*exp(a2(j)*pwvcm) ))


          endif
        enddo
        if (pwvcm < 1.0) secdiff(6) = 1.80
        if (pwvcm > 7.1) secdiff(7) = 1.50
! mji

        do k = 1, NLAY
!  ...  using e = 1334.2 cm-1.
          tem1 = co2fac * coldry(k)
          co2mult(k) = (colamt(k,2) - tem1) * 272.63                    &
     &               * exp(-1919.4/tavel(k)) / (8.7604e-4*tavel(k))
          forfac(k)  = pavel(k)*stpfac / (tavel(k)*(1.0 + h2ovmr(k)))
        enddo
        
!     if (lprnt) then
!     print *,'  coldry',coldry
!     print *,' wx(*,1) ',(wx(k,1),k=1,NLAY)
!     print *,' wx(*,2) ',(wx(k,2),k=1,NLAY)
!     print *,' wx(*,3) ',(wx(k,3),k=1,NLAY)
!     print *,' wx(*,4) ',(wx(k,4),k=1,NLAY)
!     print *,' iplon ',iplon
!     print *,'  pavel ',pavel
!     print *,'  delp ',delp
!     print *,'  tavel ',tavel
!     print *,'  tz ',tz
!     print *,' h2ovmr ',h2ovmr
!     print *,' o3vmr ',o3vmr
!     endif

!  ---  calculate cloud optical properties

        call cldprop                                                    &
!  ---  inputs:
     &     ( cldfrac, cwp1, cip1, rew1, rei1, cda1, cda2, cda3, cda4,   &
     &       NLAY, NLP1,                                                &
!  ---  output:
     &       taucloud                                                   &
     &     )

!     if (lprnt) then
!     print *,' after cldprop'
!     print *,' cwp1',cwp1
!     print *,' cip1',cip1
!     print *,' rew1',rew1
!     print *,' rei1',rei1
!     print *,' taucl',cda1
!     print *,' cldfrac',cldfrac
!     print *,' taucloud',taucloud
!     endif

!  ---  calculate information needed by the radiative transfer routine
!       that is specific to this atmosphere, especially some of the 
!       coefficients and indices needed to compute the optical depths
!       by interpolating data from stored reference atmospheres. 

        laytrop = 0
        layswtch= 0
        laylow  = 0

        do k = 1, NLAY

!  ---  find the two reference pressures on either side of the
!       layer pressure.  store them in jp and jp1.  store in fp the
!       fraction of the difference (in ln(pressure)) between these
!       two values that the layer pressure lies.

          plog(k) = log(pavel(k))
          jp(k)= max(1, min(58, int(36.0 - 5.0*(plog(k)+0.04)) ))
          jp1  = jp(k) + 1
!  ---  limit pressure extrapolation at the top
          fp   = max(f_zero, min(1.0, 5.0*(preflog(jp(k))-plog(k)) ))
!org      fp   = 5.0 * (preflog(jp(k)) - plog(k))

!  ---  determine, for each reference pressure (jp and jp1), which
!       reference temperature (these are different for each
!       reference pressure) is nearest the layer temperature but does
!       not exceed it.  store these indices in jt and jt1, resp.
!       store in ft (resp. ft1) the fraction of the way between jt
!       (jt1) and the next highest reference temperature that the
!       layer temperature falls.

          tem1 = (tavel(k) - tref(jp(k))) / 15.0
          tem2 = (tavel(k) - tref(jp1  )) / 15.0
          jt (k) = max(1, min(4, int(3.0 + tem1) ))
          jt1(k) = max(1, min(4, int(3.0 + tem2) ))
!  ---  restrict extrapolation ranges by limiting abs(det t) < 37.5 deg
          ft  = max(-0.5, min(1.5, tem1 - float(jt (k) - 3) ))
          ft1 = max(-0.5, min(1.5, tem2 - float(jt1(k) - 3) ))
!org      ft  = tem1 - float(jt (k) - 3)
!org      ft1 = tem2 - float(jt1(k) - 3)

!  ---  we have now isolated the layer ln pressure and temperature,
!       between two reference pressures and two reference temperatures
!       (for each reference pressure).  we multiply the pressure
!       fraction fp with the appropriate temperature fractions to get
!       the factors that will be needed for the interpolation that yields
!       the optical depths (performed in routines taugbn for band n).

          fac10(k) = (1.0 - fp) * ft
          fac00(k) = (1.0 - fp) * (1.0 - ft)
          fac11(k) = fp * ft1
          fac01(k) = fp * (1.0 - ft1)

        enddo

!  ---  if the pressure is less than ~100mb, perform a different
!       set of species interpolations.

        do k = 1, NLAY
          if (plog(k) > 4.56) then
            laytrop =  laytrop + 1

!  ---  for one band, the "switch" occurs at ~300 mb.
            if (plog(k) >= 5.76) layswtch = layswtch + 1
            if (plog(k) >= 6.62) laylow   = laylow   + 1

!  ---  set up factors (tem1) needed to separately include the water
!       vapor self-continuum in the calculation of absorption
!       coefficient.

            tem1 = (tavel(k) - 188.0) / 7.2

            selffac(k) = h2ovmr(k) * forfac(k)
            indself(k) = min(9, max(1, int(tem1)-7 ))
            selffrac(k)= tem1 - float(indself(k) + 7)

          else

            selffac(k) = f_zero
            indself(k) = 0
            selffrac(k)= f_zero

          endif
        enddo

!  ---  set laylow for profiles with surface pressure less than 750mb.
        if (laylow == 0) laylow = 1

!     if (lprnt) then
!      print *,'laytrop,layswtch,laylow',laytrop,layswtch,laylow
!      print *,'colh2o',(colamt(k,1),k=1,NLAY)
!      print *,'colco2',(colamt(k,2),k=1,NLAY)
!      print *,'colo3', (colamt(k,3),k=1,NLAY)
!      print *,'coln2o',(colamt(k,4),k=1,NLAY)
!      print *,'colch4',(colamt(k,5),k=1,NLAY)
!      print *,'co2mult',co2mult
!      print *,'fac00',fac00
!      print *,'fac01',fac01
!      print *,'fac10',fac10
!      print *,'fac11',fac11
!      print *,'jp',jp
!      print *,'jt',jt
!      print *,'jt1',jt1
!      print *,'selffac',selffac
!      print *,'selffrac',selffrac
!      print *,'indself',indself
!      print *,'forfac',forfac
!     endif

        call taumol                                                     &
!  ---  inputs:
     &     ( laytrop,layswtch,laylow,h2ovmr,colamt,wx,co2mult,          &
     &       fac00,fac01,fac10,fac11,jp,jt,jt1,selffac,selffrac,        &
     &       indself,forfac,secdiff,tauaer, NLAY,                       &
!  ---  outputs:
     &       itr, pfrac                                                 &
     &     )

!     if (lprnt) then
!     print *,' after taumol'
!     do k=1,NLAY
!       write(6,123) k
!123    format(' k =',i3,5x,'PFRAC')
!       write(6,122) (pfrac(j,k),j=1,NGPT)
!122    format(10e14.7)
!       write(6,124) k
!124    format(' k =',i3,5x,'ITR')
!       write(6,125) (itr(j,k),j=1,NGPT)
!125    format(10i10)
!     enddo
!     endif

!  ---  call the radiative transfer routine.

        if (iovr == 0) then

          call rtrn                                                     &
!  ---  inputs:
     &     ( tavel,tz,delp,semiss,cldfrac,taucloud,pfrac,               &
     &       secdiff, itr, NLAY, NLP1,                                  &
!  ---  outputs:
     &       totuflux,totdflux,htr, totuclfl,totdclfl,htrcl, htrb       &
     &     )

        else

          call rtrnmr                                                   &
!  ---  inputs:
     &     ( tavel,tz,delp,semiss,cldfrac,taucloud,pfrac,               &
     &       secdiff, itr, NLAY, NLP1,                                  &
!  ---  outputs:
     &       totuflux,totdflux,htr, totuclfl,totdclfl,htrcl, htrb       &
     &     )

        endif


!  ---  output total-sky and clear-sky fluxes and heating rates.

        topflx(iplon)%upfxc = totuflux(NLAY)
        topflx(iplon)%upfx0 = totuclfl(NLAY)

        sfcflx(iplon)%upfxc = totuflux(0)
        sfcflx(iplon)%upfx0 = totuclfl(0)
        sfcflx(iplon)%dnfxc = totdflux(0)
        sfcflx(iplon)%dnfx0 = totdclfl(0)

        if (iflip == 0) then        ! output from toa to sfc

!! ---  optional fluxes
          if ( lflxprf ) then
            do k = 0, NLAY
              k1 = NLP1 - k
              flxprf(iplon,k1)%upfxc = totuflux(k)
              flxprf(iplon,k1)%dnfxc = totdflux(k)
              flxprf(iplon,k1)%upfx0 = totuclfl(k)
              flxprf(iplon,k1)%dnfx0 = totdclfl(k)
            enddo
          endif

          do k = 1, NLAY
            k1 = NLP1 - k
            hlwc(iplon,k1) = htr(k)
          enddo

!! ---  optional clear sky heating rate
          if ( lhlw0 ) then
            do k = 1, NLAY
              k1 = NLP1 - k
              hlw0(iplon,k1) = htrcl(k)
            enddo
          endif

!! ---  optional spectral band heating rate
          if ( lhlwb ) then
            do j = 1, NBANDS
            do k = 1, NLAY
              k1 = NLP1 - k
              hlwb(iplon,k1,j) = htrb(k,j)
            enddo
            enddo
          endif

        else                        ! output from sfc to toa

!! ---  optional fluxes
          if ( lflxprf ) then
            do k = 0, NLAY
              flxprf(iplon,k+1)%upfxc = totuflux(k)
              flxprf(iplon,k+1)%dnfxc = totdflux(k)
              flxprf(iplon,k+1)%upfx0 = totuclfl(k)
              flxprf(iplon,k+1)%dnfx0 = totdclfl(k)
            enddo
          endif

          do k = 1, NLAY
            hlwc(iplon,k) = htr(k)
          enddo

!! ---  optional clear sky heating rate
          if ( lhlw0 ) then
            do k = 1, NLAY
              hlw0(iplon,k) = htrcl(k)
            enddo
          endif

!! ---  optional spectral band heating rate
          if ( lhlwb ) then
            do j = 1, NBANDS
            do k = 1, NLAY
              hlwb(iplon,k,j) = htrb(k,j)
            enddo
            enddo
          endif

        endif                       ! if_iflip

      enddo  lab_do_iplon

      return
!...................................
      end subroutine lwrad
!-----------------------------------



!-----------------------------------
      subroutine rlwinit                                                &
!...................................

!  ---  inputs:
     &     ( icwp, me, NLAY )
!  ---  outputs: (none)

!  *******************************************************************  !
!                                                                       !
!  rrtm longwave radiative transfer model                               !
!  atmospheric and environmental research, inc., cambridge, ma          !
!                                                                       !
!                                                                       !
!  original version:       michael j. iacono; july, 1998                !
!  revision for ncar ccm:  michael j. iacono; september, 1998           !
!                                                                       !
!  this subroutine performs calculations necessary for the initialization
!  of the lw model, rrtm.  lookup tables are computed for use in the lw !
!  radiative transfer, and input absorption coefficient data for each   !
!  spectral band are reduced from 256 g-points to 140 for use in rrtm.  !
!                                                                       !
!  *******************************************************************  !
!                                                                       !
! definitions:                                                          !
!     arrays for 5000-point look-up tables:                             !
!     tau  - clear-sky optical depth (used in cloudy radiative transfer)!
!     tf     tau transition function; i.e. the transition of the planck !
!            function from that for the mean layer temperature to that  !
!            for the layer boundary temperature as a function of optical!
!            depth. the "linear in tau" method is used to make the table!
!     trans- transmittance                                              !
!                                                                       !
!  *******************************************************************  !
!                                                                       !
!  inputs:                                                              !
!    icwp     -  flag of cloud schemes used by model                    !
!                =0: diagnostic scheme gives cloud tau, omiga, and g    !
!                =1: prognostic scheme gives cloud liq/ice path, etc.   !
!    me       - print control for parallel process                      !
!    NLAY     - number of vertical layers                               !
!                                                                       !
!  outputs: (none)                                                      !
!                                                                       !
!  control flags in module "module_radlw_cntr_para":                    !
!     ilwrate - heating rate unit selections                            !
!               =1: output in k/day                                     !
!               =2: output in k/second                                  !
!     iaerlw  - control flag for aerosols                               !
!               =0: do not include aerosol effect                       !
!               >0: include aerosol effect                              !
!     irgaslw - control flag for rare gases (ch4,n2o,o2, etc.)          !
!               =0: do not include rare gases                           !
!               =1: include all rare gases                              !
!     icfclw  - control flag for cfc gases                              !
!               =0: do not include cfc gases                            !
!               =1: include all cfc gases                               !
!     iflagliq- cloud optical properties contrl flag                    !
!               =0: input cloud opt depth from diagnostic scheme        !
!               >0: input cwp,cip, and other cloud content parameters   !
!                                                                       !
!  *******************************************************************  !
!
      implicit none
!
!  ---  inputs:
      integer, intent(in) :: icwp, me, NLAY

!  ---  outputs: none

!  ---  locals:
      real (kind=kind_phys) :: tfn, fp, rtfp, pival, explimit
      integer               :: i
!
!===> ... begin here
!

      if (me == 0) then
        print *,' - Using AER Longwave Radiation, Version: ', VTAGLW

        if (iaerlw > 0) then
          print *,'   --- Using input aerosol parameters for LW'
        else
          print *,'   --- Aerosol effect is NOT included in LW, all'    &
     &           ,' internal aerosol parameters are reset to zeros'
        endif

        if (irgaslw == 1) then
          print *,'   --- Include rare gases N2O, CH4, O2, absorptions',&
     &            ' in LW'
        else
          print *,'   --- Rare gases effect is NOT included in LW'
        endif

        if (icfclw == 1) then
          print *,'   --- Include CFC gases absorptions in LW'
        else
          print *,'   --- CFC gases effect is NOT included in LW'
        endif
      endif

!  --- ...  check cloud flags for consistency

      if ((icwp == 0 .and. iflagliq /= 0) .or.                          &
     &    (icwp == 1 .and. iflagliq == 0)) then
        print *, ' *** Model cloud scheme inconsistent with LW',        &
     &           ' radiation cloud radiative property setup !!'
        stop
      endif

!  --- ...  setup default surface emissivity for each band here

      semiss0(:) = 1.0

!  --- ...  setup constant factors for flux and heating rate
!           the 1.0e-2 is to convert pressure from mb to N/m**2

      pival = 2.0*asin(1.0)
      fluxfac = pival * 2.0d4
!     fluxfac = 62831.85307179586                   ! = 2 * pi * 1.0e4

      if (ilwrate == 1) then
!       heatfac = con_g * 86400. * 1.0e-2 / con_cp  !   (in k/day)
        heatfac = con_g * 864.0 / con_cp            !   (in k/day)
      else
        heatfac = con_g * 1.0e-2 / con_cp           !   (in k/second)
      endif

!  --- ...  compute lookup tables for transmittance, tau transition
!           function, and clear sky tau (for the cloudy sky radiative
!           transfer).  tau is computed as a function of the tau 
!           transition function, transmittance is calculated as a 
!           function of tau, and the tau transition function is 
!           calculated using the linear in tau formulation at values of
!           tau above 0.01.  tf is approximated as tau/6 for tau < 0.01.
!           all tables are computed at intervals of 0.001.  the inverse
!           of the constant used in the pade approximation to the tau
!           transition function is set to b.

      tau  (0) = f_zero
      tf   (0) = f_zero
      trans(0) = 1.0

      tau  (N5000) = 1.e10
      tf   (N5000) = 1.0
      trans(N5000) = f_zero

      explimit = aint( -log(tiny(trans(0))) )

      do i = 1, N5000-1
         tfn = real(i, kind_phys) / real(N5000-i, kind_phys)
         tau  (i) = bpade * tfn
         if (tau(i) >= explimit) then
           trans(i) = f_zero
         else
           trans(i) = exp(-tau(i))
         endif

         if (tau(i) < 0.1) then
            tf(i) = tau(i) / 6.0
         else
            tf(i) = 1. - 2.*( (1./tau(i)) - (trans(i)/(1.-trans(i))) )
         endif
      enddo

!  --- ...  calculate lookup tables for functions needed in routine
!           taumol (taugb2)

      corr1(0) = 1.0
      corr2(0) = 1.0

      corr1(N200) = 1.0
      corr2(N200) = 1.0

      do i = 1, N200-1
         fp = 0.005 * float(i)
         rtfp = sqrt(fp)
         corr1(i) = rtfp / fp
         corr2(i) = (1.0 - rtfp) / (1.0 - fp)
      enddo

!...................................
      end subroutine rlwinit
!-----------------------------------



!-----------------------------------
      subroutine cldprop                                                &
!...................................

!  ---  inputs:
     &     ( cldfrac,cliqp,cicep,reliq,reice,cdat1,cdat2,cdat3,cdat4,   &
     &       NLAY, NLP1,                                                &
!  ---  output:
     &       taucloud                                                   &
     &     )

!  *******************************************************************  !
!                                                                       !
!    purpose:  compute the cloud optical depth(s) for each cloudy layer.!
!                                                                       !
!  *******************************************************************  !
!                                                                       !
!  inputs:                                                              !
!     cldfrac - layer cloud fraction                                 L  !
!        - - -  for iflagliq > 0  (prognostic cloud sckeme)  - - -      !
!     cliqp   - layer cloud liquid water path  (g/m**2)              L  !
!     reliq   - effective radius for water cloud (micron)            L  !
!     cicep   - layer cloud ice water path  (g/m**2)                 L  !
!     reice   - effective radius for ice cloud (micron)              L  !
!     cdat1   - layer rain drop water path  (g/m**2)                 L  !
!     cdat2   - effective radius for rain drop (microm)              L  !
!     cdat3   - layer snow flake water path (g/m**2)                 L  !
!               (if use fu's formula it needs to be normalized by       !
!                snow density (g/m**3/1.0e6) to get unit of micron)     !
!     cdat4   - effective radius for snow flakes (micron)            L  !
!        - - -  for iflagliq = 0  (diagnostic cloud sckeme)  - - -      !
!     cdat1   - input cloud optical depth                            L  !
!     cdat2   - optional use                                         L  !
!     cdat3   - optional use                                         L  !
!     cdat4   - optional use                                         L  !
!     cliqp   - not used                                             L  !
!     reliq   - not used                                             L  !
!     cicep   - not used                                             L  !
!     reice   - not used                                             L  !
!                                                                       !
!     NLAY/NLP1-vertical layer/level numbers                         1  !
!                                                                       !
!    explanation of the method for each value of iflagliq, and iflagice.!
!    set up in module "module_radlw_cntr_para"                          !
!                                                                       !
!     iflagliq=0 and =1 do not distingish being liquid and ice clouds.  !
!     iflagliq=2 and =3 does distinguish between liquid and ice clouds, !
!                  and requires further user input (iflagice) to specify!
!                  the method to be used to compute the aborption due to!
!                  liquid and ice parts.                                !
!  ...................................................................  !
!                                                                       !
!     iflagliq=0:  for each cloudy layer, the cloud fraction and (gray) !
!                  optical depth are input.                             !
!     iflagliq=1:  for each cloudy layer, the cloud fraction and cloud  !
!                  water path (g/m2) are input.  using clp only. the    !
!                  (gray) cloud optical depth is computed as in ccm2.   !
!     iflagliq=2:  the optical depths due to water clouds are computed  !
!                  as in ccm3.                                          !
!     iflagliq=3:  the water droplet effective radius (microns) is input!
!                  and the opt depths due to water clouds are computed  !
!                  as in hu and stamnes, j., clim., 6, 728-742, (1993). !
!                  the values for absorption coefficients appropriate for
!                  the spectral bands in rrtm have been obtained for a  !
!                  range of effective radii by an averaging procedure   !
!                  based on the work of j. pinto (private communication).
!                  linear interpolation is used to get the absorption   !
!                  coefficients for the input effective radius.         !
!                                                                       !
!     iflagice=0:  the cloud ice path (g/m2) and ice effective radius   !
!                  (microns) are input and the optical depths due to ice!
!                  clouds are computed as in ccm3.                      !
!     iflagice=1:  the cloud ice path (g/m2) and ice effective radius   !
!                  (microns) are input and the optical depths due to ice!
!                  clouds are computed as in ebert and curry, jgr, 97,  !
!                  3831-3836 (1992).  the spectral regions in this work !
!                  have been matched with the spectral bands in rrtm to !
!                  as great an extent as possible:                      !
!                     e&c 1      ib = 5      rrtm bands 9-16            !
!                     e&c 2      ib = 4      rrtm bands 6-8             !
!                     e&c 3      ib = 3      rrtm bands 3-5             !
!                     e&c 4      ib = 2      rrtm band 2                !
!                     e&c 5      ib = 1      rrtm band 1                !
!     iflagice=2:  the cloud ice path (g/m2) and ice effective radius   !
!                  (microns) are input and the optical depths due to ice!
!                  clouds are computed as in streamer (reference: j. key,
!                  streamer user's guide, technical report 96-01,       !
!                  department of geography, boston university, 85 pp.   !
!                  (1996)).  the values of absorption coefficients      !
!                  appropriate for the spectral bands of rrtm were      !
!                  obtained by an averaging procedure based on the work !
!                  of j. pinto (private communication).                 !
!                                                                       !
!  outputs:                                                             !
!     taucloud - cloud optical depth                        NBANDS*L    !
!                                                                       !
!  *******************************************************************  !
!
      use module_radlw_cldprlw

      implicit none

!  ---  inputs:
      integer, intent(in) :: NLAY, NLP1

      real (kind=kind_phys), dimension(0:), intent(in) :: cldfrac

      real (kind=kind_phys), dimension(:),  intent(in) :: cliqp, cicep, &
     &       reliq, reice, cdat1, cdat2, cdat3, cdat4

!  ---  outputs:
      real (kind=kind_phys), dimension(:,:), intent(out) :: taucloud

!  ---  locals:
      real (kind=kind_phys) :: cliq, cice, radliq, radice, factor, fint
      real (kind=kind_phys) :: taurain, tausnow
      integer               :: j, k, index

!
!===> ... begin here
!
      do k = 1, NLAY
        do j = 1, NBANDS
          taucloud(j,k) = f_zero
        enddo
      enddo

      lab_do_k : do k = 1, NLAY

        lab_if_cld : if (cldfrac(k) > eps) then

!  ---  ice clouds and water clouds combined.
          lab_if_liq : if (iflagliq == 0) then

            do j = 1, NBANDS
              taucloud(j,k) = cdat1(k)
            enddo

          elseif (iflagliq == 1) then  lab_if_liq

            taurain = absrain * cdat1(k)                 ! ncar formula
            tausnow = abssnow0 * cdat3(k)                ! ncar formula
!           tausnow = abssnow1 * cdat3(k) / cdat4(k)     ! fu's formula

!           taucloud(1,k) = absliq1 * (cliqp(k) + cicep(k))
!           taucloud(1,k) = absliq1 * cliqp(k)
            taucloud(1,k) = absliq1*cliqp(k) + taurain + tausnow
            do j = 2,NBANDS
              taucloud(j,k) = taucloud(1,k)
            enddo

!  ---  separate treatement of ice clouds and water clouds.
          else  lab_if_liq

            taurain = absrain * cdat1(k)                 ! ncar formula
            tausnow = abssnow0 * cdat3(k)                ! ncar formula
!           tausnow = abssnow1 * cdat3(k) / cdat4(k)     ! fu's formula

            cliq = cliqp(k)
            cice = cicep(k)
            radliq = max(2.5e0, min(60.0e0, real(reliq(k)) ))
            radice = reice(k)
!           radice = max(13.e0, min(130.e0, real(reice(k)) ))

!  ---  calculation of absorption coefficients due to liquid clouds.
            if (cliq == f_zero) then
              do j = 1, NBANDS
                abscoliq(j) = f_zero
              enddo
            elseif (iflagliq == 2) then
              abscoliq(1) = cliq * absliq2
              do j = 2, NBANDS
                abscoliq(j) = abscoliq(1)
              enddo
            elseif (iflagliq == 3) then
              factor = radliq - 1.5
              index = min(57, int(factor))
              fint = factor - index
              do j = 1, NBANDS
                abscoliq(j) = cliq * (absliq3(index,j) + fint *         &
     &                    (absliq3(index+1,j) - (absliq3(index,j))))
              enddo
            endif

!  ---  calculation of absorption coefficients due to ice clouds.
            if (cice == f_zero) then
              do j = 1, NBANDS
                abscoice(j) = f_zero
              enddo
            elseif (iflagice == 0) then
              abscoice(1) = cice * (absice0(1) + absice0(2)/radice)
              do j = 2, NBANDS
                abscoice(j) = abscoice(1)
              enddo
            elseif (iflagice == 1) then
              do j = 1, NBANDS
                index = ipat(j)
                abscoice(j) = cice * (absice1(1,index)                  &
     &                      + absice1(2,index)/radice)
              enddo
            elseif (iflagice == 2) then
              factor = (radice - 10.0) / 3.0
              index = min(39, int(factor))
              fint = factor - index
              do j = 1, NBANDS
                abscoice(j) = cice * (absice2(index,j) + fint *         &
     &                    (absice2(index+1,j) - (absice2(index,j))))
              enddo
            endif

            do j = 1, NBANDS
!             taucloud(j,k) = abscoice(j) + abscoliq(j)
              taucloud(j,k) = abscoice(j) + abscoliq(j)                 &
     &                      + taurain + tausnow
            enddo

          endif  lab_if_liq

        endif  lab_if_cld

      enddo  lab_do_k

      return
!...................................
      end subroutine cldprop
!-----------------------------------



!-----------------------------------
      subroutine rtrn                                                   &
!...................................

!  ---  inputs:
     &     ( tavel,tz,delp,semiss,cldfrac,taucloud,pfrac,               &
     &       secdiff, itr, NLAY, NLP1,                                  &
!  ---  outputs:
     &       totuflux,totdflux,htr, totuclfl,totdclfl,htrcl, htrb       &
     &     )

!  *******************************************************************  !
!                                                                       !
!  rrtm longwave radiative transfer model                               !
!  atmospheric and environmental research, inc., cambridge, ma          !
!                                                                       !
!  original version:       e. j. mlawer, et al.                         !
!  revision for ncar ccm:  michael j. iacono; september, 1998           !
!  revision to use variable diffusivity angle instead of the original   !
!     fixed value of 1.66: m. j. iacono apr 2004                        !
!                                                                       !
!  this program calculates the upward fluxes, downward fluxes, and      !
!  heating rates for an arbitrary clear or cloudy atmosphere.  the input!
!  to this program is the atmospheric profile, all planck function      !
!  information, and the cloud fraction by layer.  a variable diffusivity!
!  angle (secdiff) is used forthe angle integration.  bands 2-3 and 5-9 !
!  use a value of secdiff that varies from 1.50 to 1.80 as a function of!
!  the column water vapor, and other bands use a value of 1.66.  the    !
!  gaussian weight appropriate to this angle (wtnum=0.5) is applied     !
!  here.  note that use of a single angle for the flux integration      !
!  can cause errors of 1 to 4 w/m2 within cloudy layers.                !
!                                                                       !
!  *******************************************************************  !
!
      use module_radlw_avplank
!
      implicit none

!  ---  inputs:
      integer, intent(in)  ::  NLAY, NLP1

      integer, intent(in)  ::  itr(:,:)

      real (kind=kind_phys), dimension(0:), intent(in) :: tz, cldfrac

      real (kind=kind_phys), dimension(:),  intent(in) :: tavel, delp,  &
     &       semiss, secdiff

      real (kind=kind_phys), dimension(:,:),intent(in) :: taucloud,     &
     &       pfrac

!  ---  outputs:
      real (kind=kind_phys), dimension(:),  intent(out) :: htr, htrcl
      real (kind=kind_phys), dimension(:,:),intent(out) :: htrb

      real (kind=kind_phys), dimension(0:), intent(out) ::              &
     &       totuflux, totdflux, totuclfl, totdclfl

!  ---  locals:
      real (kind=kind_phys), dimension(NGPT,NLAY)     :: gassrcu,       &
     &       cldsrcu, trans0
      real (kind=kind_phys), dimension(NGPT,0:NLAY)   :: bglev
      real (kind=kind_phys), dimension(NGPT)          :: radclru,       &
     &       radclrd, radtotu, radtotd
      real (kind=kind_phys), dimension(NBANDS,0:NLAY) :: plvl,          &
     &       totufxsb, totdfxsb
      real (kind=kind_phys), dimension(NBANDS,NLAY)   :: play, odcld,   &
     &       trncld, efcfr1
      real (kind=kind_phys), dimension(0:NLAY)        :: fnet, fnetc

      real (kind=kind_phys) :: totdrad, clrdrad, toturad, clrurad
      real (kind=kind_phys) :: delbgup, delbgdn, bglay, tau0, tauc,     &
     &       transc, cldsrcd, gassrcd, factot, odsm, tem1, tem2

      integer :: j, k, ind, inb, itm1, itm2, jtm1, jtm2

!  ====================  defination of variables  ====================  !
!                                                                       !
!  input variables:                                                     !
!    tavel   (NLAY)       ! layer temperatures (k)                      !
!    tz      (0:NLAY)     ! level (interface) temperature (k)           !
!    delp    (NLAY)       ! layer pressure thickness (mb)               !
!    semiss  (NBANDS)     ! surface emissivities for each band          !
!    cldfrac (0:NLP1)     ! layer cloud fraction (padded at 2 ends)     !
!    taucloud(NBANDS,NLAY)! layer cloud optical depth                   !
!    pfrac   (NGPT,NLAY)  ! planck fractions                            !
!    secdiff(NBANDS)      ! variable diffusivity angle defined as an    !
!                           exponential function of the column water    !
!                           amount in bands 2-3 and 5-9. this reduces   !
!                           the bias of several w/m2 in downward surface!
!                           flux in high water profiles caused by using !
!                           the constant diffusivity angle of 1.66.(mji)!
!    itr     (NGPT,NLAY)  ! integer look-up table index                 !
!    NLAY/NLP1            ! number of model layers/levels               !
!                                                                       !
!  constants or shared variables:                                       !
!    NGPT                 ! total number of g-point subintervals        !
!    NBANDS               ! number of longwave spectral bands           !
!    wtnum                ! weight for radiance to flux conversion      !
!    bpade                ! pade constant                               !
!    tau                  ! clear sky optical depth look-up table       !
!    tf                   ! tau transition function look-up table       !
!    trans                ! clear sky transmittance look-up table       !
!                                                                       !
!  output variables:                                                    !
!    totuflux(0:NLAY)     ! upward longwave flux (w/m2)                 !
!    totdflux(0:NLAY)     ! downward longwave flux (w/m2)               !
!    htr     (NLAY)       ! longwave heating rate (k/day)               !
!    totuclfl(0:NLAY)     ! clear sky upward longwave flux (w/m2)       !
!    totdclfl(0:NLAY)     ! clear sky downward longwave flux (w/m2)     !
!    htrcl   (NLAY)       ! clear sky longwave heating rate (k/day)     !
!    htrb    (NLAY,NBANDS)! spectral band lw heating rate (k/day)       !
!                                                                       !
!  local variables:                                                     !
!    odcld   (NBANDS,NLAY)! cloud optical depth                         !
!    trncld  (NBANDS,NLAY)! cloud transmittance                         !
!    efcfr1  (NBANDS,NLAY)! effective clear  sky fraction               !
!    radtotu (NGPT)       ! upward radiance                             !
!    radtotd (NGPT)       ! downward radiance                           !
!    radclru (NGPT)       ! clear sky upward radiance                   !
!    radclrd (NGPT)       ! clear sky downward radiance                 !
!    toturad              ! spectrally summed upward radiance           !
!    totdrad              ! spectrally summed downward radiance         !
!    clrurad              ! spectrally summed clear sky upward radiance !
!    clrdrad              ! spectrally summed clear sky dnward radiance !
!    totufxsb(NBANDS,NLAY)! spectral band upward longwave flux (w/m2)   !
!    totdfxsb(NBANDS,NLAY)! spectral band downward longwave flux (w/m2) !
!                                                                       !
!    fnet    (0:NLAY)     ! net longwave flux (w/m2)                    !
!    fnetc   (0:NLAY)     ! clear sky net longwave flux (w/m2)          !
!                                                                       !
!  =====================    end of definitions    ====================  !

!
!===> ... begin here
!

!  --- ... calculate the integrated planck functions at the level and
!          layer temperatures.

      itm1 = min(NPLNK, max(1, int(tz(0)-159.0) ))
      itm2 = min(NPLNK, itm1+1)
      tem1 = tz(0) - int(tz(0))
      do j = 1, NBANDS
         plvl(j,0) = delwave(j) * ( totplnk(itm1,j)                     &
     &             + tem1 * (totplnk(itm2,j) - totplnk(itm1,j)) )
      enddo

      do k = 1, NLAY
        itm1 = min(NPLNK, max(1, int(tz(k)   -159.0) ))
        itm2 = min(NPLNK, itm1+1)
        jtm1 = min(NPLNK, max(1, int(tavel(k)-159.0) ))
        jtm2 = min(NPLNK, jtm1+1)

        tem1 = tz(k)    - int(tz(k))
        tem2 = tavel(k) - int(tavel(k))

        do j = 1, NBANDS
          plvl(j,k) = delwave(j) * ( totplnk(itm1,j)                    &
     &              + tem1 * (totplnk(itm2,j) - totplnk(itm1,j)) )
          play(j,k) = delwave(j) * ( totplnk(jtm1,j)                    &
     &              + tem2 * (totplnk(jtm2,j) - totplnk(jtm1,j)) )

!  --- ... cloudy sky optical depth and absorptivity.
! mji     odcld(j,k)  = secang * taucloud(j,k)
          odcld(j,k)  = secdiff(j) * taucloud(j,k)
          trncld(j,k) = exp( -odcld(j,k) )
          efcfr1(j,k) = 1.0 - cldfrac(k) + trncld(j,k)*cldfrac(k)
        enddo

        do j = 1, NGPT
          inb = ngb(j)                 ! band index
          bglev(j,k-1) = pfrac(j,k) * plvl(inb,k-1)
        enddo
      enddo

!  --- ...  initialize for radiative transfer.

      if ( lhlwb ) then
        do k = 0, NLAY
          do j = 1, NBANDS
            totufxsb(j,k) = f_zero
            totdfxsb(j,k) = f_zero
          enddo
        enddo
      endif

      do j = 1, NGPT
         inb = ngb(j)                 ! band index
         radclrd(j) = f_zero
         radtotd(j) = f_zero
         bglev(j,NLAY) = pfrac(j,NLAY) * plvl(inb,NLAY)
      enddo

!===> ...  downward radiative transfer
!          totdrad holds summed radiance for total sky stream
!          clrdrad holds summed radiance for clear sky stream

      do k = NLAY, 1, -1

        totdrad = f_zero
        clrdrad = f_zero

        if (cldfrac(k) > eps) then
!  --- ... cloudy layer

          do j = 1, NGPT
!  --- ... get lookup table index
            ind = itr(j,k)
            inb = ngb(j)                 ! band index

!  --- ... get clear sky transmittance from lookup table
            tau0 = tf(ind)
            trans0(j,k) = trans(ind)
            transc      = trans0(j,k) * trncld(inb,k)

!  --- ... add clear sky and cloud optical depths
            odsm = tau(ind) + odcld(inb,k)
            tauc = odsm / (bpade + odsm)

            bglay = pfrac(j,k) * play(inb,k)
            delbgup = bglev(j,k) - bglay
            tem1 = bglay + tau0*delbgup
            tem2 = bglay + tauc*delbgup
            gassrcu(j,k) = tem1 - trans0(j,k)*tem1
            cldsrcu(j,k) = tem2 - transc     *tem2

            delbgdn = bglev(j,k-1) - bglay
            tem1 = bglay + tau0*delbgdn
            tem2 = bglay + tauc*delbgdn
            gassrcd = tem1 - trans0(j,k)*tem1
            cldsrcd = tem2 - transc     *tem2

!  --- ... total sky radiance
            radtotd(j) = radtotd(j)*trans0(j,k)*efcfr1(inb,k)           &
     &                 + gassrcd + cldfrac(k)*(cldsrcd - gassrcd)
            totdrad = totdrad + radtotd(j)

!  --- ... clear sky radiance
            radclrd(j) = radclrd(j)*trans0(j,k) + gassrcd
            clrdrad = clrdrad + radclrd(j)
          enddo

        else

!  --- ... clear layer

          do j = 1, NGPT
            ind = itr(j,k)
            inb = ngb(j)                 ! band index

!  --- ... get clear sky transmittance from lookup table
            tau0 = tf(ind)
            trans0(j,k) = trans(ind)

            bglay = pfrac(j,k) * play(inb,k)

            delbgup = bglev(j,k) - bglay
            tem1 = bglay + tau0*delbgup
            gassrcu(j,k) = tem1 - trans0(j,k)*tem1
!           cldsrcu(j,k) = 0.0

            delbgdn  = bglev(j,k-1) - bglay
            tem2 = bglay + tau0*delbgdn
            gassrcd = tem2 - trans0(j,k)*tem2

!  --- ... total sky radiance
            radtotd(j) = radtotd(j)*trans0(j,k) + gassrcd
            totdrad = totdrad + radtotd(j)

!  --- ... clear sky radiance
            radclrd(j) = radclrd(j)*trans0(j,k) + gassrcd
            clrdrad = clrdrad + radclrd(j)
          enddo

        endif

        totdflux(k-1) = totdrad
        totdclfl(k-1) = clrdrad

!  --- ... total sky radiance for each of the spectral bands
        if ( lhlwb ) then
          do j = 1, NGPT
            inb = ngb(j)                 ! band index
            totdfxsb(inb,k-1) = totdfxsb(inb,k-1) + radtotd(j)
          enddo

          totdfxsb(:,NLAY) = f_zero
        endif

      enddo   ! end do_k_loop

      totdflux(NLAY) = f_zero
      totdclfl(NLAY) = f_zero

!  --- ...  spectral emissivity & reflectance
!           include the contribution of spectrally varying longwave
!           emissivity and reflection from the surface to the upward
!           radiative transfer.
!    note: spectral and lambertian reflection are identical for the one
!           angle flux integration used here.

      toturad = f_zero
      clrurad = f_zero

      do j = 1, NGPT
        inb = ngb(j)                 ! band index
        tem1 = 1.0 - semiss(inb)
        tem2 = bglev(j,0) * semiss(inb)

!  --- ... total sky radiance
        radtotu(j) = tem2 + tem1 * radtotd(j)
        toturad = toturad + radtotu(j)

!  --- ... clear sky radiance
        radclru(j) = tem2 + tem1 * radclrd(j)
        clrurad = clrurad + radclru(j)
      enddo

      totuflux(0) = toturad
      totuclfl(0) = clrurad

!  --- ... total sky radiance for each of the spectral bands
      if ( lhlwb ) then
        do j = 1, NGPT
          inb = ngb(j)                 ! band index
          totufxsb(inb,0) = totufxsb(inb,0) + radtotu(j)
        enddo
      endif

!     print *,' toturad(0)=',totuflux(0)
!     print *,' clrurad(0)=',totuclfl(0)

!===> ...  upward radiative transfer
!          toturad holds summed radiance for total sky stream
!          clrurad holds summed radiance for clear sky stream

      do k = 1, NLAY

        toturad = f_zero
        clrurad = f_zero

!  --- ... check flag for cloud in current layer
        if (cldfrac(k) > eps) then

!  --- ... cloudy layers

          do j = 1, NGPT
            inb   = ngb(j)                 ! band index

!  --- ... total sky radiance
            radtotu(j) = radtotu(j)*trans0(j,k)*efcfr1(inb,k)           &
     &         + gassrcu(j,k) + cldfrac(k)*(cldsrcu(j,k)-gassrcu(j,k))
            toturad = toturad + radtotu(j)

!  --- ... clear sky radiance
            radclru(j) = radclru(j)*trans0(j,k) + gassrcu(j,k)
            clrurad = clrurad + radclru(j)
          enddo

        else

!  --- ... clear layer

          do j = 1, NGPT

!  --- ... total sky radiance
            radtotu(j) = radtotu(j)*trans0(j,k) + gassrcu(j,k)
            toturad = toturad + radtotu(j)

!  --- ... clear sky radiance
            radclru(j) = radclru(j)*trans0(j,k) + gassrcu(j,k)
            clrurad = clrurad + radclru(j)
          enddo

        endif

        totuflux(k) = toturad
        totuclfl(k) = clrurad

!  --- ... total sky radiance for each of the spectral bands
        if ( lhlwb ) then
          do j = 1, NGPT
            inb = ngb(j)                 ! band index
            totufxsb(inb,k) = totufxsb(inb,k) + radtotu(j)
          enddo
        endif

      enddo

!===> ...  convert radiances to fluxes and heating rates for total sky.
!          calculates clear sky surface and toa values.  to compute clear
!          sky profiles, uncomment relevant lines below.

      factot = wtnum * fluxfac

      totuflux(0) = totuflux(0) * factot
      totdflux(0) = totdflux(0) * factot
      totuclfl(0) = totuclfl(0) * factot
      totdclfl(0) = totdclfl(0) * factot
      fnet(0) = totuflux(0) - totdflux(0)

      do k = 1, NLAY
        totuflux(k) = totuflux(k) * factot
        totdflux(k) = totdflux(k) * factot

        totuclfl(k) = totuclfl(k) * factot
        totdclfl(k) = totdclfl(k) * factot

        fnet(k) = totuflux(k) - totdflux(k)
        htr (k) = heatfac * (fnet(k-1) - fnet(k)) / delp(k)
      enddo

!! --- ...  optional clear sky heating rates
      if ( lhlw0 ) then
        fnetc(0) = totuclfl(0) - totdclfl(0)

        do k = 1, NLAY
          fnetc(k) = totuclfl(k) - totdclfl(k)
          htrcl(k) = heatfac * (fnetc(k-1) - fnetc(k)) / delp(k)
        enddo
      endif

!! --- ...  optional spectral band heating rates
      if ( lhlwb ) then
        do j = 1, NBANDS
          totufxsb(j,0) = totufxsb(j,0) * factot
          totdfxsb(j,0) = totdfxsb(j,0) * factot
          fnet(0) = totufxsb(j,0) - totdfxsb(j,0)

          do k = 1, NLAY
            totufxsb(j,k) = totufxsb(j,k) * factot
            totdfxsb(j,k) = totdfxsb(j,k) * factot
            fnet(k) = totufxsb(j,k) - totdfxsb(j,k)
            htrb(k,j) = heatfac * (fnet(k-1) - fnet(k)) / delp(k)
          enddo
        enddo
      endif

      return
!...................................
      end subroutine rtrn
!-----------------------------------



!-----------------------------------
      subroutine rtrnmr                                                 &
!...................................

!  ---  inputs:
     &     ( tavel,tz,delp,semiss,cldfrac,taucloud,pfrac,               &
     &       secdiff, itr, NLAY, NLP1,                                  &
!  ---  outputs:
     &       totuflux,totdflux,htr, totuclfl,totdclfl,htrcl, htrb       &
     &     )

!  *******************************************************************  !
!                                                                       !
!  rrtm longwave radiative transfer model                               !
!  atmospheric and environmental research, inc., cambridge, ma          !
!                                                                       !
!  original version:       e. j. mlawer, et al.                         !
!  revision for ncar ccm:  michael j. iacono; september, 1998           !
!  revision to use variable diffusivity angle instead of the original   !
!     fixed value of 1.66: m. j. iacono apr 2004                        !
!                                                                       !
!  this program calculates the upward fluxes, downward fluxes, and      !
!  heating rates for an arbitrary clear or cloudy atmosphere.  the input!
!  to this program is the atmospheric profile, all planck function      !
!  information, and the cloud fraction by layer.  a variable diffusivity!
!  angle (secdiff) is used forthe angle integration.  bands 2-3 and 5-9 !
!  use a value of secdiff that varies from 1.50 to 1.80 as a function of!
!  the column water vapor, and other bands use a value of 1.66.  the    !
!  gaussian weight appropriate to this angle (wtnum=0.5) is applied     !
!  here.  note that use of a single angle for the flux integration      !
!  can cause errors of 1 to 4 w/m2 within cloudy layers.                !
!  this routine computes a generalized maximum/random cloud overlap.    !
!  adjacent cloud layers are treated with maximum overlap in which up   !
!  to two previous layers of cloud information is considered.  non-     !
!  adjacent groups of clouds are treated with random overlap.           !
!                                                                       !
!  *******************************************************************  !
!
      use module_radlw_avplank
!
      implicit none

!  ---  inputs:
      integer, intent(in)  ::  NLAY, NLP1

      integer, intent(in)  ::  itr(:,:)

      real (kind=kind_phys), dimension(0:), intent(in) :: tz, cldfrac

      real (kind=kind_phys), dimension(:),  intent(in) :: tavel, delp,  &
     &       semiss, secdiff

      real (kind=kind_phys), dimension(:,:),intent(in) :: taucloud,     &
     &       pfrac

!  ---  outputs:
      real (kind=kind_phys), dimension(:),  intent(out) :: htr, htrcl
      real (kind=kind_phys), dimension(:,:),intent(out) :: htrb

      real (kind=kind_phys), dimension(0:), intent(out) ::              &
     &       totuflux, totdflux, totuclfl, totdclfl

!  ---  locals:
!  dimensions for radiative transfer
      real (kind=kind_phys), dimension(NGPT,NLAY)     :: gassrcu,       &
     &       cldsrcu, trans0, transc
      real (kind=kind_phys), dimension(NGPT,0:NLAY)   :: bglev
      real (kind=kind_phys), dimension(NGPT)          :: radclru,       &
     &       radclrd, radtotu, radtotd
      real (kind=kind_phys), dimension(NBANDS,0:NLAY) :: plvl,          &
     &       totufxsb, totdfxsb
      real (kind=kind_phys), dimension(NBANDS,NLAY)   :: play,          &
     &       odcld, trncld
      real (kind=kind_phys), dimension(0:NLAY)        :: fnet, fnetc

      real (kind=kind_phys) :: totdrad, clrdrad, toturad, clrurad
      real (kind=kind_phys) :: delbgup, delbgdn, bglay, tau0, tauc,     &
     &       cldsrcd, gassrcd, factot, odsm, tem1, tem2

      integer :: j, k, ind, inb, itm1, itm2, jtm1, jtm2

!  dimensions for cloud overlap adjustment
      real (kind=kind_phys), dimension(NGPT)   ::  clrradu, cldradu,    &
     &       clrradd, cldradd, rad
      real (kind=kind_phys), dimension(1:NLP1) ::  faccld1u, faccld2u,  &
     &       facclr1u, facclr2u, faccmb1u, faccmb2u
      real (kind=kind_phys), dimension(0:NLAY) ::  faccld1d, faccld2d,  &
     &       facclr1d, facclr2d, faccmb1d, faccmb2d
      real (kind=kind_phys) :: fmax, fmin, rat1, rat2, radmod, cldsrc

      logical :: istcldu(NLAY), istcldd(NLAY)

!  ====================  defination of variables  ====================  !
!                                                                       !
!  input variables:                                                     !
!    tavel   (NLAY)       ! layer temperatures (k)                      !
!    tZ      (0:NLAY)     ! level (interface) temperatures (k)          !
!    delp    (NLAY)       ! layer pressure thickness (mb)               !
!    semiss  (NBANDS)     ! surface emissivities for each band          !
!    cldfrac (0:NLP1)     ! layer cloud fraction (padded at 2 ends)     !
!    taucloud(NBANDS,NLAY)! layer cloud optical depth                   !
!    pfrac   (NGPT,NLAY)  ! planck fractions                            !
!    secdiff(NBANDS)      ! variable diffusivity angle defined as an    !
!                           exponential function of the column water    !
!                           amount in bands 2-3 and 5-9. this reduces   !
!                           the bias of several w/m2 in downward surface!
!                           flux in high water profiles caused by using !
!                           the constant diffusivity angle of 1.66.(mji)!
!    itr     (NGPT,NLAY)  ! integer look-up table index                 !
!    NLAY/NLP1            ! number of model layers/levels               !
!                                                                       !
!  constants or shared variables:                                       !
!    NGPT                 ! total number of g-point subintervals        !
!    NBANDS               ! number of longwave spectral bands           !
!    wtnum                ! weight for radiance to flux conversion      !
!    bpade                ! pade constant                               !
!    tau                  ! clear sky optical depth look-up table       !
!    tf                   ! tau transition function look-up table       !
!    trans                ! clear sky transmittance look-up table       !
!                                                                       !
!  output variables:                                                    !
!    totuflux(0:NLAY)     ! upward longwave flux (w/m2)                 !
!    totdflux(0:NLAY)     ! downward longwave flux (w/m2)               !
!    htr     (NLAY)       ! longwave heating rate (k/d or k/s)          !
!    totuclfl(0:NLAY)     ! clear sky upward longwave flux (w/m2)       !
!    totdclfl(0:NLAY)     ! clear sky downward longwave flux (w/m2)     !
!    htrcl   (NLAY)       ! clear sky longwave heating rate (k/d or k/s)!
!    htrb    (NLAY,NBANDS)! spectral band lw heating rate (k/d or k/s)  !
!                                                                       !
!  local variables:                                                     !
!    odcld   (NBANDS,NLAY)! cloud optical depth                         !
!    trncld  (NBANDS,NLAY)! cloud transmittance                         !
!    radtotu (NGPT)       ! upward radiance                             !
!    radtotd (NGPT)       ! downward radiance                           !
!    radclru (NGPT)       ! clear sky upward radiance                   !
!    radclrd (NGPT)       ! clear sky downward radiance                 !
!    toturad              ! spectrally summed upward radiance           !
!    totdrad              ! spectrally summed downward radiance         !
!    clrurad              ! spectrally summed clear sky upward radiance !
!    clrdrad              ! spectrally summed clear sky dnward radiance !
!    totufxsb(NBANDS,NLAY)! spectral band upward longwave flux (w/m2)   !
!    totdfxsb(NBANDS,NLAY)! spectral band downward longwave flux (w/m2) !
!                                                                       !
!    fnet    (0:NLAY)     ! net longwave flux (w/m2)                    !
!    fnetc   (0:NLAY)     ! clear sky net longwave flux (w/m2)          !
!                                                                       !
!  =====================    end of definitions    ====================  !
!

!
!===> ... begin here
!
      do k = 1, NLP1
        faccld1u(k) = f_zero
        faccld2u(k) = f_zero
        facclr1u(k) = f_zero
        facclr2u(k) = f_zero
        faccmb1u(k) = f_zero
        faccmb2u(k) = f_zero
      enddo

      istcldu(1) = cldfrac(1) > eps
      rat1 = f_zero
      rat2 = f_zero

      do k = 1, NLAY-1

        istcldu(k+1) = cldfrac(k+1)>eps .and. cldfrac(k)<=eps

        if (cldfrac(k) > eps) then
!  --- ... maximum/random cloud overlap

          if (cldfrac(k+1) >= cldfrac(k)) then
            if (istcldu(k)) then
              if (cldfrac(k) < 1.0) then
                facclr2u(k+1) = (cldfrac(k+1) - cldfrac(k))             &
     &                        / (1.0 - cldfrac(k))
              endif
              facclr2u(k) = f_zero
              faccld2u(k) = f_zero
            else
              fmax = max(cldfrac(k), cldfrac(k-1))
              if (cldfrac(k+1) > fmax) then
                facclr1u(k+1) = rat2
                facclr2u(k+1) = (cldfrac(k+1) - fmax)/(1.0 - fmax)
              elseif (cldfrac(k+1) < fmax) then
                facclr1u(k+1) = (cldfrac(k+1) - cldfrac(k))             &
     &                        / (cldfrac(k-1) - cldfrac(k))
              else
                facclr1u(k+1) = rat2
              endif
            endif

            if (facclr1u(k+1)>f_zero .or. facclr2u(k+1)>f_zero) then
              rat1 = 1.0
              rat2 = f_zero
            else
              rat1 = f_zero
              rat2 = f_zero
            endif
          else
            if (istcldu(k)) then
              faccld2u(k+1) = (cldfrac(k) - cldfrac(k+1)) /  cldfrac(k)
              facclr2u(k) = f_zero
              faccld2u(k) = f_zero
            else
              fmin = min(cldfrac(k), cldfrac(k-1))
              if (cldfrac(k+1) <= fmin) then
                faccld1u(k+1) = rat1
                faccld2u(k+1) = (fmin - cldfrac(k+1)) / fmin
              else
                faccld1u(k+1) = (cldfrac(k) - cldfrac(k+1))             &
     &                        / (cldfrac(k) - fmin)
              endif
            endif

            if (faccld1u(k+1)>f_zero .or. faccld2u(k+1)>f_zero) then
              rat1 = f_zero
              rat2 = 1.0
            else
              rat1 = f_zero
              rat2 = f_zero
            endif
          endif

          faccmb1u(k+1) = facclr1u(k+1) * faccld2u(k) * cldfrac(k-1)
          faccmb2u(k+1) = faccld1u(k+1) * facclr2u(k)                   &
     &                  * (1.0 - cldfrac(k-1))
        endif

      enddo

      do k = 0, NLAY
        faccld1d(k) = f_zero
        faccld2d(k) = f_zero
        facclr1d(k) = f_zero
        facclr2d(k) = f_zero
        faccmb1d(k) = f_zero
        faccmb2d(k) = f_zero
      enddo

      istcldd(NLAY) = cldfrac(NLAY) > eps
      rat1 = f_zero
      rat2 = f_zero

      do k = NLAY, 2, -1

        istcldd(k-1) = cldfrac(k-1) > eps .and. cldfrac(k)<=eps

        if (cldfrac(k) > eps) then

          if (cldfrac(k-1) >= cldfrac(k)) then
            if (istcldd(k)) then
              if (cldfrac(k) < 1.0) then
                facclr2d(k-1) = (cldfrac(k-1) - cldfrac(k))             &
     &                        / (1.0 - cldfrac(k))
              endif
              facclr2d(k) = f_zero
              faccld2d(k) = f_zero
            else
              fmax = max(cldfrac(k), cldfrac(k+1))

              if (cldfrac(k-1) > fmax) then
                facclr1d(k-1) = rat2
                facclr2d(k-1) = (cldfrac(k-1) - fmax)/(1.0 - fmax)
              elseif (cldfrac(k-1) < fmax) then
                facclr1d(k-1) = (cldfrac(k-1) - cldfrac(k))             &
     &                        / (cldfrac(k+1) - cldfrac(k))
              else
                facclr1d(k-1) = rat2
              endif
            endif

            if (facclr1d(k-1)>f_zero .or. facclr2d(k-1)>f_zero) then
              rat1 = 1.0
              rat2 = f_zero
            else
              rat1 = f_zero
              rat2 = f_zero
            endif
          else
            if (istcldd(k)) then
              faccld2d(k-1) = (cldfrac(k) - cldfrac(k-1)) / cldfrac(k)
              facclr2d(k) = f_zero
              faccld2d(k) = f_zero
            else
              fmin = min(cldfrac(k), cldfrac(k+1))

              if (cldfrac(k-1) <= fmin) then
                faccld1d(k-1) = rat1
                faccld2d(k-1) = (fmin - cldfrac(k-1)) / fmin
              else
                faccld1d(k-1) = (cldfrac(k) - cldfrac(k-1))             &
     &                        / (cldfrac(k) - fmin)
              endif
            endif

            if (faccld1d(k-1)>f_zero .or. faccld2d(k-1)>f_zero) then
              rat1 = f_zero
              rat2 = 1.0
            else
              rat1 = f_zero
              rat2 = f_zero
            endif
          endif

          faccmb1d(k-1) = facclr1d(k-1) * faccld2d(k) * cldfrac(k+1)
          faccmb2d(k-1) = faccld1d(k-1) * facclr2d(k)                   &
     &                  * (1.0 - cldfrac(k+1))

        endif

      enddo

!  --- ... calculate the integrated planck functions at the level and
!          layer temperatures.

      itm1 = min(NPLNK, max(1, int(tz(0)-159.0) ))
      itm2 = min(NPLNK, itm1+1)
      tem1 = tz(0) - int(tz(0))
      do j = 1, NBANDS
        plvl(j,0) = delwave(j) * ( totplnk(itm1,j)                      &
     &            + tem1 * (totplnk(itm2,j) - totplnk(itm1,j)) )
      enddo

      do k = 1, NLAY
        itm1 = min(NPLNK, max(1, int(tz(k)   -159.0) ))
        itm2 = min(NPLNK, itm1+1)
        jtm1 = min(NPLNK, max(1, int(tavel(k)-159.0) ))
        jtm2 = min(NPLNK, jtm1+1)

        tem1 = tz(k)    - int(tz(k))
        tem2 = tavel(k) - int(tavel(k))

        do j = 1, NBANDS
          plvl(j,k) = delwave(j) * ( totplnk(itm1,j)                    &
     &              + tem1 * (totplnk(itm2,j) - totplnk(itm1,j)) )
          play(j,k) = delwave(j) * ( totplnk(jtm1,j)                    &
     &              + tem2 * (totplnk(jtm2,j) - totplnk(jtm1,j)) )

!  --- ... cloudy sky optical depth and absorptivity.
! mji     odcld(j,k)  = secang * taucloud(j,k)
          odcld(j,k)  = secdiff(j) * taucloud(j,k)
          trncld(j,k) = exp( -odcld(j,k) )
        enddo

        do j = 1, NGPT
          inb = ngb(j)                 ! band index
          bglev(j,k-1) = pfrac(j,k) * plvl(inb,k-1)
        enddo
      enddo

!  --- ...  initialize for radiative transfer.

      if ( lhlwb ) then
        do k = 0, NLAY
          do j = 1, NBANDS
            totufxsb(j,k) = f_zero
            totdfxsb(j,k) = f_zero
          enddo
        enddo
      endif

      do j = 1, NGPT
        inb = ngb(j)                 ! band index
        radclrd(j) = f_zero
        radtotd(j) = f_zero
        bglev (j,NLAY) = pfrac(j,NLAY) * plvl(inb,NLAY)
      enddo

!===> ...  downward radiative transfer
!          totdrad holds summed radiance for total sky stream
!          clrdrad holds summed radiance for clear sky stream

      do k = NLAY, 1, -1

        totdrad = f_zero
        clrdrad = f_zero

        if (istcldd(k)) then
          do j = 1, NGPT
            cldradd(j) = cldfrac(k) * radtotd(j)
            clrradd(j) = radtotd(j) - cldradd(j)
            rad    (j) = f_zero
          enddo
        endif

        if (cldfrac(k) > eps) then
!  --- ... cloudy layer

          do j = 1, NGPT
!  --- ... get lookup table index
            ind = itr(j,k)
            inb = ngb(j)                 ! band index

!  --- ...  get tf from lookup table
            tau0 = tf(ind)
            trans0(j,k) = trans(ind)
            transc(j,k) = trans(ind) * trncld(inb,k)

!  --- ... add clear sky and cloud optical depths
            odsm = tau(ind) + odcld(inb,k)
            tauc = odsm / (bpade + odsm)

            bglay = pfrac(j,k) * play(inb,k)
            delbgup = bglev(j,k) - bglay
            tem1 = bglay + tau0*delbgup
            tem2 = bglay + tauc*delbgup
            gassrcu(j,k) = tem1 - trans0(j,k)*tem1
            cldsrcu(j,k) = tem2 - transc(j,k)*tem2

            delbgdn = bglev(j,k-1) - bglay
            tem1 = bglay + tau0*delbgdn
            tem2 = bglay + tauc*delbgdn
            gassrcd = tem1 - trans0(j,k)*tem1
            cldsrcd = tem2 - transc(j,k)*tem2

            cldradd(j) = cldradd(j)*transc(j,k) + cldfrac(k)*cldsrcd
            clrradd(j) = clrradd(j)*trans0(j,k)                         &
     &                                    + (1.0-cldfrac(k))*gassrcd

!  --- ... total sky radiance
            radtotd(j) = cldradd(j) + clrradd(j)
            totdrad = totdrad + radtotd(j)

!  --- ... clear sky radiance
            radclrd(j) = radclrd(j)*trans0(j,k) + gassrcd
            clrdrad = clrdrad + radclrd(j)

            radmod = rad(j)                                             &
     &        * (facclr1d(k-1)*trans0(j,k) + faccld1d(k-1)*transc(j,k)) &
     &         - faccmb1d(k-1)*gassrcd + faccmb2d(k-1)*cldsrcd

            rad(j) = -radmod + facclr2d(k-1)*(clrradd(j) + radmod)      &
     &                       - faccld2d(k-1)*(cldradd(j) - radmod)
            cldradd(j) = cldradd(j) + rad(j)
            clrradd(j) = clrradd(j) - rad(j)

          enddo

        else

!  --- ... clear layer

          do j = 1, NGPT
            ind = itr(j,k)
            inb = ngb(j)                 ! band index

!  --- ... get tf from lookup table
            tau0 = tf(ind)
            trans0(j,k) = trans(ind)
            transc(j,k) = f_zero

            bglay = pfrac(j,k) * play(inb,k)

            delbgup = bglev(j,k) - bglay
            tem1 = bglay + tau0*delbgup
            gassrcu(j,k) = tem1 - trans0(j,k)*tem1
!           cldsrcu(j,k) = 0.0

            delbgdn  = bglev(j,k-1) - bglay
            tem2 = bglay + tau0*delbgdn
            gassrcd = tem2 - trans0(j,k)*tem2

!  --- ... total sky radiance
            radtotd(j) = radtotd(j)*trans0(j,k) + gassrcd
            totdrad = totdrad + radtotd(j)

!  --- ... clear sky radiance
            radclrd(j) = radclrd(j)*trans0(j,k) + gassrcd
            clrdrad = clrdrad + radclrd(j)
          enddo

        endif

        totdflux(k-1) = totdrad
        totdclfl(k-1) = clrdrad

!  --- ... total sky radiance for each of the spectral bands
        if ( lhlwb ) then
          do j = 1, NGPT
            inb = ngb(j)                 ! band index
            totdfxsb(inb,k-1) = totdfxsb(inb,k-1) + radtotd(j)
          enddo

          totdfxsb(:,NLAY) = f_zero
        endif

      enddo   ! end do_k_loop

      totdflux(NLAY) = f_zero
      totdclfl(NLAY) = f_zero

!  --- ...  spectral emissivity & reflectance
!           include the contribution of spectrally varying longwave
!           emissivity and reflection from the surface to the upward
!           radiative transfer.
!    note: spectral and lambertian reflection are identical for the one
!           angle flux integration used here.

      toturad = f_zero
      clrurad = f_zero

      do j = 1, NGPT
        inb = ngb(j)                 ! band index
        tem1 = 1.0 - semiss(inb)
        tem2 = bglev(j,0) * semiss(inb)

!  --- ... total sky radiance
        radtotu(j) = tem2 + tem1 * radtotd(j)
        toturad = toturad + radtotu(j)

!  --- ... clear sky radiance
        radclru(j) = tem2 + tem1 * radclrd(j)
        clrurad = clrurad + radclru(j)
      enddo

      totuflux(0) = toturad
      totuclfl(0) = clrurad

!  --- ... total sky radiance for each of the spectral bands
      if ( lhlwb ) then
        do j = 1, NGPT
          inb = ngb(j)                 ! band index
          totufxsb(inb,0) = totufxsb(inb,0) + radtotu(j)
        enddo
      endif

!===> ...   upward radiative transfer
!           toturad holds the summed radiance for total sky stream
!           clrurad holds the summed radiance for clear sky stream

      do k = 1, NLAY

        toturad = f_zero
        clrurad = f_zero

        if (istcldu(k)) then
          do j = 1, NGPT
            cldradu(j) = radtotu(j) * cldfrac(k)
            clrradu(j) = radtotu(j) - cldradu(j)
            rad(j)     = f_zero
          enddo
        endif

!  --- ... check flag for cloud in current layer
        if (cldfrac(k) > eps) then

!  --- ... cloudy layers

          do j = 1, NGPT
            cldradu(j) = cldradu(j)*transc(j,k)+cldfrac(k)*cldsrcu(j,k)
            clrradu(j) = clrradu(j)*trans0(j,k)                         &
     &                                + (1.0 - cldfrac(k))*gassrcu(j,k)

!  --- ... total sky radiance
            radtotu(j) = cldradu(j) + clrradu(j)
            toturad = toturad + radtotu(j)

!  --- ... clear sky radiance
            radclru(j) = radclru(j)*trans0(j,k) + gassrcu(j,k)
            clrurad = clrurad + radclru(j)

            radmod = rad(j)                                             &
     &        * (facclr1u(k+1)*trans0(j,k) + faccld1u(k+1)*transc(j,k)) &
     &        - faccmb1u(k+1)*gassrcu(j,k) + faccmb2u(k+1)*cldsrcu(j,k)

            rad(j) = -radmod + facclr2u(k+1)*(clrradu(j) + radmod)      &
     &                       - faccld2u(k+1)*(cldradu(j) - radmod)
            cldradu(j) = cldradu(j) + rad(j)
            clrradu(j) = clrradu(j) - rad(j)
          enddo

        else

!  --- ... clear layer

          do j = 1, NGPT

!  --- ... total sky radiance
            radtotu(j) = radtotu(j)*trans0(j,k) + gassrcu(j,k)
            toturad = toturad + radtotu(j)

!  --- ... clear sky radiance
!          upward clear and total sky streams must remain separate
!          because surface reflectance is different for each.
            radclru(j) = radclru(j)*trans0(j,k) + gassrcu(j,k)
            clrurad = clrurad + radclru(j)
          enddo

        endif

        totuflux(k) = toturad
        totuclfl(k) = clrurad

!  --- ... total sky radiance for each of the spectral bands
        if ( lhlwb ) then
          do j = 1, NGPT
            inb = ngb(j)                 ! band index
            totufxsb(inb,k) = totufxsb(inb,k) + radtotu(j)
          enddo
        endif

      enddo

!===> ...  convert radiances to fluxes and heating rates for total sky.
!          calculates clear sky surface and toa values. to compute clear
!          sky profiles, uncomment relevant lines below.

      factot = fluxfac * wtnum

      totuflux(0) = totuflux(0) * factot
      totdflux(0) = totdflux(0) * factot
      totuclfl(0) = totuclfl(0) * factot
      totdclfl(0) = totdclfl(0) * factot
      fnet(0) = totuflux(0) - totdflux(0)

      do k = 1, NLAY
        totuflux(k) = totuflux(k) * factot
        totdflux(k) = totdflux(k) * factot

        totuclfl(k) = totuclfl(k) * factot
        totdclfl(k) = totdclfl(k) * factot

        fnet(k) = totuflux(k) - totdflux(k)
        htr (k) = heatfac * (fnet(k-1) - fnet(k)) / delp(k)
      enddo

!! --- ...  optional clear sky heating rates
      if ( lhlw0 ) then
        fnetc(0) = totuclfl(0) - totdclfl(0)

        do k = 1, NLAY
          fnetc(k) = totuclfl(k) - totdclfl(k)
          htrcl(k) = heatfac * (fnetc(k-1) - fnetc(k)) / delp(k)
        enddo
      endif

!! --- ...  optional spectral band heating rates
      if ( lhlwb ) then
        do j = 1, NBANDS
          totufxsb(j,0) = totufxsb(j,0) * factot
          totdfxsb(j,0) = totdfxsb(j,0) * factot
          fnet(0) = totufxsb(j,0) - totdfxsb(j,0)

          do k = 1, NLAY
            totufxsb(j,k) = totufxsb(j,k) * factot
            totdfxsb(j,k) = totdfxsb(j,k) * factot
            fnet(k) = totufxsb(j,k) - totdfxsb(j,k)
            htrb(k,j) = heatfac * (fnet(k-1) - fnet(k)) / delp(k)
          enddo
        enddo
      endif

      return
!...................................
      end subroutine rtrnmr
!-----------------------------------




!-----------------------------------
      subroutine taumol                                                 &
!...................................
!  ---  inputs:
     &     ( laytrop,layswtch,laylow,h2ovmr,colamt,wx,co2mult,          &
     &       fac00,fac01,fac10,fac11,jp,jt,jt1,selffac,selffrac,        &
     &       indself,forfac,secdiff,tauaer, NLAY,                       &
!  ---  outputs:
     &       itr, pfrac                                                 &
     &     )

!  ************    original subprogram description    ***************  *
!                                                                      *
!                  optical depths developed for the                    *
!                                                                      *
!                rapid radiative transfer model (rrtm)                 *
!                                                                      *
!            atmospheric and environmental research, inc.              *
!                        840 memorial drive                            *
!                        cambridge, ma 02139                           *
!                                                                      *
!                           eli j. mlawer                              *
!                         steven j. taubman                            *
!                                                                      *
!                       email:  mlawer@aer.com                         *
!                                                                      *
!        the authors wish to acknowledge the contributions of the      *
!        following people:  patrick d. brown, michael j. iacono,       *
!        ronald e. farren, luke chen, robert bergstrom.                *
!                                                                      *
!  revision for ncar ccm:  michael j. iacono; september, 1998          *
!                                                                      *
!     taumol                                                           *
!                                                                      *
!     this file contains the subroutines taugbn (where n goes from     *
!     1 to 16).  taugbn calculates the optical depths and planck       *
!     fractions per g-value and layer for band n.                      *
!                                                                      *
!  output:  transmittance look-up table index (unitless)               *
!           fractions needed to compute planck functions at every layer*
!           and g-value                                                *
!                                                                      !
!  description:                                                        !
!     NG##        - number of g-values in band ## (##=01-16)           !
!     nspa(iband) - for the lower atmosphere, the number of reference  !
!                   atmospheres that are stored for band iband per     !
!                   pressure level and temperature.  each of these     !
!                   atmospheres has different relative amounts of the  !
!                   key species for the band (i.e. different binary    !
!                   species parameters).                               !
!     nspb(iband) - same for upper atmosphere                          !
!     oneminus    - since problems are caused in some cases by         !
!                   interpolation parameters equal to or greater than  !
!                   1, for these cases these parameters are set to this!
!                   value, slightly < 1.                               !
!     laytrop, layswtch,laylow                                         !
!                 - layer at which switch is made from one combination !
!                   of key species to another                          !
!     h2ovmr(NLAY)- layer h2o volume mixing ratio (vmr)                !
!     colamt(NLAY,MAXGAS)                                              !
!                 - column amounts of water vapor,carbon dioxide,      !
!                   ozone, nitrous oxide, methane, o2, co, respectively!
!                   (molecules/cm**2)                                  !
!     co2mult(NLAY)-for bands in which carbon dioxide is implemented   !
!                   as a trace species, this is the factor used to     !
!                   multiply the band's average co2 absorption         !
!                   coefficient to get the added contribution to the   !
!                   optical depth relative to 355 ppm.                 !
!     facij (NLAY)- for layer lay, these are factors that are needed to!
!                   compute the interpolation factors that multiply the!
!                   appropriate reference k-values.  a value of 0 (1)  !
!                   for i,j indicates that the corresponding factor    !
!                   multiplies reference k-value for the lower (higher)!
!                   of the two appropriate temperatures, and altitudes,!
!                   respectively.                                      !
!     jp (NLAY)   - the index of the lower (in altitude) of the two    !
!                   appropriate reference pressure levels needed for   !
!                   interpolation.                                     !
!     jt, jt1(NLAY)-the indices of the lower of the two appropriate    !
!                   reference temperatures needed for interpolation    !
!                   (for pressure levels jp and jp+1, respectively)    !
!     selffac(NLAY)-scale factor needed to water vapor self-continuum, !
!                   equals (water vapor density)/(atmospheric density  !
!                   at 296k and 1013 mb)                               !
!     selffrac(NLAY)factor needed for temperature interpolation of     !
!                   reference water vapor self-continuum data          !
!     indself(NLAY)-index of the lower of the two appropriate reference!
!                   temperatures needed for the self-continuum         !
!                   interpolation                                      !
!     secdiff(NBANDS)- variable diffusivity angle defined as an        !
!                   exponential function of the column water amount in !
!                   bands 2-3 and 5-9. this reduces the bias of several!
!                   w/m2 in downward surface flux in high water        !
!                   profiles caused by using the constant diffusivity  !
!                   angle of 1.66.(mji)                                !
!     tauaer(NBANDS,NLAY)- aerosols optical depth                      !
!                                                                      !
!  data input                                                          !
!     absa(nspa(nn),5,13,NGnn), absb(nspb(nn),5,13:59,NGnn),           !
!     selfref(10,NGnn)                                                 !
!                      (note:  nn is the band number)                  !
!                                                                      !
!     absa    - k-values for low reference atmospheres (no water vapor !
!               self-continuum) (units: cm**2/molecule)                !
!     absb    - k-values for high reference atmospheres (all sources)  !
!               (units: cm**2/molecule)                                !
!     selfref - k-values for water vapor self-continuum for reference  !
!               atmospheres (used below laytrop)                       !
!               (units: cm**2/molecule)                                !
!                                                                      !
!  ******************************************************************  !
!
      implicit none
!
!  ---  inputs:
      integer, intent(in) :: laytrop, layswtch, laylow, NLAY

      integer, dimension(:), intent(in) :: jp, jt, jt1, indself

      real (kind=kind_phys), dimension(:),  intent(in) :: h2ovmr,       &
     &       co2mult, fac00, fac01, fac10, fac11, selffac, selffrac,    &
     &       forfac, secdiff

      real (kind=kind_phys), dimension(:,:),intent(in) :: colamt, wx,   &
     &       tauaer

!  ---  outputs:
      real (kind=kind_phys), dimension(:,:), intent(out) :: pfrac

      integer,               dimension(:,:), intent(out) :: itr

!  ---  locals:
      real (kind=kind_phys) :: taug(NGPT,NLAY), tem1, tem2
      integer :: j, k, ja, jb, kk, id0(NLAY,NBANDS), id1(NLAY,NBANDS),  &
     &           inb
!
!===> ... begin here
!
      do j = 1, NBANDS
        ja = 13
        jb = 12
        kk = laytrop
        if (j == 8) then
          ja = 7
          jb = 6
          kk = layswtch
        endif

        do k = 1, kk
          id0(k,j) = ((jp(k)-1) *5 + jt (k) - 1) * nspa(j)
          id1(k,j) = ( jp(k)    *5 + jt1(k) - 1) * nspa(j)
        enddo
        do k = kk+1, NLAY
          id0(k,j) = ((jp(k)-ja)*5 + jt (k) - 1) * nspb(j)
          id1(k,j) = ((jp(k)-jb)*5 + jt1(k) - 1) * nspb(j)
        enddo
      enddo

      call taugb01
      call taugb02
      call taugb03
      call taugb04
      call taugb05
      call taugb06
      call taugb07
      call taugb08
      call taugb09
      call taugb10
      call taugb11
      call taugb12
      call taugb13
      call taugb14
      call taugb15
      call taugb16

! mji do k = 1, NLAY
!       do j = 1, NGPT
!         tem1 = max( f_zero, secang*taug(j,k) )
!         tem2 = tem1 / (bpade + tem1)
!         itr(j,k) = 5.0e3 * tem2 + 0.5
!       enddo
! mji enddo

      do j = 1, NGPT
        inb = ngb(j)

        do k = 1, NLAY
!         tem1 = max( f_zero, secdiff(inb)*taug(j,k) )
          tem1 = max( f_zero, secdiff(inb)*(taug(j,k)+tauaer(inb,k)) )
          tem2 = tem1 / (bpade + tem1)
          itr(j,k) = max(0, min(N5000, int(5.0e3*tem2+0.5) ))
        enddo
      enddo


! =================
      contains
! =================

!-----------------------------------
      subroutine taugb01
!...................................

!  ------------------------------------------------------------------  !
!     written by eli j. mlawer, atmospheric & environmental research.  !
!     revised by michael j.iacono, atmospheric & environmental research!
!                                                                      !
!     band 1:  10-250 cm-1 (low - h2o; high - h2o)                     !
!                                                                      !
!     compute the optical depth by interpolating in ln(pressure) and   !
!     temperature.  below laytrop, the water vapor self-continuum      !
!     is interpolated (in temperature) separately.                     !
!  ------------------------------------------------------------------  !
!
      use module_radlw_kgb01
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind11, ind12, inds

      do k = 1, laytrop
        ind01 = id0(k,1) + 1
        ind02 = ind01 + 1
        ind11 = min(MSA01, id1(k,1) + 1 )
        ind12 = min(MSA01, ind11 + 1)
        inds = indself(k)

        do j = 1, NG01
          taug(j,k) = colamt(k,1)                                       &
     &         * ( fac00(k)*absa(ind01,j) + fac10(k)*absa(ind02,j) +    &
     &             fac01(k)*absa(ind11,j) + fac11(k)*absa(ind12,j) +    &
     &             selffac(k)*( selfref(inds,j) + selffrac(k) *         &
     &            (selfref(inds+1,j) - selfref(inds,j)) ) +             &
     &             forfac(k)*forref(j) )

          pfrac(j,k) = fracrefa(j)
        enddo
      enddo

      do k = laytrop+1, NLAY
        ind01 = id0(k,1) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,1) + 1
        ind12 = ind11 + 1

        do j = 1, NG01
          taug(j,k) = colamt(k,1)                                       &
     &         * ( fac00(k)*absb(ind01,j) + fac10(k)*absb(ind02,j) +    &
     &             fac01(k)*absb(ind11,j) + fac11(k)*absb(ind12,j) +    &
     &             forfac(k) * forref(j) )

          pfrac(j,k) = fracrefb(j)
        enddo
      enddo

      return
!...................................
      end subroutine taugb01
!-----------------------------------


!-----------------------------------
      subroutine taugb02
!...................................

!     band 2:  250-500 cm-1 (low - h2o; high - h2o)
!
      use module_radlw_kgb02
!
      implicit none
!
      integer :: i, j, k, ind01, ind02, ind11, ind12, inds, ifrac, ifp
!
      real (kind=kind_phys) :: fc00, fc01, fc10, fc11, h2oparam,        &
     &      fracint, one
      data  one / 1.0 /

!     compute the optical depth by interpolating in ln(pressure) and 
!     temperature.  below laytrop, the water vapor self-continuum is 
!     interpolated (in temperature) separately.

      do k = 1, laytrop
        h2oparam = h2ovmr(k) / (h2ovmr(k) + 0.002)

        ifrac = 13
!!changed b_do_ifrac : do i = 2, 12
        lab_do_ifrac : do i = 2, 13
          if (h2oparam >= refparam(i)) then
            ifrac = i
            exit lab_do_ifrac
          endif
        enddo lab_do_ifrac

        fracint = max(-one, min(one, (h2oparam - refparam(ifrac))       &
     &          / (refparam(ifrac-1) - refparam(ifrac)) ))

        ifp = max( 0, int(2.e2*(fac11(k)+fac01(k))+0.5) )
        fc00 = fac00(k) * corr2(ifp)
        fc10 = fac10(k) * corr2(ifp)
        fc01 = fac01(k) * corr1(ifp)
        fc11 = fac11(k) * corr1(ifp)

        ind01 = id0(k,2) + 1
        ind02 = ind01 + 1
        ind11 = min(MSA02, id1(k,2) + 1 )
        ind12 = min(MSA02, ind11 + 1 )
        inds = indself(k)

        do j = 1, NG02
          taug(NS02+j,k) = colamt(k,1)                                  &
     &         * ( fc00*absa(ind01,j) + fc10*absa(ind02,j) +            &
     &             fc01*absa(ind11,j) + fc11*absa(ind12,j) +            &
     &             selffac(k)*(selfref(inds,j) + selffrac(k) *          &
     &            ( selfref(inds+1,j) - selfref(inds,j)) ) +            &
     &             forfac(k) * forref(j) )

          pfrac(NS02+j,k) = fracrefa(j,ifrac) + fracint                 &
     &         * (fracrefa(j,ifrac-1) - fracrefa(j,ifrac))
        enddo
      enddo

      do k = laytrop+1, NLAY
        ifp = max( 0, int(2.e2*(fac11(k)+fac01(k))+0.5) )
        fc00 = fac00(k) * corr2(ifp)
        fc10 = fac10(k) * corr2(ifp)
        fc01 = fac01(k) * corr1(ifp)
        fc11 = fac11(k) * corr1(ifp)

        ind01 = id0(k,2) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,2) + 1
        ind12 = ind11 + 1

        do j = 1, NG02
          taug(NS02+j,k) = colamt(k,1)                                  &
     &         * ( fc00*absb(ind01,j) + fc10*absb(ind02,j) +            &
     &             fc01*absb(ind11,j) + fc11*absb(ind12,j) +            &
     &             forfac(k) * forref(j) )

          pfrac(NS02+j,k) = fracrefb(j)
        enddo
      enddo

      return
!...................................
      end subroutine taugb02
!-----------------------------------


!-----------------------------------
      subroutine taugb03
!...................................

!     band 3:  500-630 cm-1 (low - h2o,co2; high - h2o,co2)
!
      use module_radlw_kgb03
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind03, ind04, ind11, ind12, ind13, &
     &           ind14, inds, js, ns, jp0, jp1
!
      real (kind=kind_phys) :: fac000, fac010, fac100, fac110, fac001,  &
     &      fac011, fac101, fac111, strrat, speccomb, specmult,         &
     &      fs, fs1, fp, ratio, n2omult, tem0, tem1, tem2

      strrat = 1.19268

!     compute the optical depth by interpolating in ln(pressure), 
!     temperature, and appropriate species.  below laytrop, the water
!     vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, laytrop
        speccomb = colamt(k,1) + strrat*colamt(k,2)
        specmult = 8.0 * min(oneminus, colamt(k,1)/speccomb)

        js = 1 + int(specmult)
        fs = specmult - int(specmult)
        if (js == 8) then
          if (fs >= 0.9) then
            js = 9
            fs = 10.0 * (fs - 0.9)
          else
            fs = fs / 0.9
          endif
        endif

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,3) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 10
        ind04 = ind01 + 11
        ind11 = min(MSA03, id1(k,3) + js )
        ind12 = min(MSA03, ind11 + 1 )
        ind13 = min(MSA03, ind11 + 10)
        ind14 = min(MSA03, ind11 + 11)
        inds = indself(k)

        jp0 = jp(k)
        jp1 = jp0 + 1
        ns = js + int(fs + 0.5)
        if (ns == 10) then
          tem1 = n2oref(jp0) / h2oref(jp0)
          tem2 = n2oref(jp1) / h2oref(jp1)
        else
          tem0 = (1.0 - etaref(ns)) / strrat
          tem1 = tem0 * n2oref(jp0) / co2ref(jp0)
          tem2 = tem0 * n2oref(jp1) / co2ref(jp1)
        endif
        ratio = tem1 + (fac01(k) + fac11(k)) * (tem2 - tem1)
        n2omult = colamt(k,4) - speccomb*ratio

        do j = 1, NG03
          taug(NS03+j,k) = speccomb                                     &
     &         * ( fac000*absa(ind01,j) + fac100*absa(ind02,j) +        &
     &             fac010*absa(ind03,j) + fac110*absa(ind04,j) +        &
     &             fac001*absa(ind11,j) + fac101*absa(ind12,j) +        &
     &             fac011*absa(ind13,j) + fac111*absa(ind14,j) )        &
     &         + colamt(k,1) * ( selffac(k)*(selfref(inds,j) +          &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) ) +  &
     &             forfac(k) * forref(j) )                              &
     &         + n2omult * absn2oa(j)

          pfrac(NS03+j,k) = fracrefa(j,js)                              &
     &          + fs * (fracrefa(j,js+1) - fracrefa(j,js))
        enddo
      enddo

      do k = laytrop+1, NLAY
        speccomb = colamt(k,1) + strrat*colamt(k,2)
        specmult = 4.0 * min(oneminus, colamt(k,1)/speccomb)

        js = 1 + int(specmult)
        fs = specmult - int(specmult)

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,3) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 5
        ind04 = ind01 + 6
        ind11 = id1(k,3) + js
        ind12 = ind11 + 1
        ind13 = ind11 + 5
        ind14 = ind11 + 6

        jp0 = jp(k)
        jp1 = jp0 + 1
        ns = js + int(fs + 0.5)
        if (ns == 5) then
          tem1 = n2oref(jp0) / h2oref(jp0)
          tem2 = n2oref(jp1) / h2oref(jp1)
        else
          tem0 = (1.0 - etaref(ns)) / strrat
          tem1 = tem0 * n2oref(jp0) / co2ref(jp0)
          tem2 = tem0 * n2oref(jp1) / co2ref(jp1)
        endif
        ratio = tem1 + (fac01(k) + fac11(k)) * (tem2 - tem1)
        n2omult = colamt(k,4) - speccomb * ratio

        do j = 1, NG03
          taug(NS03+j,k) = speccomb                                     &
     &         * ( fac000*absb(ind01,j) + fac100*absb(ind02,j) +        &
     &             fac010*absb(ind03,j) + fac110*absb(ind04,j) +        &
     &             fac001*absb(ind11,j) + fac101*absb(ind12,j) +        &
     &             fac011*absb(ind13,j) + fac111*absb(ind14,j) )        &
     &         + colamt(k,1) * forfac(k)*forref(j)                      &
     &         + n2omult * absn2ob(j)

          pfrac(NS03+j,k) = fracrefb(j,js)                              &
     &          + fs * (fracrefb(j,js+1) - fracrefb(j,js))
        enddo
      enddo

      return
!...................................
      end subroutine taugb03
!-----------------------------------


!-----------------------------------
      subroutine taugb04
!...................................

!     band 4:  630-700 cm-1 (low - h2o,co2; high - o3,co2)
!
      use module_radlw_kgb04
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind03, ind04, ind11, ind12, ind13, &
     &           ind14, inds, js, ns
!
      real (kind=kind_phys) :: fac000, fac010, fac100, fac110, fac001,  &
     &      fac011, fac101, fac111, strrat1, strrat2, speccomb,         &
     &      specmult, fs, fs1

      strrat1 = 850.577
      strrat2 = 35.7416

!     compute the optical depth by interpolating in ln(pressure), 
!     temperature, and appropriate species.  below laytrop, the water
!     vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, laytrop
        speccomb = colamt(k,1) + strrat1*colamt(k,2)
        specmult = 8.0 * min(oneminus, colamt(k,1)/speccomb)

        js = 1 + int(specmult)
        fs = specmult - int(specmult)

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,4) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 9
        ind04 = ind01 + 10
        ind11 = min(MSA04, id1(k,4) + js )
        ind12 = min(MSA04, ind11 + 1 )
        ind13 = min(MSA04, ind11 + 9 )
        ind14 = min(MSA04, ind11 + 10)
        inds = indself(k)

        do j = 1, NG04
          taug(NS04+j,k) = speccomb                                     &
     &         * ( fac000*absa(ind01,j) + fac100*absa(ind02,j) +        &
     &             fac010*absa(ind03,j) + fac110*absa(ind04,j) +        &
     &             fac001*absa(ind11,j) + fac101*absa(ind12,j) +        &
     &             fac011*absa(ind13,j) + fac111*absa(ind14,j) )        &
     &         + colamt(k,1) * selffac(k)*( selfref(inds,j) +           &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) )

          pfrac(NS04+j,k) = fracrefa(j,js)                              &
     &         + fs * (fracrefa(j,js+1) - fracrefa(j,js))
        enddo
      enddo

      do k = laytrop+1, NLAY
        speccomb = colamt(k,3) + strrat2*colamt(k,2)
        specmult = 4.0 * min(oneminus, colamt(k,3)/speccomb)

        js = 1 + int(specmult)
        fs = specmult - int(specmult)
        if (js > 1) then
          js = js + 1
        elseif (fs >= 0.0024) then
          js = 2
          fs = (fs - 0.0024) / 0.9976
        else
          js = 1
          fs = fs / 0.0024
        endif

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,4) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 6
        ind04 = ind01 + 7
        ind11 = id1(k,4) + js
        ind12 = ind11 + 1
        ind13 = ind11 + 6
        ind14 = ind11 + 7

        do j = 1, NG04
          taug(NS04+j,k) = speccomb                                     &
     &         * ( fac000*absb(ind01,j) + fac100*absb(ind02,j) +        &
     &             fac010*absb(ind03,j) + fac110*absb(ind04,j) +        &
     &             fac001*absb(ind11,j) + fac101*absb(ind12,j) +        &
     &             fac011*absb(ind13,j) + fac111*absb(ind14,j) )

          pfrac(NS04+j,k) = fracrefb(j,js)                              &
     &          + fs * (fracrefb(j,js+1) - fracrefb(j,js))
        enddo

!     empirical modification to code to improve stratospheric cooling rates
!     for co2. revised to apply weighting for g-point reduction in this band.
!     from mike iacono, aer, april 01, 2003.

        taug(NS04+8, k) = taug(NS04+8, k) * 0.92
        taug(NS04+9, k) = taug(NS04+9, k) * 0.88
        taug(NS04+10,k) = taug(NS04+10,k) * 1.07
        taug(NS04+11,k) = taug(NS04+11,k) * 1.10
        taug(NS04+12,k) = taug(NS04+12,k) * 0.99
        taug(NS04+13,k) = taug(NS04+13,k) * 0.88
        taug(NS04+14,k) = taug(NS04+14,k) * 0.943

      enddo

      return
!...................................
      end subroutine taugb04
!-----------------------------------


!-----------------------------------
      subroutine taugb05
!...................................

!     band 5:  700-820 cm-1 (low - h2o,co2; high - o3,co2)
!
      use module_radlw_kgb05
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind03, ind04, ind11, ind12, ind13, &
     &           ind14, inds, js, ns
!
      real (kind=kind_phys) :: fac000, fac010, fac100, fac110, fac001,  &
     &      fac011, fac101, fac111, strrat1, strrat2, speccomb,         &
     &      specmult, fs, fs1

      strrat1 = 90.4894
      strrat2 = 0.900502

!     compute the optical depth by interpolating in ln(pressure), 
!     temperature, and appropriate species.  below laytrop, the water
!     vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, laytrop
        speccomb = colamt(k,1) + strrat1*colamt(k,2)
        specmult = 8.0 * min(oneminus, colamt(k,1)/speccomb)

        js = 1 + int(specmult)
        fs = specmult - int(specmult)

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,5) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 9
        ind04 = ind01 + 10
        ind11 = min(MSA05, id1(k,5) + js )
        ind12 = min(MSA05, ind11 + 1 )
        ind13 = min(MSA05, ind11 + 9 )
        ind14 = min(MSA05, ind11 + 10)
        inds = indself(k)

        do j = 1, NG05
          taug(NS05+j,k) = speccomb                                     &
     &         * ( fac000*absa(ind01,j) + fac100*absa(ind02,j) +        &
     &             fac010*absa(ind03,j) + fac110*absa(ind04,j) +        &
     &             fac001*absa(ind11,j) + fac101*absa(ind12,j) +        &
     &             fac011*absa(ind13,j) + fac111*absa(ind14,j) )        &
     &         + colamt(k,1) * selffac(k)*( selfref(inds,j) +           &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) )    &
     &         + wx(k,1) * ccl4(j)

          pfrac(NS05+j,k) = fracrefa(j,js)                              &
     &          + fs * (fracrefa(j,js+1) - fracrefa(j,js))
        enddo
      enddo

      do k = laytrop+1, NLAY
        speccomb = colamt(k,3) + strrat2*colamt(k,2)
        specmult = 4.0 * min(oneminus, colamt(k,3)/speccomb)

        js = 1 + int(specmult)
        fs = specmult - int(specmult)

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,5) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 5
        ind04 = ind01 + 6
        ind11 = id1(k,5) + js
        ind12 = ind11 + 1
        ind13 = ind11 + 5
        ind14 = ind11 + 6

        do j = 1, NG05
          taug(NS05+j,k) = speccomb                                     &
     &         * ( fac000*absb(ind01,j) + fac100*absb(ind02,j) +        &
     &             fac010*absb(ind03,j) + fac110*absb(ind04,j) +        &
     &             fac001*absb(ind11,j) + fac101*absb(ind12,j) +        &
     &             fac011*absb(ind13,j) + fac111*absb(ind14,j) )        &
     &         + wx(k,1) * ccl4(j)

          pfrac(NS05+j,k) = fracrefb(j,js)                              &
     &          + fs * (fracrefb(j,js+1) - fracrefb(j,js))
        enddo
      enddo

      return
!...................................
      end subroutine taugb05
!-----------------------------------


!-----------------------------------
      subroutine taugb06
!...................................

!     band 6:  820-980 cm-1 (low - h2o; high - nothing)
!
      use module_radlw_kgb06
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind11, ind12, inds, js, ns

!     compute the optical depth by interpolating in ln(pressure) and
!     temperature. the water vapor self-continuum is interpolated
!     (in temperature) separately.  

      do k = 1, laytrop
        ind01 = id0(k,6) + 1
        ind02 = ind01 + 1
        ind11 = min(MSA06, id1(k,6) + 1 )
        ind12 = min(MSA06, ind11 + 1 )
        inds = indself(k)

        do j = 1, NG06
          taug(NS06+j,k) = colamt(k,1)                                  &
     &         * ( fac00(k)*absa(ind01,j) + fac10(k)*absa(ind02,j) +    &
     &             fac01(k)*absa(ind11,j) + fac11(k)*absa(ind12,j) +    &
     &             selffac(k) *( selfref(inds,j) +                      &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) ) )  &
     &         + wx(k,2) * cfc11adj(j) + wx(k,3) * cfc12(j)             &
     &         + co2mult(k) * absco2(j)

          pfrac(NS06+j,k) = fracrefa(j)
        enddo
      enddo

!     nothing important goes on above laytrop in this band.
      do k = laytrop+1, NLAY
        do j = 1, NG06
          taug(NS06+j,k) = wx(k,2) * cfc11adj(j) + wx(k,3) * cfc12(j)

          pfrac(NS06+j,k) = fracrefa(j)
        enddo
      enddo

      return
!...................................
      end subroutine taugb06
!-----------------------------------


!-----------------------------------
      subroutine taugb07
!...................................

!     band 7:  980-1080 cm-1 (low - h2o,o3; high - o3)
!
      use module_radlw_kgb07
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind03, ind04, ind11, ind12, ind13, &
     &           ind14, inds, js
!
      real (kind=kind_phys) :: fac000, fac010, fac100, fac110, fac001,  &
     &      fac011, fac101, fac111, strrat, speccomb, specmult, fs, fs1

      strrat = 8.21104e4

!     compute the optical depth by interpolating in ln(pressure), 
!     temperature, and appropriate species.  below laytrop, the water
!     vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, laytrop
        speccomb = colamt(k,1) + strrat*colamt(k,3)
        specmult = 8.0 * min(oneminus, colamt(k,1)/speccomb)

        js = 1 + int(specmult)
        fs = specmult - int(specmult)

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,7) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 9
        ind04 = ind01 + 10
        ind11 = min(MSA07, id1(k,7) + js )
        ind12 = min(MSA07, ind11 + 1 )
        ind13 = min(MSA07, ind11 + 9 )
        ind14 = min(MSA07, ind11 + 10)
        inds = indself(k)

        do j = 1, NG07
          taug(NS07+j,k) = speccomb                                     &
     &         * ( fac000*absa(ind01,j) + fac100*absa(ind02,j) +        &
     &             fac010*absa(ind03,j) + fac110*absa(ind04,j) +        &
     &             fac001*absa(ind11,j) + fac101*absa(ind12,j) +        &
     &             fac011*absa(ind13,j) + fac111*absa(ind14,j) )        &
     &         + colamt(k,1) * selffac(k)*( selfref(inds,j) +           &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) )    &
     &         + co2mult(k) * absco2(j)

          pfrac(NS07+j,k) = fracrefa(j,js)                              &
     &              + fs * (fracrefa(j,js+1) - fracrefa(j,js))
        enddo
      enddo

      do k = laytrop+1, NLAY

        ind01 = id0(k,7) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,7) + 1
        ind12 = ind11 + 1

        do j = 1, NG07
          taug(NS07+j,k) = colamt(k,3)                                  &
     &         * ( fac00(k)*absb(ind01,j) + fac10(k)*absb(ind02,j) +    &
     &             fac01(k)*absb(ind11,j) + fac11(k)*absb(ind12,j) )    &
     &         + co2mult(k) * absco2(j)

          pfrac(NS07+j,k) = fracrefb(j)
        enddo

!     empirical modification to code to improve stratospheric cooling rates
!     for o3. revised to apply weighting for g-point reduction in this band.
!     from mike iacono, aer, april 01, 2003.

        taug(NS07+6, k) = taug(NS07+6, k) * 0.92
        taug(NS07+7, k) = taug(NS07+7, k) * 0.88
        taug(NS07+8, k) = taug(NS07+8, k) * 1.07
        taug(NS07+9, k) = taug(NS07+9, k) * 1.10
        taug(NS07+10,k) = taug(NS07+10,k) * 0.99
        taug(NS07+11,k) = taug(NS07+11,k) * 0.855

      enddo

      return
!...................................
      end subroutine taugb07
!-----------------------------------


!-----------------------------------
      subroutine taugb08
!...................................

!     band 8:  1080-1180 cm-1 (low (i.e.>~300mb) - h2o; high - o3)
!
      use module_radlw_kgb08
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind11, ind12, inds, jp0, jp1
!
      real (kind=kind_phys) :: ratio, n2omult, tem1, tem2

!     compute the optical depth by interpolating in ln(pressure) and 
!     temperature.  

      do k = 1, layswtch
        ind01 = id0(k,8) + 1
        ind02 = ind01 + 1
        ind11 = min(MSA08, id1(k,8) + 1 )
        ind12 = min(MSA08, ind11 + 1 )
        inds = indself(k)

        jp0 = jp(k)
        jp1 = jp0 + 1
        tem1 = n2oref(jp0) / h2oref(jp0)
        tem2 = n2oref(jp1) / h2oref(jp1)
        ratio = tem1 + (fac01(k) + fac11(k)) * (tem2 - tem1)
        n2omult = colamt(k,4) - colamt(K,1)*ratio

        do j = 1, NG08
          taug(NS08+j,k) = colamt(k,1)                                  &
     &         * ( fac00(k)*absa(ind01,j) + fac10(k)*absa(ind02,j) +    &
     &             fac01(k)*absa(ind11,j) + fac11(k)*absa(ind12,j) +    &
     &             selffac(k)*( selfref(inds,j) +                       &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) ) )  &
     &         + wx(k,3) * cfc12(j) + wx(k,4) * cfc22adj(j)             &
     &         + co2mult(k) * absco2a(j) + n2omult * absn2oa(j)

          pfrac(NS08+j,k) = fracrefa(j)
        enddo
      enddo

      do k = layswtch+1, NLAY
        ind01 = id0(k,8) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,8) + 1
        ind12 = ind11 + 1

        jp0 = jp(k)
        jp1 = jp0 + 1
        tem1 = n2oref(jp0) / o3ref(jp0)
        tem2 = n2oref(jp1) / o3ref(jp1)
        ratio = tem1 + (fac01(k) + fac11(k)) * (tem2 - tem1)
        n2omult = colamt(k,4) - colamt(k,3) * ratio

        do j = 1, NG08
          taug(NS08+j,k) = colamt(k,3)                                  &
     &         * ( fac00(k)*absb(ind01,j) + fac10(k)*absb(ind02,j) +    &
     &             fac01(k)*absb(ind11,j) + fac11(k)*absb(ind12,j) )    &
     &         + wx(k,3) * cfc12(j) + wx(k,4) * cfc22adj(j)             &
     &         + co2mult(k) * absco2b(j) + n2omult * absn2ob(j)

          pfrac(NS08+j,k) = fracrefb(j)
        enddo
      enddo

      return
!...................................
      end subroutine taugb08
!-----------------------------------


!-----------------------------------
      subroutine taugb09
!...................................

!     band 9:  1180-1390 cm-1 (low - h2o,ch4; high - ch4)
!
      use module_radlw_kgb09
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind03, ind04, ind11, ind12, ind13, &
     &           ind14, inds, js, ns, jfrac, ioff, jp0, jp1
!
      real (kind=kind_phys) :: fac000, fac010, fac100, fac110, fac001,  &
     &      fac011, fac101, fac111, strrat, speccomb, specmult, fs, fs1,&
     &      ffrac, ratio, n2omult, tem0, tem1, tem2

      strrat = 21.6282
      ioff = 0

!     compute the optical depth by interpolating in ln(pressure), 
!     temperature, and appropriate species.  below laytrop, the water
!     vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, laytrop
        if (k == laylow) ioff = NG09
        if (k == layswtch) ioff = 2 * NG09

        speccomb = colamt(k,1) + strrat*colamt(k,5)
        specmult = 8.0 * min(oneminus, colamt(k,1)/speccomb)

        js = 1 + int(specmult)
        jfrac = js
        fs = specmult - int(specmult)
        ffrac = fs
        if (js == 8) then
          if (fs <= 0.68) then
            fs = fs / 0.68
          elseif (fs <= 0.92) then
            js = js + 1
            fs = (fs - 0.68) / 0.24
          else
            js = js + 2
            fs = (fs - 0.92) / 0.08
          endif
        elseif (js == 9) then
          js = 10
          fs = 1.0
          jfrac = 8
          ffrac = 1.0
        endif

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,9) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 11
        ind04 = ind01 + 12
        ind11 = min(MSA09, id1(k,9) + js )
        ind12 = min(MSA09, ind11 + 1 )
        ind13 = min(MSA09, ind11 + 11)
        ind14 = min(MSA09, ind11 + 12)
        inds = indself(k)

        jp0 = jp(k)
        jp1 = jp0 + 1
        ns = js + int(fs + 0.5)
        tem0 = (1.0 - etaref(ns)) / strrat
        if (ns == 11) then
          tem1 = n2oref(jp0) / h2oref(jp0)
          tem2 = n2oref(jp1) / h2oref(jp1)
        else
          tem1 = tem0 * n2oref(jp0) / ch4ref(jp0)
          tem2 = tem0 * n2oref(jp1) / ch4ref(jp1)
        endif
        ratio = tem1 + (fac01(k) + fac11(k)) * (tem2 - tem1)
        n2omult = colamt(k,4) - speccomb*ratio

        do j = 1, NG09
          taug(NS09+j,k) = speccomb                                     &
     &         * ( fac000*absa(ind01,j) + fac100*absa(ind02,j) +        &
     &             fac010*absa(ind03,j) + fac110*absa(ind04,j) +        &
     &             fac001*absa(ind11,j) + fac101*absa(ind12,j) +        &
     &             fac011*absa(ind13,j) + fac111*absa(ind14,j) )        &
     &         + colamt(k,1) * selffac(k)*( selfref(inds,j) +           &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) )    &
     &         + n2omult * absn2o(j+ioff)

          pfrac(NS09+j,k) = fracrefa(j,jfrac)                           &
     &          + ffrac * (fracrefa(j,jfrac+1)-fracrefa(j,jfrac))
        enddo
      enddo

      do k = laytrop+1, NLAY
        ind01 = id0(k,9) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,9) + 1
        ind12 = ind11 + 1

        do j = 1, NG09
          taug(NS09+j,k) = colamt(k,5)                                  &
     &         * ( fac00(k)*absb(ind01,j) + fac10(k)*absb(ind02,j) +    &
     &             fac01(k)*absb(ind11,j) + fac11(k)*absb(ind12,j) )

          pfrac(NS09+j,k) = fracrefb(j)
        enddo
      enddo

      return
!...................................
      end subroutine taugb09
!-----------------------------------


!-----------------------------------
      subroutine taugb10
!...................................

!     band 10:  1390-1480 cm-1 (low - h2o; high - h2o)
!
      use module_radlw_kgb10
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind11, ind12
!
!     compute the optical depth by interpolating in ln(pressure) and 
!     temperature.  

      do k = 1, laytrop
        ind01 = id0(k,10) + 1
        ind02 = ind01 + 1
        ind11 = min(MSA10, id1(k,10) + 1 )
        ind12 = min(MSA10, ind11 + 1 )

        do j = 1, NG10
          taug(NS10+j,k) = colamt(k,1)                                  &
     &         * ( fac00(k)*absa(ind01,j) + fac10(k)*absa(ind02,j) +    &
     &             fac01(k)*absa(ind11,j) + fac11(k)*absa(ind12,j) ) 

          pfrac(NS10+j,k) = fracrefa(j)
        enddo
      enddo

      do k = laytrop+1, NLAY
        ind01 = id0(k,10) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,10) + 1
        ind12 = ind11 + 1

        do j = 1, NG10
          taug(NS10+j,k) = colamt(k,1)                                  &
     &         * ( fac00(k)*absb(ind01,j) + fac10(k)*absb(ind02,j) +    &
     &             fac01(k)*absb(ind11,j) + fac11(k)*absb(ind12,j) ) 

          pfrac(NS10+j,k) = fracrefb(j)
        enddo
      enddo

      return
!...................................
      end subroutine taugb10
!-----------------------------------


!-----------------------------------
      subroutine taugb11
!...................................

!     band 11:  1480-1800 cm-1 (low - h2o; high - h2o)
!
      use module_radlw_kgb11
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind11, ind12, inds
!

!     compute the optical depth by interpolating in ln(pressure) and 
!     temperature.  below laytrop, the water vapor self-continuum 
!     is interpolated (in temperature) separately.  

      do k = 1, laytrop
        ind01 = id0(k,11) + 1
        ind02 = ind01 + 1
        ind11 = min(MSA11, id1(k,11) + 1 )
        ind12 = min(MSA11, ind11 + 1 )
        inds = indself(k)

        do j = 1, NG11
          taug(NS11+j,k) = colamt(k,1)                                  &
     &         * ( fac00(k)*absa(ind01,j) + fac10(k)*absa(ind02,j) +    &
     &             fac01(k)*absa(ind11,j) + fac11(k)*absa(ind12,j) +    &
     &             selffac(k)*( selfref(inds,j) +  selffrac(k) *        &
     &            (selfref(inds+1,j)-selfref(inds,j)) ) )

          pfrac(NS11+j,k) = fracrefa(j)
        enddo
      enddo

      do k = laytrop+1, NLAY
        ind01 = id0(k,11) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,11) + 1
        ind12 = ind11 + 1

        do j = 1, NG11
          taug(NS11+j,k) = colamt(k,1)                                  &
     &         * ( fac00(k)*absb(ind01,j) + fac10(k)*absb(ind02,j) +    &
     &             fac01(k)*absb(ind11,j) + fac11(k)*absb(ind12,j) ) 

          pfrac(NS11+j,k) = fracrefb(j)
        enddo
      enddo

      return
!...................................
      end subroutine taugb11
!-----------------------------------


!-----------------------------------
      subroutine taugb12
!...................................

!     band 12:  1800-2080 cm-1 (low - h2o,co2; high - nothing)
!
      use module_radlw_kgb12
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind03, ind04, ind11, ind12, ind13, &
     &           ind14, inds, js
!
      real (kind=kind_phys) :: fac000, fac010, fac100, fac110, fac001,  &
     &      fac011, fac101, fac111, strrat, speccomb, specmult, fs, fs1

      strrat = 0.009736757

!     compute the optical depth by interpolating in ln(pressure), 
!     temperature, and appropriate species.  below laytrop, the water
!     vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, laytrop
        speccomb = colamt(k,1) + strrat*colamt(k,2)
        specmult = 8.0 * min(oneminus, colamt(k,1)/speccomb)

        js = 1 + int(specmult)
        fs = specmult - int(specmult)

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,12) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 9
        ind04 = ind01 + 10
        ind11 = min(MSA12, id1(k,12) + js )
        ind12 = min(MSA12, ind11 + 1 )
        ind13 = min(MSA12, ind11 + 9 )
        ind14 = min(MSA12, ind11 + 10)
        inds = indself(k)

        do j = 1, NG12
          taug(NS12+j,k) = speccomb                                     &
     &         * ( fac000*absa(ind01,j) + fac100*absa(ind02,j) +        &
     &             fac010*absa(ind03,j) + fac110*absa(ind04,j) +        &
     &             fac001*absa(ind11,j) + fac101*absa(ind12,j) +        &
     &             fac011*absa(ind13,j) + fac111*absa(ind14,j) )        &
     &         + colamt(k,1)*selffac(k)*( selfref(inds,j) +             &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) )

          pfrac(NS12+j,k) = fracrefa(j,js)                              &
     &          + fs * (fracrefa(j,js+1) - fracrefa(j,js))
        enddo
      enddo

      do k = laytrop+1, NLAY
        do j = 1, NG12
          taug(NS12+j,k) = f_zero
          pfrac(NS12+j,k) = f_zero
        enddo
      enddo

      return
!...................................
      end subroutine taugb12
!-----------------------------------


!-----------------------------------
      subroutine taugb13
!...................................

!     band 13:  2080-2250 cm-1 (low - h2o,n2o; high - nothing)
!
      use module_radlw_kgb13
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind03, ind04, ind11, ind12, ind13, &
     &           ind14, inds, js
!
      real (kind=kind_phys) :: fac000, fac010, fac100, fac110, fac001,  &
     &      fac011, fac101, fac111, strrat, speccomb, specmult, fs, fs1

      strrat = 16658.87

!     compute the optical depth by interpolating in ln(pressure), 
!     temperature, and appropriate species.  below laytrop, the water
!     vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, laytrop
        speccomb = colamt(k,1) + strrat*colamt(k,4)
        specmult = 8.0 * min(oneminus, colamt(k,1)/speccomb)

        js = 1 + int(specmult)
        fs = specmult - int(specmult)

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,13) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 9
        ind04 = ind01 + 10
        ind11 = min(MSA13, id1(k,13) + js )
        ind12 = min(MSA13, ind11 + 1 )
        ind13 = min(MSA13, ind11 + 9 )
        ind14 = min(MSA13, ind11 + 10)
        inds = indself(k)

        do j = 1, NG13
          taug(NS13+j,k) = speccomb                                     &
     &         * ( fac000*absa(ind01,j) + fac100*absa(ind02,j) +        &
     &             fac010*absa(ind03,j) + fac110*absa(ind04,j) +        &
     &             fac001*absa(ind11,j) + fac101*absa(ind12,j) +        &
     &             fac011*absa(ind13,j) + fac111*absa(ind14,j) )        &
     &         + colamt(k,1)*selffac(k)*( selfref(inds,j) +             &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) )

          pfrac(NS13+j,k) = fracrefa(j,js)                              &
     &          + fs * (fracrefa(j,js+1) - fracrefa(j,js))
        enddo
      enddo

      do k = laytrop+1, NLAY
        do j = 1, NG13
          taug(NS13+j,k) = f_zero
          pfrac(NS13+j,k) = f_zero
        enddo
      enddo

      return
!...................................
      end subroutine taugb13
!-----------------------------------


!-----------------------------------
      subroutine taugb14
!...................................

!     band 14:  2250-2380 cm-1 (low - co2; high - co2)
!
      use module_radlw_kgb14
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind11, ind12, inds
!

!     compute the optical depth by interpolating in ln(pressure) and 
!     temperature.  below laytrop, the water vapor self-continuum 
!     is interpolated (in temperature) separately.  

      do k = 1, laytrop
        ind01 = id0(k,14) + 1
        ind02 = ind01 + 1
        ind11 = min(MSA14, id1(k,14) + 1 )
        ind12 = min(MSA14, ind11 + 1 )
        inds = indself(k)

        do j = 1, NG14
          taug(NS14+j,k) = colamt(k,2)                                  &
     &         * ( fac00(k)*absa(ind01,j) + fac10(k)*absa(ind02,j) +    &
     &             fac01(k)*absa(ind11,j) + fac11(k)*absa(ind12,j) )    &
     &         + colamt(k,1)*selffac(k)*( selfref(inds,j) +             &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) )

          pfrac(NS14+j,k) = fracrefa(j)
        enddo
      enddo

      do k = laytrop+1, NLAY
        ind01 = id0(k,14) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,14) + 1
        ind12 = ind11 + 1

        do j = 1, NG14
          taug(NS14+j,k) = colamt(k,2)                                  &
     &         * ( fac00(k)*absb(ind01,j) + fac10(k)*absb(ind02,j) +    &
     &             fac01(k)*absb(ind11,j) + fac11(k)*absb(ind12,j) ) 

          pfrac(NS14+j,k) = fracrefb(j)
        enddo
      enddo

      return
!...................................
      end subroutine taugb14
!-----------------------------------


!-----------------------------------
      subroutine taugb15
!...................................

!     band 15:  2380-2600 cm-1 (low - n2o,co2; high - nothing)
!
      use module_radlw_kgb15
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind03, ind04, ind11, ind12, ind13, &
     &           ind14, inds, js
!
      real (kind=kind_phys) :: fac000, fac010, fac100, fac110, fac001,  &
     &      fac011, fac101, fac111, strrat, speccomb, specmult, fs, fs1

      strrat = 0.2883201

!     compute the optical depth by interpolating in ln(pressure), 
!     temperature, and appropriate species.  below laytrop, the water
!     vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, laytrop
        speccomb = colamt(k,4) + strrat*colamt(k,2)
        specmult = 8.0 * min(oneminus, colamt(k,4)/speccomb)

        js = 1 + int(specmult)
        fs = specmult - int(specmult)

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,15) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 9
        ind04 = ind01 + 10
        ind11 = min(MSA15, id1(k,15) + js )
        ind12 = min(MSA15, ind11 + 1 )
        ind13 = min(MSA15, ind11 + 9 )
        ind14 = min(MSA15, ind11 + 10)
        inds = indself(k)

        do j = 1, NG15
          taug(NS15+j,k) = speccomb                                     &
     &         * ( fac000*absa(ind01,j) + fac100*absa(ind02,j) +        &
     &             fac010*absa(ind03,j) + fac110*absa(ind04,j) +        &
     &             fac001*absa(ind11,j) + fac101*absa(ind12,j) +        &
     &             fac011*absa(ind13,j) + fac111*absa(ind14,j) )        &
     &         + colamt(k,1)*selffac(k)*( selfref(inds,j) +             &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) )

          pfrac(NS15+j,k) = fracrefa(j,js)                              &
     &          + fs * (fracrefa(j,js+1) - fracrefa(j,js))
        enddo
      enddo

      do k = laytrop+1, NLAY
        do j = 1, NG15
          taug(NS15+j,k) = f_zero
          pfrac(NS15+j,k) = f_zero
        enddo
      enddo

      return
!...................................
      end subroutine taugb15
!-----------------------------------


!-----------------------------------
      subroutine taugb16
!...................................

!     band 16:  2600-3000 cm-1 (low - h2o,ch4; high - nothing)
!
      use module_radlw_kgb16
!
      implicit none
!
      integer :: j, k, ind01, ind02, ind03, ind04, ind11, ind12, ind13, &
     &           ind14, inds, js
!
      real (kind=kind_phys) :: fac000, fac010, fac100, fac110, fac001,  &
     &      fac011, fac101, fac111, strrat, speccomb, specmult, fs, fs1

      strrat = 830.411

!     compute the optical depth by interpolating in ln(pressure), 
!     temperature, and appropriate species.  below laytrop, the water
!     vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, laytrop
        speccomb = colamt(k,1) + strrat*colamt(k,5)
        specmult = 8.0 * min(oneminus, colamt(k,1)/speccomb)

        js = 1 + int(specmult)
        fs = specmult - int(specmult)

        fs1 = 1.0 - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,16) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 9
        ind04 = ind01 + 10
        ind11 = min(MSA16, id1(k,16) + js )
        ind12 = min(MSA16, ind11 + 1 )
        ind13 = min(MSA16, ind11 + 9 )
        ind14 = min(MSA16, ind11 + 10)
        inds = indself(k)

        do j = 1, NG16
          taug(NS16+j,k) = speccomb                                     &
     &         * ( fac000*absa(ind01,j) + fac100*absa(ind02,j) +        &
     &             fac010*absa(ind03,j) + fac110*absa(ind04,j) +        &
     &             fac001*absa(ind11,j) + fac101*absa(ind12,j) +        &
     &             fac011*absa(ind13,j) + fac111*absa(ind14,j) )        &
     &         + colamt(k,1)*selffac(k)*( selfref(inds,j) +             &
     &             selffrac(k)*(selfref(inds+1,j)-selfref(inds,j)) )

          pfrac(NS16+j,k) = fracrefa(j,js)                              &
     &          + fs * (fracrefa(j,js+1) - fracrefa(j,js))
        enddo
      enddo

      do k = laytrop+1, NLAY
        do j = 1, NG16
          taug(NS16+j,k) = f_zero
          pfrac(NS16+j,k) = f_zero
        enddo
      enddo

      return
!...................................
      end subroutine taugb16
!-----------------------------------


!...................................
      end subroutine taumol
!-----------------------------------


!
!........................................!
      end module module_radlw_main       !
!========================================!

