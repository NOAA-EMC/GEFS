!!!!!  ==========================================================  !!!!!
!!!!!            sw-rrtm2 radiation package description            !!!!!
!!!!!  ==========================================================  !!!!!
!                                                                      !
!   the sw-rrtm2 package includes these parts:                         !
!                                                                      !
!      'radsw_rrtm2_param.f'                                           !
!      'radsw_rrtm2_datatb.f'                                          !
!      'radsw_rrtm2_main.f'                                            !
!                                                                      !
!   the 'radsw_rrtm2_param.f' contains:                                !
!                                                                      !
!      'module_radsw_parameters'  -- band parameters set up            !
!      'module_radsw_cntr_para'   -- control parameters set up         !
!                                                                      !
!   the 'radsw_rrtm2_datatb.f' contains:                               !
!                                                                      !
!      'module_radsw_cldprtb'     -- cloud property coefficients table !
!      'module_radsw_kgbnn'       -- absorption coeffients for 14      !
!                                    bands, where nn = 16-29           !
!                                                                      !
!   the 'radsw_rrtm2_main.f' contains:                                 !
!                                                                      !
!      'module_radsw_main'        -- main sw radiation transfer        !
!                                                                      !
!   in the main module 'module_radsw_main' there are only two          !
!   externally callable subroutines:                                   !
!                                                                      !
!      'swrad'      -- main rrtm2 sw radiation routine                 !
!         inputs:                                                      !
!           (plyr,plvl,tlyr,tlvl,qlyr,olyr,gasvmr,                     !
!            clouds,iovr,aerosols,sfcalb,                              !
!            cosz,solcon,NDAY,idxday,                                  !
!            IMAX, NLAY, NLP1, iflip, lprnt,                           !
!         outputs:                                                     !
!            hswc,topflx,sfcflx,                                       !
!!        optional outputs:                                            !
!            HSW0,HSWB,FLXPRF,FDNCMP                                   !
!           )                                                          !
!                                                                      !
!      'rswinit'    -- initialization routine                          !
!         inputs:                                                      !
!           ( icwp, me, NLAY )                                         !
!         outputs:                                                     !
!           (none)                                                     !
!                                                                      !
!   all the sw radiation subprograms become contained subprograms      !
!   in module 'module_radsw_main' and many of them are not directly    !
!   accessable from places outside the module.                         !
!                                                                      !
!    derived data type constructs used:                                !
!                                                                      !
!     1. radiation flux at toa: (from module 'module_radsw_parameters')!
!          topfsw_type   -  derived data type for toa rad fluxes       !
!            upfxc              total sky upward flux at toa           !
!            dnfxc              total sky downward flux at toa         !
!            upfx0              clear sky upward flux at toa           !
!                                                                      !
!     2. radiation flux at sfc: (from module 'module_radsw_parameters')!
!          sfcfsw_type   -  derived data type for sfc rad fluxes       !
!            upfxc              total sky upward flux at sfc           !
!            dnfxc              total sky downward flux at sfc         !
!            upfx0              clear sky upward flux at sfc           !
!            dnfx0              clear sky downward flux at sfc         !
!                                                                      !
!     3. radiation flux profiles(from module 'module_radsw_parameters')!
!          profsw_type    -  derived data type for rad vertical prof   !
!            upfxc              total sky level upward flux            !
!            dnfxc              total sky level downward flux          !
!            upfx0              clear sky level upward flux            !
!            dnfx0              clear sky level downward flux          !
!                                                                      !
!     4. surface component fluxes(from module 'module_radsw_parameters'!
!          cmpfsw_type    -  derived data type for component sfc flux  !
!            uvbfc              total sky downward uv-b flux at sfc    !
!            uvbf0              clear sky downward uv-b flux at sfc    !
!            nirbm              surface downward nir direct beam flux  !
!            nirdf              surface downward nir diffused flux     !
!            visbm              surface downward uv+vis direct beam flx!
!            visdf              surface downward uv+vis diffused flux  !
!                                                                      !
!                                                                      !
!   external modules referenced:                                       !
!                                                                      !
!       'module machine'                                               !
!       'module physcons'                                              !
!                                                                      !
!   compilation sequence is:                                           !
!                                                                      !
!      'radsw_rrtm2_param.f'                                           !
!      'radsw_rrtm2_datatb.f'                                          !
!      'radsw_rrtm2_main.f'                                            !
!                                                                      !
!   and all should be put in front of routines that use sw modules     !
!                                                                      !
!                                                                      !
!                                                                      !
!                                                                      !
!   the original program declarations:                                 !
!                                                                      !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                      !
! Copyright 2002, 2003, 2004, Atmospheric & Environmental Research, Inc!
! (AER). This software may be used, copied, or redistributed as long as!
! it is not sold and this copyright notice is reproduced on each copy  !
! made. This model is provided as is without any express or implied    !
! warranties.                                                          !
!                      (http://www.rtweb.aer.com/)                     !
!                                                                      !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                      !
!                               rrtm_sw                                !
!                                                                      !
!                   a rapid radiative transfer model                   !
!                    for the solar spectral region                     !
!                                                                      !
!            atmospheric and environmental research, inc.              !
!                        840 memorial drive                            !
!                        cambridge, ma 02139                           !
!                                                                      !
!                           eli j. mlawer                              !
!                         jennifer delamere                            !
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
!    references:                                                       !
!      mlawer, e.j., s.j.taubman, p.d. brown, m.j. iacono, and         !
!      s.a. clough (1997): radiative transfer for inhomogeneous        !
!      atmospheres: rrtm, a validated correlated-k model for the       !
!      longwave.                                                       !
!                                                                      !
!                                                                      !
!                                                                      !
!   ncep modifications history log:                                    !
!                                                                      !
!       sep 2003,  yu-tai hou                                          !
!                  received aer's rrtm-sw gcm version code (v224)      !
!       jan 2004,  yu-tai hou                                          !
!                  modified code into standard modular f90 code for    !
!                  ncep models.                                        !
!       jun 2004,  yu-tai hou                                          !
!                  modified code based on aer's faster version         !
!                  rrtmg_sw (v2.0) with 112 g-points.                  !
!       mar 2005,  yu-tai hou                                          !
!                  correct cloud double scaling according to aer.      !
!                  total sky prop. are delta scaled after combining    !
!                  clear and cloudy parts. test criterion of s.s.a.    !
!                  is saved before scaling.                            !
!       apr 2005,  yu-tai hou                                          !
!                  modified on module structures                       !
!       apr 2007,  yu-tai hou                                          !
!                  add spectral band heating as optional output        !
!                                                                      !
!                                                                      !
!                                                                      !
!!!!!  ==========================================================  !!!!!
!!!!!                       end descriptions                       !!!!!
!!!!!  ==========================================================  !!!!!



!========================================!
      module module_radsw_main           !
!........................................!
!
      use machine,          only : kind_phys
      use physcons,         only : con_g, con_cp, con_avgd, con_amd,    &
     &                             con_amw, con_amo3

      use module_radsw_parameters
      use module_radsw_cntr_para
!
      implicit none
!
      private
!
!  ...  version tag and last revision date
!
!     character(24), parameter :: VTAGSW='RRTM-SW 112v2.0 jul 2004'
!     character(24), parameter :: VTAGSW='RRTM-SW 112v2.3 mar 2005'
      character(24), parameter :: VTAGSW='RRTM-SW 112v2.3 Apr 2007'

!  ---  constant values
      real (kind=kind_phys), parameter :: eps     = 1.0e-6
      real (kind=kind_phys), parameter :: oneminus= 1.0 - eps
      real (kind=kind_phys), parameter :: ftiny   = 1.0e-12
      real (kind=kind_phys), parameter :: stpfac  = 296.0/1013.0
      real (kind=kind_phys), parameter :: s0      = 1368.22  ! solar const hard coded in freq bands
      real (kind=kind_phys), parameter :: zero    = 0.0
      real (kind=kind_phys), parameter :: one     = 1.0

!  ...  atomic weights for conversion from mass to volume mixing ratios
      real (kind=kind_phys), parameter :: amdw    = con_amd/con_amw
      real (kind=kind_phys), parameter :: amdo3   = con_amd/con_amo3

!  ...  band indices
      integer, dimension(NBLOW:NBHGH) :: NSPA, NSPB, IDXALB, IDXSFC

      data NSPA(:) /  9, 9, 9, 9, 1, 9, 9, 1, 9, 1, 0, 1, 9, 1 /
      data NSPB(:) /  1, 5, 1, 1, 1, 5, 1, 0, 1, 0, 0, 1, 5, 1 /

      data IDXALB(:) /  1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1 /
      data IDXSFC(:) /  1, 1, 1, 1, 1, 1, 1, 1, 0, 2, 2, 2, 2, 1 /

!  ...  band wavenumber intervals
!     real (kind=kind_phys), dimension(NBLOW:NBHGH):: wavenum1,wavenum2
!     data wavenum1(:)  /                                               &
!    &         2600.0, 3250.0, 4000.0, 4650.0, 5150.0, 6150.0, 7700.0,  &
!    &         8050.0,12850.0,16000.0,22650.0,29000.0,38000.0,  820.0 /
!     data wavenum2(:)  /                                               &
!              3250.0, 4000.0, 4650.0, 5150.0, 6150.0, 7700.0, 8050.0,  &
!    &        12850.0,16000.0,22650.0,29000.0,38000.0,50000.0, 2600.0 /
!     real (kind=kind_phys), dimension(NBLOW:NBHGH) :: delwave
!     data delwave(:)   /                                               &
!    &          650.0,  750.0,  650.0,  500.0, 1000.0, 1550.0,  350.0,  &
!    &         4800.0, 3150.0, 6650.0, 6350.0, 9000.0,12000.0, 1780.0 /

      integer, parameter :: nuvb = 27            !uv-b band index

!  ---  reference pressure and temperature
!     real (kind=kind_phys), dimension(59) :: pref, preflog, tref
      real (kind=kind_phys), dimension(59) :: preflog, tref

!  ...  these pressures are chosen such that the ln of the first pressure
!       has only a few non-zero digits (i.e. ln(pref(1)) = 6.96000) and
!       each subsequent ln(pressure) differs from the previous one by 0.2.

!     data  pref(:)  /                                                  &
!    &     1.05363e+03,8.62642e+02,7.06272e+02,5.78246e+02,4.73428e+02, &
!    &     3.87610e+02,3.17348e+02,2.59823e+02,2.12725e+02,1.74164e+02, &
!    &     1.42594e+02,1.16746e+02,9.55835e+01,7.82571e+01,6.40715e+01, &
!    &     5.24573e+01,4.29484e+01,3.51632e+01,2.87892e+01,2.35706e+01, &
!    &     1.92980e+01,1.57998e+01,1.29358e+01,1.05910e+01,8.67114e+00, &
!    &     7.09933e+00,5.81244e+00,4.75882e+00,3.89619e+00,3.18993e+00, &
!    &     2.61170e+00,2.13828e+00,1.75067e+00,1.43333e+00,1.17351e+00, &
!    &     9.60789e-01,7.86628e-01,6.44036e-01,5.27292e-01,4.31710e-01, &
!    &     3.53455e-01,2.89384e-01,2.36928e-01,1.93980e-01,1.58817e-01, &
!    &     1.30029e-01,1.06458e-01,8.71608e-02,7.13612e-02,5.84256e-02, &
!    &     4.78349e-02,3.91639e-02,3.20647e-02,2.62523e-02,2.14936e-02, &
!    &     1.75975e-02,1.44076e-02,1.17959e-02,9.65769e-03 /

      data  preflog(:)  /                                               &
     &      6.9600e+00, 6.7600e+00, 6.5600e+00, 6.3600e+00, 6.1600e+00, &
     &      5.9600e+00, 5.7600e+00, 5.5600e+00, 5.3600e+00, 5.1600e+00, &
     &      4.9600e+00, 4.7600e+00, 4.5600e+00, 4.3600e+00, 4.1600e+00, &
     &      3.9600e+00, 3.7600e+00, 3.5600e+00, 3.3600e+00, 3.1600e+00, &
     &      2.9600e+00, 2.7600e+00, 2.5600e+00, 2.3600e+00, 2.1600e+00, &
     &      1.9600e+00, 1.7600e+00, 1.5600e+00, 1.3600e+00, 1.1600e+00, &
     &      9.6000e-01, 7.6000e-01, 5.6000e-01, 3.6000e-01, 1.6000e-01, &
     &     -4.0000e-02,-2.4000e-01,-4.4000e-01,-6.4000e-01,-8.4000e-01, &
     &     -1.0400e+00,-1.2400e+00,-1.4400e+00,-1.6400e+00,-1.8400e+00, &
     &     -2.0400e+00,-2.2400e+00,-2.4400e+00,-2.6400e+00,-2.8400e+00, &
     &     -3.0400e+00,-3.2400e+00,-3.4400e+00,-3.6400e+00,-3.8400e+00, &
     &     -4.0400e+00,-4.2400e+00,-4.4400e+00,-4.6400e+00 /

!  ...  these are the temperatures associated with the respective
!       pressures for the MLS standard atmosphere.
      data  tref(:)  /                                                  &
     &      2.9420e+02, 2.8799e+02, 2.7894e+02, 2.6925e+02, 2.5983e+02, &
     &      2.5017e+02, 2.4077e+02, 2.3179e+02, 2.2306e+02, 2.1578e+02, &
     &      2.1570e+02, 2.1570e+02, 2.1570e+02, 2.1706e+02, 2.1858e+02, &
     &      2.2018e+02, 2.2174e+02, 2.2328e+02, 2.2479e+02, 2.2655e+02, &
     &      2.2834e+02, 2.3113e+02, 2.3401e+02, 2.3703e+02, 2.4022e+02, &
     &      2.4371e+02, 2.4726e+02, 2.5085e+02, 2.5457e+02, 2.5832e+02, &
     &      2.6216e+02, 2.6606e+02, 2.6999e+02, 2.7340e+02, 2.7536e+02, &
     &      2.7568e+02, 2.7372e+02, 2.7163e+02, 2.6955e+02, 2.6593e+02, &
     &      2.6211e+02, 2.5828e+02, 2.5360e+02, 2.4854e+02, 2.4348e+02, &
     &      2.3809e+02, 2.3206e+02, 2.2603e+02, 2.2000e+02, 2.1435e+02, &
     &      2.0887e+02, 2.0340e+02, 1.9792e+02, 1.9290e+02, 1.8809e+02, &
     &      1.8329e+02, 1.7849e+02, 1.7394e+02, 1.7212e+02 /

!! ...  logical flags for optional output fields

      logical :: lhswb  = .false.
      logical :: lhsw0  = .false.
      logical :: lflxprf= .false.
      logical :: lfdncmp= .false.

!  ---  those data will be set up only once by "rswinit"

!  ...  heatfac is the factor for heating rates
!       (in k/day, or k/sec set by subroutine 'rlwinit')

      real (kind=kind_phys) :: heatfac

      public swrad, rswinit


! =================
      contains
! =================

!-----------------------------------
      subroutine swrad                                                  &
!...................................

!  ---  inputs:
     &     ( plyr,plvl,tlyr,tlvl,qlyr,olyr,gasvmr,                      &
     &       clouds,iovr,aerosols,sfcalb,                               &
     &       cosz,solcon,NDAY,idxday,                                   &
     &       IMAX, NLAY, NLP1, iflip, lprnt,                            &
!  ---  outputs:
     &       hswc,topflx,sfcflx                                         &
!! ---  optional:
     &,      HSW0,HSWB,FLXPRF,FDNCMP                                    &
     &     )

!  ====================  defination of variables  ====================  !
!                                                                       !
!  input variables:                                                     !
!   plyr (IMAX,NLAY) : model layer mean pressure in mb                  !
!   plvl (IMAX,NLP1) : model level pressure in mb                       !
!   tlyr (IMAX,NLAY) : model layer mean temperature in k                !
!   tlvl (IMAX,NLP1) : model level temperature in k    (not in use)     !
!   qlyr (IMAX,NLAY) : layer specific humidity in gm/gm   *see inside   !
!   olyr (IMAX,NLAY) : layer ozone concentration in gm/gm               !
!   gasvmr(IMAX,NLAY,:): atmospheric constent gases:                    !
!                      (check module_radiation_gases for definition)    !
!      gasvmr(:,:,1)  - co2 volume mixing ratio                         !
!      gasvmr(:,:,2)  - n2o volume mixing ratio                         !
!      gasvmr(:,:,3)  - ch4 volume mixing ratio                         !
!      gasvmr(:,:,4)  - o2  volume mixing ratio                         !
!      gasvmr(:,:,5)  - co  volume mixing ratio        (not used)       !
!      gasvmr(:,:,6)  - cfc11 volume mixing ratio      (not used)       !
!      gasvmr(:,:,7)  - cfc12 volume mixing ratio      (not used)       !
!      gasvmr(:,:,8)  - cfc22 volume mixing ratio      (not used)       !
!      gasvmr(:,:,9)  - ccl4  volume mixing ratio      (not used)       !
!   clouds(IMAX,NLAY,:): cloud profile                                  !
!                      (check module_radiation_clouds for definition)   !
!                ---  for  iflagliq > 0  ---                            !
!       clouds(:,:,1)  -   layer total cloud fraction                   !
!       clouds(:,:,2)  -   layer cloud liq water path      (g/m**2)     !
!       clouds(:,:,3)  -   mean eff radius for liq cloud   (micron)     !
!       clouds(:,:,4)  -   layer cloud ice water path      (g/m**2)     !
!       clouds(:,:,5)  -   mean eff radius for ice cloud   (micron)     !
!       clouds(:,:,6)  -   layer rain drop water path      (g/m**2)     !
!       clouds(:,:,7)  -   mean eff radius for rain drop   (micron)     !
!       clouds(:,:,8)  -   layer snow flake water path     (g/m**2)     !
!   ** fu's scheme need to be normalized by snow density (g/m**3/1.0e6) !
!       clouds(:,:,9)  -   mean eff radius for snow flake  (micron)     !
!                ---  for  iflagliq = 0  ---                            !
!       clouds(:,:,1)  -   layer total cloud fraction                   !
!       clouds(:,:,2)  -   layer cloud optical depth                    !
!       clouds(:,:,3)  -   layer cloud single scattering albedo         !
!       clouds(:,:,4)  -   layer cloud asymmetry factor                 !
!   iovr             : control flag for cloud overlapping (approxi only)!
!                     =0: random overlapping clouds                     !
!                     =1: max/ran overlapping clouds                    !
!   aerosols(IMAX,NLAY,NBDSW,:) : aerosol optical properties            !
!                      (check module_radiation_aerosols for definition) !
!         (:,:,:,1)   - optical depth                                   !
!         (:,:,:,2)   - single scattering albedo                        !
!         (:,:,:,3)   - asymmetry parameter                             !
!   sfcalb(IMAX, : ) : surface albedo in fraction                       !
!                      (check module_radiation_surface for definition)  !
!         ( :, 1 )    - near ir direct beam albedo                      !
!         ( :, 2 )    - near ir diffused albedo                         !
!         ( :, 3 )    - uv+vis direct beam albedo                       !
!         ( :, 4 )    - uv+vis diffused albedo                          !
!   cosz  (IMAX)     : cosine of solar zenith angle                     !
!   solcon           : solar constant                      (w/m**2)     !
!   NDAY             : num of daytime points                            !
!   idxday(IMAX)     : index array for daytime points                   !
!   IMAX             : number of horizontal points                      !
!   NLAY,NLP1        : vertical layer/lavel numbers                     !
!   iflip            : control flag for direction of vertical index     !
!                     =0: index from toa to surface                     !
!                     =1: index from surface to toa                     !
!   lprnt            : logical check print flag                         !
!                                                                       !
!  control parameters in module "module_radsw_cntr_para":               !
!   iswrate: heating rate unit selections                               !
!            =1: output in k/day                                        !
!            =2: output in k/second                                     !
!   iaersw : flags for aerosols effect                                  !
!            =0: without aerosol effect                                 !
!            >0: include aerosol effect                                 !
!   imodsw : control flag for 2-stream transfer scheme                  !
!            =1; delta-eddington    (joseph et al., 1976)               !
!            =2: pifm               (zdunkowski et al., 1980)           !
!            =3: discrete ordinates (liou, 1973)                        !
!   irgassw: control flag for rare gases (ch4,n2o,o2, etc.)             !
!            =0: do not include rare gases                              !
!            =1: include all rare gases                                 !
!   iflagliq:control flag for liq-cloud optical properties              !
!            =0: input cloud optical depth, fixed ssa, asy              !
!            =1: use hu and stamnes(1993) method for liq cld            !
!            =2: not used                                               !
!   iflagice:control flag for ice-cloud optical properties              !
!            =0: not used                                               !
!            =1: not used                                               !
!            =2: not used                                               !
!            =3: use fu's method (1996) for ice clouds                  !
!                                                                       !
!  output variables:                                                    !
!   hswc  (IMAX,NLAY): total sky heating rates (k/sec or k/day)         !
!   topflx(IMAX)     : radiation fluxes at toa (w/m**2), components:    !
!                      (check module_radsw_parameters for definition)   !
!     upfxc            - total sky upward flux at toa                   !
!     dnflx            - total sky downward flux at toa                 !
!     upfx0            - clear sky upward flux at toa                   !
!   sfcflx(IMAX)     : radiation fluxes at sfc (w/m**2), components:    !
!                      (check module_radsw_parameters for definition)   !
!     upfxc            - total sky upward flux at sfc                   !
!     dnfxc            - total sky downward flux at sfc                 !
!     upfx0            - clear sky upward flux at sfc                   !
!     dnfx0            - clear sky downward flux at sfc                 !
!                                                                       !
!!optional outputs:                                                     !
!   hswb(IMAX,NLAY,NBDSW): spectral band total sky heating rates        !
!   hsw0  (IMAX,NLAY): clear sky heating rates (k/sec or k/day)         !
!   flxprf(IMAX,NLP1): level radiation fluxes (w/m**2), components:     !
!                      (check module_radsw_parameters for definition)   !
!     dnfxc            - total sky downward flux at interface           !
!     upfxc            - total sky upward flux at interface             !
!     dnfx0            - clear sky downward flux at interface           !
!     upfx0            - clear sky upward flux at interface             !
!   fdncmp(IMAX)     : component surface downward fluxes (w/m**2):      !
!                      (check module_radsw_parameters for definition)   !
!     uvbfc            - total sky downward uv-b flux at sfc            !
!     uvbf0            - clear sky downward uv-b flux at sfc            !
!     nirbm            - downward surface nir direct beam flux          !
!     nirdf            - downward surface nir diffused flux             !
!     visbm            - downward surface uv+vis direct beam flux       !
!     visdf            - downward surface uv+vis diffused flux          !
!                                                                       !
!  module parameters, control and local variables:                      !
!     NBLOW,NBHGH           - lower and upper limits of spectral bands  !
!     MAXGAS                - maximum number of absorbing gaseous       !
!     NGPT                  - total number of g-point subintervals      !
!     NGnn   (nn=16-29)     - number of g-points in band nn             !
!     NSPA,NSPB(NBLOW:NBHGH)- number of lower/upper ref atm's per band  !
!     pavel  (NLAY)         - layer pressures (mb)                      !
!     delp   (NLAY)         - layer pressure thickness (mb)             !
!     tavel  (NLAY)         - layer temperatures (k)                    !
!     coldry (NLAY)         - dry air column amount                     !
!                                   (1.e-20*molecules/cm**2)            !
!     colamt (NLAY,MAXGAS)  - column amounts of absorbing gases         !
!                             1-MAXGAS are for watervapor, carbon       !
!                             dioxide, ozone, nitrous oxide, methane,   !
!                             oxigen, respectively (molecules/cm**2)    !
!     facij  (NLAY)         - indicator of interpolation factors        !
!                             =0/1: indicate lower/higher temp & height !
!     selffac(NLAY)         - scale factor for self-continuum, equals   !
!                          (w.v. density)/(atm density at 296K,1013 mb) !
!     selffrac(NLAY)        - factor for temp interpolation of ref      !
!                             self-continuum data                       !
!     indself(NLAY)         - index of the lower two appropriate ref    !
!                             temp for the self-continuum interpolation !
!     laytrop                                                           !
!                           - layer at which switch is made from one    !
!                             combination of key species to another     !
!     pfup  (IMAX,NLP1)     - total sky upward flux (w/m2)              !
!     pcup  (IMAX,NLP1)     - clear sky upward flux (w/m2)              !
!     pfdown(IMAX,NLP1)     - total sky downward flux (w/m2)            !
!     pcdown(IMAX,NLP1)     - clear sky downward flux (w/m2)            !
!     pheat (IMAX,NLP1)     - total sky heating rate (k/day or k/sec)   !
!     pheac (IMAX,NLP1)     - clear sky heating rate (k/day or k/sec)   !
!                                                                       !
!                                                                       !
!  program history:                                                     !
!    2003-02-25   j.-j. morcrette, ecmwf, interface to rrtm_sw;         !
!                  conversion to f90 formatting; addition of 2-stream   !
!                  radiative transfer.                                  !
!    2003-08-xx   m. j. iacono, aer inc., additional modifications      !
!                  for gcm application.                                 !
!    2004-01-20   y.-t. hou, ncep,        modified for ncep gfs models, !
!                  recode into standard modular format.                 !
!    2004-06-28   y.-t. hou, ncep,        modified to use aer's rrtmg-sw!
!                  v2.0 reduced g-point code (112 points).              !
!    2005-03-10   y.-t. hou, ncep,        modified to use aer's rrtmg-sw!
!                  v2.3 to correct double scaling in early version      !
!                                                                       !
!  =====================    end of definitions    ====================  !
!
      implicit none

!  ---  inputs:
      integer, intent(in) :: IMAX, NLAY, NLP1, iovr, iflip, NDAY

      integer, intent(in) :: idxday(:)

      logical, intent(in) :: lprnt

      real (kind=kind_phys), dimension(:,:), intent(in) :: plvl, tlvl,  &
     &       plyr, tlyr, qlyr, olyr, sfcalb

      real (kind=kind_phys), dimension(:,:,:),   intent(in) :: gasvmr,  &
     &       clouds
      real (kind=kind_phys), dimension(:,:,:,:), intent(in) :: aerosols

      real (kind=kind_phys), intent(in) :: cosz(:), solcon

!  ---  outputs:
      real (kind=kind_phys), dimension(:,:), intent(out) :: hswc

      type (topfsw_type),    dimension(:),   intent(out) :: topflx
      type (sfcfsw_type),    dimension(:),   intent(out) :: sfcflx

!! ---  optional outputs:
      real (kind=kind_phys),dimension(:,:,:),optional,intent(out):: hswb
      real (kind=kind_phys),dimension(:,:),  optional,intent(out):: hsw0
      type (profsw_type), dimension(:,:),optional, intent(out) :: flxprf
      type (cmpfsw_type), dimension(:),  optional, intent(out) :: fdncmp

!  ---  locals:
      real (kind=kind_phys), dimension(NLAY) :: pavel, tavel, delp,     &
     &       coldry, colmol, h2ovmr, o3vmr, temcol

      real (kind=kind_phys), dimension(NLAY) :: cfrac, cliqp, reliq,    &
     &       cicep, reice, cdat1, cdat2, cdat3, cdat4, zclfr

      real (kind=kind_phys), dimension(NLAY) :: plog, forfac, forfrac,  &
     &       selffac, selffrac, fac00, fac01, fac10, fac11

      real (kind=kind_phys), dimension(NLAY,NBLOW:NBHGH) ::             &
     &       taucw, ssacw, asycw, tauae, ssaae, asyae

      real (kind=kind_phys), dimension(2) :: albbm, albdf

      real (kind=kind_phys) ::  colamt(NLAY,MAXGAS)

      real (kind=kind_phys), dimension(NLP1) :: fnetc, flxdc, flxuc,    &
     &       flxd0, flxu0
      real (kind=kind_phys), dimension(NLP1,NBDSW) :: flxdcb, flxucb,   &
     &       flxd0b, flxu0b

      real (kind=kind_phys) :: cosz1, sntz1, tem0, tem1, tem2, s0fac,   &
     &       fp, fp1, ft, ft1, ssolar, zdpgcp, zcf0, zcf1

!! ---  for optional outputs
      real (kind=kind_phys), dimension(2) :: sfbmc, sfbm0, sfdfc, sfdf0
      real (kind=kind_phys) :: suvbf0, suvbfc
      real (kind=kind_phys) :: fnet0(NLP1), fnetb(NLP1,NBDSW)

      integer, dimension(NLAY) :: indfor, indself, jp, jt, jt1

      integer :: i, ib, ipts, j1, j2, k, kk, jp1, laytrop, mb

!
!===> ... begin here
!

      lhswb  = present ( hswb )
      lhsw0  = present ( hsw0 )
      lflxprf= present ( flxprf )
      lfdncmp= present ( fdncmp )

!  ---  s0, the solar constant at toa in w/m**2, is hard-coded with
!       each spectra band, the total flux is about 1368.22 w/m**2.

      s0fac = solcon / s0

!  ---  initial output arrays

      hswc(:,:) = zero
      topflx = topfsw_type ( zero, zero, zero )
      sfcflx = sfcfsw_type ( zero, zero, zero, zero )

!! ---  initial optional outputs
      if ( lflxprf ) then
        flxprf = profsw_type ( zero, zero, zero, zero )
      endif

      if ( lfdncmp ) then
        fdncmp = cmpfsw_type ( zero, zero, zero, zero, zero, zero )
      endif

      if ( lhsw0 ) then
        hsw0(:,:) = zero
      endif

      if ( lhswb ) then
        hswb(:,:,:) = zero
      endif

!  ---  loop over each daytime grid point

      lab_do_ipts : do ipts = 1, NDAY

        j1 = idxday(ipts)

        cosz1  = cosz(j1)
        sntz1  = one / cosz(j1)
        ssolar = s0fac * cosz(j1)
        zcf0   = one
        zcf1   = one
        laytrop= NLAY

!  ---  surface albedo
        albbm(1) = sfcalb(j1,1)
        albdf(1) = sfcalb(j1,2)
        albbm(2) = sfcalb(j1,3)
        albdf(2) = sfcalb(j1,4)

!  ---  prepare atmospheric profile for use in rrtm
!       the vertical index of internal array is from surface to top

        if (iflip == 0) then        ! input from toa to sfc

          tem1 = 100.0 * con_g
          tem2 = 1.0e-20 * 1.0e3 * con_avgd

          do k = 1, NLAY
            kk = NLP1 - k
            pavel(k) = plyr(j1,kk)
            tavel(k) = tlyr(j1,kk)
            delp (k) = plvl(j1,kk+1) - plvl(j1,kk)

!  ---  set absorber amount
!test use
!           h2ovmr(k)= max(zero,qlyr(j1,kk)*amdw)                   ! input mass mixing ratio
!           h2ovmr(k)= max(zero,qlyr(j1,kk))                        ! input vol mixing ratio
!ncep model use
            h2ovmr(k)= max(zero,qlyr(j1,kk)*amdw/(1.0-qlyr(j1,kk))) ! input specific humidity
            o3vmr (k)= max(zero,olyr(j1,kk)*amdo3)                  ! input mass mixing ratio
!test use   o3vmr (k)= max(zero,olyr(j1,kk))                        ! input vol mixing ratio

            tem0 = (one - h2ovmr(k))*con_amd + h2ovmr(k)*con_amw
            coldry(k) = tem2 * delp(k) / (tem1*tem0*(one + h2ovmr(k)))
            temcol(k) = 1.0e-12 * coldry(k)

            colamt(k,1) =                coldry(k)*h2ovmr(k)          ! h2o
            colamt(k,2) = max(temcol(k), coldry(k)*gasvmr(j1,kk,1))   ! co2
            colamt(k,3) =                coldry(k)*o3vmr(k)           ! o3
          enddo

!  ---  set aerosol optical properties

          if (iaersw > 0) then
            do ib = 1, NBDSW
              j2 = NBLOW + ib -1

              do k = 1, NLAY
                kk = NLP1 - k

                tauae(k,j2) = aerosols(j1,kk,ib,1)
                ssaae(k,j2) = aerosols(j1,kk,ib,2)
                asyae(k,j2) = aerosols(j1,kk,ib,3)
              enddo
            enddo
          else
            tauae(:,:) = zero
            ssaae(:,:) = zero
            asyae(:,:) = zero
          endif

          if (iflagliq > 0) then   ! use prognostic cloud method
            do k = 1, NLAY
              kk = NLP1 - k
              cfrac(k) = clouds(j1,kk,1)
              cliqp(k) = clouds(j1,kk,2)
              reliq(k) = clouds(j1,kk,3)
              cicep(k) = clouds(j1,kk,4)
              reice(k) = clouds(j1,kk,5)
              cdat1(k) = clouds(j1,kk,6)
              cdat2(k) = clouds(j1,kk,7)
              cdat3(k) = clouds(j1,kk,8)
              cdat4(k) = clouds(j1,kk,9)
            enddo
          else                     ! use diagnostic cloud method
            do k = 1, NLAY
              kk = NLP1 - k
              cfrac(k) = clouds(j1,kk,1)
              cdat1(k) = clouds(j1,kk,2)
              cdat2(k) = clouds(j1,kk,3)
              cdat3(k) = clouds(j1,kk,4)
            enddo
          endif                    ! end if_iflagliq

        else                        ! input from sfc to toa

          tem1 = 100.0 * con_g
          tem2 = 1.0e-20 * 1.0e3 * con_avgd

          do k = 1, NLAY
            pavel(k) = plyr(j1,k)
            tavel(k) = tlyr(j1,k)
            delp (k) = plvl(j1,k) - plvl(j1,k+1)

!  ---  set absorber amount
!test use
!           h2ovmr(k)= max(zero,qlyr(j1,k)*amdw)                    ! input mass mixing ratio
!           h2ovmr(k)= max(zero,qlyr(j1,k))                         ! input vol mixing ratio
!ncep model use
            h2ovmr(k)= max(zero,qlyr(j1,k)*amdw/(1.0-qlyr(j1,k)))   ! input specific humidity
            o3vmr (k)= max(zero,olyr(j1,k)*amdo3)                   ! input mass mixing ratio
!test use   o3vmr (k)= max(zero,olyr(j1,k))                         ! input vol mixing ratio

            tem0 = (one - h2ovmr(k))*con_amd + h2ovmr(k)*con_amw
            coldry(k) = tem2 * delp(k) / (tem1*tem0*(one + h2ovmr(k)))
            temcol(k) = 1.0e-12 * coldry(k)

            colamt(k,1) =                coldry(k)*h2ovmr(k)          ! h2o
            colamt(k,2) = max(temcol(k), coldry(k)*gasvmr(j1,k,1))    ! co2
            colamt(k,3) =                coldry(k)*o3vmr(k)           ! o3
          enddo

!  ---  set aerosol optical properties

          if (iaersw > 0) then
            do ib = 1, NBDSW
              j2 = NBLOW + ib -1

              do k = 1, NLAY
                tauae(k,j2) = aerosols(j1,k,ib,1)
                ssaae(k,j2) = aerosols(j1,k,ib,2)
                asyae(k,j2) = aerosols(j1,k,ib,3)
              enddo
            enddo
          else
            tauae(:,:) = zero
            ssaae(:,:) = zero
            asyae(:,:) = zero
          endif

          if (iflagliq > 0) then   ! use prognostic cloud method
            do k = 1, NLAY
              cfrac(k) = clouds(j1,k,1)
              cliqp(k) = clouds(j1,k,2)
              reliq(k) = clouds(j1,k,3)
              cicep(k) = clouds(j1,k,4)
              reice(k) = clouds(j1,k,5)
              cdat1(k) = clouds(j1,k,6)
              cdat2(k) = clouds(j1,k,7)
              cdat3(k) = clouds(j1,k,8)
              cdat4(k) = clouds(j1,k,9)
            enddo
          else                     ! use diagnostic cloud method
            do k = 1, NLAY
              cfrac(k) = clouds(j1,k,1)
              cdat1(k) = clouds(j1,k,2)
              cdat2(k) = clouds(j1,k,3)
              cdat3(k) = clouds(j1,k,4)
            enddo
          endif                    ! end if_iflagliq

        endif                       ! if_iflip

!  ---  set up gas column amount, convert from volume mixing ratio to
!       molec/cm2 based on coldry (scaled to 1.0e-20)

        if (iflip == 0) then        ! input from toa to sfc

          if (irgassw == 1) then
            do k = 1, NLAY
              kk = NLP1 - k
              colamt(k,4) = max(temcol(k), coldry(k)*gasvmr(j1,kk,2))  ! n2o
              colamt(k,5) = max(temcol(k), coldry(k)*gasvmr(j1,kk,3))  ! ch4
              colamt(k,6) = max(temcol(k), coldry(k)*gasvmr(j1,kk,4))  ! o2
!             colamt(k,7) = max(temcol(k), coldry(k)*gasvmr(j1,kk,5))  ! co - notused
            enddo
          else
            do k = 1, NLAY
              colamt(k,4) = temcol(k)                                  ! n2o
              colamt(k,5) = temcol(k)                                  ! ch4
              colamt(k,6) = temcol(k)                                  ! o2
!             colamt(k,7) = temcol(k)                                  ! co - notused
            enddo
          endif

        else                        ! input from sfc to toa

          if (irgassw == 1) then
            do k = 1, NLAY
              colamt(k,4) = max(temcol(k), coldry(k)*gasvmr(j1,k,2))   ! n2o
              colamt(k,5) = max(temcol(k), coldry(k)*gasvmr(j1,k,3))   ! ch4
              colamt(k,6) = max(temcol(k), coldry(k)*gasvmr(j1,k,4))   ! o2
!             colamt(k,7) = max(temcol(k), coldry(k)*gasvmr(j1,k,5))   ! co - notused
            enddo
          else
            do k = 1, NLAY
              colamt(k,4) = temcol(k)                                  ! n2o
              colamt(k,5) = temcol(k)                                  ! ch4
              colamt(k,6) = temcol(k)                                  ! o2
!             colamt(k,7) = temcol(k)                                  ! co - notused
            enddo
          endif

        endif                       ! if_iflip

!  ---  compute fractions of clear sky view

        if (iovr == 0) then                      ! random overlapping
          do k = 1, NLAY
            zcf0 = zcf0 * (one - cfrac(k))
          enddo
        else if (iovr == 1) then                 ! max/ran overlapping
          do k = 1, NLAY
            if (cfrac(k) > eps) then                  ! cloudy layer
              zcf1 = min ( zcf1, one-cfrac(k) )
            elseif (zcf1 < one) then                  ! clear layer
              zcf0 = zcf0 * zcf1
              zcf1 = one
            endif
          enddo
          zcf0 = zcf0 * zcf1
        else if (iovr == 2) then                 ! maximum overlapping
          do k = 1, NLAY
            zcf0 = min ( zcf0, one-cfrac(k) )
          enddo
        else
          print *,'  invalid specification of iovr =',iovr
          stop
        endif

        if (zcf0 <= eps) zcf0 = zero
        if (zcf0 > oneminus) zcf0 = one
        zcf1 = one - zcf0

        if (zcf1 <= eps) then
          do k=1, NLAY
            zclfr(k) = zero
          enddo
        else
          do k=1, NLAY
            zclfr(k) = cfrac(k) / zcf1
          enddo
        endif
!
!===> ...  compute cloud optical properties
!

        call cldprop                                                    &
!  ---  inputs:
     &     ( cfrac,cliqp,reliq,cicep,reice,cdat1,cdat2,cdat3,cdat4,     &
     &       NLAY,                                                      &
!  ---  output:
     &       taucw, ssacw, asycw                                        &
     &     )

!  ---  calculate needed column amounts. using e = 1334.2 cm-1.

        do k = 1, NLAY
          colmol(k) = coldry(k) + colamt(k,1)
          forfac(k) = pavel(k)*stpfac / (tavel(k)*(one + h2ovmr(k)))
        enddo

        do k = 1, NLAY

!  ---  find the two reference pressures on either side of the
!       layer pressure.  store them in jp and jp1.  store in fp the
!       fraction of the difference (in ln(pressure)) between these
!       two values that the layer pressure lies.

          plog(k) = log(pavel(k))
          jp(k) = max(1, min(58, int(36.0 - 5.0*(plog(k)+0.04)) ))
          jp1   = jp(k) + 1
          fp    = 5.0 * (preflog(jp(k)) - plog(k))

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
          ft  = tem1 - float(jt (k) - 3)
          ft1 = tem2 - float(jt1(k) - 3)

!  ---  we have now isolated the layer ln pressure and temperature,
!       between two reference pressures and two reference temperatures
!       (for each reference pressure).  we multiply the pressure
!       fraction fp with the appropriate temperature fractions to get
!       the factors that will be needed for the interpolation that yields
!       the optical depths (performed in routines taugbn for band n).

          fp1 = one - fp
          fac10(k) = fp1 * ft
          fac00(k) = fp1 * (one - ft)
          fac11(k) = fp  * ft1
          fac01(k) = fp  * (one - ft1)

        enddo    ! end_do_k_loop

        do k = 1, NLAY

!  ---  if the pressure is less than ~100mb, perform a different
!       set of species interpolations.

          if (plog(k) > 4.56) then
            laytrop = k

!  ---  set up factors needed to separately include the water vapor
!       foreign-continuum in the calculation of absorption coefficient.

            tem1 = (332.0 - tavel(k)) / 36.0
            indfor (k) = min(2, max(1, int(tem1)))
            forfrac(k) = tem1 - float(indfor(k))

!  ---  set up factors needed to separately include the water vapor
!       self-continuum in the calculation of absorption coefficient.

            tem2 = (tavel(k) - 188.0) / 7.2
            indself (k) = min(9, max(1, int(tem2)-7))
            selffrac(k) = tem2 - float(indself(k) + 7)
            selffac (k) = h2ovmr(k) * forfac(k)
          else
!  ---  set up factors needed to separately include the water vapor
!       foreign-continuum in the calculation of absorption coefficient.

            tem1 = (tavel(k) - 188.0) / 36.0
            indfor (k) = 3
            forfrac(k) = tem1 - one

            indself (k) = 0
            selffrac(k) = zero
            selffac (k) = zero
          endif

        enddo    ! end_do_k_loop

        if ( lfdncmp ) then

          call spcvrt                                                   &
!  ---  inputs:
     &     ( colamt, colmol, coldry, cosz1, sntz1, albbm, albdf,        &
     &       zcf1, zclfr, taucw, ssacw, asycw, tauae, ssaae, asyae,     &
     &       forfac, forfrac, indfor, selffac, selffrac, indself,       &
     &       fac00, fac01, fac10, fac11, jp, jt, jt1, laytrop,          &
     &       NLAY, NLP1,                                                &
!  ---  outputs:
     &       flxdcb, flxucb, flxd0b, flxu0b                             &
!! ---  optional outputs:
     &,      SFBMC=sfbmc,SFDFC=sfdfc,SFBM0=sfbm0,SFDF0=sfdf0            &
     &,      SUVBF0=suvbf0,SUVBFC=suvbfc                                &
     &     )

!! ---  optional uv-b surface downward flux
          fdncmp(j1)%uvbf0 = ssolar * suvbf0
          fdncmp(j1)%uvbfc = ssolar * (zcf1*suvbfc + zcf0*suvbf0)

!! ---  optional beam and diffuse sfc fluxes
          fdncmp(j1)%nirbm = ssolar * (zcf1*sfbmc(1) + zcf0*sfbm0(1))
          fdncmp(j1)%nirdf = ssolar * (zcf1*sfdfc(1) + zcf0*sfdf0(1))
          fdncmp(j1)%visbm = ssolar * (zcf1*sfbmc(2) + zcf0*sfbm0(2))
          fdncmp(j1)%visdf = ssolar * (zcf1*sfdfc(2) + zcf0*sfdf0(2))

        else

          call spcvrt                                                   &
!  ---  inputs:
     &     ( colamt, colmol, coldry, cosz1, sntz1, albbm, albdf,        &
     &       zcf1, zclfr, taucw, ssacw, asycw, tauae, ssaae, asyae,     &
     &       forfac, forfrac, indfor, selffac, selffrac, indself,       &
     &       fac00, fac01, fac10, fac11, jp, jt, jt1, laytrop,          &
     &       NLAY, NLP1,                                                &
!  ---  outputs:
     &       flxdcb, flxucb, flxd0b, flxu0b                             &
     &     )

        endif    ! end if_lfdncmp

        do mb = 1, NBDSW
          do k = 1, NLP1
            flxucb(k,mb) = zcf1*flxucb(k,mb) + zcf0*flxu0b(k,mb)
            flxdcb(k,mb) = zcf1*flxdcb(k,mb) + zcf0*flxd0b(k,mb)
          enddo
        enddo

        do k = 1, NLP1
          flxuc(k) = zero
          flxdc(k) = zero
          flxu0(k) = zero
          flxd0(k) = zero
        enddo

        do k = 1, NLP1
          do mb = 1, NBDSW
            flxuc(k) = flxuc(k) + flxucb(k,mb)
            flxdc(k) = flxdc(k) + flxdcb(k,mb)
            flxu0(k) = flxu0(k) + flxu0b(k,mb)
            flxd0(k) = flxd0(k) + flxd0b(k,mb)
          enddo
        enddo

        do k = 1, NLP1
          flxuc(k) = ssolar * flxuc(k)
          flxdc(k) = ssolar * flxdc(k)
          flxu0(k) = ssolar * flxu0(k)
          flxd0(k) = ssolar * flxd0(k)
          fnetc(k) = flxdc(k) - flxuc(k)
        enddo

!  ---  toa and sfc fluxes
        topflx(j1)%upfxc = flxuc(NLP1)
        topflx(j1)%dnfxc = flxdc(NLP1)
        topflx(j1)%upfx0 = flxu0(NLP1)

        sfcflx(j1)%upfxc = flxuc(1)
        sfcflx(j1)%dnfxc = flxdc(1)
        sfcflx(j1)%upfx0 = flxu0(1)
        sfcflx(j1)%dnfx0 = flxd0(1)

        if (iflip == 0) then        ! output from toa to sfc

!  ---  compute heating rates
          do k = 1, NLAY
            kk = NLP1 - k
            hswc(j1,kk) = (fnetc(k+1) - fnetc(k)) * heatfac / delp(k)
          enddo

!! ---  optional flux profiles
          if ( lflxprf ) then
            do k = 1, NLP1
              kk = NLP1 - k + 1
              flxprf(j1,kk)%upfxc = flxuc(k)
              flxprf(j1,kk)%dnfxc = flxdc(k)
              flxprf(j1,kk)%upfx0 = flxu0(k)
              flxprf(j1,kk)%dnfx0 = flxd0(k)
            enddo
          endif

!! ---  optional clear sky heating rates
          if ( lhsw0 ) then
            fnet0(:) = flxd0(:) - flxu0(:)

            do k = 1, NLAY
              kk = NLP1 - k
              hsw0(j1,kk) = (fnet0(k+1) - fnet0(k)) * heatfac / delp(k)
            enddo
          endif

!! ---  optional spectral band heating rates
          if ( lhswb ) then
            fnetb(:,:) = ssolar * (flxdcb(:,:) - flxucb(:,:))

            do k = 1, NLAY
              kk = NLP1 - k
              do mb = 1, NBDSW
                hswb(j1,kk,mb) = (fnetb(k+1,mb) - fnetb(k,mb))          &
     &                         * heatfac / delp(k)
              enddo
            enddo
          endif

        else                        ! output from sfc to toa

!  ---  compute heating rates
          do k = 1, NLAY
            hswc(j1,k) = (fnetc(k+1) - fnetc(k)) * heatfac / delp(k)
          enddo

!! ---  optional flux profiles
          if ( lflxprf ) then
            do k = 1, NLP1
              flxprf(j1,k)%upfxc = flxuc(k)
              flxprf(j1,k)%dnfxc = flxdc(k)
              flxprf(j1,k)%upfx0 = flxu0(k)
              flxprf(j1,k)%dnfx0 = flxd0(k)
            enddo
          endif

!! ---  optional clear sky heating rates
          if ( lhsw0 ) then
            fnet0(:) = flxd0(:) - flxu0(:)

            do k = 1, NLAY
              hsw0(j1,k) = (fnet0(k+1) - fnet0(k)) * heatfac / delp(k)
            enddo
          endif

!! ---  optional spectral band heating rates
          if ( lhswb ) then
            fnetb(:,:) = ssolar * (flxdcb(:,:) - flxucb(:,:))

            do k = 1, NLAY
              do mb = 1, NBDSW
                hswb(j1,k,mb) = (fnetb(k+1,mb) - fnetb(k,mb))           &
     &                        * heatfac / delp(k)
              enddo
            enddo
          endif

        endif                       ! if_iflip

      enddo   lab_do_ipts

      return
!...................................
      end subroutine swrad
!-----------------------------------



!-----------------------------------
      subroutine rswinit                                                &
!...................................

!  ---  inputs:
     &     ( icwp, me, NLAY )
!  ---  outputs: (none)

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
!  control flags in module "module_radsw_cntr_para":                    !
!     iswrate - heating rate unit selections                            !
!               =1: output in k/day                                     !
!               =2: output in k/second                                  !
!     iaersw  - flags for aerosols effect                               !
!               =0: without aerosol effect                              !
!               >0: include aerosol effect                              !
!     imodsw  - control flag for 2-stream transfer scheme               !
!               =1; delta-eddington    (joseph et al., 1976)            !
!               =2: pifm               (zdunkowski et al., 1980)        !
!               =3: discrete ordinates (liou, 1973)                     !
!     irgassw - control flag for rare gases (ch4,n2o,o2, etc.)          !
!               =0: do not include rare gases                           !
!               =1: include all rare gases                              !
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

!
!===> ... begin here
!

      if (me == 0) then
        print *,' - Using AER Shortwave Radiation, Version: ',VTAGSW

        if (imodsw == 1) then
          print *,'   --- Delta-eddington 2-stream transfer scheme'
        else if (imodsw == 2) then
          print *,'   --- PIFM 2-stream transfer scheme'
        else if (imodsw == 3) then
          print *,'   --- Discrete ordinates 2-stream transfer scheme'
        endif

        if (iaersw == 0) then
          print *,'   --- Aerosol effect is NOT included in SW, all'    &
     &           ,' internal aerosol parameters are reset to zeros'
        else
          print *,'   --- Using input aerosol parameters for SW'
        endif

        if (irgassw == 0) then
          print *,'   --- Rare gases absorption is NOT included in SW'
        else
          print *,'   --- Include rare gases N2O, CH4, O2, absorptions',&
     &            ' in SW'
        endif
      endif

!  --- ...  check cloud flags for consistency

      if ((icwp == 0 .and. iflagliq /= 0) .or.                          &
     &    (icwp == 1 .and. iflagliq == 0)) then
        print *, ' *** Model cloud scheme inconsistent with SW',        &
     &           ' radiation cloud radiative property setup !!'
        stop
      endif

!  --- ...  setup constant factors for heating rate
!           the 1.0e-2 is to convert pressure from mb to N/m**2

      if (iswrate == 1) then
!       heatfac = con_g * 86400. * 1.0e-2 / con_cp  !   (in k/day)
        heatfac = con_g * 864.0 / con_cp            !   (in k/day)
!       heatfac = 8.4391
      else
        heatfac = con_g * 1.0e-2 / con_cp           !   (in k/second)
      endif

      return
!...................................
      end subroutine rswinit
!-----------------------------------



!-----------------------------------
      subroutine cldprop                                                &
!...................................

!  ---  inputs:
     &     ( cfrac,cliqp,reliq,cicep,reice,cdat1,cdat2,cdat3,cdat4,     &
     &       NLAY,                                                      &
!  ---  output:
     &       taucw, ssacw, asycw                                        &
     &     )

!  *******************************************************************  !
!                                                                       !
!  compute the cloud optical property functions for each cloudy layer   !
!                                                                       !
!                                                                       !
!  inputs:                                                              !
!     cfrac(NLAY)  - layer cloud fraction                               !
!                ---  for  iflagliq > 0  ---                            !
!     cliqp(NLAY)  - layer cloud liq water path (g/m**2)                !
!     reliq(NLAY)  - mean effective radius for liq cloud (micron)       !
!     cicep(NLAY)  - layer cloud ice water path (g/m**2)                !
!     reice(NLAY)  - mean effective radius for ice cloud (micron)       !
!     cdat1(NLAY)  - layer rain drop water path (g/m**2)    -not used-  !
!     cdat2(NLAY)  - mean eff radius for rain drop (micron) -not used-  !
!     cdat3(NLAY)  - layer snow flake water path(g/m**2)    -not used-  !
!   ** fu's scheme need to be normalized by snow density (g/m**3/1.0e6) !
!     cdat4(NLAY)  - mean effective radius for snow flake(micron)       !
!                ---  for  iflagliq = 0  ---                            !
!     cdat1(NLAY)  - layer cloud optical depth                          !
!     cdat2(NLAY)  - layer cloud single scattering albedo               !
!     cdat3(NLAY)  - layer cloud asymmetry factor                       !
!     cdat4(NLAY)  -     optional use                                   !
!     cliqp(NLAY)  -     not used                                       !
!     reliq(NLAY)  -     not used                                       !
!     cicep(NLAY)  -     not used                                       !
!     reice(NLAY)  -     not used                                       !
!                                                                       !
!     NLAY         - vertical layer number                              !
!                                                                       !
!  outputs:                                                             !
!     taucw(NLAY,NBLOW:NBHGH) - cloud optical depth                     !
!     ssacw(NLAY,NBLOW:NBHGH) - weighted cloud single scattering albedo !
!                               (ssa = ssacw / taucw)                   !
!     asycw(NLAY,NBLOW:NBHGH) - weighted cloud asymmetry factor         !
!                               (asy = asycw / ssacw)                   !
!                                                                       !
!  *******************************************************************  !
!                                                                       !
!    explanation of the method for each value of iflagliq, and iflagice.!
!    set up in module "module_radlw_cntr_para"                          !
!                                                                       !
!     iflagliq=0 : input cloud optical property (tau, ssa, asy).        !
!                  (used for diagnostic cloud method)                   !
!     iflagliq>0 : input cloud liq/ice path and effective radius, also  !
!                  require the user of 'iflagice' to specify the method !
!                  used to compute aborption due to water/ice parts.    !
!  ...................................................................  !
!                                                                       !
!     iflagliq=0:  for each cloudy layer, the cloud fraction and (gray) !
!                  optical depth, single scattring albedo, and asymmetry!
!                  factor are inputs.                                   !
!     iflagliq=1:  for each cloudy layer, the cloud liquid path, eff.   !
!                  radius for water cloud are inputs, use method by     !
!                  hu and stamnes, 1993, j., clim.                      !
!                                                                       !
!     iflagice used only when iglagliq >= 1                             !
!                                                                       !
!     iflagice=3:  the cloud ice path (g/m2) and ice effective radius   !
!                  (microns) are input and the optical depths due to ice!
!                  clouds are computed as in fu, 1996, j. clim.         !
!                                                                       !
!                                                                       !
!  *******************************************************************  !
!                       original description:                           !
!                                                                       !
!  path:      $source: /storm/rc1/cvsroot/rc/rrtm_sw/src/cldprop_sw.f,v !
!  author:    $author: jdelamer                                         !
!  revision:  $revision: 2.6                                            !
!  created:   $date: 2002/04/04 18:29:47                                !
!                                                                       !
!  purpose:  compute the cloud optical depth(s) for each cloudy         !
!  layer.  note:  only inflag = 0 and inflag=2/liqflag=1/iceflag=3      !
!  (hu & stamnes, q. fu) are implemented.                               !
!                                                                       !
!  *******************************************************************  !
!
      use module_radsw_cldprtb
!
      implicit none

!  ---  inputs:
      integer, intent(in) :: NLAY

      real (kind=kind_phys), dimension(:), intent(in) :: cfrac,         &
     &       cliqp, reliq, cicep, reice, cdat1, cdat2, cdat3, cdat4

!  ---  outputs:
      real (kind=kind_phys), dimension(:,NBLOW:), intent(out) ::        &
     &       taucw, ssacw, asycw

!  ---  locals:
      real (kind=kind_phys), dimension(NBLOW:NBHGH) ::  fdelta,         &
     &       extcoice, ssacoice, asycoice, forcoice,                    &
     &       extcoliq, ssacoliq, asycoliq, forcoliq

      real (kind=kind_phys) :: ffliq0, ffliq1, ffice0, ffice1,          &
     &       cldliq, refliq, tauliq, ssaliq, asyliq, factor, fint,      &
     &       cldice, refice, tauice, ssaice, asyice,                    &
     &       cldrain, refrain, taurain, ssarain, asyrain,               &
     &       cldsnow, refsnow, tausnow, ssasnow, asysnow

      integer :: ib, k, index
 
!
!===> ... begin here
!
      do ib = NBLOW, NBHGH
        do k = 1, NLAY
          taucw(k,ib) = zero
          ssacw(k,ib) = zero
          asycw(k,ib) = zero
        enddo
      enddo

      lab_do_k : do k = 1, NLAY

        lab_if_cfrac : if (cfrac(k) >= eps) then

!  --- ...  ice clouds and water clouds combined.
          lab_if_liq : if (iflagliq == 0) then

            do ib = NBLOW, NBHGH
              taucw(k,ib) = cdat1(k)
              ssacw(k,ib) = cdat2(k) * cdat1(k)
              asycw(k,ib) = cdat3(k) * ssacw(k,ib)
            enddo

!  --- ...  separate treatement of ice clouds and water clouds.
          else   lab_if_liq

            cldliq = cliqp(k)
            refliq = max(2.5e0, min(59.5e0, real(reliq(k)) ))
!error      refliq = max(1.5e0, min(60.0e0, real(reliq(k)) ))

            cldice = cicep(k)
!  --- ...  based on FU, factor 1.5396=8/(3*sqrt(3)) converts
!           effective radius to generalized ice particle size
            refice = max(10.e0, min(140.e0, 1.5396*reice(k) ))

            cldrain = cdat1(k)
            refrain = cdat2(k)
            cldsnow = cdat3(k)
            refsnow = cdat4(k)

!  --- ...  calculation of absorption coefficients due to water clouds.
            if (cldliq <= zero) then
              do ib = NBLOW, NBHGH
                extcoliq(ib) = zero
                ssacoliq(ib) = one
                asycoliq(ib) = one
                forcoliq(ib) = zero
              enddo
            else
              if (iflagliq == 1) then
                factor = refliq - 1.5
                index = max(1, min(57, int(factor) ))
                fint  = factor - index

                do ib = NBLOW, NBHGH
                  extcoliq(ib) = extdatliq1(index,ib) + fint            &
     &                 * (extdatliq1(index+1,ib) - extdatliq1(index,ib))
                  ssacoliq(ib) = ssadatliq1(index,ib) + fint            &
     &                 * (ssadatliq1(index+1,ib) - ssadatliq1(index,ib))
                  asycoliq(ib) = asydatliq1(index,ib) + fint            &
     &                 * (asydatliq1(index+1,ib) - asydatliq1(index,ib))
                  forcoliq(ib) = asycoliq(ib) * asycoliq(ib)
                enddo
              else
                print *,'  Undefined selection of iflagliq =',iflagliq
                stop
              endif
            endif   ! end if_cldliq

!  --- ...  calculation of absorption coefficients due to ice clouds.
            if (cldice <= zero) then
              do ib = NBLOW, NBHGH
                extcoice(ib) = zero
                ssacoice(ib) = one
                asycoice(ib) = one
                forcoice(ib) = zero
              enddo
            else
              if (iflagice == 3) then
                factor = (refice - 5.0) / 5.0
                index  = max(1, min(26, int(factor) ))
                fint   = factor - index

                do ib = NBLOW, NBHGH
                  extcoice(ib) = extdatice3(index,ib) + fint            &
     &               * (extdatice3(index+1,ib) - extdatice3(index,ib))
                  ssacoice(ib) = ssadatice3(index,ib) + fint            &
     &               * (ssadatice3(index+1,ib) - ssadatice3(index,ib))
                  asycoice(ib) = asydatice3(index,ib) + fint            &
     &               * (asydatice3(index+1,ib) - asydatice3(index,ib))
                  fdelta  (ib) = min(one, max(zero,                     &
     &                           fdldatice3(index,ib) + fint            &
     &               * (fdldatice3(index+1,ib) - fdldatice3(index,ib))))

                  forcoice(ib) = min(asycoice(ib),                      &
     &                              fdelta(ib) + 0.5/ssacoice(ib) )   ! see fu 1996 p. 2067 
                enddo
              else
                print *,'  Undefined selection of iflagice =',iflagice
                stop
              endif
            endif   ! end if_cldice

!  --- ...  optical depth for rain and snow

            taurain = cldrain * a0r
            if (cldsnow>zero .and. refsnow>10.0) then
              tausnow = cldsnow * (a0s + a1s/refsnow)
            else
              tausnow = zero
            endif

            do ib = NBLOW, NBHGH
              tauliq = cldliq * extcoliq(ib)
              ssaliq = tauliq * ssacoliq(ib)
              asyliq = ssaliq * asycoliq(ib)

              tauice = cldice * extcoice(ib)
              ssaice = tauice * ssacoice(ib)
              asyice = ssaice * asycoice(ib)

              ssarain = taurain * (one - b0r(ib))
              asyrain = ssarain * c0r(ib)

              ssasnow = tausnow * (one - (b0s(ib)+b1s(ib)*refsnow))
              asysnow = ssasnow * c0s(ib)

              taucw(k,ib) = tauliq + tauice + taurain + tausnow
              ssacw(k,ib) = ssaliq + ssaice + ssarain + ssasnow
              asycw(k,ib) = asyliq + asyice + asyrain + asysnow

!             do istr = 1, nstr
!  --- ...  this commented code is the standard method for delta-m scaling.
!           in accordance with the 1996 fu paper, equation a.3, the moments
!           for ice were calculated as in the uncommented code.
!               xmom(istr,k,ib) =                                       &
!    &                (ssaliq*(asycoliq(ib)**istr-forcoliq(ib))/ffliq0  &
!    &               + ssaice*(asycoice(ib)**istr-forcoice(ib))/ffice0) &
!    &               / (ssaliq + ssaice)
!  --- ...  the following commented code is used by the original rrtm_sw
!               xmom(istr,k,ib) = (one / (ssaliq+ssaice))               &
!    &             * (ssaliq*(asycoliq(ib)**istr-forcoliq(ib))/ffliq0   &
!    &             +  ssaice*((asycoice(ib)-forcoice(ib))/ffice0)**istr)
!             enddo
            enddo

          endif  lab_if_liq

        endif  lab_if_cfrac

      enddo  lab_do_k

      return
!...................................
      end subroutine cldprop
!-----------------------------------



!-----------------------------------
      subroutine spcvrt                                                 &
!...................................

!  ---  inputs:
     &     ( colamt, colmol, coldry, cosz, sntz, albbm, albdf,          &
     &       cf1, pclfr, taucw, ssacw, asycw, tauae, ssaae, asyae,      &
     &       forfac, forfrac, indfor, selffac, selffrac, indself,       &
     &       fac00, fac01, fac10, fac11, jp, jt, jt1, laytrop,          &
     &       NLAY, NLP1,                                                &
!  ---  outputs: 
     &       flxdc, flxuc, flxd0, flxu0                                 &
!! ---  optional outputs:
     &,      sfbmc, sfdfc, sfbm0, sfdf0                                 &
     &,      suvbf0, suvbfc                                             &
     &     )

!  *******************************************************************  !
!                                                                       !
!   purpose:  computes the shortwave radiation fluxes using two-stream  !
!             method of howard barker                                   !
!                                                                       !
!   interface:  "spcvrt" is called by "swrad"                           !
!                                                                       !
!  *******************************************************************  !
!                                                                       !
!  input variables:                                                     !
!     colamt(NLAY,MAXGAS)- column amounts of absorbing gases the index  !
!                          1-MAXGAS are for water vapor, carbon diozide,!
!                          ozone, nitrous oxide, methane, and oxigen,   !
!                          respectively (molecules/cm**2)               !
!     coldry(NLAY)       - dry air column amount(1.e-20*molecules/cm**2)!
!     colmol(NLAY)       - total column amount (dry air+water vapor)    !
!     cosz               - cosine of solar zenith angle                 !
!     sntz               - secant of solar zenith angle                 !
!     albbm (2)          - direct beam surface albedo for nir and uv+vis!
!     albdf (2)          - diffuse surface albedo for nir and uv+vis    !
!     cf1                - effective total cloud cover at surface       !
!     pclfr (NLAY)       - layer cloud fraction                         !
!     taucw,ssacw,asycw (NLAY,NBLOW:NBHGH)                              !
!                        - layer cloud optical depth, single scattering !
!                          albedo, and asymmetry factor (weighted value)!
!     tauae,ssaae,asyae (NLAY,NBLOW:NBHGH)                              !
!                        - layer aerosols optical depth, single scatt.  !
!                          albedo, and asymmetry factor                 !
!     forfac (NLAY)      - scale factor needed to foreign-continuum.    !
!     forfrac(NLAY)      - factor needed for temperature interpolation  !
!     indfor (NLAY)      - index of the lower of the two appropriate    !
!                          reference temperatures needed for foreign-   !
!                          continuum interpolation                      !
!     selffac(NLAY)      - scale factor needed to h2o self-continuum.   !
!     selffrac(NLAY)     - factor needed for temperature interpolation  !
!                          of reference h2o self-continuum data         !
!     indself(NLAY)      - index of the lower of the two appropriate    !
!                          reference temperatures needed for the self-  !
!                          continuum interpolation                      !
!     facij  (NLAY)      - for each layer, these are factors that are   !
!                          needed to compute the interpolation factors  !
!                          that multiply the appropriate reference k-   !
!                          values.  a value of 0/1 for i,j indicates    !
!                          that the corresponding factor multiplies     !
!                          reference k-value for the lower/higher of the!
!                          two appropriate temperatures, and altitudes, !
!                          respectively.                                !
!     jp     (NLAY)      - the index of the lower (in altitude) of the  !
!                          two appropriate ref pressure levels needed   !
!                          for interpolation.                           !
!     jt, jt1(NLAY)      - the indices of the lower of the two approp   !
!                          ref temperatures needed for interpolation    !
!                        (for pressure levels jp and jp+1, respectively)!
!     laytrop            - layer at which switch is made from one       !
!                          combination of key species to another        !
!     NLAY, NLP1         - number of layers/levels                      !
!                                                                       !
!  output variables:                                                    !
!     flxdc (NLP1,NBDSW) - downward flux for cloudy sky                 !
!     flxuc (NLP1,NBDSW) - upward flux for cloudy sky                   !
!     flxd0 (NLP1,NBDSW) - downward flux for clear sky                  !
!     flxu0 (NLP1,NBDSW) - upward flux for clear sky                    !
!                                                                       !
!! optional output variables:                                           !
!     sfbmc (2)          - cloudy sky sfc down beam fluxes (nir,uv+vis) !
!     sfdfc (2)          - clousy sky sfc down diff fluxes (nir,uv+vis) !
!     sfbm0 (2)          - clear sky sfc  down beam fluxes (nir,uv+vis) !
!     sfdf0 (2)          - clear sky sfc  down diff fluxes (nir,uv+vis) !
!     suvbfc             - cloudy sky sfc  down uv-b fluxes             !
!     suvbf0             - clear sky sfc  down uv-b fluxes              !
!                                                                       !
!  internal subroutines called:  taumol16-29, swflux                    !
!  external subroutines called:  none                                   !
!                                                                       !
!  reference:  see radiation's part of the ecmwf research department    !
!              documentation                                            !
!                                                                       !
!  program history:                                                     !
!    2003-02-27  jean-jacques morcrette, ecmwf   original author        !
!    2004-01-20  yu-tai hou    modified for ncep models                 !
!                                                                       !
!                                                                       !
!  *******************************************************************  !
!
      use module_radsw_sflux, only : sfluxref01, sfluxref02,            &
     &                               sfluxref03, strrat, specwt,        &
     &                               scalekur, layreffr, ix1, ix2, ibx
!
      implicit none

!  ---  inputs:
      integer,               intent(in) :: NLAY, NLP1, laytrop

      integer, dimension(:), intent(in) :: indfor, indself, jp, jt, jt1

      real (kind=kind_phys), dimension(:),  intent(in) :: pclfr,        &
     &       coldry, colmol, forfac, forfrac, selffac, selffrac,        &
     &       fac00, fac01, fac10, fac11, albbm, albdf

      real (kind=kind_phys), dimension(:,:),intent(in) :: colamt

      real (kind=kind_phys), dimension(:,NBLOW:),intent(in)::           &
     &       taucw, ssacw, asycw, tauae, ssaae, asyae

      real (kind=kind_phys), intent(in) :: cosz, sntz, cf1

!  ---  outputs:
      real (kind=kind_phys), dimension(:,:), intent(out) :: flxdc,      &
     &       flxuc, flxd0, flxu0

!! ---  optional outputs:
      real (kind=kind_phys), dimension(:), optional, intent(out) ::     &
     &       sfbmc, sfdfc, sfbm0, sfdf0
      real (kind=kind_phys), optional, intent(out) :: suvbfc, suvbf0

!  ---  locals:
      real (kind=kind_phys) :: fs, speccomb, specmult, colm1, colm2

      real (kind=kind_phys), dimension(:,:), pointer :: sflxptr

      integer, dimension(NLAY,NBLOW:NBHGH) :: id0, id1
      integer :: ibd, ifb, j, jb, js, k, klow, khgh, klim, ks, njb

!  ---  direct outputs from "taumol##":
      real (kind=kind_phys), dimension(NLAY,NGMAX) :: taug, taur
      real (kind=kind_phys), dimension(NGMAX)      :: sfluxzen

!  ---  direct outputs from "swflux":
      real (kind=kind_phys), dimension(NLP1,2)     :: fxdn, fxup

!! ---  for optional output from "swflux":
      real (kind=kind_phys) :: sflxbc,sflxdc,sflxb0,sflxd0

!
!===> ... begin here
!
!  --- initialization of output fluxes
      do ibd = 1, NBDSW
        do k = 1, NLP1
          flxdc(k,ibd)= zero
          flxuc(k,ibd)= zero
          flxd0(k,ibd)= zero
          flxu0(k,ibd)= zero
        enddo
      enddo

      if ( lfdncmp ) then
!! ---  optional uv-b surface downward flux
        suvbfc  = zero
        suvbf0  = zero

!! ---  optional output surface fluxes
        sfbmc(1) = zero
        sfbmc(2) = zero
        sfdfc(1) = zero
        sfdfc(2) = zero
        sfbm0(1) = zero
        sfbm0(2) = zero
        sfdf0(1) = zero
        sfdf0(2) = zero
      endif

      do jb = NBLOW, NBHGH

!  ---  indices for layer optical depth
        do k = 1, laytrop
          id0(k,jb) = ((jp(k)-1)*5 + (jt (k)-1)) * NSPA(jb)
          id1(k,jb) = ( jp(k)   *5 + (jt1(k)-1)) * NSPA(jb)
        enddo

        do k = laytrop+1, NLAY
          id0(k,jb) = ((jp(k)-13)*5 + (jt (k)-1)) * NSPB(jb)
          id1(k,jb) = ((jp(k)-12)*5 + (jt1(k)-1)) * NSPB(jb)
        enddo

      enddo

!  ---  loop over each spectral band

      lab_do_jb : do jb = NBLOW, NBHGH

!  ---  calculate spectral flux at toa

        ibd = ibx(jb)
        njb = NG(jb)

        NULLIFY (sflxptr)

        select case (jb)

          case (16, 20, 23, 25, 26, 29)
            
            sflxptr => sfluxref01(:,:,ibd)

            do j = 1, njb
              sfluxzen(j) = sflxptr(j,1)
            enddo

          case (27)
            
            sflxptr => sfluxref01(:,:,ibd)

            do j = 1, njb
              sfluxzen(j) = scalekur * sflxptr(j,1)
            enddo

          case default

            if (jb==17 .or. jb==28) then
              sflxptr => sfluxref02(:,:,ibd)
              klow = laytrop
              khgh = NLAY - 1
              klim = NLAY
            else
              sflxptr => sfluxref03(:,:,ibd)
              klow = 1
              khgh = laytrop - 1
              klim = laytrop
            endif

            ks = klim
            lab_do_k : do k = klow, khgh
              if (jp(k)<layreffr(jb) .and. jp(k+1) >= layreffr(jb)) then
                ks = k + 1
                exit lab_do_k
              endif
            enddo  lab_do_k

            colm1 = colamt(ks,ix1(jb))
            colm2 = colamt(ks,ix2(jb))
            speccomb = colm1 + strrat(jb)*colm2
            specmult = specwt(jb) * min( oneminus, colm1/speccomb )
            js = 1 + int( specmult )
            fs = mod(specmult, one)

            do j = 1, njb
              sfluxzen(j) = sflxptr(j,js)                               &
     &                    + fs * (sflxptr(j,js+1) - sflxptr(j,js))
            enddo

        end select

!  ---  call taumol## to calculate layer optical depth

        if (jb == 16) then

          call taumol16

        else if (jb == 17) then

          call taumol17

        else if (jb == 18) then

          call taumol18

        else if (jb == 19) then

          call taumol19

        else if (jb == 20) then

          call taumol20

        else if (jb == 21) then

          call taumol21

        else if (jb == 22) then

          call taumol22

        else if (jb == 23) then

          call taumol23

        else if (jb == 24) then

          call taumol24

        else if (jb == 25) then

!--- visible 16000-22650 cm-1   0.4415 - 0.6250 um

          call taumol25

        else if (jb == 26) then

!--- uv-a 22650-29000 cm-1   0.3448 - 0.4415 um

          call taumol26

        else if (jb == 27) then

!--- uv-b 29000-38000 cm-1   0.2632 - 0.3448 um

          call taumol27

        else if (jb == 28) then

!--- uv-c 38000-50000 cm-1   0.2000 - 0.2632 um

          call taumol28

        else if (jb == 29) then

          call taumol29

        endif

!  ---  compute radiation fluxes

        call swflux ( jb )

!  ---  accumulation of spectral fluxes over whole spectrum

        ifb = jb - NBLOW + 1
        do k = 1, NLP1
          flxuc(k,ifb) = fxup(k,2)
          flxdc(k,ifb) = fxdn(k,2)
          flxu0(k,ifb) = fxup(k,1)
          flxd0(k,ifb) = fxdn(k,1)
        enddo

        if ( lfdncmp ) then
!! ---  optional uv-b surface downward flux
          if (jb == nuvb) then
            suvbf0 = suvbf0 + fxdn(1,1)
            suvbfc = suvbfc + fxdn(1,2)
          endif

!! ---  optional surface downward flux components
          ifb = IDXSFC(jb)
          if (ifb .eq. 0) then
            sfbmc(1) = sfbmc(1) + 0.5*sflxbc
            sfdfc(1) = sfdfc(1) + 0.5*sflxdc
            sfbm0(1) = sfbm0(1) + 0.5*sflxb0
            sfdf0(1) = sfdf0(1) + 0.5*sflxd0
            sfbmc(2) = sfbmc(2) + 0.5*sflxbc
            sfdfc(2) = sfdfc(2) + 0.5*sflxdc
            sfbm0(2) = sfbm0(2) + 0.5*sflxb0
            sfdf0(2) = sfdf0(2) + 0.5*sflxd0
          else
            sfbmc(ifb) = sfbmc(ifb) + sflxbc
            sfdfc(ifb) = sfdfc(ifb) + sflxdc
            sfbm0(ifb) = sfbm0(ifb) + sflxb0
            sfdf0(ifb) = sfdf0(ifb) + sflxd0
          endif
        endif    ! end if_lfdncmp

      enddo  lab_do_jb


! =================
      contains
! =================

!-----------------------------------
      subroutine swflux ( ib )
!...................................

!  *******************************************************************  !
!                                                                       !
!   purpose:  computes the upward and downward radiation fluxes         !
!             this program combines the original "reftra" and "vrtqdr"  !
!                                                                       !
!             first (reftra) it computes the reflectivity and           !
!             transmissivity of a clear or cloudy layer using a choice  !
!             of various approximations.                                !
!                                                                       !
!             then (vrtqdr) performs the vertical quadrature            !
!             integration to obtain level fluxes.                       !
!                                                                       !
!   interface:  "swflux" is called by "spcvrt"                          !
!                                                                       !
!  *******************************************************************  !
!                                                                       !
!  input variables:                                                     !
!    ib               - spectral band index                             !
!  input variables (direct from "spcvrt"):                              !
!    taug (NLAY,NGMAX)- spectral optical depth for gases                !
!    taur (NLAY,NGMAX)- spectral optical depth for rayleigh scattering  !
!    sfluxzen  (NGMAX)- spectral distribution of incoming solar flux    !
!    taucw(NLAY,NBLOW:NBHGH) - weighted cloud optical depth             !
!    ssacw(NLAY,NBLOW:NBHGH) - weighted cloud single scattering albedo  !
!    asycw(NLAY,NBLOW:NBHGH) - weighted cloud asymmetry factor          !
!    tauae(NLAY,NBLOW:NBHGH) - aerosols optical depth                   !
!    ssaae(NLAY,NBLOW:NBHGH) - aerosols single scattering albedo        !
!    asyae(NLAY,NBLOW:NBHGH) - aerosols asymmetry factor                !
!    cf1              - >0: cloudy sky, otherwise: clear sky            !
!    pclfr(NLAY)      - layer cloud fraction                            !
!    cosz             - cosine solar zenith angle                       !
!    sntz             - secant solar zenith angle                       !
!    albbm(2)         - surface albedo for direct beam radiation        !
!    albdf(2)         - surface albedo for diffused radiation           !
!    NLAY, NLP1    - number of layers/levels                            !
!                                                                       !
!  control parameters in module "module_radsw_cntr_para":               !
!    imodsw        - control flag for 2-stream transfer schemes         !
!                    = 1 delta-eddington    (joseph et al., 1976)       !
!                    = 2 pifm               (zdunkowski et al., 1980)   !
!                    = 3 discrete ordinates (liou, 1973)                !
!                                                                       !
!  output variables (direct to "spcvrt"):                               !
!    fxdn (NLP1,2)   - downward flux, 1: clear sky, 2: cloudy sky       !
!    fzup (NLP1,2)   - upward flux,   1: clear sky, 2: cloudy sky       !
!                                                                       !
!! optional output variables (direct to "spcvrt"):                      !
!    sflxbc        - cloudy sky sfc downward beam flux                  !
!    sflxdc        - cloudy sky sfc downward diff flux                  !
!    sflxb0        - clear sky sfc downward beam flux                   !
!    sflxd0        - clear sky sfc downward diff flux                   !
!                                                                       !
!  internal variables:                                                  !
!    zrefb(NLP1,2) - direct beam reflectivity for clear and cloudy      !
!    zrefd(NLP1,2) - diffuse reflectivity for clear and cloudy          !
!    ztrab(NLP1,2) - direct beam transmissivity for clear and cloudy    !
!    ztrad(NLP1,2) - diffuse transmissivity for clear and cloudy        !
!    zldbt(NLP1,2) - layer mean beam transmittance for clear and cloudy !
!    ztdbt(NLP1,2) - total beam transmittance at levels for clr and cld !
!    jg               - g-point index                                   !
!                                                                       !
!  external subroutines called:  none                                   !
!                                                                       !
!  program history:                                                     !
!    2003-02-27  jean-jacques morcrette, ecmwf   original author        !
!    2004-01-20  yu-tai hou    modified for ncep models                 !
!    2005-03-10  yu-tai hou    modified delta scaling                   !
!                                                                       !
!  *******************************************************************  !
!
      implicit none

      real (kind=kind_phys), parameter :: zcrit = 0.9995   ! thresold for conservative scattering
      real (kind=kind_phys), parameter :: zsr3  = sqrt(3.0)

!  ---  inputs:
      integer, intent(in) :: ib

!  ---  locals:
      real (kind=kind_phys), dimension(NLAY,2) :: ztau, zssa, zasy,     &
     &       zssa0, zexpt

      real (kind=kind_phys), dimension(NLP1,2) :: zrefb, zrefd, ztrab,  &
     &       ztrad, zldbt, ztdbt

      real (kind=kind_phys), dimension(NLAY) :: ztaus, zssas, zasys

      real (kind=kind_phys), dimension(NLP1) :: zrupb, zrupd, zrdnd,    &
     &       ztdn, zfd, zfu

      real (kind=kind_phys) :: ztau1, zssa1, zasy1, zasy3, zwo,         &
     &       zgam1, zgam2, zgam3, zgam4, zc0, zc1, za1, za2, zrk, zrk2, &
     &       zrp, zrp1, zrm1, zrpp, zrkg1, zrkg3, zrkg4, zexp1, zexm1,  &
     &       zexp2, zexm2, zden1, ze1r45

      real (kind=kind_phys) :: zr1, zr2, zr3, zr4, zr5, zt1, zt2, zt3

!! ---  for optional surface fluxes
      real (kind=kind_phys), dimension(2) :: sfxbm, sfxdf

      integer :: k, kp, jg, ngt, ipa, iab
!
!===> ... begin here
!
      ngt = NG(ib)             ! number of g-point in each band
      iab = IDXALB(ib)         ! surface albedo spectral index

      do k = 1, NLP1
        fxdn(k,1) = zero
        fxdn(k,2) = zero
        fxup(k,1) = zero
        fxup(k,2) = zero
      enddo

!! ---  optional surface fluxes
      if ( lfdncmp ) then
        sfxbm(1) = zero
        sfxbm(2) = zero
        sfxdf(1) = zero
        sfxdf(2) = zero
      endif

!  ---  loop over all g-points in each band

      lab_do_jg : do jg = 1, ngt

!  ---  compute clear-sky optical parameters

        do k = 1, NLAY
          ztaus(k) = max(ftiny, taur(k,jg) + taug(k,jg) + tauae(k,ib))
          zssas(k) = taur(k,jg) + tauae(k,ib)*ssaae(k,ib)
          zasys(k) = asyae(k,ib)*ssaae(k,ib)*tauae(k,ib)
          zssa1 = min(oneminus, zssas(k) / ztaus(k))
          zasy1 = zasys(k) / max(ftiny, zssas(k))
          zssa0(k,1) = zssa1

!  ---  delta scaling
          za1 = zasy1 * zasy1
          za2 = zssa1 * za1

          ztau (k,1) = (one - za2) * ztaus(k)
          zssa (k,1) = (zssa1 - za2) / (one - za2)
          zasy (k,1) = (zasy1 - za1) / (one - za1)
          zexpt(k,1) = exp ( - min( ztau(k,1)*sntz, 200.0) )
        enddo

!  ---  compute total sky optical parameters

        if (cf1 > eps) then

          do k = 1, NLAY
            if (pclfr(k) > eps) then
              ztau1 = ztaus(k) + taucw(k,ib)
              zc0   = zssas(k) + ssacw(k,ib)
              zc1   = zasys(k) + asycw(k,ib)
              zssa1 = min(oneminus, zc0 / ztau1)
              zasy1 = zc1 / max(ftiny, zc0)
              zssa0(k,2) = zssa1

!  ---  delta scaling
              za1 = zasy1 * zasy1
              za2 = zssa1 * za1

              ztau (k,2) = (one - za2) * ztau1
              zssa (k,2) = (zssa1 - za2) / (one - za2)
              zasy (k,2) = (zasy1 - za1) / (one - za1)
              zexpt(k,2) = exp ( - min( ztau(k,2)*sntz, 200.0) )
            else
              ztau (k,2) = ztau (k,1)
              zssa (k,2) = zssa (k,1)
              zasy (k,2) = zasy (k,1)
              zssa0(k,2) = zssa0(k,1)
              zexpt(k,2) = zexpt(k,1)
            endif
          enddo

        else

          do k = 1, NLAY
            ztau (k,2) = ztau (k,1)
            zssa (k,2) = zssa (k,1)
            zasy (k,2) = zasy (k,1)
            zssa0(k,2) = zssa0(k,1)
            zexpt(k,2) = zexpt(k,1)
          enddo

        endif  ! end_if_cf1

!  ---  compute layer reflectance and transmittance

        lab_do_ipa1 : do ipa = 1, 2             ! 1: clear-sky,  2, cloudy-sky

          do k = 1, NLAY
            kp = k + 1

            lab_if_pclfr : if (ipa==1 .or. pclfr(k)>eps) then   ! cloudy-layer
!  ---  save original ssa to test for conservative solution
              zwo   = zssa0(k,ipa)

              ztau1 = ztau(k,ipa)
              zssa1 = zssa(k,ipa)
              zasy1 = zasy(k,ipa)  
              zasy3 = 3.0 * zasy1

!  ---  general two-stream expressions
              if (imodsw == 1) then                      ! delta-eddington
                zgam1 = (7.0 - zssa1  * (4.0 + zasy3)) * 0.25
                zgam2 =-(1.0 - zssa1  * (4.0 - zasy3)) * 0.25
                zgam3 = (2.0 - zasy3 * cosz) * 0.25
              else if (imodsw == 2) then                 ! pifm
                zgam1 = (8.0 - zssa1  * (5.0 + zasy3)) * 0.25
                zgam2 =  3.0 * (zssa1 * (1.0 - zasy1)) * 0.25
                zgam3 = (2.0 - zasy3 * cosz) * 0.25
              else if (imodsw == 3) then                 ! discrete ordinates
                zgam1 = zsr3 * (2.0 - zssa1 * (1.0 + zasy1)) * 0.5
                zgam2 = zsr3 * zssa1 * (1.0 - zasy1) * 0.5
                zgam3 = (1.0 - zsr3 * zasy1 * cosz) * 0.5
              endif
              zgam4 = one - zgam3

!  ---  for conservative scattering

              lab_if_zwo : if (zwo >= zcrit) then
                za1 = zgam1 * cosz - zgam3
                za2 = zgam1 * ztau1

!  ---  compute homogeneous reflectance and transmittance
                zexm1 = zexpt(k,ipa)
  
!       ...  collimated beam
                zrefb(kp,ipa) = max(zero,                               &
     &                          (za2 - za1*(one - zexm1))/(one + za2))
                ztrab(kp,ipa) = max(zero, one - zrefb(kp,ipa))

!       ...  isotropic incidence
                zrefd(kp,ipa) = max(zero, za2 / (one + za2))
                ztrad(kp,ipa) = max(zero, one - zrefd(kp,ipa))

!  ---  for non-conservative scattering
              else  lab_if_zwo

                za1 = zgam1*zgam4 + zgam2*zgam3
                za2 = zgam1*zgam3 + zgam2*zgam4
!               zrk = sqrt (zgam1**2 - zgam2**2)
                zrk = sqrt ( (zgam1 - zgam2) * (zgam1 + zgam2) )
                zrk2= 2.0 * zrk

                zrp  = zrk * cosz
                zrp1 = one + zrp
                zrm1 = one - zrp
                zrpp = one - zrp*zrp
                zrkg1= zrk + zgam1
                zrkg3= zrk * zgam3
                zrkg4= zrk * zgam4

                zr1  = zrm1 * (za2 + zrkg3)
                zr2  = zrp1 * (za2 - zrkg3)
                zr3  = zrk2 * (zgam3 - za2*cosz)
                zr4  = zrpp * zrkg1
                zr5  = zrpp * (zrk - zgam1)

                zt1  = zrp1 * (za1 + zrkg4)
                zt2  = zrm1 * (za1 - zrkg4)
                zt3  = zrk2 * (zgam4 + za1*cosz)

!  ---  compute homogeneous reflectance and transmittance
                zexp1 = exp( min(200.0, zrk*ztau1) )
                zexm1 = one / zexp1
                zexm2 = zexpt(k,ipa)
                zexp2 = one / zexm2
                ze1r45 = zr4*zexp1 + zr5*zexm1

!       ...  collimated beam
                zden1 = zssa1 / ze1r45
                zrefb(kp,ipa) = max(zero,                               &
     &                          (zr1*zexp1-zr2*zexm1-zr3*zexm2)*zden1 )
                ztrab(kp,ipa) = max(zero, zexm2*(one                    &
     &                        - (zt1*zexp1-zt2*zexm1-zt3*zexp2)*zden1))

!       ...  diffuse beam
                zden1 = zr4 / (ze1r45 * zrkg1)
                zrefd(kp,ipa) =  max(zero, zgam2*(zexp1-zexm1)*zden1)
                ztrad(kp,ipa) =  max(zero, zrk2*zden1)
              endif  lab_if_zwo

            else   lab_if_pclfr                                 ! clear-layer

              zrefb(kp,2) = zrefb(kp,1)
              ztrab(kp,2) = ztrab(kp,1)
              zrefd(kp,2) = zrefd(kp,1)
              ztrad(kp,2) = ztrad(kp,1)

            endif  lab_if_pclfr
          enddo    ! end do_k_loop

          if (ipa==1 .and. cf1<=eps) then
            do k = 2, NLP1
              zrefb(k,2) = zrefb(k,1)
              ztrab(k,2) = ztrab(k,1)
              zrefd(k,2) = zrefd(k,1)
              ztrad(k,2) = ztrad(k,1)
            enddo

            exit lab_do_ipa1
          endif

        enddo  lab_do_ipa1                    ! end do_ipa_loop

!  --- set up toa direct beam for clear-sky and cloudy-sky
        ztdbt(NLP1,1) = one
        ztdbt(NLP1,2) = one

!  ---  combine clear and cloudy contributions for total sky

        do k = NLAY, 1, -1
          kp = k + 1

          zc0 = one - pclfr(k)
          zc1 = pclfr(k)

          zrefb(kp,2) = zc0*zrefb(kp,1) + zc1*zrefb(kp,2)
          zrefd(kp,2) = zc0*zrefd(kp,1) + zc1*zrefd(kp,2)
          ztrab(kp,2) = zc0*ztrab(kp,1) + zc1*ztrab(kp,2)
          ztrad(kp,2) = zc0*ztrad(kp,1) + zc1*ztrad(kp,2)

!  ---  direct beam transmittance
          zldbt(kp,1) = max(zero, zexpt(k,1))
          zldbt(kp,2) = zc0*zexpt(k,1) + zc1*zexpt(k,2)

          ztdbt(k,1) = max(zero, zldbt(kp,1) * ztdbt(kp,1))
          ztdbt(k,2) = max(zero, zldbt(kp,2) * ztdbt(kp,2))
        enddo

!  ---  set up surface values (beam and diffused) for clear-sky and cloudy-sky
        zldbt(1,1) = zero
        zldbt(1,2) = zero

        zrefb(1,1) = albbm(iab)
        zrefb(1,2) = albbm(iab)
        zrefd(1,1) = albdf(iab)
        zrefd(1,2) = albdf(iab)

        ztrab(1,1) = zero
        ztrab(1,2) = zero
        ztrad(1,1) = zero
        ztrad(1,2) = zero

!  ---  perform vertical quadrature

        lab_do_ipa2 : do ipa = 1, 2            ! 1=clear-sky, 2=cloudy-sky

!  ---  link lowest layer with surface
          zrupb(1) = zrefb(1,ipa)        ! direct beam
          zrupd(1) = zrefd(1,ipa)        ! diffused

!  ---  pass from bottom to top
          do k = 1, NLAY
            kp = k + 1

            zden1 = one / ( one - zrupd(k)*zrefd(kp,ipa) )
            zrupb(kp) = zrefb(kp,ipa) + ( ztrad(kp,ipa)                 &
     &                * ( (ztrab(kp,ipa) - zldbt(kp,ipa) )*zrupd(k)     &
     &                + zldbt(kp,ipa)*zrupb(k) ) )*zden1
            zrupd(kp) = zrefd(kp,ipa) + ztrad(kp,ipa)                   &
     &                * ztrad(kp,ipa)*zrupd(k)*zden1
          enddo

!  ---  upper boundary conditions
          ztdn (NLP1) = one
          zrdnd(NLP1) = zero
          ztdn (NLAY) = ztrab(NLP1,ipa)
          zrdnd(NLAY) = zrefd(NLP1,ipa)

!  ---  pass from top to bottom
          do k = NLAY, 2, -1
            zden1 = one / (one - zrefd(k,ipa)*zrdnd(k))
            ztdn (k-1) = ztdbt(k,ipa)*ztrab(k,ipa)                      &
     &                 + ( ztrad(k,ipa)*( ( ztdn(k) - ztdbt(k,ipa) )    &
     &                 + ztdbt(k,ipa)*zrefb(k,ipa)*zrdnd(k) ) )*zden1
            zrdnd(k-1) = zrefd(k,ipa) + ztrad(k,ipa)*ztrad(k,ipa)       &
     &                 * zrdnd(k)*zden1
          enddo

!  ---  up and down-welling fluxes at levels
          do k = 1, NLP1
            zden1 = one / (one - zrdnd(k)*zrupd(k))
            zfu(k) = ( ztdbt(k,ipa)*zrupb(k)                            &
     &             + ( ztdn(k) - ztdbt(k,ipa) )*zrupd(k) )*zden1
            zfd(k) = ztdbt(k,ipa) + (ztdn(k) - ztdbt(k,ipa)             &
     &             + ztdbt(k,ipa)*zrupb(k)*zrdnd(k))*zden1
          enddo

!  ---  compute upward and downward fluxes at levels
          do k = 1, NLP1
            fxup(k,ipa) = fxup(k,ipa) + sfluxzen(jg)*zfu(k)
            fxdn(k,ipa) = fxdn(k,ipa) + sfluxzen(jg)*zfd(k)
          enddo

!! ---  optional surface downward flux components
          if ( lfdncmp ) then
            sfxbm(ipa) = sfxbm(ipa)+sfluxzen(jg)*ztdbt(1,ipa)
            sfxdf(ipa) = sfxdf(ipa)+sfluxzen(jg)*(zfd(1)-ztdbt(1,ipa))
          endif

          if (ipa==1 .and. cf1<=eps) then
            exit lab_do_ipa2
          endif
        enddo  lab_do_ipa2       ! end do_ipa_loop

      enddo  lab_do_jg

      if (cf1 <= eps) then
        do k = 1, NLP1
          fxup(k,2) = fxup(k,1)
          fxdn(k,2) = fxdn(k,1)
        enddo
      endif

      if ( lfdncmp ) then
!! ---  optional surface downward flux components
        if (cf1 <= eps) then
          sfxbm(2) = sfxbm(1)
          sfxdf(2) = sfxdf(1)
        endif

!! ---  optional surface downward flux components
        sflxb0 = sfxbm(1)
        sflxd0 = sfxdf(1)
        sflxbc = sfxbm(2)
        sflxdc = sfxdf(2)
      endif

      return
!...................................
      end subroutine swflux     
!-----------------------------------



!-----------------------------------
      subroutine taumol16
!...................................

!  ------------------------------------------------------------------  !
!     written by eli j. mlawer, atmospheric & environmental research.  !
!                                                                      !
!     band 16:  2600-3250 cm-1 (low - h2o,ch4; high - ch4)             !
!                                                                      !
! modifications                                                        !
!                                                                      !
!     jjmorcrette 2003-02-24 adapted to ecmwf environment              !
!                                                                      !
!  ------------------------------------------------------------------  !
!
      use module_radsw_kgb16
!
      implicit none

!  ---  locals:
      real (kind=kind_phys) :: speccomb, specmult, tauray, fs, fs1,     &
     &       fac000,fac001,fac010,fac011, fac100,fac101,fac110,fac111

      integer :: ind01, ind02, ind03, ind04, ind11, ind12, ind13, ind14
      integer :: inds, indf, j, js, k

!
!===> ... begin here
!

!  ---  compute the optical depth by interpolating in ln(pressure), 
!       temperature, and appropriate species.  below laytrop, the water
!       vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, NLAY
        tauray = colmol(k) * rayl

        do j = 1 , NG16
          taur(k,j) = tauray
        enddo
      enddo

      do k = 1, laytrop
        speccomb = colamt(k,1) + strrat(16)*colamt(k,5)
        specmult = 8.0 * min( oneminus, colamt(k,1)/speccomb )

        js = 1 + int( specmult )
        fs = mod( specmult, one )
        fs1= one - fs
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
        ind11 = id1(k,16) + js
        ind12 = ind11 + 1
        ind13 = ind11 + 9
        ind14 = ind11 + 10
        inds = indself(k)
        indf = indfor (k)

        do j = 1, NG16
          taug(k,j) = speccomb                                          &
     &        *( fac000 * absa(ind01,j) + fac100 * absa(ind02,j)        &
     &        +  fac010 * absa(ind03,j) + fac110 * absa(ind04,j)        &
     &        +  fac001 * absa(ind11,j) + fac101 * absa(ind12,j)        &
     &        +  fac011 * absa(ind13,j) + fac111 * absa(ind14,j) )      &
     &        + colamt(k,1) * (selffac(k) * (selfref(inds,j)            &
     &        + selffrac(k) * (selfref(inds+1,j)-selfref(inds,j)))      &
     &        + forfac(k) * (forref(indf,j) + forfrac(k)                &
     &        * (forref(indf+1,j) - forref(indf,j))))
        enddo
      enddo

      do k = laytrop+1, NLAY
        ind01 = id0(k,16) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,16) + 1
        ind12 = ind11 + 1

        do j = 1, NG16
          taug(k,j) = colamt(k,5)                                       &
     &      * ( fac00(k)*absb(ind01,j) + fac10(k)*absb(ind02,j)         &
     &      +   fac01(k)*absb(ind11,j) + fac11(k)*absb(ind12,j) )
        enddo
      enddo

      return
!...................................
      end subroutine taumol16
!-----------------------------------



!-----------------------------------
      subroutine taumol17
!...................................

!  ------------------------------------------------------------------  !
!     written by eli j. mlawer, atmospheric & environmental research.  !
!                                                                      !
!     band 17:  3250-4000 cm-1 (low - h2o,co2; high - h2o,co2)         !
!                                                                      !
! modifications                                                        !
!                                                                      !
!     jjmorcrette 2003-02-24 adapted to ecmwf environment              !
!  ------------------------------------------------------------------  !
!
      use module_radsw_kgb17
!
      implicit none

!  ---  locals:
      real (kind=kind_phys) :: speccomb, specmult, tauray, fs, fs1,     &
     &       fac000,fac001,fac010,fac011, fac100,fac101,fac110,fac111

      integer :: ind01, ind02, ind03, ind04, ind11, ind12, ind13, ind14
      integer :: inds, indf, j, js, k, ks

!
!===> ... begin here
!

!  ---  compute the optical depth by interpolating in ln(pressure), 
!       temperature, and appropriate species.  below laytrop, the water
!       vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, NLAY
        tauray = colmol(k) * rayl

        do j = 1 , NG17
          taur(k,j) = tauray
        enddo
      enddo

      do k = 1, laytrop
        speccomb = colamt(k,1) + strrat(17)*colamt(k,2)
        specmult = 8.0 * min(oneminus, colamt(k,1) / speccomb)

        js = 1 +int(specmult)
        fs = mod(specmult, one)
        fs1= one - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,17) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 9
        ind04 = ind01 + 10
        ind11 = id1(k,17) + js
        ind12 = ind11 + 1
        ind13 = ind11 + 9
        ind14 = ind11 + 10
        inds = indself(k)
        indf = indfor (k)

        do j = 1, NG17
          taug(k,j) = speccomb                                          &
     &        * ( fac000 * absa(ind01,j) + fac100 * absa(ind02,j)       &
     &        +   fac010 * absa(ind03,j) + fac110 * absa(ind04,j)       &
     &        +   fac001 * absa(ind11,j) + fac101 * absa(ind12,j)       &
     &        +   fac011 * absa(ind13,j) + fac111 * absa(ind14,j) )     &
     &        + colamt(k,1) * (selffac(k) * (selfref(inds,j)            &
     &        + selffrac(k) * (selfref(inds+1,j)-selfref(inds,j)))      &
     &        + forfac(k) * (forref(indf,j) + forfrac(k)                &
     &        * (forref(indf+1,j) - forref(indf,j)))) 
        enddo
      enddo

      do k = laytrop+1, NLAY
        speccomb = colamt(k,1) + strrat(17)*colamt(k,2)
        specmult = 4.0 * min(oneminus, colamt(k,1) / speccomb)

        js = 1 + int(specmult)
        fs = mod(specmult, one)
        fs1= one - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,17) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 5
        ind04 = ind01 + 6
        ind11 = id1(k,17) + js
        ind12 = ind11 + 1
        ind13 = ind11 + 5
        ind14 = ind11 + 6
        indf = indfor(k)

        do j = 1, NG17
          taug(k,j) = speccomb                                          &
     &        * ( fac000 * absb(ind01,j) + fac100 * absb(ind02,j)       &
     &        +   fac010 * absb(ind03,j) + fac110 * absb(ind04,j)       &
     &        +   fac001 * absb(ind11,j) + fac101 * absb(ind12,j)       &
     &        +   fac011 * absb(ind13,j) + fac111 * absb(ind14,j) )     &
     &        + colamt(k,1) * forfac(k) * (forref(indf,j)               &
     &        + forfrac(k) * (forref(indf+1,j) - forref(indf,j))) 
        enddo
      enddo

      return
!...................................
      end subroutine taumol17
!-----------------------------------


!-----------------------------------
      subroutine taumol18
!...................................

!  ------------------------------------------------------------------  !
!     written by eli j. mlawer, atmospheric & environmental research.  !
!                                                                      !
!     band 18:  4000-4650 cm-1 (low - h2o,ch4; high - ch4)             !
!  ------------------------------------------------------------------  !
!
      use module_radsw_kgb18
!
      implicit none

!  ---  locals:
      real (kind=kind_phys) :: speccomb, specmult, tauray, fs, fs1,     &
     &       fac000,fac001,fac010,fac011, fac100,fac101,fac110,fac111

      integer :: ind01, ind02, ind03, ind04, ind11, ind12, ind13, ind14
      integer :: inds, indf, j, js, k, ks

!
!===> ... begin here
!

!  ---  compute the optical depth by interpolating in ln(pressure), 
!       temperature, and appropriate species.  below laytrop, the water
!       vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, NLAY
        tauray = colmol(k) * rayl

        do j = 1 , NG18
          taur(k,j) = tauray
        enddo
      enddo

      do k = 1, laytrop
        speccomb = colamt(k,1) + strrat(18)*colamt(k,5)
        specmult = 8.0 * min(oneminus, colamt(k,1) / speccomb)

        js = 1 +int(specmult)
        fs = mod(specmult, one)
        fs1= one - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,18) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 9
        ind04 = ind01 + 10
        ind11 = id1(k,18) + js
        ind12 = ind11 + 1
        ind13 = ind11 + 9
        ind14 = ind11 + 10
        inds = indself(k)
        indf = indfor (k)

        do j = 1, NG18
          taug(k,j) = speccomb                                          &
     &        * ( fac000 * absa(ind01,j) + fac100 * absa(ind02,j)       &
     &        +   fac010 * absa(ind03,j) + fac110 * absa(ind04,j)       &
     &        +   fac001 * absa(ind11,j) + fac101 * absa(ind12,j)       &
     &        +   fac011 * absa(ind13,j) + fac111 * absa(ind14,j) )     &
     &        + colamt(k,1) * (selffac(k) * (selfref(inds,j)            &
     &        + selffrac(k) * (selfref(inds+1,j)-selfref(inds,j)))      &
     &        + forfac(k) * (forref(indf,j) + forfrac(k)                &
     &        * (forref(indf+1,j) - forref(indf,j)))) 
        enddo
      enddo

      do k = laytrop+1, NLAY
        ind01 = id0(k,18) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,18) + 1
        ind12 = ind11 + 1

        do j = 1, NG18
          taug(k,j) = colamt(k,5)                                       &
     &        * ( fac00(k)*absb(ind01,j) + fac10(k)*absb(ind02,j)       &
     &        +   fac01(k)*absb(ind11,j) + fac11(k)*absb(ind12,j) ) 
        enddo
      enddo

      return
!...................................
      end subroutine taumol18
!-----------------------------------


!-----------------------------------
      subroutine taumol19
!...................................

!  ------------------------------------------------------------------  !
!     written by eli j. mlawer, atmospheric & environmental research.  !
!                                                                      !
!     band 19:  4650-5150 cm-1 (low - h2o,co2; high - co2)             !
!                                                                      !
! modifications                                                        !
!                                                                      !
!     jjmorcrette 2003-02-24 adapted to ecmwf environment              !
!  ------------------------------------------------------------------  !
!
      use module_radsw_kgb19
!
      implicit none

!  ---  locals:
      real (kind=kind_phys) :: speccomb, specmult, tauray, fs, fs1,     &
     &       fac000,fac001,fac010,fac011, fac100,fac101,fac110,fac111

      integer :: ind01, ind02, ind03, ind04, ind11, ind12, ind13, ind14
      integer :: inds, indf, j, js, k, ks

!
!===> ... begin here
!

!  ---  compute the optical depth by interpolating in ln(pressure), 
!       temperature, and appropriate species.  below laytrop, the water
!       vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, NLAY
        tauray = colmol(k) * rayl

        do j = 1 , NG19
          taur(k,j) = tauray
        enddo
      enddo

      do k = 1, laytrop
        speccomb = colamt(k,1) + strrat(19)*colamt(k,2)
        specmult = 8.0 * min(oneminus, colamt(k,1) / speccomb)

        js = 1 +int(specmult)
        fs = mod(specmult, one)
        fs1= one - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,19) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 9
        ind04 = ind01 + 10
        ind11 = id1(k,19) + js
        ind12 = ind11 + 1
        ind13 = ind11 + 9
        ind14 = ind11 + 10
        inds = indself(k)
        indf = indfor (k)

        do j = 1, NG19
          taug(k,j) = speccomb                                          &
     &        * ( fac000 * absa(ind01,j) + fac100 * absa(ind02,j)       &
     &        +   fac010 * absa(ind03,j) + fac110 * absa(ind04,j)       &
     &        +   fac001 * absa(ind11,j) + fac101 * absa(ind12,j)       &
     &        +   fac011 * absa(ind13,j) + fac111 * absa(ind14,j) )     &
     &        + colamt(k,1) * (selffac(k) * (selfref(inds,j)            &
     &        + selffrac(k) * (selfref(inds+1,j)-selfref(inds,j)))      & 
     &        + forfac(k) * (forref(indf,j) + forfrac(k)                &
     &        * (forref(indf+1,j) - forref(indf,j)))) 
        enddo
      enddo

      do k = laytrop+1, NLAY
        ind01 = id0(k,19) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,19) + 1
        ind12 = ind11 + 1

        do j = 1, NG19
          taug(k,j) = colamt(k,2)                                       &
     &        * ( fac00(k)*absb(ind01,j) + fac10(k)*absb(ind02,j)       &
     &        +   fac01(k)*absb(ind11,j) + fac11(k)*absb(ind12,j) ) 
        enddo
      enddo

      return
!...................................
      end subroutine taumol19
!-----------------------------------


!-----------------------------------
      subroutine taumol20
!...................................

!  ------------------------------------------------------------------  !
!     written by eli j. mlawer, atmospheric & environmental research.  !
!                                                                      !
!     band 20:  5150-6150 cm-1 (low - h2o; high - h2o)                 !
!                                                                      !
! modifications                                                        !
!                                                                      !
!     jjmorcrette 2003-02-24 adapted to ecmwf environment              !
!  ------------------------------------------------------------------  !
!
      use module_radsw_kgb20
!
      implicit none

!  ---  locals:
      real (kind=kind_phys) :: tauray

      integer :: ind01, ind02, ind11, ind12
      integer :: inds, indf, j, k

!
!===> ... begin here
!

!  ---  compute the optical depth by interpolating in ln(pressure), 
!       temperature, and appropriate species.  below laytrop, the water
!       vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, NLAY
        tauray = colmol(k) * rayl

        do j = 1 , NG20
          taur(k,j) = tauray
        enddo
      enddo

      do k = 1, laytrop
        ind01 = id0(k,20) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,20) + 1
        ind12 = ind11 + 1
        inds = indself(k)
        indf = indfor (k)

        do j = 1, NG20
          taug(k,j) = colamt(k,1)                                       &
     &        * ( (fac00(k)*absa(ind01,j) + fac10(k)*absa(ind02,j)      &
     &        +    fac01(k)*absa(ind11,j) + fac11(k)*absa(ind12,j))     &
     &        +   selffac(k) * (selfref(inds,j) + selffrac(k)           &
     &        *   (selfref(inds+1,j) - selfref(inds,j)))                &
     &        +   forfac(k) * (forref(indf,j) + forfrac(k)              &
     &        *   (forref(indf+1,j) - forref(indf,j))) )                &
     &        + colamt(k,5) * absch4(j)
        enddo
      enddo

      do k = laytrop+1, NLAY
        ind01 = id0(k,20) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,20) + 1
        ind12 = ind11 + 1
        indf = indfor(k)

        do j = 1, NG20
          taug(k,j) = colamt(k,1)                                       &
     &        * ( fac00(k)*absb(ind01,j) + fac10(k)*absb(ind02,j)       &
     &        +   fac01(k)*absb(ind11,j) + fac11(k)*absb(ind12,j)       &
     &        +   forfac(k) * (forref(indf,j) + forfrac(k)              &
     &        *   (forref(indf+1,j) - forref(indf,j))) )                &
     &        + colamt(k,5) * absch4(j)
        enddo
      enddo

      return
!...................................
      end subroutine taumol20
!-----------------------------------


!-----------------------------------
      subroutine taumol21
!...................................

!  ------------------------------------------------------------------  !
!     written by eli j. mlawer, atmospheric & environmental research.  !
!                                                                      !
!     band 21:  6150-7700 cm-1 (low - h2o,co2; high - h2o,co2)         !
!                                                                      !
! modifications                                                        !
!                                                                      !
!     jjmorcrette 2003-02-24 adapted to ecmwf environment              !
!  ------------------------------------------------------------------  !
!
      use module_radsw_kgb21
!
      implicit none

!  ---  locals:
      real (kind=kind_phys) :: speccomb, specmult, tauray, fs, fs1,     &
     &       fac000,fac001,fac010,fac011, fac100,fac101,fac110,fac111

      integer :: ind01, ind02, ind03, ind04, ind11, ind12, ind13, ind14
      integer :: inds, indf, j, js, k, ks

!
!===> ... begin here
!

!  ---  compute the optical depth by interpolating in ln(pressure), 
!       temperature, and appropriate species.  below laytrop, the water
!       vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, NLAY
        tauray = colmol(k) * rayl

        do j = 1 , NG21
          taur(k,j) = tauray
        enddo
      enddo

      do k = 1, laytrop
        speccomb = colamt(k,1) + strrat(21)*colamt(k,2)
        specmult = 8.0 * min(oneminus, colamt(k,1) / speccomb)

        js = 1 + int(specmult)
        fs = mod(specmult, one)
        fs1= one - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,21) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 9
        ind04 = ind01 + 10
        ind11 = id1(k,21) + js
        ind12 = ind11 + 1
        ind13 = ind11 + 9
        ind14 = ind11 + 10
        inds = indself(k)
        indf = indfor (k)

        do j = 1 , NG21
          taug(k,j) = speccomb                                          &
     &        * ( fac000 * absa(ind01,j) + fac100 * absa(ind02,j)       &
     &        +   fac010 * absa(ind03,j) + fac110 * absa(ind04,j)       &
     &        +   fac001 * absa(ind11,j) + fac101 * absa(ind12,j)       &
     &        +   fac011 * absa(ind13,j) + fac111 * absa(ind14,j) )     &
     &        + colamt(k,1) * (selffac(k) * (selfref(inds,j)            &
     &        + selffrac(k) * (selfref(inds+1,j)-selfref(inds,j)))      &
     &        + forfac(k) * (forref(indf,j) + forfrac(k)                &
     &        * (forref(indf+1,j) - forref(indf,j))))
        enddo
      enddo

      do k = laytrop+1, NLAY
        speccomb = colamt(k,1) + strrat(21)*colamt(k,2)
        specmult = 4.0 * min(oneminus, colamt(k,1) / speccomb)

        js = 1 + int(specmult)
        fs = mod(specmult, one)
        fs1= one - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,21) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 5
        ind04 = ind01 + 6
        ind11 = id1(k,21) + js
        ind12 = ind11 + 1
        ind13 = ind11 + 5
        ind14 = ind11 + 6
        indf = indfor(k)

        do j = 1 , NG21
          taug(k,j) = speccomb                                          &
     &        * ( fac000 * absb(ind01,j) + fac100 * absb(ind02,j)       &
     &        +   fac010 * absb(ind03,j) + fac110 * absb(ind04,j)       &
     &        +   fac001 * absb(ind11,j) + fac101 * absb(ind12,j)       &
     &        +   fac011 * absb(ind13,j) + fac111 * absb(ind14,j) )     &
     &        + colamt(k,1) * forfac(k) * (forref(indf,j)               &
     &        + forfrac(k) * (forref(indf+1,j) - forref(indf,j)))
        enddo
      enddo

      return
!...................................
      end subroutine taumol21
!-----------------------------------


!-----------------------------------
      subroutine taumol22
!...................................

!  ------------------------------------------------------------------  !
!     written by eli j. mlawer, atmospheric & environmental research.  !
!                                                                      !
!     band 22:  7700-8050 cm-1 (low - h2o,o2; high - o2)               !
!                                                                      !
! modifications                                                        !
!                                                                      !
!     jjmorcrette 2003-02-24 adapted to ecmwf environment              !
!  ------------------------------------------------------------------  !
!
      use module_radsw_kgb22
!
      implicit none

!  ---  locals:
      real (kind=kind_phys) :: speccomb, specmult, tauray, fs, fs1,     &
     &       fac000,fac001,fac010,fac011, fac100,fac101,fac110,fac111,  &
     &       o2adj, o2cont, o2tem

      integer :: ind01, ind02, ind03, ind04, ind11, ind12, ind13, ind14
      integer :: inds, indf, j, js, k, ks

!
!===> ... begin here
!
!  ---  the following factoris the ratio of total o2 bandintensity (lines 
!       and mate continuum) to o2 bandintensity (line only). itis needed
!       to adjust the optical depths since the k'sinclude only lines.

      o2adj = 1.6
      o2tem = 4.35e-4 / (350.0*2.0)
      
!  ---  compute the optical depth by interpolating in ln(pressure), 
!       temperature, and appropriate species.  below laytrop, the water
!       vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, NLAY
        tauray = colmol(k) * rayl

        do j = 1 , NG22
          taur(k,j) = tauray
        enddo
      enddo

      do k = 1, laytrop
        o2cont   = o2tem * colamt(k,6)
        speccomb = colamt(k,1) + strrat(22)*colamt(k,6)
        specmult = 8.0 * min(oneminus, colamt(k,1) / speccomb)

        js = 1 + int(specmult)
        fs = mod(specmult, one)
        fs1= one - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,22) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 9
        ind04 = ind01 + 10
        ind11 = id1(k,22) + js
        ind12 = ind11 + 1
        ind13 = ind11 + 9
        ind14 = ind11 + 10
        inds = indself(k)
        indf = indfor (k)

        do j = 1 , NG22
          taug(k,j) = speccomb                                          &
     &        * ( fac000 * absa(ind01,j) + fac100 * absa(ind02,j)       &
     &        +   fac010 * absa(ind03,j) + fac110 * absa(ind04,j)       &
     &        +   fac001 * absa(ind11,j) + fac101 * absa(ind12,j)       &
     &        +   fac011 * absa(ind13,j) + fac111 * absa(ind14,j) )     &
     &        + colamt(k,1) * (selffac(k) * (selfref(inds,j)            &
     &        + selffrac(k) * (selfref(inds+1,j)-selfref(inds,j)))      &
     &        + forfac(k) * (forref(indf,j) + forfrac(k)                &
     &        * (forref(indf+1,j) - forref(indf,j)))) + o2cont
        enddo
      enddo

      do k = laytrop+1, NLAY
        o2cont = o2tem * colamt(k,6)

        ind01 = id0(k,22) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,22) + 1
        ind12 = ind11 + 1

        do j = 1 , NG22
          taug(k,j) = colamt(k,6) * o2adj                               &
     &        * ( fac00(k)*absb(ind01,j) + fac10(k)*absb(ind02,j)       &
     &        +   fac01(k)*absb(ind11,j) + fac11(k)*absb(ind12,j) )     &
     &        + o2cont
        enddo
      enddo

      return
!...................................
      end subroutine taumol22
!-----------------------------------


!-----------------------------------
      subroutine taumol23
!...................................

!  ------------------------------------------------------------------  !
!     written by eli j. mlawer, atmospheric & environmental research.  !
!                                                                      !
!     band 23:  8050-12850 cm-1 (low - h2o; high - nothing)            !
!                                                                      !
! modifications                                                        !
!                                                                      !
!     jjmorcrette 2003-02-24 adapted to ecmwf environment              !
!  ------------------------------------------------------------------  !
!
      use module_radsw_kgb23
!
      implicit none

!  ---  locals:
      integer :: ind01, ind02, ind11, ind12
      integer :: inds, indf, j, k

!
!===> ... begin here
!

!  ---  compute the optical depth by interpolating in ln(pressure), 
!       temperature, and appropriate species.  below laytrop, the water
!       vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, NLAY
        do j = 1 , NG23
          taur(k,j) = colmol(k) * rayl(j) 
        enddo
      enddo


      do k = 1, laytrop
        ind01 = id0(k,23) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,23) + 1
        ind12 = ind11 + 1
        inds = indself(k)
        indf = indfor (k)

        do j = 1 , NG23
          taug(k,j) = colamt(k,1) * (givfac                             &
     &        * ( fac00(k)*absa(ind01,j) + fac10(k)*absa(ind02,j)       &
     &        +   fac01(k)*absa(ind11,j) + fac11(k)*absa(ind12,j) )     &
     &        + selffac(k) * (selfref(inds,j) + selffrac(k)             &
     &        * (selfref(inds+1,j) - selfref(inds,j)))                  &
     &        + forfac(k) * (forref(indf,j) + forfrac(k)                &
     &        * (forref(indf+1,j) - forref(indf,j)))) 
        enddo
      enddo

      do k = laytrop+1, NLAY
        do j = 1 , NG23
          taug(k,j) = zero
        enddo
      enddo

      return
!...................................
      end subroutine taumol23
!-----------------------------------


!-----------------------------------
      subroutine taumol24
!...................................

!  ------------------------------------------------------------------  !
!     written by eli j. mlawer, atmospheric & environmental research.  !
!                                                                      !
!     band 24:  12850-16000 cm-1 (low - h2o,o2; high - o2)             !
!                                                                      !
! modifications                                                        !
!                                                                      !
!     jjmorcrette 2003-02-24 adapted to ecmwf environment              !
!  ------------------------------------------------------------------  !
!
      use module_radsw_kgb24
!
      implicit none

!  ---  locals:
      real (kind=kind_phys) :: speccomb, specmult, fs, fs1,             &
     &       fac000,fac001,fac010,fac011, fac100,fac101,fac110,fac111

      integer :: ind01, ind02, ind03, ind04, ind11, ind12, ind13, ind14
      integer :: inds, indf, j, js, k, ks

!
!===> ... begin here
!

!  ---  compute the optical depth by interpolating in ln(pressure), 
!       temperature, and appropriate species.  below laytrop, the water
!       vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, laytrop
        speccomb = colamt(k,1) + strrat(24)*colamt(k,6)
        specmult = 8.0 * min(oneminus, colamt(k,1) / speccomb)

        js = 1 + int(specmult)
        fs = mod(specmult, one)
        fs1= one - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,24) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 9
        ind04 = ind01 + 10
        ind11 = id1(k,24) + js
        ind12 = ind11 + 1
        ind13 = ind11 + 9
        ind14 = ind11 + 10
        inds = indself(k)
        indf = indfor (k)

        do j = 1, NG24
          taug(k,j) = speccomb                                          &
     &        * ( fac000 * absa(ind01,j) + fac100 * absa(ind02,j)       &
     &        +   fac010 * absa(ind03,j) + fac110 * absa(ind04,j)       &
     &        +   fac001 * absa(ind11,j) + fac101 * absa(ind12,j)       &
     &        +   fac011 * absa(ind13,j) + fac111 * absa(ind14,j) )     &
     &        + colamt(k,3) * abso3a(j) +  colamt(k,1)                  & 
     &        * (selffac(k) * (selfref(inds,j) + selffrac(k)            &
     &        * (selfref(inds+1,j) - selfref(inds,j)))                  &
     &        + forfac(k) * (forref(indf,j) + forfrac(k)                &
     &        * (forref(indf+1,j) - forref(indf,j))))

          taur(k,j) =  colmol(k)                                        &
     &           * (rayla(j,js) + fs*(rayla(j,js+1) - rayla(j,js)))
        enddo
      enddo

      do k = laytrop+1, NLAY
        ind01 = id0(k,24) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,24) + 1
        ind12 = ind11 + 1

        do j = 1, NG24
          taug(k,j) = colamt(k,6)                                       &
     &        * ( fac00(k)*absb(ind01,j) + fac10(k)*absb(ind02,j)       &
     &        +   fac01(k)*absb(ind11,j) + fac11(k)*absb(ind12,j) )     &
     &        + colamt(k,3) * abso3b(j)

          taur(k,j) = colmol(k) * raylb(j)
        enddo
      enddo

      return
!...................................
      end subroutine taumol24
!-----------------------------------


!-----------------------------------
      subroutine taumol25
!...................................

!  ------------------------------------------------------------------  !
!     written by eli j. mlawer, atmospheric & environmental research.  !
!                                                                      !
!     band 25:  16000-22650 cm-1 (low - h2o; high - nothing)           !
!                                                                      !
! modifications                                                        !
!                                                                      !
!     jjmorcrette 2003-02-24 adapted to ecmwf environment              !
!  ------------------------------------------------------------------  !
!
      use module_radsw_kgb25
!
      implicit none

!  ---  locals:
      integer :: ind01, ind02, ind11, ind12
      integer :: j, k

!
!===> ... begin here
!

!  ---  compute the optical depth by interpolating in ln(pressure), 
!       temperature, and appropriate species.  below laytrop, the water
!       vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, NLAY
        do j = 1, NG25
          taur(k,j) = colmol(k) * rayl(j)
        enddo
      enddo

      do k = 1, laytrop
        ind01 = id0(k,25) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,25) + 1
        ind12 = ind11 + 1

        do j = 1, NG25
          taug(k,j) = colamt(k,1)                                       &
     &        * ( fac00(k)*absa(ind01,j) + fac10(k)*absa(ind02,j)       &
     &        +   fac01(k)*absa(ind11,j) + fac11(k)*absa(ind12,j) )     &
     &        + colamt(k,3) * abso3a(j) 
        enddo
      enddo

      do k = laytrop+1, NLAY
        do j = 1, NG25
          taug(k,j) = colamt(k,3) * abso3b(j) 
        enddo
      enddo

      return
!...................................
      end subroutine taumol25
!-----------------------------------


!-----------------------------------
      subroutine taumol26
!...................................

!  ------------------------------------------------------------------  !
!     written by eli j. mlawer, atmospheric & environmental research.  !
!                                                                      !
!     band 26:  22650-29000 cm-1 (low - nothing; high - nothing)       !
!                                                                      !
! modifications                                                        !
!                                                                      !
!     jjmorcrette 2003-02-24 adapted to ecmwf environment              !
!  ------------------------------------------------------------------  !
!
      use module_radsw_kgb26
!
      implicit none

!  ---  locals:
      integer :: j, k

!
!===> ... begin here
!

!  ---  compute the optical depth by interpolating in ln(pressure), 
!       temperature, and appropriate species.  below laytrop, the water
!       vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, NLAY
        do j = 1, NG26 
          taug(k,j) = zero
          taur(k,j) = colmol(k) * rayl(j) 
        enddo
      enddo

      return
!...................................
      end subroutine taumol26
!-----------------------------------


!-----------------------------------
      subroutine taumol27
!...................................

!  ------------------------------------------------------------------  !
!     written by eli j. mlawer, atmospheric & environmental research.  !
!                                                                      !
!     band 27:  29000-38000 cm-1 (low - o3; high - o3)                 !
!                                                                      !
! modifications                                                        !
!                                                                      !
!     jjmorcrette 2003-02-24 adapted to ecmwf environment              !
!  ------------------------------------------------------------------  !
!
      use module_radsw_kgb27
!
      implicit none

!  ---  locals:
      integer :: ind01, ind02, ind11, ind12
      integer :: j, k

!
!===> ... begin here
!

!  ---  compute the optical depth by interpolating in ln(pressure), 
!       temperature, and appropriate species.  below laytrop, the water
!       vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, NLAY
        do j = 1, NG27
          taur(k,j) = colmol(k) * rayl(j)
        enddo
      enddo

      do k = 1, laytrop
        ind01 = id0(k,27) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,27) + 1
        ind12 = ind11 + 1

        do j = 1, NG27
          taug(k,j) = colamt(k,3)                                       &
     &        * ( fac00(k)*absa(ind01,j) + fac10(k)*absa(ind02,j)       &
     &        +   fac01(k)*absa(ind11,j) + fac11(k)*absa(ind12,j) )
        enddo
      enddo

      do k = laytrop+1, NLAY
        ind01 = id0(k,27) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,27) + 1
        ind12 = ind11 + 1

        do j = 1, NG27
          taug(k,j) = colamt(k,3)                                       &
     &        * ( fac00(k)*absb(ind01,j) + fac10(k)*absb(ind02,j)       &
     &        +   fac01(k)*absb(ind11,j) + fac11(k)*absb(ind12,j) )
        enddo
      enddo

      return
!...................................
      end subroutine taumol27
!-----------------------------------


!-----------------------------------
      subroutine taumol28
!...................................

!  ------------------------------------------------------------------  !
!     written by eli j. mlawer, atmospheric & environmental research.  !
!                                                                      !
!     band 28:  38000-50000 cm-1 (low - o3,o2; high - o3,o2)           !
!                                                                      !
! modifications                                                        !
!                                                                      !
!     jjmorcrette 2003-02-24 adapted to ecmwf environment              !
!  ------------------------------------------------------------------  !
!
      use module_radsw_kgb28
!
      implicit none

!  ---  locals:
      real (kind=kind_phys) :: speccomb, specmult, tauray, fs, fs1,     &
     &       fac000,fac001,fac010,fac011, fac100,fac101,fac110,fac111

      integer :: ind01, ind02, ind03, ind04, ind11, ind12, ind13, ind14
      integer :: inds, indf, j, js, k, ks

!
!===> ... begin here
!

!  ---  compute the optical depth by interpolating in ln(pressure), 
!       temperature, and appropriate species.  below laytrop, the water
!       vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, NLAY
        tauray = colmol(k) * rayl

        do j = 1, NG28
          taur(k,j) = tauray
        enddo
      enddo

      do k = 1, laytrop
        speccomb = colamt(k,3) + strrat(28)*colamt(k,6)
        specmult = 8.0 * min(oneminus, colamt(k,3) / speccomb)

        js = 1 + int(specmult)
        fs = mod(specmult, one)
        fs1= one - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,28) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 9
        ind04 = ind01 + 10
        ind11 = id1(k,28) + js
        ind12 = ind11 + 1
        ind13 = ind11 + 9
        ind14 = ind11 + 10

        do j = 1, NG28
          taug(k,j) = speccomb                                          &
     &        * ( fac000 * absa(ind01,j) + fac100 * absa(ind02,j)       &
     &        +   fac010 * absa(ind03,j) + fac110 * absa(ind04,j)       &
     &        +   fac001 * absa(ind11,j) + fac101 * absa(ind12,j)       &
     &        +   fac011 * absa(ind13,j) + fac111 * absa(ind14,j) ) 
        enddo
      enddo

      do k = laytrop+1, NLAY
        speccomb = colamt(k,3) + strrat(28)*colamt(k,6)
        specmult = 4.0 * min(oneminus, colamt(k,3) / speccomb)

        js = 1 + int(specmult)
        fs = mod(specmult, one)
        fs1= one - fs
        fac000 = fs1 * fac00(k)
        fac010 = fs1 * fac10(k)
        fac100 = fs  * fac00(k)
        fac110 = fs  * fac10(k)
        fac001 = fs1 * fac01(k)
        fac011 = fs1 * fac11(k)
        fac101 = fs  * fac01(k)
        fac111 = fs  * fac11(k)

        ind01 = id0(k,28) + js
        ind02 = ind01 + 1
        ind03 = ind01 + 5
        ind04 = ind01 + 6
        ind11 = id1(k,28) + js
        ind12 = ind11 + 1
        ind13 = ind11 + 5
        ind14 = ind11 + 6

        do j = 1, NG28
          taug(k,j) = speccomb                                          &
     &        * ( fac000 * absb(ind01,j) + fac100 * absb(ind02,j)       &
     &        +   fac010 * absb(ind03,j) + fac110 * absb(ind04,j)       &
     &        +   fac001 * absb(ind11,j) + fac101 * absb(ind12,j)       &
     &        +   fac011 * absb(ind13,j) + fac111 * absb(ind14,j) ) 
        enddo
      enddo

      return
!...................................
      end subroutine taumol28
!-----------------------------------


!-----------------------------------
      subroutine taumol29
!...................................

!  ------------------------------------------------------------------  !
!     written by eli j. mlawer, atmospheric & environmental research.  !
!                                                                      !
!     band 29:  820-2600 cm-1 (low - h2o; high - co2)                  !
!                                                                      !
! modifications                                                        !
!                                                                      !
!     jjmorcrette 2002-10-03 adapted to ecmwf environment              !
!  ------------------------------------------------------------------  !
!
      use module_radsw_kgb29
  
      implicit none

!  ---  locals:
      real (kind=kind_phys) :: tauray

      integer :: ind01, ind02, ind11, ind12
      integer :: inds, indf, j, k

!
!===> ... begin here
!

!  ---  compute the optical depth by interpolating in ln(pressure), 
!       temperature, and appropriate species.  below laytrop, the water
!       vapor self-continuum is interpolated (in temperature) separately.  

      do k = 1, NLAY
        tauray = colmol(k) * rayl

        do j = 1, NG29
          taur(k,j) = tauray
        enddo
      enddo

      do k = 1, laytrop
        ind01 = id0(k,29) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,29) + 1
        ind12 = ind11 + 1
        inds = indself(k)
        indf = indfor (k)

        do j = 1, NG29
          taug(k,j) = colamt(k,1)                                       &
     &        * ( (fac00(k)*absa(ind01,j) + fac10(k)*absa(ind02,j)      &
     &        +    fac01(k)*absa(ind11,j) + fac11(k)*absa(ind12,j) )    &
     &        +  selffac(k) * (selfref(inds,j) + selffrac(k)            &
     &        *  (selfref(inds+1,j) - selfref(inds,j)))                 &
     &        +  forfac(k) * (forref(indf,j) + forfrac(k)               &
     &        *  (forref(indf+1,j) - forref(indf,j))))                  &
     &        +  colamt(k,2) * absco2(j) 
        enddo
      enddo

      do k = laytrop+1, NLAY
        ind01 = id0(k,29) + 1
        ind02 = ind01 + 1
        ind11 = id1(k,29) + 1
        ind12 = ind11 + 1

        do j = 1, NG29
          taug(k,j) = colamt(k,2)                                       &
     &        * ( fac00(k)*absb(ind01,j) + fac10(k)*absb(ind02,j)       &
     &        +   fac01(k)*absb(ind11,j) + fac11(k)*absb(ind12,j) )     &  
     &        + colamt(k,1) * absh2o(j) 
        enddo
      enddo

      return
!...................................
      end subroutine taumol29
!-----------------------------------

!...................................
      end subroutine spcvrt
!-----------------------------------

!
!........................................!
      end module module_radsw_main       !
!========================================!

