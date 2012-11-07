!!!!!  ==========================================================  !!!!!
!!!!!             'module_radiation_driver' descriptions           !!!!!
!!!!!  ==========================================================  !!!!!
!                                                                      !
!   this is the radiation driver module.  it prepares atmospheric      !
!   profiles and invokes main radiation calculations.                  !
!                                                                      !
!   in module 'module_radiation_driver' there are twe externally       !
!   callable subroutine:                                               !
!                                                                      !
!      'radinit'    -- initialization routine                          !
!         input:                                                       !
!           ( si, NLAY, iflip, NP3D,                                   !
!             ISOL, ICO2, ICWP, IALB, IEMS, IAER, idate, jdate, me )   !
!         output:                                                      !
!           ( none )                                                   !
!                                                                      !
!      'grrad'      -- setup and invoke main radiation calls           !
!         input:                                                       !
!          ( prsi,prsl,prslk,tgrs,qgrs,oz,vvl,slmsk,                   !
!            xlon,xlat,tsfc,snowd,sncovr,snoalb,zorl,hprim,            !
!            alvsf,alnsf,alvwf,alnwf,facsf,facwf,fice,tisfc,           !
!            solcon,coszen,coszdg,k1oz,k2oz,facoz,                     !
!            cv,cvt,cvb,iovrsw,iovrlw,fcice,frain,rrime,               !
!            np3d,ntcw,ncld,ntoz, NTRAC,NFXR,                          !
!            dtlw,dtsw, lsswr,lslwr,lssav,ldiag3d,sashal,norad_precip, !
!            crick_proof, ccnorm, lggfs3d,                             !
!            IX, IM, LM, iflip, me, lprnt,                             !
!         output:                                                      !
!            htrsw,sfcnsw,sfcdsw,sfalb,                                !
!            htrlw,sfcdlw,tsflw,                                       !
!         input/output:                                                !
!            fluxr,cldcov,                                             !
!         optional output:                                             !
!            HTRSWB,HTRLWB)                                            !
!                                                                      !
!                                                                      !
!   external modules referenced:                                       !
!       'module machine'                    in 'machine.f'             !
!       'module funcphys'                   in 'funcphys.f'            !
!       'module physcons'                   in 'physcons.f             !
!                                                                      !
!       'module module_radiation_gases'     in 'radiation_gases.f'     !
!       'module module_radiation_aerosols'  in 'radiation_aerosols.f'  !
!       'module module_radiation_surface'   in 'radiation_surface.f'   !
!       'module module_radiation_clouds'    in 'radiation_clouds.f'    !
!                                                                      !
!       'module module_radsw_cntr_para'     in 'radsw_xxxx_param.f'    !
!       'module module_radsw_parameters'    in 'radsw_xxxx_param.f'    !
!       'module module_radsw_main'          in 'radsw_xxxx_main.f'     !
!                                                                      !
!       'module module_radlw_cntr_para'     in 'radlw_xxxx_param.f'    !
!       'module module_radlw_parameters'    in 'radlw_xxxx_param.f'    !
!       'module module_radlw_main'          in 'radlw_xxxx_main.f'     !
!                                                                      !
!    where xxxx may vary according to different scheme selection       !
!                                                                      !
!                                                                      !
!   program history log:                                               !
!     mm-dd-yy    ncep         - created program grrad                 !
!     08-12-03    yu-tai hou   - re-written for modulized radiations   !
!     11-06-03    yu-tai hou   - modified                              !
!     01-18-05    s. moorthi   - NOAH/ICE model changes added          !
!     05-10-05    yu-tai hou   - modified module structure             !
!     12-xx-05    s. moorthi   - sfc lw flux adj by mean temperature   !
!     02-20-06    yu-tai hou   - add time variation for co2 data, and  !
!                                solar const. add sfc emiss change     !
!     03-21-06    s. Moorthi   - added surface temp over ice           !
!     07-28-06    yu-tai hou   - add stratospheric vocanic aerosols    !
!     03-14-07    yu-tai hou   - add generalized spectral band interp  !
!                                for aerosol optical prop. (sw and lw) !
!     04-10-07    yu-tai hou   - spectral band sw/lw heating rates     !
!     05-04-07    yu-tai hou   - make options for clim based and modis !
!                                based (h. wei and c. marshall) albedo !
!     09-05-08    yu-tai hou   - add the initial date and time 'idate' !
!                    and control param 'ICTM' to the passing param list!
!                    to handel different time/date requirements for    !
!                    external data (co2, aeros, solcon, ...)           !
!                                                                      !
!!!!!  ==========================================================  !!!!!
!!!!!                       end descriptions                       !!!!!
!!!!!  ==========================================================  !!!!!



!========================================!
      module module_radiation_driver     !
!........................................!
!
      use machine ,                 only : kind_phys
      use physcons,                 only : con_eps,  con_epsm1
      use funcphys,                 only : fpvs
!     use resol_def,                only : psthk

      use module_radiation_astronomy,only : solinit
      use module_radiation_gases,   only : NF_VGAS, getgases, getozn,   &
     &                                     gasinit
      use module_radiation_aerosols,only : NF_AESW, aerinit, setaer,    &
     &                                     NF_AELW
      use module_radiation_surface, only : NF_ALBD, sfcinit, setalb,    &
     &                                     setemis
      use module_radiation_clouds,  only : NF_CLDS, cldinit,            &
     &                                     progcld1, progcld2, diagcld1

      use module_radsw_cntr_para,   only : iaersw
      use module_radsw_parameters,  only : topfsw_type, sfcfsw_type,    &
     &                                     profsw_type,cmpfsw_type,NBDSW
      use module_radsw_main,        only : rswinit,  swrad

      use module_radlw_cntr_para,   only : iaerlw
      use module_radlw_parameters,  only : topflw_type, sfcflw_type,    &
     &                                     proflw_type, NBDLW
      use module_radlw_main,        only : rlwinit,  lwrad
!
      implicit   none
!
      private

!  ---  constant values
      real (kind=kind_phys) :: QMIN, QME5, QME6, EPSQ
!     parameter ( QMIN=1.0e-10, QME5=1.0e-5, QME6=1.0e-6, EPSQ=1.0e-12 )
!     parameter (QMIN=1.0e-10,QME5=1.0e-20,QME6=1.0e-20,EPSQ=1.0e-12 )
      parameter (QMIN=1.0e-10,QME5=1.0e-7,QME6=1.0e-7,EPSQ=1.0e-12 )

!     real (kind=kind_phys), parameter :: psthk = 10.0 ! sfc air press thkness
!                                                      ! (mb) for tsflw calc.

!  ---  control variables
      integer :: irad1st=1,   month0=0,   iyear0=0

      public radinit, grrad


! =================
      contains
! =================


!-----------------------------------
      subroutine radinit                                                &
!...................................

!  ---  inputs:
     &     ( si, NLAY, iflip, NP3D, ICTM,                               &
     &       ISOL, ICO2, ICWP, IALB, IEMS, IAER, idate, jdate, me )
!  ---  outputs:
!          ( none )

! =================   subprogram documentation block   ================ !
!                                                                       !
! subprogram:   radinit     initialization of radiation calculations    !
!                                                                       !
!                                                                       !
! program history log:                                                  !
!   08-14-2003   yu-tai hou   created                                   !
!                                                                       !
! usage:        call radinit                                            !
!                                                                       !
! attributes:                                                           !
!   language:  fortran 90                                               !
!   machine:   ibm sp                                                   !
!                                                                       !
!  ====================  defination of variables  ====================  !
!                                                                       !
! input parameters:                                                     !
!   si               : model vertical sigma interface                   !
!   NLAY             : number of model vertical layers                  !
!   iflip            : control flag for direction of vertical index     !
!                     =0: index from toa to surface                     !
!                     =1: index from surface to toa                     !
!   NP3D             :=3: ferrier's microphysics cloud scheme           !
!                     =4: zhao/carr/sundqvist microphysics cloud        !
!   ICTM             :=yyyy#, external data time/date control flag      !
!                     =    0: use data at initial cond time, if not     !
!                             available, use latest, no extrapolation.  !
!                     =    1: use data at the forecast time, if not     !
!                             available, use latest and extrapolation.  !
!                     =yyyy0: use yyyy data for the forecast time,      !
!                             no further data extrapolation.            !
!                     =yyyy1: use yyyy data for the fcst. if needed, do !
!                             extrapolation to match the fcst time.     !
!   ISOL             :=0: use a fixed solar constant value              !
!                     =1: use 11-year cycle solar constant table        !
!   ICO2             :=0: use prescribed global mean co2 (old  oper)    !
!                     =1: use observed co2 annual mean value only       !
!                     =2: use obs co2 monthly data with 2-d variation   !
!   ICWP             : control flag for cloud generation schemes        !
!                     =0: use diagnostic cloud scheme                   !
!                     =1: use prognostic cloud scheme (default)         !
!   IALB             : control flag for surface albedo schemes          !
!                     =0: climatology, based on surface veg types       !
!                     =1: modis retrieval based surface albedo scheme   !
!   IEMS             : control flag for surface emissivity schemes      !
!                     =0: fixed value of 1.0                            !
!                     =1: varying value based on surface veg types      !
!   IAER             : flag for aerosols scheme selection               !
!                     = 1: opac climatology, without volc forcing       !
!                     =11: opac climatology, with volcanic forcing      !
!                     = 2: gocart prognostic, without volc forcing      !
!                     =12: gocart prognostic, with volcanic forcing     !
!   idate(8)         : ncep absolute date and time of initial condition !
!                      (yr, mon, day, t-zone, hr, min, sec, mil-sec)    !
!   jdate(8)         : ncep absolute date and time at fcst time         !
!                      (yr, mon, day, t-zone, hr, min, sec, mil-sec)    !
!   me               : print control flag                               !
!                                                                       !
!  outputs: (none)                                                      !
!                                                                       !
!  usage:       call radinit                                            !
!                                                                       !
!  subroutines called:    cldinit, aerinit, rlwinit, rswinit, gasinit   !
!                                                                       !
!  ===================================================================  !
!
      implicit none

!  ---  inputs:
      integer, intent(in) :: NLAY, iflip, NP3D, ICTM, ISOL, ICO2, ICWP, &
     &                       IALB, IEMS, IAER, me
      integer, intent(in) :: idate(:), jdate(:)

      real (kind=kind_phys), intent(in) :: si(:)

!  ---  outputs: (none)

!  ---  locals:
      integer :: iyear, month, iday, ihour

!
!===> ...  begin here
!

      if ( irad1st == 1 ) then
        if (me == 0) then
!          print *,' NEW RADIATION PROGRAM STRUCTURES -- SEP 01 2004'
           print *,' NEW RADIATION PROGRAM STRUCTURES BECAME OPER. ',   &
     &             '  May 01 2007'
        endif
      endif

      if ( ICTM == 0 ) then      ! get external data at initial condition time
        iyear = idate(1)
        month = idate(2)
!       iday  = idate(3)
!       ihour = idate(5)
      else                       ! get external data at fcst or specified time
        iyear = jdate(1)
        month = jdate(2)
!       iday  = jdate(3)
!       ihour = jdate(5)
      endif   ! end if_ICTM_block

!  --- ...  call aerosols and co2 initialization routines

      if ( month0 /= month ) then
        month0 = month
        if ( iaersw==1 .or. iaerlw==1 ) then

          call aerinit ( iyear, month, IAER, me )

        endif

        call gasinit ( iyear, month, ICTM, ICO2, me )
      endif

!  --- ...  call astronomy initialization routine

      if ( ISOL == 0 ) then

        if ( irad1st == 1) then
          call solinit ( ISOL, iyear, me )
        endif

      else

        if ( iyear0 /= iyear ) then
          iyear0 = iyear
          call solinit ( ISOL, iyear, me )
        endif

      endif

!  --- ...  followings only need to be called once

      if ( irad1st == 1 ) then

        irad1st = 0

!  --- ...  call surface initialization routine

        call sfcinit ( NLAY, iflip, IALB, IEMS, me )

!  --- ...  call cloud initialization routine

        call cldinit ( si, NLAY, iflip, NP3D, ICWP, me )

!  --- ...  call lw radiation initialization routine

        call rlwinit ( ICWP, me, NLAY )

!  --- ...  call sw radiation initialization routine

        call rswinit ( ICWP, me, NLAY )

      endif      ! end of if_irad1st_block
!
      return
!...................................
      end subroutine radinit
!-----------------------------------


!-----------------------------------
      subroutine grrad                                                  &
!...................................

!  ---  inputs:
     &     ( prsi,prsl,prslk,tgrs,qgrs,oz,vvl,slmsk,                    &
     &       xlon,xlat,tsfc,snowd,sncovr,snoalb,zorl,hprim,             &
     &       alvsf,alnsf,alvwf,alnwf,facsf,facwf,fice,tisfc,            &
     &       solcon,coszen,coszdg,k1oz,k2oz,facoz,                      &
     &       cv,cvt,cvb,iovrsw,iovrlw,fcice,frain,rrime,flgmin,         &
     &       np3d,ntcw,ncld,ntoz, NTRAC,NFXR,                           &
     &       dtlw,dtsw, lsswr,lslwr,lssav,ldiag3d,sashal,norad_precip,  &
     &       crick_proof, ccnorm, lggfs3d,                              &
!    &       dtlw,dtsw, lsswr,lslwr,lssav,ldiag3d,                      &
     &       IX, IM, LM, iflip, me, lprnt,                              &
!  ---  outputs:
     &       htrsw,sfcnsw,sfcdsw,sfalb,                                 &
     &       htrlw,sfcdlw,tsflw,                                        &
!  ---  input/output:
     &       fluxr,cldcov                                               &
!! ---  optional outputs:
     &,      HTRSWB,HTRLWB                                              &
     &     )

! =================   subprogram documentation block   ================ !
!                                                                       !
!    this program is the driver of radiation calculation subroutines. * !
!    It sets up profile variables for radiation input, including      * !
!    clouds, surface albedos, atmospheric aerosols, ozone, etc.       * !
!                                                                     * !
!    usage:        call grrad                                         * !
!                                                                     * !
!    subprograms called:                                              * !
!                  setalb, setemis, setaer, getozn, getgases,         * !
!                  progcld1, progcld2, diagcds,                       * !
!                  swrad, lwrad, fpvs                                 * !
!                                                                     * !
!    attributes:                                                      * !
!      language:   fortran 90                                         * !
!      machine:    ibm-sp, sgi                                        * !
!                                                                     * !
!                                                                     * !
!  ====================  defination of variables  ====================  !
!                                                                       !
!    input variables:                                                   !
!      prsi  (IX,LM+1) : model level pressure in cb (kPa)               !
!      prsl  (IX,LM)   : model layer mean pressure in cb (kPa)          !
!      prslk (IX,LM)   : pressure in cb (kPa)                           !
!      tgrs  (IX,LM)   : model layer mean temperature in k              !
!      qgrs  (IX,LM)   : layer specific humidity in gm/gm               !
!      oz  (IX,LM,NTRAC):layer ozone mass mixing ratio                  !
!      vvl   (IX,LM)   : layer mean vertical velocity in cb/sec         !
!      slmsk (IM)      : sea/land mask array (sea:0,land:1,sea-ice:2)   !
!      xlon,xlat (IM)  : grid longitude/latitude in radians             !
!      tsfc  (IM)      : surface temperature in k                       !
!      snowd (IM)      : snow depth water equivalent in mm              !
!      sncovr(IM)      : snow cover in fraction                         !
!      snoalb(IM)      : maximum snow albedo in fraction                !
!      zorl  (IM)      : surface roughness in cm                        !
!      hprim (IM)      : topographic standard deviation in m            !
!      alvsf (IM)      : mean vis albedo with strong cosz dependency    !
!      alnsf (IM)      : mean nir albedo with strong cosz dependency    !
!      alvwf (IM)      : mean vis albedo with weak cosz dependency      !
!      alnwf (IM)      : mean nir albedo with weak cosz dependency      !
!      facsf (IM)      : fractional coverage with strong cosz dependen  !
!      facwf (IM)      : fractional coverage with weak cosz dependency  !
!      fice  (IM)      : ice fraction over open water grid              !
!      tisfc (IM)      : surface temperature over ice fraction          !
!      solcon          : solar constant (sun-earth distant adjusted)    !
!      coszen(IM)      : mean cos of zenith angle over rad call period  !
!      coszdg(IM)      : daytime mean cosz over rad call period         !
!      k1oz,k2oz,facoz : parameters for climatological ozone            !
!      cv    (IM)      : fraction of convective cloud                   !
!      cvt, cvb (IM)   : convective cloud top/bottom pressure in cb     !
!      iovrsw/iovrlw   : control flag for cloud overlap (sw/lw rad)     !
!                        =0 random overlapping clouds                   !
!                        =1 max/ran overlapping clouds                  !
!      fcice           : fraction of cloud ice  (in ferrier scheme)     !
!      frain           : fraction of rain water (in ferrier scheme)     !
!      rrime           : mass ratio of total to unrimed ice ( >= 1 )    !
!      flgmin          : minimim large ice fraction                     !
!      np3d            : =3 brad ferrier microphysics scheme            !
!                        =4 zhao/carr/sundqvist microphysics scheme     !
!      ntcw            : =0 no cloud condensate calculated              !
!                        >0 array index location for cloud condensate   !
!      ncld            : only used when ntcw .gt. 0                     !
!      ntoz            : =0 climatological ozone profile                !
!                        >0 interactive ozone profile                   !
!      NTRAC           : dimension veriable for array oz                !
!      NFXR            : second dimension of input/output array fluxr   !
!      dtlw, dtsw      : time duration for lw/sw radiation call in sec  !
!      lsswr, lslwr    : logical flags for sw/lw radiation calls        !
!      lssav           : logical flag for store 3-d cloud field         !
!      ldiag3d         : logical flag for store 3-d diagnostic fields   !
!      sashal          : logical flag for Jongil's shallow convection   !
!      norad_precip    : logical flag for not using precip in radiation !
!      crick_proof     : logical flag for eliminating CRICK             !
!      ccnorm          : logical flag for incloud condensate mixing ratio!
!      lggfs3d         : logical flag for storing 3d fields for GOCART  !
!      IX,IM           : horizontal dimention and num of used points    !
!      LM              : vertical layer dimension                       !
!      iflip           : control flag for in/out vertical indexing      !
!                        =0 index from toa to surface                   !
!                        =1 index from surface to toa                   !
!      me              : control flag for parallel process              !
!      lprnt           : control flag for diagnostic print out          !
!                                                                       !
!    output variables:                                                  !
!      htrsw (IX,LM)   : total sky sw heating rate in k/sec             !
!      sfcnsw(IM)      : total sky surface net sw flux in w/m**2        !
!      sfcdsw(IM)      : total sky surface downward sw flux in w/m**2   !
!      sfalb (IM)      : mean surface diffused albedo                   !
!      htrlw (IX,LM)   : total sky lw heating rate in k/sec             !
!      sfcdlw(IM)      : total sky surface downward lw flux in w/m**2   !
!      tsflw (IM)      : surface air temp during lw calculation in k    !
!                                                                       !
!    input and output variables:                                        !
!      fluxr (IX,NFXR) : to save 2-d fields                             !
!      cldcov(IX,LM)   : to save 3-d cloud fraction                     !
!                                                                       !
!    optional output variables:                                         !
!      htrswb(IX,LM,NBDSW) : spectral band total sky sw heating rate    !
!      htrlwb(IX,LM,NBDLW) : spectral band total sky lw heating rate    !
!                                                                       !
!                                                                       !
!    definitions of internal variable arrays:                           !
!                                                                       !
!     1. fixed gases:         (defined in 'module_radiation_gases')     !
!          gasvmr(:,:,1)  -  co2 volume mixing ratio                    !
!          gasvmr(:,:,2)  -  n2o volume mixing ratio                    !
!          gasvmr(:,:,3)  -  ch4 volume mixing ratio                    !
!          gasvmr(:,:,4)  -  o2  volume mixing ratio                    !
!          gasvmr(:,:,5)  -  co  volume mixing ratio                    !
!          gasvmr(:,:,6)  -  cf11 volume mixing ratio                   !
!          gasvmr(:,:,7)  -  cf12 volume mixing ratio                   !
!          gasvmr(:,:,8)  -  cf22 volume mixing ratio                   !
!          gasvmr(:,:,9)  -  ccl4 volume mixing ratio                   !
!                                                                       !
!     2. cloud profiles:      (defined in 'module_radiation_clouds')    !
!                ---  for  prognostic cloud  ---                        !
!          clouds(:,:,1)  -  layer total cloud fraction                 !
!          clouds(:,:,2)  -  layer cloud liq water path                 !
!          clouds(:,:,3)  -  mean effective radius for liquid cloud     !
!          clouds(:,:,4)  -  layer cloud ice water path                 !
!          clouds(:,:,5)  -  mean effective radius for ice cloud        !
!          clouds(:,:,6)  -  layer rain drop water path                 !
!          clouds(:,:,7)  -  mean effective radius for rain drop        !
!          clouds(:,:,8)  -  layer snow flake water path                !
!          clouds(:,:,9)  -  mean effective radius for snow flake       !
!                ---  for  diagnostic cloud  ---                        !
!          clouds(:,:,1)  -  layer total cloud fraction                 !
!          clouds(:,:,2)  -  layer cloud optical depth                  !
!          clouds(:,:,3)  -  layer cloud single scattering albedo       !
!          clouds(:,:,4)  -  layer cloud asymmetry factor               !
!                                                                       !
!     3. surface albedo:      (defined in 'module_radiation_surface')   !
!          sfcalb( :,1 )  -  near ir direct beam albedo                 !
!          sfcalb( :,2 )  -  near ir diffused albedo                    !
!          sfcalb( :,3 )  -  uv+vis direct beam albedo                  !
!          sfcalb( :,4 )  -  uv+vis diffused albedo                     !
!                                                                       !
!     4. sw aerosol profiles: (defined in 'module_radiation_aerosols')  !
!          faersw(:,:,:,1)-  sw aerosols optical depth                  !
!          faersw(:,:,:,2)-  sw aerosols single scattering albedo       !
!          faersw(:,:,:,3)-  sw aerosols asymmetry parameter            !
!                                                                       !
!     5. lw aerosol profiles: (defined in 'module_radiation_aerosols')  !
!          faerlw(:,:,:,1)-  lw aerosols optical depth                  !
!          faerlw(:,:,:,2)-  lw aerosols single scattering albedo       !
!          faerlw(:,:,:,3)-  lw aerosols asymmetry parameter            !
!                                                                       !
!     6. sw fluxes at toa:    (defined in 'module_radsw_main')          !
!        (topfsw_type -- derived data type for toa rad fluxes)          !
!          topfsw(:)%upfxc  -  total sky upward flux at toa             !
!          topfsw(:)%dnfxc  -  total sky downward flux at toa           !
!          topfsw(:)%upfx0  -  clear sky upward flux at toa             !
!                                                                       !
!     7. lw fluxes at toa:    (defined in 'module_radlw_main')          !
!        (topflw_type -- derived data type for toa rad fluxes)          !
!          topflw(:)%upfxc  -  total sky upward flux at toa             !
!          topflw(:)%upfx0  -  clear sky upward flux at toa             !
!                                                                       !
!     8. sw fluxes at sfc:    (defined in 'module_radsw_main')          !
!        (sfcfsw_type -- derived data type for sfc rad fluxes)          !
!          sfcfsw(:)%upfxc  -  total sky upward flux at sfc             !
!          sfcfsw(:)%dnfxc  -  total sky downward flux at sfc           !
!          sfcfsw(:)%upfx0  -  clear sky upward flux at sfc             !
!          sfcfsw(:)%dnfx0  -  clear sky downward flux at sfc           !
!                                                                       !
!     9. lw fluxes at sfc:    (defined in 'module_radlw_main')          !
!        (sfcflw_type -- derived data type for sfc rad fluxes)          !
!          sfcflw(:)%upfxc  -  total sky upward flux at sfc             !
!          sfcflw(:)%dnfxc  -  total sky downward flux at sfc           !
!          sfcflw(:)%dnfx0  -  clear sky downward flux at sfc           !
!                                                                       !
!! optional radiation outputs:                                          !
!!   10. sw flux profiles:    (defined in 'module_radsw_main')          !
!!       (profsw_type -- derived data type for rad vertical profiles)   !
!!         fswprf(:,:)%upfxc - total sky upward flux                    !
!!         fswprf(:,:)%dnfxc - total sky downward flux                  !
!!         fswprf(:,:)%upfx0 - clear sky upward flux                    !
!!         fswprf(:,:)%dnfx0 - clear sky downward flux                  !
!!                                                                      !
!!   11. lw flux profiles:    (defined in 'module_radlw_main')          !
!!       (proflw_type -- derived data type for rad vertical profiles)   !
!!         flwprf(:,:)%upfxc - total sky upward flux                    !
!!         flwprf(:,:)%dnfxc - total sky downward flux                  !
!!         flwprf(:,:)%upfx0 - clear sky upward flux                    !
!!         flwprf(:,:)%dnfx0 - clear sky downward flux                  !
!!                                                                      !
!!   12. sw sfc components:   (defined in 'module_radsw_main')          !
!!       (cmpfsw_type -- derived data type for component sfc fluxes)    !
!!         scmpsw(:)%uvbfc  -  total sky downward uv-b flux at sfc      !
!!         scmpsw(:)%uvbf0  -  clear sky downward uv-b flux at sfc      !
!!         scmpsw(:)%nirbm  -  total sky sfc downward nir direct flux   !
!!         scmpsw(:)%nirdf  -  total sky sfc downward nir diffused flux !
!!         scmpsw(:)%visbm  -  total sky sfc downward uv+vis direct flx !
!!         scmpsw(:)%visdf  -  total sky sfc downward uv+vis diff flux  !
!                                                                       !
!  ======================  end of definations  =======================  !
!
      implicit none
 
!  ---  constant parameter

!  ---  inputs: (horizontal dimensioned by IX)
      integer,  intent(in) :: IX,IM, LM, NTRAC,NFXR, iflip, me,         &
     &       k1oz, k2oz, iovrsw, iovrlw, np3d, ntoz, ntcw, ncld

      logical,  intent(in) :: lsswr,  lslwr, lssav, ldiag3d, lprnt,     &
     &                        sashal, norad_precip, crick_proof, ccnorm,&
     &                        lggfs3d

      real (kind=kind_phys), dimension(IX,LM+1), intent(in) ::  prsi

      real (kind=kind_phys), dimension(IX,LM),   intent(in) ::  prsl,   &
     &       prslk, tgrs, qgrs, vvl, fcice, frain, rrime
      real (kind=kind_phys), dimension(IM), intent(in) :: flgmin

      real (kind=kind_phys), dimension(IM),      intent(in) ::  slmsk,  &
     &       xlon, xlat, tsfc, snowd, zorl, hprim, alvsf, alnsf, alvwf, &
     &       alnwf, facsf, facwf, coszen, coszdg, cv, cvt, cvb, fice,   &
     &       tisfc, sncovr, snoalb

      real (kind=kind_phys),  intent(in) ::  solcon, facoz, dtlw, dtsw, &
     &       oz(IX,LM,NTRAC)

!  ---  outputs: (horizontal dimensioned by IX)
      real (kind=kind_phys), dimension(IX,LM),intent(out):: htrsw,htrlw

      real (kind=kind_phys), dimension(IM),   intent(out):: sfcnsw,     &
     &       sfcdlw, tsflw, sfcdsw, sfalb

!  ---  variables are for both input and output:
      real (kind=kind_phys),                  intent(inout) ::          &
     &       fluxr(IX,NFXR), cldcov(IX,LM)

!! ---  optional outputs:
      real (kind=kind_phys), dimension(IX,LM,NBDSW), optional,          &
     &                       intent(out) :: htrswb
      real (kind=kind_phys), dimension(IX,LM,NBDLW), optional,          &
     &                       intent(out) :: htrlwb

!  ---  local variables: (horizontal dimensioned by IM)
      real (kind=kind_phys), dimension(IM,LM+1) :: plvl, tlvl

      real (kind=kind_phys), dimension(IM,LM)   :: plyr, tlyr, qlyr,    &
     &       olyr, rhly, qstl, vvel, clw, tem2da, tem2db

      real (kind=kind_phys), dimension(IM) :: tsfa, cvt1, cvb1, tem1d,  &
     &       sfcemis

      real (kind=kind_phys), dimension(IM,LM,NF_CLDS) :: clouds
      real (kind=kind_phys), dimension(IM,LM,NF_VGAS) :: gasvmr
      real (kind=kind_phys), dimension(IM,   NF_ALBD) :: sfcalb

      real (kind=kind_phys), dimension(IM,LM,NBDSW,NF_AESW) :: faersw
      real (kind=kind_phys), dimension(IM,LM,NBDLW,NF_AELW) :: faerlw

      real (kind=kind_phys), dimension(IM,LM) :: htswc
      real (kind=kind_phys), dimension(IM,LM) :: htlwc

      type (topfsw_type),    dimension(IM) :: topfsw
      type (topflw_type),    dimension(IM) :: topflw

      type (sfcfsw_type),    dimension(IM) :: sfcfsw
      type (sfcflw_type),    dimension(IM) :: sfcflw

!! ---  may be used for optional sw/lw outputs:
!!      take out "!!" as needed
!!    real (kind=kind_phys), dimension(IM,LM)   :: htsw0
!!    type (profsw_type),    dimension(IM,LM+1) :: fswprf
      type (cmpfsw_type),    dimension(IM)      :: scmpsw
      real (kind=kind_phys), dimension(IM,LM,NBDSW) :: htswb

!!    real (kind=kind_phys), dimension(IM,LM)   :: htlw0
!!    type (proflw_type),    dimension(IM,LM+1) :: flwprf
      real (kind=kind_phys), dimension(IM,LM,NBDLW) :: htlwb

      real (kind=kind_phys) :: raddt, es, qs, delt, tem0d, cldsa(IM,5)

      integer :: i, j, k, k1, lv, icec, itop, ibtc, nday, idxday(IM),   &
     &       mbota(IM,3), mtopa(IM,3), LP1

!  ---  for debug test use
!     real (kind=kind_phys) :: temlon, temlat, alon, alat
!     integer :: ipt
!     logical :: lprnt1

!
!===> ...  begin here
!
      LP1 = LM + 1

      raddt = min(dtsw, dtlw)

!  --- ...  for debug test
!     alon = 120.0
!     alat = 29.5
!     ipt = 0
!     do i = 1, IM
!       temlon = xlon(i) * 57.29578
!       if (temlon < 0.0) temlon = temlon + 360.0
!       temlat = xlat(i) * 57.29578
!       lprnt1 = abs(temlon-alon) < 1.1 .and. abs(temlat-alat) < 1.1
!       if ( lprnt1 ) then
!         ipt = i
!         exit
!       endif
!     enddo

!     print *,' in grrad : raddt=',raddt
!  --- ...  compute relative humidity

      do k = 1, LM
        do i = 1, IM
          es  = min( prsl(i,k), 0.001 * fpvs( tgrs(i,k) ) )   ! fpvs in pa
          qs  = max( QMIN, con_eps * es / (prsl(i,k) + con_epsm1*es) )
          rhly(i,k) = max( 0.0, min( 1.0, max(QMIN, qgrs(i,k))/qs ) )
          qstl(i,k) = qs
        enddo
      enddo

!  --- ...  get layer ozone mass mixing ratio

      if (ntoz > 0) then            ! interactive ozone generation

        do k = 1, LM
          do i = 1, IM
            olyr(i,k) = oz(i,k,ntoz)
          enddo
        enddo

      else                          ! climatological ozone

        do k = 1, LM
          do i = 1, IM
            tem2da(i,k) = prslk(i,k)
          enddo
        enddo

!     print *,' in grrad : calling getozn'
        call getozn                                                     &
!  ---  inputs:
     &     ( tem2da,xlat,k1oz,k2oz,facoz,                               &
     &       IM, LM, iflip,                                             &
!  ---  outputs:
     &       olyr                                                       &
     &     )

      endif                            ! end_if_ntoz

!  --- ...  prepare atmospheric profiles for radiation input
!           convert pressure unit from cb to mb

      do k = 1, LM
        do i = 1, IM
          plvl(i,k) = 10.0 * prsi(i,k)
          plyr(i,k) = 10.0 * prsl(i,k)
          vvel(i,k) = 10.0 * vvl (i,k)
          tlyr(i,k) = tgrs(i,k)
          olyr(i,k) = max( QMIN, olyr(i,k) )
        enddo
      enddo

      do i = 1, IM
        plvl(i,LP1) = 10.0 * prsi(i,LP1)
      enddo

!  --- ...  set up non-prognostic gas volume mixing ratioes

      call getgases                                                     &
!  ---  inputs:
     &    ( plvl, xlon, xlat,                                           &
     &      IM, LM, iflip,                                              &
!  ---  outputs:
     &      gasvmr                                                      &
     &     )

!  --- ...  get temperature at layer interface, and layer moisture

      do k = 2, LM
        do i = 1, IM
          tem2da(i,k) = log( plyr(i,k) )
          tem2db(i,k) = log( plvl(i,k) )
        enddo
      enddo

      if (iflip == 0) then               ! input data from toa to sfc

        do i = 1, IM
          tem1d (i)   = QME6
          tem2da(i,1) = log( plyr(i,1) )
          tem2db(i,1) = 1.0
          tsfa  (i)   = tlyr(i,LM)                   ! sfc layer air temp
          tlvl(i,1)   = tlyr(i,1)
          tlvl(i,LP1) = tsfc(i)
        enddo

        do k = 1, LM
          do i = 1, IM
            qlyr(i,k) = max( tem1d(i), qgrs(i,k) )
            tem1d(i)  = min( QME5, qlyr(i,k) )
          enddo
        enddo

        do k = 2, LM
          do i = 1, IM
            tlvl(i,k) = tlyr(i,k) + (tlyr(i,k-1) - tlyr(i,k))           &
     &                * (tem2db(i,k)   - tem2da(i,k))                   &
     &                / (tem2da(i,k-1) - tem2da(i,k))
          enddo
        enddo

      else                               ! input data from sfc to toa

        do i = 1, IM
          tem1d (i)   = QME6
          tem2da(i,1) = log( plyr(i,1) )
          tem2db(i,1) = log( plvl(i,1) )
          tsfa  (i)   = tlyr(i,1)                    ! sfc layer air temp
          tlvl(i,1)   = tsfc(i)
          tlvl(i,LP1) = tlyr(i,LM)
        enddo

        do k = LM, 1, -1
          do i = 1, IM
            qlyr(i,k) = max( tem1d(i), qgrs(i,k) )
            tem1d(i)  = min( QME5, qlyr(i,k) )
          enddo
        enddo

        do k = 1, LM-1
          do i = 1, IM
            tlvl(i,k+1) = tlyr(i,k) + (tlyr(i,k+1) - tlyr(i,k))         &
     &                  * (tem2db(i,k+1) - tem2da(i,k))                 &
     &                  / (tem2da(i,k+1) - tem2da(i,k))
          enddo
        enddo

      endif                              ! end_if_iflip

!  ---  check for daytime points

      nday = 0
      do i = 1, IM
        if (coszen(i) >= 0.0001) then
          nday = nday + 1
          idxday(nday) = i
        endif
      enddo

!  --- ...  setup aerosols property profile for radiation

      faersw(:,:,:,:) = 0.0
      faerlw(:,:,:,:) = 0.0

      if (iaersw==1 .or. iaerlw==1) then

!check  print *,' in grrad : calling setaer '

        call setaer                                                     &
!  ---  inputs:
     &     ( xlon,xlat,plvl,plyr,tlyr,qlyr,rhly,                        &
     &       IM,LM,LP1, iflip, lsswr,lslwr,                             &
!  ---  outputs:
     &       faersw,faerlw                                              &
     &     )

      endif           ! end_if_iaersw_iaerlw

!  --- ...  obtain cloud information for radiation calculations

      if (ntcw > 0) then                   ! prognostic cloud scheme

        do k = 1, LM
          do i = 1, IM
            clw(i,k) = 0.0
          enddo

          do j = 1, ncld
            lv = ntcw + j - 1
            do i = 1, IM
              clw(i,k) = clw(i,k) + oz(i,k,lv)    ! cloud condensate amount
            enddo
          enddo
        enddo

        where (clw < EPSQ)
          clw = 0.0
        endwhere

        if (np3d == 4) then              ! zhao/moorthi's prognostic cloud scheme

          call progcld1                                                 &
!  ---  inputs:
     &     ( plyr,plvl,tlyr,qlyr,qstl,rhly,clw,                         &
     &       xlat,xlon,slmsk,                                           &
     &       IM, LM, LP1, iflip, iovrsw, sashal, crick_proof, ccnorm,   &
!    &       IM, LM, LP1, iflip, iovrsw,                                &
!  ---  outputs:
     &       clouds,cldsa,mtopa,mbota                                   &
     &      )

        elseif (np3d == 3) then          ! ferrier's microphysics

!     print *,' in grrad : calling progcld2'
          call progcld2                                                 &
!  ---  inputs:
     &     ( plyr,plvl,tlyr,qlyr,qstl,rhly,clw,                         &
     &       xlat,xlon,slmsk, fcice,frain,rrime,flgmin,                 &
     &       IM, LM, LP1, iflip, iovrsw, sashal, norad_precip,          &
     &       crick_proof, ccnorm,                                       &
!    &       IM, LM, LP1, iflip, iovrsw,                                &
!  ---  outputs:
     &       clouds,cldsa,mtopa,mbota                                   &
     &      )

        endif                            ! end if_np3d

      else                                 ! diagnostic cloud scheme

        do i = 1, IM
          cvt1(i) = 10.0 * cvt(i)
          cvb1(i) = 10.0 * cvb(i)
        enddo

!  ---  compute diagnostic cloud related quantities

        call diagcld1                                                   &
!  ---  inputs:
     &     ( plyr,plvl,tlyr,rhly,vvel,cv,cvt1,cvb1,                     &
     &       xlat,xlon,slmsk,                                           &
     &       IM, LM, LP1, iflip, iovrsw,                                &
!  ---  outputs:
     &       clouds,cldsa,mtopa,mbota                                   &
     &      )

      endif                                ! end_if_ntcw

!  --- ...  start radiation calculations 
!           remember to set heating rate unit to k/sec!

      if (lsswr) then

!  ---  setup surface albedo for sw radiation, incl xw (nov04) sea-ice

        call setalb                                                     &
!  ---  inputs:
     &     ( slmsk,snowd,sncovr,snoalb,zorl,coszen,tsfc,tsfa,hprim,     &
     &       alvsf,alnsf,alvwf,alnwf,facsf,facwf,fice,tisfc,            &
     &       IM,                                                        &
!  ---  outputs:
     &       sfcalb                                                     &
     &     )

!  --- lu [+4L]: derive SFALB from vis- and nir- diffuse surface albedo
        do i = 1, IM
          sfalb(i) = max(0.01, 0.5 * (sfcalb(i,2) + sfcalb(i,4)))
        enddo

        if (nday > 0) then

!     print *,' in grrad : calling swrad'

          if ( present(htrswb) ) then

            call swrad                                                  &
!  ---  inputs:
     &     ( plyr,plvl,tlyr,tlvl,qlyr,olyr,gasvmr,                      &
     &       clouds,iovrsw,faersw,sfcalb,                               &
     &       coszen,solcon, nday,idxday,                                &
     &       IM, LM, LP1, iflip, lprnt,                                 &
!  ---  outputs:
     &       htswc,topfsw,sfcfsw                                        &
!! ---  optional:
!!   &,      HSW0=htsw0,FLXPRF=fswprf                                   &
     &,      HSWB=htswb,FDNCMP=scmpsw                                   &
     &     )

            do j = 1, NBDSW
              do k = 1, LM
              do i = 1, IM
                htrswb(i,k,j) = htswb(i,k,j)
              enddo
              enddo
            enddo

          else

            call swrad                                                  &
!  ---  inputs:
     &     ( plyr,plvl,tlyr,tlvl,qlyr,olyr,gasvmr,                      &
     &       clouds,iovrsw,faersw,sfcalb,                               &
     &       coszen,solcon, nday,idxday,                                &
     &       IM, LM, LP1, iflip, lprnt,                                 &
!  ---  outputs:
     &       htswc,topfsw,sfcfsw                                        &
!! ---  optional:
!!   &,      HSW0=htsw0,FLXPRF=fswprf,HSWB=htswb                        &
     &,      FDNCMP=scmpsw                                              &
     &     )

          endif

          do i = 1, IM
            sfcnsw(i) = sfcfsw(i)%upfxc - sfcfsw(i)%dnfxc
            sfcdsw(i) = sfcfsw(i)%dnfxc
          enddo

          do k = 1, LM
            do i = 1, IM
              htrsw(i,k) = htswc(i,k)
            enddo
          enddo

        else                   ! if_nday_block

          htrsw(:,:) = 0.0
          sfcnsw (:) = 0.0
          sfcdsw (:) = 0.0

          sfcfsw= sfcfsw_type( 0.0, 0.0, 0.0, 0.0 )
          topfsw= topfsw_type( 0.0, 0.0, 0.0 )
          scmpsw= cmpfsw_type( 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 )

!! ---  optional:
!!        fswprf= profsw_type( 0.0, 0.0, 0.0, 0.0 )

          if ( present(htrswb) ) htrswb(:,:,:) = 0.0

        endif                  ! end_if_nday

      endif                                ! end_if_lsswr

      if (lslwr) then

!  ---  save surface air temp for diurnal adjustment at model t-steps

        tsflw(1:IM) = tsfa(1:IM)

!  ---  setup surface emissivity for lw radiation

          call setemis                                                  &
!  ---  inputs:
     &     ( xlon,xlat,slmsk,snowd,sncovr,zorl,tsfc,tsfa,hprim,         &
     &       IM,                                                        &
!  ---  outputs:
     &       sfcemis                                                    &
     &     )

!     print *,' in grrad : calling lwrad'

        if ( present(htrlwb) ) then

          call lwrad                                                    &
!  ---  inputs:
     &     ( plyr,plvl,tlyr,tlvl,qlyr,olyr,gasvmr,                      &
     &       clouds,iovrlw,faerlw,sfcemis,                              &
     &       IM, LM, LP1, iflip, lprnt,                                 &
!  ---  outputs:
     &       htlwc,topflw,sfcflw                                        &
!! ---  optional:
!!   &,      HLW0=htlw0,FLXPRF=flwprf                                   &
     &,      HLWB=htlwb                                                 &
     &     )

          do j = 1, NBDLW
            do k = 1, LM
            do i = 1, IM
              htrlwb(i,k,j) = htlwb(i,k,j)
            enddo
            enddo
          enddo

        else

          call lwrad                                                    &
!  ---  inputs:
     &     ( plyr,plvl,tlyr,tlvl,qlyr,olyr,gasvmr,                      &
     &       clouds,iovrlw,faerlw,sfcemis,                              &
     &       IM, LM, LP1, iflip, lprnt,                                 &
!  ---  outputs:
     &       htlwc,topflw,sfcflw                                        &
!! ---  optional:
!!   &,      HLW0=htlw0,FLXPRF=flwprf,HLWB=htlwb                        &
     &     )

        endif

        do i = 1, IM
          sfcdlw(i) = sfcflw(i)%dnfxc
!  ---  save surface air temp for diurnal adjustment at model t-steps
          tsflw (i) = tsfa(i)
        enddo

        do k = 1, LM
          do i = 1, IM
            htrlw(i,k) = htlwc(i,k)
          enddo
        enddo

      endif                                ! end_if_lslwr

!  --- ...  collect the fluxr data for wrtsfc

!  ---  in previous codes, fluxr(17) contained various attempts at
!        calculating surface albedo...it has proven unsatisfactory!!
!       so now, sfc albedo will be calculated in wrtsfc as the
!        ratio of the time-mean of the sfcsw fluxes .. kac+mi dec98

      if (lssav) then

        if (lslwr) then
          do i = 1, IM
            fluxr(i,1 ) = fluxr(i,1 ) + dtlw * topflw(i)%upfxc   ! total sky top lw up
            fluxr(i,19) = fluxr(i,19) + dtlw * sfcflw(i)%dnfxc   ! total sky sfc lw dn
            fluxr(i,20) = fluxr(i,20) + dtlw * sfcflw(i)%upfxc   ! total sky sfc lw up
!*RADFLX*
            fluxr(i,28) = fluxr(i,28) + dtlw * topflw(i)%upfx0   ! clear sky top lw up
            fluxr(i,30) = fluxr(i,30) + dtlw * sfcflw(i)%dnfx0   ! clear sky sfc lw dn
            fluxr(i,33) = fluxr(i,33) + dtlw * sfcflw(i)%upfx0   ! clear sky sfc lw up
!*RADFLX*
          enddo
        endif

!  ---  proper diurnal sw wgt..coszro=mean cosz over daylight, while
!       coszdg= mean cosz over entire interval

        if (lsswr) then
          do i = 1, IM
            if (coszen(i) > 0.) then
              tem0d = dtsw * coszdg(i) / coszen(i)
              fluxr(i,2 ) = fluxr(i,2)  + topfsw(i)%upfxc * tem0d  ! total sky top sw up
              fluxr(i,3 ) = fluxr(i,3)  + sfcfsw(i)%upfxc * tem0d  ! total sky sfc sw up
              fluxr(i,4 ) = fluxr(i,4)  + sfcfsw(i)%dnfxc * tem0d  ! total sky sfc sw dn
              fluxr(i,18) = fluxr(i,18) + topfsw(i)%dnfxc * tem0d  ! total sky top sw dn
!  ---  sw uv-b fluxes
              fluxr(i,21) = fluxr(i,21) + scmpsw(i)%uvbfc * tem0d  ! total sky uv-b sw dn
              fluxr(i,22) = fluxr(i,22) + scmpsw(i)%uvbf0 * tem0d  ! clear sky uv-b sw dn
!*RADFLX*
              fluxr(i,29) = fluxr(i,29) + topfsw(i)%upfx0 * tem0d  ! clear sky top sw up
              fluxr(i,31) = fluxr(i,31) + sfcfsw(i)%upfx0 * tem0d  ! clear sky sfc sw up
              fluxr(i,32) = fluxr(i,32) + sfcfsw(i)%dnfx0 * tem0d  ! clear sky sfc sw dn
!*RADFLX*
            endif
          enddo
        endif

!  ---  save total cloud and bl cloud

        if (lsswr .or. lslwr) then
          do i = 1, IM
            fluxr(i,26) = fluxr(i,26) + raddt * cldsa(i,4)
            fluxr(i,27) = fluxr(i,27) + raddt * cldsa(i,5)
          enddo

!  ---  save cld frac,toplyr,botlyr and top temp, note that the order
!       of h,m,l cloud is reversed for the fluxr output.
!  ---  save interface pressure (cb) of top/bot

          do k = 1, 3
            do i = 1, IM
              tem0d = raddt * cldsa(i,k)
              itop  = mtopa(i,k)
              ibtc  = mbota(i,k)
              fluxr(i, 8-k) = fluxr(i, 8-k) + tem0d
              fluxr(i,11-k) = fluxr(i,11-k) + prsi(i,itop+1) * tem0d
              fluxr(i,14-k) = fluxr(i,14-k) + prsi(i,ibtc)   * tem0d
              fluxr(i,17-k) = fluxr(i,17-k) + tgrs(i,itop)   * tem0d
            enddo
          enddo
        endif

        if (ldiag3d .or. lggfs3d) then
          do k = 1, LM
            do i = 1, IM
              cldcov(i,k) = cldcov(i,k) + clouds(i,k,1) * raddt
            enddo
          enddo
        endif

      endif                                ! end_if_lssav
!
      return
!...................................
      end subroutine grrad
!-----------------------------------


!
!........................................!
      end module module_radiation_driver !
!========================================!
