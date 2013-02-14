!-----------------------------------------------------------------------
      subroutine compns(deltim,  iret,
     &                  ntrac,   nxpt,    nypt,  jintmx, jcap,
     &                  levs,    levr,    lonf,  lonr,   latg,    latr,
     &                  ntoz,    ntcw,    ncld,  lsoil,  nmtvr,
     &                  num_p3d, num_p2d, me,    nlunit, gfs_namelist)
!
!$$$  Subprogram Documentation Block
!
! Subprogram:  compns     Check and compute namelist frequencies
!   Prgmmr: Iredell       Org: NP23          Date: 1999-01-26
!
! Abstract: This subprogram checks global spectral model namelist
!           frequencies in hour units for validity.  If they are valid,
!           then the frequencies are computed in timestep units.
!           The following rules are applied:
!             1. the timestep must be positive;
!             2. the output frequency must be positive and
!                a multiple of the timestep to within tolerance;
!             3. the shortwave frequency must be positive and
!                a multiple of the timestep to within tolerance;
!             4. the longwave frequency must be positive and
!                a multiple of the timestep to within tolerance and
!                a multiple of the shortwave frequency;
!             5. the zeroing frequency must be positive and
!                a multiple of the timestep to within tolerance and
!                a multiple of the output frequency;
!             6. the restart frequency must be positive and
!                a multiple of the timestep to within tolerance and
!                a multiple of the longwave frequency and
!                a multiple of the zeroing frequency;
!             7. the initialization window must be non-negative and
!                a multiple of the timestep to within tolerance and
!                a multiple of the longwave frequency and
!                no longer than the restart frequency;
!             8. the cycling frequency must be non-negative and
!                a multiple of the timestep to within tolerance and
!                a multiple of the longwave frequency.
!
! Program History Log:
!   1999-01-26  Iredell
!
! Usage:    call compns(deltim,
!    &                  fhout,fhswr,fhlwr,fhzer,fhres,fhdfi,fhcyc,
!    &                  nsout,nsswr,nslwr,nszer,nsres,nsdfi,nscyc,
!    &                  iret)
!   Input Arguments:
!     tol      - real error tolerance allowed for input frequencies
!                (e.g. 0.01 for 1% of timestep maximum error allowed)
!     deltim   - real timestep in seconds
!     fhout    - real output frequency in hours
!     fhswr    - real shortwave frequency in hours
!     fhlwr    - real longwave frequency in hours
!     fhzer    - real zeroing frequency in hours
!     fhres    - real restart frequency in hours
!     fhdfi    - real initialization window in hours
!     fhcyc    - real cycling frequency in hours
!   Output Arguments:
!     nsout    - integer output frequency in timesteps
!     nsswr    - integer shortwave frequency in timesteps
!     nslwr    - integer longwave frequency in timesteps
!     nszer    - integer zeroing frequency in timesteps
!     nsres    - integer restart frequency in timesteps
!     nsdfi    - integer initialization window in timesteps
!     nscyc    - integer cycling frequency in timesteps
!     iret     - integer return code (0 if successful or
!                between 1 and 8 for which rule above was broken)
!     LDIAG3D  - switch for 3D diagnostic- (default = false)
!hchuang code change [+1L]
!     LGGFS3D  - switch for 3D GFS-GOCARRT fields (default = false)
!
! Attributes:
!   Language: Fortran 90
!
!$$$

      
      use namelist_def
!cmy mpi_def holds liope
      use mpi_def, only : liope
      implicit none

      real tol
 
      character (len=*), intent(in) :: gfs_namelist
      integer, intent(in)           :: me, nlunit
      real,intent(inout)            :: deltim
      integer,intent(out)           :: iret
      integer ntrac,nxpt,nypt,jintmx,jcap,levs,lonf,lonr,latg,latr
      integer levr
      integer ntoz,ntcw,ncld,lsoil,nmtvr,num_p3d,num_p2d,member_num
      real    tfiltc
      logical lgoc3d

csela - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c     if output (fhout) more frequently than zeroing ,get partial rains
 
      namelist /nam_mrf/FHMAX,FHOUT,FHRES,FHZER,FHSEG,FHROT,DELTIM,IGEN,
     & NGPTC,fhdfi,fhswr,fhlwr,fhcyc,ras,LGOC3D,FHGOC3D,LDIAG3D,
     & shuff_lats_a,shuff_lats_r,reshuff_lats_a,reshuff_lats_r,
     & adiab,explicit,pre_rad,hybrid,gen_coord_hybrid,random_xkt2,liope, ! hmhj
     & run_enthalpy,out_virttemp,                                        ! hmhj
     & ntrac,nxpt,nypt,jintmx,jcap,levs,lonf,lonr,latg,latr,levr,
     & ntoz,ntcw,ncld,lsoil,nmtvr,zhao_mic,nsout,lsm,tfiltc,
     & isol, ico2, ialb, iems, iaer, iovr_sw, iovr_lw, ictm,
     & ncw, crtrh,old_monin,flgmin,gfsio_in,gfsio_out,ref_temp,cnvgwd,
     & ccwf,sashal,newsas,zflxtvd,crick_proof,ccnorm,ctei_rm,mom4ice,
     & norad_precip,num_reduce,mstrat,trans_trac,
     & nsst_active,nsst_restart,tr_analysis,lsea,cal_pre,
     & fhout_hf,fhmax_hf
!
      num_reduce = -4
!
      fhmax    = 0
      fhout    = 0
      fhres    = 0
      fhzer    = 0
      fhseg    = 0
      fhrot    = 0
      fhout_hf = 1
      fhmax_hf = 0
      deltim   = 0
      igen     = 0
      fhdfi    = 0
      fhswr    = 0
      fhlwr    = 0
      fhcyc    = 0
!     tfiltc   = 0.92
      tfiltc   = 0.85
      ccwf     = 1.0
      ctei_rm  = 10.0
      NGPTC    = lonr
!
      ras              = .false.
      zhao_mic         = .true.
      LDIAG3D          = .false.
!
      LGGFS3D          = .false.
      LGOC3D           = .false.
      fhgoc3d          = 72.0
      sashal           = .true.
      crick_proof      = .false.
      ccnorm           = .false.
      newsas           = .true.
      norad_precip     = .false.   ! This is effective only for Ferrier/Moorthi
      mom4ice          = .false.   ! True when coupled to MOM4 OM
      mstrat           = .false.
      trans_trac       = .false.   ! This is effective only for RAS
      cal_pre          = .false.   ! true for huiya's precip type algorithm
!
      shuff_lats_a     = .false.
      shuff_lats_r     = .true.
      reshuff_lats_a   = .false.
      reshuff_lats_r   = .false.
!
      adiab            = .false.
      explicit         = .false.
      pre_rad          = .false.
      hybrid           = .false.
      gen_coord_hybrid = .false.
      random_xkt2      = .true.
      liope            = .true.
!
      old_monin        = .false.
      cnvgwd           = .false.
      zflxtvd          = .true.
      run_enthalpy     = .false.
      out_virttemp     = .true.
!
!     ncw(1)           = 75
      ncw(1)           = 50
      ncw(2)           = 150
      crtrh(:)         = 0.85
      flgmin(:)        = 0.20

!                                         For NSST model
      nsst_active      = .false.
      nsst_restart     = .false.
      tr_analysis      = .false.
      lsea             = 0
!
      ref_temp         = 300.0
!
      gfsio_in         = .true.
      gfsio_out        = .true.
!
      nsout   = 0
      nsout_hf = 0
      lsm     = 1         ! NOAH LSM is the default when lsm=1
      levr    = 0
!     Default values for some radiation controls
      isol    = 0         ! use prescribed solar constant
      ico2    = 0         ! prescribed global mean value (old opernl)
      ialb    = 0         ! use climatology alb, based on sfc type
!     ialb    = 1         ! use modis based alb
      iems    = 0         ! use fixed value of 1.0
      iaer    = 1         ! default aerosol
      iovr_sw = 1         ! sw: max-random overlap clouds
      iovr_lw = 1         ! lw: max-random overlap clouds
      ictm    = 1         ! ictm=0 => use data at initial cond time, if not
                          ! available, use latest, no extrapolation.
                          ! ictm=1 => use data at the forecast time, if not
                          ! available, use latest and extrapolation.
                          ! ictm=yyyy0 => use yyyy data for the forecast time,
                          ! no further data extrapolation.
                          ! ictm=yyyy1 = > use yyyy data for the fcst.
                          ! if needed, do extrapolation to match the fcst time.

!
      print *,' nlunit=',nlunit,' gfs_namelist=',gfs_namelist
c$$$      read(5,nam_mrf)
      open(unit=nlunit,file=gfs_namelist)
      rewind (nlunit)
      read(nlunit,nam_mrf)
      print *,' fhmax=',fhmax,' nsst_active =',nsst_active,'
     &  nsst_restart =',nsst_restart, ' tr_analysis = ',tr_analysis,
     &  'lsea =',lsea
c
      if (me.eq.0) write(6,nam_mrf)
      if(reshuff_lats_r.and.adiab)then
       reshuff_lats_r=.false.
       print*,' CAN NOT reshuff loopr and loopb adiabatically'
      endif
      filta = tfiltc
!
      LGGFS3D = LGOC3D
!
      if (levs > 100 .and. ref_temp < 400.0) ref_temp = 1500.0
!
      if (me == 0) then
        print *,' The time filter coefficient tfiltc=',tfiltc
        if (adiab) then
          print *,' This is an adiabatic run'
        else
          if (lsm == 1) then
            print *,' NOAH Land Surface Model used'
          elseif (lsm == 0) then
            print *,' OSU Land Surface Model used'
          else
            print *,' Unsupported LSM type - job aborted'
     &,                          ' - lsm=',lsm
            call mpi_quit(2222)
          endif
!
          if (ras) then
            print *,' RAS Convection scheme used with ccwf=',ccwf
          else
            if (newsas) then
              print *,' New modified SAS Convection scheme used'
            else
              print *,' OPR SAS Convection scheme used'
            endif
          endif
          if (.not. old_monin) print *,' New PBL scheme used'
          if (sashal) print *,' New Massflux based shallow convection'
     &,                       ' used'
          if (cnvgwd) print *,' Convective GWD parameterization used'
          if (crick_proof) print *,' CRICK-Proof cloud water used in'
     &,                            ' radiation '
          if (ccnorm) print *,' Cloud condensate normalized by cloud'
     &,                       ' cover for radiation'
        endif
      endif
!
      if (levr == 0) then
        levr = levs
      endif
      if (me .eq. 0) then
        if (.not. adiab) then
          print *,' Radiative heating calculated at',levr, ' layers'
          if (iovr_sw == 0) then
            print *,' random cloud overlap for Shortwave IOVR_SW='
     &,             iovr_sw
          else
            print *,' max-random cloud overlap for Shortwave IOVR_SW='
     &,             iovr_sw
          endif
          if (iovr_lw == 0) then
            print *,' random cloud overlap for Longwave IOVR_LW='
     &,             iovr_lw
          else
            print *,' max-random cloud overlap for Longwave IOVR_LW='
     &,             iovr_lw
          endif
        endif
      endif
!
      if (zhao_mic) then        ! default setup for Zhao Microphysics
        num_p3d = 4
        num_p2d = 3
        if (me .eq. 0 .and. .not. adiab)
     &                 print *,' Using Zhao Microphysics : nump3d='
     &,                num_p3d,' num_p2d=',num_p2d,' crtrh=',crtrh
      else                      ! Brad Ferrier's Microphysics
        num_p3d = 3
        num_p2d = 1
        if (me .eq. 0 .and. .not. adiab)
     &                 print *,' Using Ferrier Microphysics : nump3d='
     &,                num_p3d,' num_p2d=',num_p2d
     &,               ' crtrh=',crtrh,' ncw=',ncw,' flgmin=',flgmin
      endif
!
!sela - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      tol=0.01
!  Check rule 1.
      if(deltim.le.0) then
        iret=1
        return
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Compute nsout and check rule 2.
      if(nsout.gt.0) fhout=nsout*deltim/3600.
      nsout=nint(fhout*3600./deltim)
      if(nsout.le.0.or.abs(nsout-fhout*3600./deltim).gt.tol) then
        iret=2
        return
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Compute nsout_hf and check rule 21.
!     if(nsout_hf.gt.0) fhout=nsout_hf*deltim/3600.
      nsout_hf=nint(fhout_hf*3600./deltim)
      if(nsout_hf <= 0.or.abs(nsout_hf-fhout_hf*3600./deltim)>tol) then
        iret=2
        return
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Compute nsswr and check rule 3.
      nsswr=nint(fhswr*3600./deltim)
      if(nsswr.le.0.or.abs(nsswr-fhswr*3600./deltim).gt.tol) then
        iret=3
        return
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Compute nslwr and check rule 4.
      nslwr=nint(fhlwr*3600./deltim)
      if(nslwr.le.0.or.abs(nslwr-fhlwr*3600./deltim).gt.tol.or.
     &   mod(nslwr,nsswr).ne.0) then
        iret=4
        return
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Compute nszer and check rule 5.
      nszer=nint(fhzer*3600./deltim)
      if(nszer.le.0.or.abs(nszer-fhzer*3600./deltim).gt.tol.or.
     &   mod(nszer,nsout).ne.0) then
        iret=5
        return
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Compute nsres and check rule 6.
      nsres=nint(fhres*3600./deltim)
      if(nsres.le.0.or.abs(nsres-fhres*3600./deltim).gt.tol.or.
     &   mod(nsres,nslwr).ne.0.or.mod(nsres,nszer).ne.0) then
        iret=6
        return
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Compute nsdfi and check rule 7.
      if(fhdfi.eq.0.) then
        nsdfi=0
      else
        nsdfi=nint(fhdfi*3600./deltim)
        if(nsdfi.le.0.or.abs(nsdfi-fhdfi*3600./deltim).gt.tol.or.
     &     mod(nsdfi,nslwr).ne.0.or.nsdfi.gt.nsres) then
          iret=7
          return
        endif
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Compute nscyc and check rule 8.
      if(fhcyc.eq.0.) then
        nscyc=0
      else
        nscyc=nint(fhcyc*3600./deltim)
        if(nscyc.le.0.or.abs(nscyc-fhcyc*3600./deltim).gt.tol.or.
     &     mod(nscyc,nslwr).ne.0) then
          iret=8
          return
        endif
      endif
!!
      IF (NGPTC.GT.LONR) THEN
         NGPTC=LONR
         WRITE(*,*) "NGPTC IS TOO BIG, RESET NGPTC TO LONR",NGPTC
      ENDIF
      IF (ME.EQ.0)   WRITE(*,*) "NGPTC IS SET TO NGPTC :",NGPTC
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  All checks are successful.
      iret=0
c

      end
