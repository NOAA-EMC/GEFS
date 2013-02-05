
      SUBROUTINE SFC_NSSTAC(IM,KM,PS,U1,V1,T1,Q1,
     &               TSKIN,QSURF,DM,GFLUX,CM,CH,
     &               RCL,PRSL1,PRSLKI,SLIMSK,
     &               xlon,sinlat,stress,DLWFLX,SLRAD,
     &               rain,timestep,kdt,                                                  ! input
     +               ifd,time_old,time_ins,I_Sw,I_Q,I_Qrain,
     +               I_M,I_Tau,I_Sw_Zw,I_Q_Ts,I_M_Ts,
     +               Tref,dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d,

     &               CMM,CHH,
     &               EVAP,HFLX,EP,DDVEL,flag_iter,
     &               lprnt, ipr)
!
      USE MACHINE , ONLY : kind_phys
      USE FUNCPHYS, ONLY : fpvs
      USE PHYSCONS, HVAP => con_HVAP
     &,             CP => con_CP,        HFUS => con_HFUS
     &,             EPS => con_eps,      EPSM1 => con_epsm1
     &,             RVRDM1 => con_FVirt, RD => con_RD
     &,             RHW0 => con_rhw0,    SBC => con_sbc, pi => con_pi
      USE date_def, only: idate
      USE module_nsst_parameters, ONLY : t0K,cp_w
      USE module_nsst_water_prop, ONLY : solar_time_from_julian,
     &                                  density, rhocoef, compjd 
      USE ocean_model, ONLY : 
     &                   cool_skin,warm_layer,jacobi_temp
!     USE resol_def, ONLY : nr_ocn,nf_ocn
      USE layout1
      implicit none
!
      integer              IM, km
      integer              ipr
      logical              lprnt, lprint
!
      real(kind=kind_phys), parameter :: cpinv=1.0/cp, HVAPI=1.0/HVAP,
     &                                   rad2deg=180./pi
!     real (kind=kind_phys), parameter :: EMISSIV=0.97
      real (kind=kind_phys), parameter :: EMISSIV=1.0
      real (kind=kind_phys), parameter :: f24   = 24.0     ! hours/day
      real (kind=kind_phys), parameter :: f1440 = 1440.0   ! minutes/day
      real(kind=kind_phys) PS(IM),       U1(IM),      V1(IM),
     &                     T1(IM),       Q1(IM),     
     &                     TSKIN(IM),    QSURF(IM),   DM(IM),  
     &                     GFLUX(IM),   
     &                     CM(IM),       CH(IM),      RCL(IM),
     &                     PRSL1(IM),    PRSLKI(IM),  SLIMSK(IM),
     &                     EVAP(IM),     HFLX(IM),
     &                     EP(IM),       DDVEL(IM)
     &,                    CMM(IM),      CHH(IM)

      logical              flag_iter(im), FLAG(IM)

!
!     Locals
!
      integer :: k,i,kdt
!
      real(kind=kind_phys) PSURF(IM),   PS1(IM),     Q0(IM),
     &                     QSS(IM),     RCH(IM),     RHO(IM),
     &                     THETA1(IM),  TV1(IM),     XRCL(IM),
     &                     WIND(IM),    STRESS(IM),  USTAR_A(IM),
     &                     USTAR_W(IM), XLON(IM),    SINLAT(IM),
     &                     ULWFLX(IM),  DLWFLX(IM),  SLRAD(IM),
     &                     NSWSFC(IM),  ALPHA(IM),   BETA(IM),
     &                     RHW(IM),     QPRATE(IM),  PRATE(IM),
     &                     F_nsol(IM),  rain(IM),    z_w_prev(IM),
     &                     soltim(IM),  Qrain(IM)

       real(kind=kind_phys) ifd(im),     time_old(im), time_ins(im),
     &                      I_Sw(im),    I_Q(im),      I_Qrain(im),
     &                      I_M(im),     I_Tau(im),    I_Sw_Zw(im),
     &                      I_Q_Ts(im),  I_M_Ts(im),   Tref(im),
     &                      dt_cool(im), z_c(im),      dt_warm(im),
     &                      z_w(im),     c_0(im),      c_d(im),
     &                      w_0(im),     w_d(im)

!===============================================================================
! Li added for oceanic components
!
!      variables required for restart oceanic model
!         Index of time integral started mode               : ifd
!         Solar time at previous time                       : time_old
!         The period of time integral                       : time_ins
!         Time integral of solar radiation flux             : I_Sw
!         Time integral of non-solar heat flux              : I_Q
!         Time integral of rain caused sensible heat flux   : I_Qrain
!         Time integral of mass flux S(E-P)                 : I_M
!         Time integral of momentum flux                    : I_Tau
!         Time integral of d(I_Sw)/d(z_w)                   : I_Sw_Zw
!         Time integral of d(I_Q)/d(Ts)                     : I_Q_Ts
!         Time integral of d(I_M)/d(Ts)                     : I_M_Ts

!      variables required for GSI
!         Reference/foundation temperature                  : T_ref
!         Sub-layer cooling amount                          : dt_cool
!         Sub-layer cooling thickness                       : z_c
!         Diurnal warming amount                            : dt_warm
!         Diurnal warming layer depth                       : z_w
!         W_0 (time integral)
!         W_d (time integral)
!         C_0 (current time step)
!         C_d (current time step)

      integer :: iyear,imon,iday,ihr,imin,jd
      integer :: idat(8),jdat(8)
      real(kind=kind_phys) elocp,tem,sss,Le,dwat,dtmp,wetc,alfac
      real(kind=kind_phys) Hs_Ts,Hl_Ts,RF_Ts
      real(kind=kind_phys) rinc(5) 
      real(kind=kind_phys) timestep,jday,jday_old
      real(kind=kind_phys) t12,t14,alon,es,Qs


      real (kind=kind_phys) ::  jdf_tmp,fjd
      real (kind=kind_phys) ::  fjd1,jd1,jd0

!  external functions called: iw3jdn
      integer :: iw3jdn
!======================================================================================================
cc
      PARAMETER (ELOCP=HVAP/CP)


      sss = 34.0             ! temporarily, when sea surface salinity data is not ready

      idat(1) = idate(4)
      idat(2) = idate(2)
      idat(3) = idate(3)
      idat(4) = 0
      idat(5) = idate(1)
      idat(6) = 0
      idat(7) = 0
      idat(8) = 0
!     write(*,*) ' sfc_nsstac, idat : ',idat
      rinc(1) = 0.
      rinc(2) = 0.
      rinc(3) = float(kdt)*timestep/60.0
      rinc(4) = 0.
      rinc(5) = 0.
      call w3movdat(rinc, idat, jdat)

      iyear = jdat(1)
      imon  = jdat(2)
      iday  = jdat(3)
      ihr   = jdat(5)
      imin  = jdat(6)

!  --- ...  calculate forecast julian day and fraction of julian day

      jd0 = iw3jdn(1899,12,31)
      jd1 = iw3jdn(iyear,imon,iday)

!  --- ...  unlike in normal applications, where day starts from 0 hr,
!           in astronomy applications, day stats from noon.

      if (ihr < 12) then
        jd1 = jd1 - 1
        fjd1= 0.5 + float(ihr)/f24 + float(imin)/f1440
      else
        fjd1= float(ihr - 12)/f24 + float(imin)/f1440
      endif

      jday  = jd1 - jd0 + fjd1 
C
C FLAG for open water
C
      DO I = 1, IM
         FLAG(I) = SLIMSK(I).EQ. 0. .AND. flag_iter(i)
      ENDDO

C
C  INITIALIZE VARIABLES. ALL UNITS ARE M.K.S. UNLESS SPECIFIED
C  PSURF IS IN PASCALS
C  WIND IS WIND SPEED, THETA1 IS ADIABATIC SURFACE TEMP FROM LEVEL 1
C  RHO IS AIR DENSITY, QSS IS SAT. HUM. AT SURFACE
C
!     if (lprnt) print *,' t1=',t1(ipr),' prslki=',prslki(ipr)
!!
      DO I=1,IM
        IF(FLAG(I)) THEN
!         ocnf(i,1) = TSKIN(I)
          NSWSFC(I) = -SLRAD(I) - DLWFLX(I)                 ! net solar radiation at the air-sea surface (positive = downward)

          XRCL(I)   = SQRT(RCL(I))
          PSURF(I)  = 1000. * PS(I)
          PS1(I)    = 1000. * PRSL1(I)
          WIND(I)   = XRCL(I) * SQRT(U1(I) * U1(I) + V1(I) * V1(I))
     &              + MAX(0.0, MIN(DDVEL(I), 30.0))
          WIND(I)   = MAX(WIND(I),1.)
          Q0(I)     = MAX(Q1(I),1.E-8)
          THETA1(I) = T1(I) * PRSLKI(I)
          TV1(I)    = T1(I) * (1. + RVRDM1 * Q0(I))
          RHO(I)    = PS1(I) / (RD * TV1(I))
          QSS(I)    = FPVS(TSKIN(I))                              ! Pa
          QSS(I)    = EPS * QSS(I) / (PSURF(I) + EPSM1 * QSS(I))  ! Pa

          CALL density(TSKIN(I),SSS,RHW(I))                    ! Sea water density
          CALL rhocoef(TSKIN(I),SSS,RHW(I),ALPHA(I),BETA(I))   ! alpha & beta

          USTAR_A(I) = SQRT(STRESS(I)/RHO(I))                  ! friction velocity in air
!
          EVAP(I)    = 0.
          HFLX(I)    = 0.
          GFLUX(I)   = 0.
          EP(I)      = 0.
!
!  RCP = RHO CP CH V
!
          RCH(I)     = RHO(I) * CP * CH(I) * WIND(I)
Cwei added 10/24/2006
          CMM(I)     = CM(I)* WIND(I)
          CHH(I)     = RHO(I)*CH(I)* WIND(I)
!
!  LATENT and SENSIBLE HEAT FLUX OVER OPEN WATER with TSKIN 
!  at previous time step
          EVAP(I)    = ELOCP * RCH(I) * (QSS(I) - Q0(I))
          QSURF(I)   = QSS(I)
          HFLX(I)    = RCH(I) * (TSKIN(I) - THETA1(I))
        ENDIF
      ENDDO
!!
!     if (lprnt) print *,' tskin=',tskin(ipr),' theta1=',
!    &theta1(ipr),' hflx=',hflx(ipr)
!
!   CALCULATE (1) Sub-layer cooling amount and thickness
!             (2) Diurnal warming amount and depth
!             (3) Coefficients to calculate Jacobian or sensitivity
!                 of Tz to Tref required in GSI
!   UPDATE TSKIN
!

      DO I = 1, IM
        lprint = .false.
        if (lprnt .and. i == ipr) lprint = .true.
        IF(FLAG(I)) THEN
          t12       = TSKIN(i)*TSKIN(i)
!         t14       = t12*t12
          ULWFLX(I) = emissiv*sbc*t12*t12
          alon      = xlon(i)*rad2deg

          CALL solar_time_from_julian(jday,alon,soltim(i))

          time_old(i) = max(0.0, soltim(i)-timestep)
!         if ( time_old(i) < 0.0 ) then
!           time_old(i) = 0.0
!         endif

          F_nsol(i) = HFLX(I) + EVAP(I) + ULWFLX(I) - DLWFLX(I)                       ! input heat flux as upward = positive to models here

          call cool_skin(ustar_a(i),alpha(i),beta(i),rhw(i),rho(i),
     &                   TSKIN(i),
     &                   F_nsol(i),NSWSFC(i),sss,evap(i),sinlat(i),
     &                   dt_cool(i),z_c(i)) 
!
!       for sensible heat flux caused by rainfall
!
          Le      = (2.501-.00237*TSKIN(I))*1e6
          dwat    = 2.11e-5*(T1(I)/t0K)**1.94                                                                !! water vapour diffusivity
          dtmp    = (1.+3.309e-3*(T1(I)-t0K)-1.44e-6*(T1(I)-t0K)*
     &              (T1(I)-t0K))*0.02411/(RHO(I)*CP)                                                         !! heat diffusivity
          wetc     = 622.0*Le*QSS(I)/(RD*T1(I)*T1(I))
          alfac    =  1/(1+(wetc*Le*dwat)/(CP*dtmp))                                                         !! wet bulb factor
          Qrain(I) = (1000.*rain(I)/RHW0)*alfac*cp_w*
     &            (TSKIN(I)-T1(I)+(1000.*QSS(I)-1000.*Q0(I))*Le/CP)

      if (lprnt .and. i == ipr) then
        print *,' dt_cool=',dt_cool(i),' tskin=',tskin(i),' qrain=',
     & qrain(i),' qss=',qss(i),' q0=',q0(i),' rain=',rain(i),
     & ' t1=',t1(i)
      endif

          call warm_layer(alpha(i),beta(i),sss,rhw(i),NSWSFC(i),
     &                    F_nsol(i),stress(i),evap(i),rain(i),
     &                    Qrain(i),TSKIN(i),
     &                    timestep,sinlat(i),soltim(i),
     +                    ifd(i),time_old(i),time_ins(i),I_Sw(i),
     &                    I_Q(i),I_Qrain(i),
     +                    I_M(i),I_Tau(i),dt_warm(i),z_w(i),
     &                    kdt,lprint)

      if (lprnt .and. i == ipr) print *,' kdt=',kdt,' dt_warm=',
     &   dt_warm(ipr)

          Hs_Ts = rch(i)
          Hl_Ts = rch(i)*elocp*eps*hvap*qss(i)/(rd*t12)
          RF_Ts = (1000.*rain(i)/RHW0)*alfac*cp_w*(1+rch(i)*Hl_Ts)

          call Jacobi_Temp
     &   (alpha(i),beta(i),sss,T1(i),PS1(i),Q0(i),ustar_a(i),rhw(i),
     &   rho(i),rain(i),EVAP(i),HFLX(i),DLWFLX(i),ULWFLX(i),NSWSFC(i),
     &   TSKIN(i),timestep,sinlat(i),soltim(i),Hs_Ts,Hl_Ts,RF_Ts,
     &   I_Sw(i),I_Q(i),I_M(i),I_Sw_Zw(i),I_Q_Ts(i),I_M_Ts(i),
     &   dt_cool(i),z_c(i),dt_warm(i),z_w(i),
     &   c_0(i),c_d(i),w_0(i),w_d(i),
     &   kdt)

!         if ( rain(i) > 0.001 ) then
!           write(*,'(a,2F10.6,2F10.3,5F10.6,F11.4,3F10.6)') 'Qprate: ',
!    &      QSS(i),Q0(i),TSKIN(i),T1(i),dwat,dtmp,
!    &      wetc,alfac,rain(i),Qrain(i),
!    &      Hs_Ts,Hl_Ts,RF_Ts
!         endif

        ENDIF
      ENDDO

!
!  LATENT AND SENSIBLE HEAT FLUX OVER OPEN WATER with updated TSKIN
!
      DO I = 1, IM
        IF(FLAG(I)) THEN
          TSKIN(i) = Tref(i) + dt_warm(i) - dt_cool(i)
          QSS(I)   = FPVS(TSKIN(I))
          QSS(I)   = EPS * QSS(I) / (PSURF(I) + EPSM1 * QSS(I))
          DM(I)    = 1.
          QSURF(I) = QSS(I)
          tem      = 1.0 / rho(i)
          EVAP(I)  = ELOCP * RCH(I) * (QSS(I) - Q0(I)) * tem * hvapi
          HFLX(I)  = RCH(I) * (TSKIN(I) - THETA1(I)) * tem * cpinv
        ENDIF
      ENDDO
!!
C
C  THE REST OF THE OUTPUT   <----  Note: Redundant calculation 
C
!*      DO I = 1, IM
!*        IF(SLIMSK(I).EQ.0.) THEN
!*        QSURF(I) = Q1(I) + EVAP(I) / (ELOCP * RCH(I))
!*        DM(I) = 1.
!*      ENDDO
!
      RETURN
      END
