      SUBROUTINE SFC_SICE(IM,KM,PS,U1,V1,T1,Q1,
     &                  HICE ,FICE ,TICE, SFCDSW,
     &                  SHELEG,SNWDPH,TSKIN,QSURF,TPRCP,SRFLAG,STC,DM,
     &                  DLWFLX,SLRAD,SNOWMT,DELT,GFLUX,CM,CH,
     &                  RCL,PRSL1,PRSLKI,SLIMSK,
     +                  CMM,CHH,ZLVL,
Clu_q2m_iter [-1L/+1L]: add flag_iter
!*   &                  EVAP,HFLX,EP,DDVEL)
     &                  EVAP,HFLX,EP,DDVEL,flag_iter,MOM4ICE,lsm)
!
      USE MACHINE , ONLY : kind_phys
      USE FUNCPHYS, ONLY : fpvs
      USE PHYSCONS, SBC => con_sbc, HVAP => con_HVAP, TGICE => con_tice
     &,             CP => con_CP, HFUS => con_HFUS, JCAL => con_JCAL
     &,             EPS => con_eps, EPSM1 => con_epsm1, grav => con_g
     &,             RVRDM1 => con_FVirt, T0C => con_T0C, RD => con_RD
      implicit none
!
!     include 'constant.h'
!
      integer              IM, km, kmi, lsm
!
      real(kind=kind_phys), parameter :: cpinv=1.0/cp, HVAPI=1.0/HVAP
      real(kind=kind_phys) DELT
      real(kind=kind_phys) PS(IM),       U1(IM),      V1(IM),
     &                     T1(IM),       Q1(IM),      SHELEG(IM),
     &                     TSKIN(IM),    QSURF(IM),   STC(IM,KM),
     &                     DM(IM),       DLWFLX(IM),  SLRAD(IM),
     &                     SNOWMT(IM),   GFLUX(IM),
     &                     CM(IM),      CH(IM),
     &                     RCL(IM),      PRSL1(IM),   PRSLKI(IM),
     &                     SLIMSK(IM),   EVAP(IM),    HFLX(IM),
     &                     RNET(IM),     EP(IM),      DDVEL(IM)
     &,                    TPRCP(IM),    SRFLAG(IM)
     &,                    SFCDSW(IM),   FICE(IM),    HICE(IM)
     &,                    CMM(IM),CHH(IM),ZLVL(IM)
     &,                    SNWDPH(IM)
      real(kind=kind_phys) TICE(IM),     FFW(IM),     EVAPI(IM),
     &                     EVAPW(IM),    HFLXI(IM),   HFLXW(IM),
     &                     SNETI(IM),    SNETW(IM),   QSSI(IM),
     &                     QSSW(IM)
      real(kind=kind_phys) hf(IM),       hfd(IM),     hfi(IM),
     &                     hfw(IM),      focn(IM),    snof(IM)
      real(kind=kind_phys) hi_save(IM),  hs_save(IM)

Clu_q2m_iter [+1L]: add flag_iter
      logical              flag_iter(im)

!
!     Locals
!
      integer              k,i
!
      real(kind=kind_phys) 
     &                     PSURF(IM),   Q0(IM),      QS1(IM),
     &                     QSS(IM),     RCAP(IM),    RCH(IM),
     &                     RHO(IM),     SLWD(IM),   SNET(IM),
     &                     SNOEVP(IM),  SNOWD(IM),THETA1(IM),  
     &                     TSURF(IM),   TV1(IM),    XRCL(IM),    
     &                     PS1(IM),     WIND(IM)
!
      real(kind=kind_phys)   
     &                     elocp,  sigma,   cimin,   himin,
     &                     himax,  hsmax,   timin,
     &                     t12,      t14,   tem
      real(kind=kind_phys) albfw,  emissiv
!
cc
      PARAMETER (kmi=2)           ! 2-layer of ice
      PARAMETER (sigma=sbc)
      PARAMETER (ELOCP=HVAP/CP)
!     PARAMETER (CIMIN=0.15)      ! minimum ice concentration required
      PARAMETER (HIMAX=8.0)       ! maximum ice thickness allowed
      PARAMETER (HIMIN=0.1)       ! minimum ice thickness required
      PARAMETER (HSMAX=2.)        ! maximum snow depth allowed
      PARAMETER (TIMIN=173.)      ! minimum temperature allowed for snow/ice
!     PARAMETER (CICE=1880.*917.)
      PARAMETER (ALBFW=0.06)     ! Albedo for lead
!     PARAMETER (EMISSIV=0.95)   ! emissivity of snow and ice
      PARAMETER (EMISSIV=1.0)    ! emissivity of snow and ice
      real, PARAMETER :: DSI=1.0/0.33
!
      LOGICAL FLAG(IM), FLAGSNW(IM)
      LOGICAL MOM4ICE
      real(kind=kind_phys) STSICE(IM,KMI)

      IF (MOM4ICE) then
         CIMIN=0.15          ! MOM4ICE and MASK
      ELSE
         CIMIN=0.50          ! GFS ONLY
      ENDIF

!
C
C FLAG for sea-ice
C
      DO I = 1, IM
         FLAG(I) = SLIMSK(I).GE.2. .AND. flag_iter(i)
      ENDDO
      IF (MOM4ICE) then
        DO I = 1, IM
         IF(FLAG(I)) THEN
           HI_save(I)=HICE(I)
           HS_save(I)=SHELEG(I)*0.001
         ENDIF
        ENDDO
      ENDIF
      DO I = 1, IM
         IF (flag_iter(i).AND.SLIMSK(I).LT.1.5) THEN
             HICE(I )= 0.
             FICE(I )= 0.
         ENDIF
      ENDDO
C
C snow-rain detection
C
      if (.not. mom4ice .and. lsm > 0) then
        DO I=1,IM
          IF(FLAG(I)) THEN
            IF(SRFLAG(I) .EQ. 1.) THEN
              EP(I) = 0.
              SHELEG(i) = SHELEG(i) + 1.E3*TPRCP(i)
             TPRCP(i)   = 0.
            ENDIF
          ENDIF
        ENDDO
      endif

C
C  INITIALIZE VARIABLES. ALL UNITS ARE SUPPOSEDLY M.K.S. UNLESS SPECIFIE
C  PSURF IS IN PASCALS
C  WIND IS WIND SPEED, THETA1 IS ADIABATIC SURFACE TEMP FROM LEVEL 1
C  RHO IS DENSITY, QS1 IS SAT. HUM. AT LEVEL1 AND QSS IS SAT. HUM. AT
C  SURFACE
C  CONVERT SLRAD TO THE CIVILIZED UNIT FROM LANGLEY MINUTE-1 K-4
C
!     qs1 = fpvs(t1)
!     qss = fpvs(tskin)
      DO I=1,IM
        IF(FLAG(I)) THEN
          XRCL(I)  = SQRT(RCL(I))
          PSURF(I) = 1000. * PS(I)
          PS1(I)   = 1000. * PRSL1(I)
          SLWD(I)  = SLRAD(I)
c
c  DLWFLX has been given a negative sign for downward longwave
c  snet is the net shortwave flux
c
          SNET(I) = -SLWD(I) - DLWFLX(I)
          WIND(I) = XRCL(I) * SQRT(U1(I) * U1(I) + V1(I) * V1(I))
     &            + MAX(0.0, MIN(DDVEL(I), 30.0))
          WIND(I) = MAX(WIND(I),1.)
          Q0(I) = MAX(Q1(I),1.E-8)
          TSURF(I) = TSKIN(I)
          THETA1(I) = T1(I) * PRSLKI(I)
          TV1(I) = T1(I) * (1. + RVRDM1 * Q0(I))
          RHO(I) = PS1(I) / (RD * TV1(I))
cjfe      QS1(I) = 1000. * FPVS(T1(I))
          qs1(i) = fpvs(t1(i))
          QS1(I) = EPS * QS1(I) / (PS1(I) + EPSM1 * QS1(I))
          QS1(I) = MAX(QS1(I), 1.E-8)
          Q0(I) = min(QS1(I),Q0(I))
cjfe      QSS(I) = 1000. * FPVS(TSURF(I))
!         qss(i) = fpvs(tskin(i))
!         QSS(I) = EPS * QSS(I) / (PSURF(I) + EPSM1 * QSS(I))
          FFW(I) = 1.0-FICE(I)
!         IF (FICE(I) .GE. cimin) THEN
!            TICE(I) = (TSKIN(I)-TGICE*FFW(I))/FICE(I)
!         ELSE
          IF (FICE(I) .LT. cimin) THEN
             PRINT *,'WARNING: ice fraction is low:', FICE(I)
             FICE(I )= cimin
             FFW(I)  = 1.0-FICE(I)
             TICE(I) = tgice
             TSKIN(I)= tgice
             PRINT *,'Fix ice fraction: reset it to:', FICE(I)
          ENDIF
          qssi(i) = fpvs(tice(i))
          QSSI(I) = EPS * QSSI(I) / (PSURF(I) + EPSM1 * QSSI(I))
          qssw(i) = fpvs(tgice)
          QSSW(I) = EPS * QSSW(I) / (PSURF(I) + EPSM1 * QSSW(I))
C
C  SNOW DEPTH IN WATER EQUIVALENT IS CONVERTED FROM MM TO M UNIT
C
          IF (MOM4ICE) then
             SNOWD(I) = SHELEG(I) * 0.001 / FICE(I)
          ELSE
             SNOWD(I) = SHELEG(I) / 1000.
          ENDIF
          FLAGSNW(I) = .FALSE.
C
C  WHEN SNOW DEPTH IS LESS THAN 1 MM, A PATCHY SNOW IS ASSUMED AND
C  SOIL IS ALLOWED TO INTERACT WITH THE ATMOSPHERE.
C  WE SHOULD EVENTUALLY MOVE TO A LINEAR COMBINATION OF SOIL AND
C  SNOW UNDER THE CONDITION OF PATCHY SNOW.
C
          IF(SNOWD(I).GT..001) FLAGSNW(I) = .TRUE.
        ENDIF
      ENDDO
!!

C
C  RCP = RHO CP CH V
C
      DO I = 1, IM
        IF(FLAG(I)) THEN
          RCH(I) = RHO(I) * CP * CH(I) * WIND(I)
Cwei added 10/24/2006
        CMM(I)=CM(I)* WIND(I)
        CHH(I)=RHO(I)*CH(I)* WIND(I)
        ZLVL(I) = -RD * TV1(I) * LOG(PS1(I)/PSURF(I)) / GRAV
        ENDIF
      ENDDO

C
C  SENSIBLE AND LATENT HEAT FLUX OVER OPEN WATER & SEA ICE
C
      DO I = 1, IM
        IF(FLAG(I)) THEN
          EVAPI(I) = ELOCP * RCH(I) * (QSSI(I) - Q0(I))
          EVAPW(I) = ELOCP * RCH(I) * (QSSW(I) - Q0(I))
!         EVAP(I)  = FICE(I)*EVAPI(I) + FFW(I)*EVAPW(I)
        ENDIF
      ENDDO

C
C  UPDATE SEA ICE TEMPERATURE
C
      DO K = 1, KMI
        DO I=1,IM
          IF(FLAG(I)) THEN
            STSICE(I,K) = STC(I,K)
          ENDIF
        ENDDO
      ENDDO
      DO I=1,IM
        IF(FLAG(I)) THEN
          SNETW(I) = SFCDSW(I)*(1.0-ALBFW)
          SNETW(I) = min(3.*SNET(I)/(1.+2.*FFW(I)),SNETW(I))
          SNETI(I) = (SNET(I)-FFW(I)*SNETW(I))/FICE(I)
          t12=tice(i)*tice(i)
          t14=t12*t12
C         hfi = Net non-solar and upIR heat flux @ ice surface
          hfi(i) =-dlwflx(i)+emissiv*sigma*t14 + evapi(i)
     &           +rch(i)*(tice(i)-theta1(i))
C         hfd = Heat flux derivat @ surface
C             = 4 emissivity T**3 + RCH [ 1 + L/CP * DQS/DT) ]
          hfd(i) = 4.*emissiv*sigma*tice(i)*t12
     &            +(1.+elocp*eps*hvap*qs1(i)/(rd*t12))*rch(i)
          t12=tgice*tgice
          t14=t12*t12
C         hfw = Net heat flux @ water surface (within ice)
          hfw(i) =-dlwflx(i)+emissiv*sigma*t14 + evapw(i)
     &           +rch(i)*(tgice-theta1(i))-snetw(i)
          focn(i) = 2. ! heat flux from ocean - should be from ocn model
          snof(i) = 0. ! snowfall rate - snow accumulates in gbphys
CCC...QC
          if (hice(i) .GT. himax) hice(i)=himax
          if (hice(i) .LT. himin) hice(i)=himin
          if (snowd(i) .GT. hsmax) snowd(i)=hsmax
          if (snowd(i) .GT. (2.*hice(i))) then
              print *,'WARNING: too much snow :',snowd(i)
              snowd(i)=2.*hice(i)
              print *,'FIX: decrease snow depth to:',snowd(i)
          endif
CCC...QC END
        ENDIF
      ENDDO
      call ice3lay(IM,kmi,snowd,hice,fice,flag,
     &             stsice,tice,hfi,sneti,hfd,hfw,focn,
     &             snof,snowmt,gflux,delt)
      IF (MOM4ICE) then
        DO I = 1, IM
         IF(FLAG(I)) THEN
           HICE(I)  = HI_save(I)
           SNOWD(I) = HS_save(I)
         ENDIF
        ENDDO
      ENDIF
      DO I=1,IM
        IF(FLAG(I)) THEN
          if (tice(i).LT.timin) then
           print *,'WARNING: snow/ice temperature is too low:',tice(i)
           tice(i)=timin
           PRINT *,'Fix snow/ice temperature: reset it to:',tice(i)
          endif
          if (stsice(i,1).LT.timin) then
           print *,'WARNING: Layer 1 ice temp is too low:',stsice(i,1)
           stsice(i,1)=timin
           PRINT *,'Fix Layer 1 ice temp: reset it to:',stsice(i,1)
          endif
          if (stsice(i,2).LT.timin) then
           print *,'WARNING: Layer 2 ice temp is too low:',stsice(i,2)
           stsice(i,2)=timin
           PRINT *,'Fix Layer 2 ice temp: reset it to:',stsice(i,2)
          endif
          tskin(i) = tice(i)*fice(i) + tgice*ffw(i)
        ENDIF
      ENDDO
      DO k=1,KMI
        DO I=1,IM
          IF(FLAG(I)) THEN
             stc(i,k) = min(stsice(i,k),T0C)
          ENDIF
        ENDDO
      ENDDO
C
C  CALCULATE SENSIBLE HEAT FLUX (& EVAP over SEA ICE)
C
      DO I = 1, IM
        IF (FLAG(I)) THEN
          HFLXI(I) = RCH(I) * (TICE(I) - THETA1(I))
          HFLXW(I) = RCH(I) * (TGICE - THETA1(I))
          HFLX(I) = FICE(I)*HFLXI(I) + FFW(I)*HFLXW(I)
          EVAP(I) = FICE(I)*EVAPI(I) + FFW(I)*EVAPW(I)
        ENDIF
      ENDDO

C
C  THE REST OF THE OUTPUT
C
      DO I = 1, IM
        IF(FLAG(I)) THEN
          QSURF(I) = Q1(I) + EVAP(I) / (ELOCP * RCH(I))
          DM(I) = 1.
C
C  CONVERT SNOW DEPTH BACK TO MM OF WATER EQUIVALENT
C
          SHELEG(I) = SNOWD(I) * 1000.
          SNWDPH(I) = SHELEG(I) * DSI   ! Snow depth in mm
        ENDIF
      ENDDO
!
      do i=1,im
        IF(FLAG(I)) THEN
          tem     = 1.0 / rho(i)
          hflx(i) = hflx(i) * tem * cpinv
          evap(i) = evap(i) * tem * hvapi
        ENDIF
      enddo
!
      RETURN
      END

      SUBROUTINE ICE3LAY(IM,kmi,hs,hi,fice,flag,
     &                   TI,TS,HF,SOL,HFD,FRZMLT,FB,
     &                   SNOW,SNOWMT,GFLUX,DT)
C
C**************************************************************************
C                                                                         *
C            THREE-LAYER SEA ICE VERTICAL THERMODYNAMICS                  *
C                                                                         *
C Based on:  M. Winton, "A reformulated three-layer sea ice model",       *
C Journal of Atmospheric and Oceanic Technology, 2000                     *
C                                                                         *
C                                                                         *
C        -> +---------+ <- ts - diagnostic surface temperature ( <= 0C )  *
C       /   |         |                                                   *
C     hs    |  snow   | <- 0-heat capacity snow layer                     *
C       \   |         |                                                   *
C        => +---------+                                                   *
C       /   |         |                                                   *
C      /    |         | <- t1 - upper 1/2 ice temperature; this layer has *
C     /     |         |         a variable (T/S dependent) heat capacity  *
C   hi      |...ice...|                                                   *
C     \     |         |                                                   *
C      \    |         | <- t2 - lower 1/2 ice temp. (fixed heat capacity) *
C       \   |         |                                                   *
C        -> +---------+ <- base of ice fixed at seawater freezing temp.   *
C                                                                         *
C**************************************************************************
C
      USE MACHINE , ONLY : kind_phys
      implicit none
C
C ***************************************************************
C                                                               *
C    Argument    Description            Units       Changed?    *
C    --------    -----------            -----       --------    *
C                                                               *
C INPUT/OUTPUT:                                                 *
C                                                               *
C      hs       Snow Thickness           m               y      *
C      hi       Ice Thickness            m               y      *
C      ts       Surface Temperature      deg C           y      *
C      ti(1)    Temp @ Midpt of Ice1     deg C           y      *
C      ti(2)    Temp @ Midpt of Ice2     deg C           y      *
C      hf(+)    Net non-solar and upIR                          *
C               heat flux @ surface      watt/m^2        n      *
C      sol(+)   Net solar incoming top   watt/m^2        n      *
C      hfd(+)   Heat flux derivat @ sfc  watt/(m^2deg-C) n      *
C      fb(+)    Heat Flux from Ocean     watt/m^2        n      *
C      snow     Snowfall Rate            m/sec           n      *
C      dt       timestep                 sec             n      *
C                                                               *
C ADDITIONAL:                                                   *
C                                                               *
C      snowmt   snow melt during dt      m               y      *
C      gflux    conductive heat flux     watt/m^2        y      *
C      flag     Ice mask flag                            n      *
C                                                               *
C LOCAL:                                                        *
C                                                               *
C      hdi      ice-water interface      m               y      *
C      hsni     snow-ice                 m               y      *
C ***************************************************************
C
      integer              IM, kmi
      real (kind=kind_phys) KS, DS, I0, KI, DI, CI, DICI,
     &                      LI, SI, MU, TFI, TFW, DILI, DSLI 
      real (kind=kind_phys) DW,tffresh,TFI0
      real (kind=kind_phys) ki4, dt, dt2, dt4, dt6
      real (kind=kind_phys) r0,r1,r2,r4,p5
C
C variables for temperature calculation [see Winton (2000) 2.a]
C
      real (kind=kind_phys) A(IM), B(IM), Ip(IM),
     &                      A1(IM),B1(IM),C1(IM),
     &                      A10(IM),B10(IM),
     &                      K12(IM),K32(IM),
     &                      TSF(IM),SNOWD(IM),
     &                      H1(IM),H2(IM),DH(IM),
     &                      F1(IM),TMELT(IM),BMELT(IM)
      real (kind=kind_phys) HF(IM),HFD(IM),
     &                      HS(IM),TS(IM),SOL(IM),
     &                      GFLUX(IM),SNOWMT(IM),
     &                      FB(IM),SNOW(IM),
     &                      HI(IM),TI(IM,KMI)
      real (kind=kind_phys) FICE(IM),FRZMLT(IM)
      real (kind=kind_phys) HSNI(IM),HDI(IM)
      LOGICAL FLAG(IM)
      integer              I

C properties of ice, snow, and seawater (local)
      PARAMETER (DS=330.0)       ! snow (over sea ice) density - 330 kg/(m^3)
      PARAMETER (DW=1000.0)      ! fresh water density - 1000 kg/(m^3)
      PARAMETER (TFFRESH=273.15) ! Freezing temp of fresh ice (K)
      PARAMETER (KS = 0.31)      ! conductivity of snow - 0.31 W/(mK)
      PARAMETER (I0 = 0.3)       ! ice surface penetrating solar fraction
      PARAMETER (KI = 2.03)      ! conductivity of ice  - 2.03 W/(mK)
      PARAMETER (DI = 917.0)     ! density of ice  - 917 kg/(m^3)
      PARAMETER (CI = 2054.0)    ! heat capacity of fresh ice - 2054 J/(kg K)
      PARAMETER (LI = 3.34e5)    ! latent heat of fusion - 334e3 J/(kg-ice)
      PARAMETER (SI = 1.0)       ! salinity of sea ice
      PARAMETER (MU = 0.054)     ! relates freezing temp. to salinity
      PARAMETER (TFI = -MU*SI)   ! sea ice freezing temp. = -mu*salinity
      PARAMETER (TFW = -1.8)     ! TFW - seawater freezing temperature -1.8 C
      PARAMETER (r0 = 0.)        ! r0=0
      PARAMETER (r1 = 1.)        ! r1=1
      PARAMETER (r2 = 2.)        ! r2=2
      PARAMETER (r4 = 4.)        ! r4=4
      PARAMETER (p5 = r1/r2)     ! p5=1/2
      PARAMETER (TFI0 = TFI-0.0001) ! TFI-0.0001
      PARAMETER (DICI = DI*CI)
      PARAMETER (DILI = DI*LI)
      PARAMETER (DSLI = DS*LI)
      PARAMETER (KI4 = KI*r4)

      dt2 = dt*r2
      dt4 = dt*r4
      dt6 = dt*6.

      DO I=1,IM
        IF(FLAG(I)) THEN
          hs(i) = hs(i)*DW/DS
          hdi(i) = (DS*hs(i)+DI*hi(i))/DW
          if (hi(i).LT.hdi(i)) then
             hs(i)=hs(i)+hi(i)-hdi(i)
             hsni(i)=(hdi(i)-hi(i))*DS/DI
             hi(i)=hi(i)+hsni(i)
          endif
          snow(i) = snow(i)*DW/DS
          ts(i) = TS(I)-TFFRESH               ! degC
          ti(i,1) = min(TI(I,1)-TFFRESH,TFI0) ! degC
          ti(i,2) = min(TI(I,2)-TFFRESH,TFI0) ! degC
          Ip(i) = I0*sol(i) ! Ip +v (in Winton Ip=-I0*sol as sol -v)
          if (hs(i) .gt. r0) then
            tsf(i) = r0
            Ip(i) = r0
          else
            tsf(i) = TFI
            Ip(i) = I0*sol(i) ! Ip +v here (in Winton Ip=-I0*sol)
          endif
          ts(i) = min(TS(I),TSF(I))
C
C Compute ice temperature
C
      B(i) = hfd(i)
      A(i) = hf(i)-sol(i)+Ip(i)-ts(i)*B(i)    ! +v sol input here
      K12(i) = KI4*KS/(KS*hi(i)+KI4*hs(i))
      K32(i) = r2*KI/hi(i)

      A10(i) = DICI*hi(i)/dt2 + K32(i)*(dt4*K32(i)+DICI*hi(i))
     &                       /(dt6*K32(i)+DICI*hi(i))
      B10(i) = -DI*hi(i)*(CI*ti(i,1)+LI*TFI/ti(i,1))/dt2 - Ip(i)
     &          -K32(i)*(dt4*K32(i)*TFW+DICI*hi(i)*ti(i,2))
     &              /(dt6*K32(i)+DICI*hi(i))

      A1(i) = A10(i)+K12(i)*B(i)/(K12(i)+B(i))
      B1(i) = B10(i)+A(i)*K12(i)/(K12(i)+B(i))
      C1(i) = DILI*TFI/dt2*hi(i)
      ti(i,1) = -(sqrt(B1(i)*B1(i)-r4*A1(i)*C1(i))+B1(i))/(r2*A1(i))
      ts(i) = (K12(i)*ti(i,1)-A(i))/(K12(i)+B(i))
      if (ts(i) .gt. tsf(i)) then
         A1(i) = A10(i)+K12(i)
         B1(i) = B10(i)-K12(i)*tsf(i)
         ti(i,1) = -(sqrt(B1(i)*B1(i)-r4*A1(i)*C1(i))+B1(i))/(r2*A1(i))
         ts(i) = tsf(i)
         tmelt(i) = (K12(i)*(ti(i,1)-tsf(i))-(A(i)+B(i)*tsf(i)))*dt
      else
         tmelt(i) = r0
         hs(i) = hs(i) + snow(i)*dt
      endif

      ti(i,2) = (dt2*K32(i)*(ti(i,1)+r2*TFW)+DICI*hi(i)*ti(i,2))
     &     /(dt6*K32(i)+DICI*hi(i))

      bmelt(i) = (fb(i)+KI4*(ti(i,2)-TFW)/hi(i))*dt
C
C Resize the ice ...
C
      h1(i) = p5*hi(i)
      h2(i) = p5*hi(i)
C
C ... top ...
C
      if (tmelt(i) .le. hs(i)*DSLI) then
         snowmt(i) = tmelt(i)/DSLI
         hs(i) = hs(i) - tmelt(i)/DSLI
      else
         snowmt(i) = hs(i)
         h1(i)=h1(i)
     &        -(tmelt(i)-hs(i)*DSLI)/(DI*(CI-LI/ti(i,1))*(TFI-ti(i,1)))
         hs(i) = r0
      endif
C
C ... and bottom
C
      if (bmelt(i) .lt. r0) then
         dh(i) = -bmelt(i)/(DILI+DICI*(TFI-TFW))
         ti(i,2) = (h2(i)*ti(i,2)+dh(i)*TFW)/(h2(i)+dh(i))
         h2(i) = h2(i)+dh(i)
      else
         h2(i) = h2(i)-bmelt(i)/(DILI+DICI*(TFI-ti(i,2)))
      endif
C
C if ice remains, even up 2 layers, else, pass negative energy back in snow
C
      hi(i) = h1(i) + h2(i)

      if (hi(i) .gt. r0) then
         if (h1(i) .gt. p5*hi(i)) then
            f1(i) = r1-r2*h2(i)/hi(i)
            ti(i,2)=f1(i)*(ti(i,1)+LI*TFI/(CI*ti(i,1)))
     &             +(r1-f1(i))*ti(i,2)
            if (ti(i,2).GT.TFI) then
               hi(i)=hi(i)-h2(i)*CI*(TI(i,2)-TFI)/(LI*dt)
               ti(i,2)=TFI
            endif
         else
            f1(i) = r2*h1(i)/hi(i)
            ti(i,1)=f1(i)*(ti(i,1)+LI*TFI/(CI*ti(i,1)))
     &             +(r1-f1(i))*ti(i,2)
            ti(i,1) = (ti(i,1)-sqrt(ti(i,1)*ti(i,1)-r4*TFI*LI/CI))/r2
         endif
         K12(i) = KI4*KS/(KS*hi(i)+KI4*hs(i))
         gflux(i) = k12(i) * (ti(i,1) - ts(i))
      else
         hs(i) = hs(i) + (h1(i)*(CI*(ti(i,1)-TFI)-LI*(r1-TFI/ti(i,1)))
     &             +h2(i)*(CI*(ti(i,2)-TFI)-LI))/LI
         hi(i) = max(r0,hs(i)*DS/DI)
         hs(i) = r0
         ti(i,1) = TFW
         ti(i,2) = TFW
         gflux(i) = r0
      endif

      gflux(i) = fice(i)*gflux(i)
      snowmt(i)=snowmt(i)*DS/DW
      hs(i)=hs(i)*DS/DW
      ts(i)=ts(i)+tffresh
      ti(i,1)=ti(i,1)+tffresh
      ti(i,2)=ti(i,2)+tffresh
      ENDIF ! FLAG
      ENDDO ! IM
      return
      end
