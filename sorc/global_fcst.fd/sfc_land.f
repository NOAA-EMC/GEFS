      SUBROUTINE SFC_LAND(IM,KM,PS,U1,V1,T1,Q1,
Clu_Rev6: add TPRCP and SRFLAG
!**  &                  SHELEG,TSKIN,QSURF,
     &                  SHELEG,TSKIN,QSURF,TPRCP,SRFLAG,
     &                  SMC,STC,DM,SOILTYP,SIGMAF,VEGTYPE,CANOPY,
     &                  DLWFLX,SLRAD,SNOWMT,DELT,Z0RL,TG3,
     &                  GFLUX,ZSOIL,
     &                  CM, CH, RHSCNPY,RHSMC,AIM,BIM,CIM,
     &                  RCL,PRSL1,PRSLKI,SLIMSK,
     &                  DRAIN,EVAP,HFLX,EP,DDVEL,
     +                  CMM,CHH,Z1,EVBS,EVCW,TRANS,SBSNO,
     +                  SNOWC,STM,SNOHF,TWILT,TREF,
     &                  tsurf,flag_iter, flag_guess)
!
      USE MACHINE , ONLY : kind_phys
      USE FUNCPHYS, ONLY : fpvs
      USE PHYSCONS, grav => con_g, SBC => con_sbc, HVAP => con_HVAP
     &,             CP => con_CP, HFUS => con_HFUS, JCAL => con_JCAL
     &,             EPS => con_eps, EPSM1 => con_epsm1, t0c => con_t0c
     &,             RVRDM1 => con_FVirt, RD => con_RD
      implicit none
!
!     include 'constant.h'
!
      integer              IM, km
!
      real(kind=kind_phys), parameter :: cpinv=1.0/cp, HVAPI=1.0/HVAP
      real(kind=kind_phys) DELT
      INTEGER              SOILTYP(IM),  VEGTYPE(IM)
      real(kind=kind_phys) PS(IM),       U1(IM),      V1(IM),
     &                     T1(IM),       Q1(IM),      SHELEG(IM),
     &                     TSKIN(IM),    QSURF(IM),   SMC(IM,KM),
     &                     STC(IM,KM),   DM(IM),      SIGMAF(IM),
     &                     CANOPY(IM),   DLWFLX(IM),  SLRAD(IM),
     &                     SNOWMT(IM),   Z0RL(IM),    TG3(IM),
     &                     GFLUX(IM),
     &                     ZSOIL(IM,KM), CM(IM),      CH(IM),
     &                     RHSCNPY(IM), RHSMC(IM,KM),
     &                     AIM(IM,KM),   BIM(IM,KM),  CIM(IM,KM),
     &                     RCL(IM),      PRSL1(IM),   PRSLKI(IM),
     &                     SLIMSK(IM),   DRAIN(IM),   EVAP(IM),
     &                     HFLX(IM),     RNET(IM),    EP(IM),
     &                     WIND(IM),     DDVEL(IM)
Clu_Rev6: add TPRCP and SRFLAG
     &,                    TPRCP(IM),    SRFLAG(IM)      
Cwei added 10/24/2006
     &, CHH(IM),CMM(IM),EVBS(IM),EVCW(IM)
     &, TRANS(IM),SBSNO(IM),SNOWC(IM),STM(IM),SNOHF(IM)

!
!     land-related prognostic fields
!
      real(kind=kind_phys) SHELEG_OLD(IM),
     +                     TPRCP_OLD(IM), SRFLAG_OLD(IM),
     +                     TSKIN_OLD(IM), CANOPY_OLD(IM),
     +                     STC_OLD(IM,KM)


      logical              flag_iter(im), flag_guess(im)
!
!     Locals
!
      integer              k,i
!
      real(kind=kind_phys) CANFAC(IM),
     &                     DDZ(IM),     DDZ2(IM),    DELTA(IM),
     &                     DEW(IM),     DF1(IM),     DFT0(IM),
     &                     DFT2(IM),    DFT1(IM),
     &                     DMDZ(IM),    DMDZ2(IM),   DTDZ1(IM),
     &                     DTDZ2(IM),   DTV(IM),     EC(IM),
     &                     EDIR(IM),    ETPFAC(IM),
     &                     FACTSNW(IM), FH2(IM),
     &                     FX(IM),      GX(IM),
     &                     HCPCT(IM),   HL1(IM),     HL12(IM),
     &                     HLINF(IM),   PARTLND(IM), PH(IM),
     &                     PH2(IM),     PM(IM),      PM10(IM),
     &                     PSURF(IM),   Q0(IM),      QS1(IM),
     &                     QSS(IM),     RAT(IM),     RCAP(IM),
     &                     RCH(IM),     RHO(IM),     RS(IM),
     &                     RSMALL(IM),  SLWD(IM),    SMCZ(IM),
     &                     SNET(IM),    SNOEVP(IM),  SNOWD(IM),
     &                     T1O(IM),     T2MO(IM),    TERM1(IM),
     &                     TERM2(IM),   THETA1(IM),  THV1(IM),
     &                     TREF(IM),    TSURF(IM),   TV1(IM),
     &                     TVS(IM),     TSEA(IM),  TWILT(IM),
     &                     XX(IM),      XRCL(IM),    YY(IM),
     &                     Z0(IM),      Z0MAX(IM),   Z1(IM),
     &                     ZTMAX(IM),   ZZ(IM),      PS1(IM)
!
      real(kind=kind_phys) a0,    a0p,      a1,    a1p,     aa,  aa0,
     &                     aa1,   adtv,     alpha, arnu,    b1,  b1p,
     &                     b2,    b2p,      bb,    bb0,     bb1, bb2,
     &                     bfact, ca,       cc,    cc1,     cc2, cfactr,
     &                     ch2o,  charnock, cice,  convrad, cq,  csoil,
     &                     ctfil1,ctfil2,   delt2, df2,     dfsnow,
     &                     elocp, eth,      ff,  FMS,
     &                     fhs,   funcdf,   funckt,g,       hl0, hl0inf,
     &                     hl110, hlt,      hltinf,OLINF,   rcq, rcs,
     &                     rct,   restar,   rhoh2o,rnu,     RSI,
     &                     rss,   scanop,   sig2k, sigma,   smcdry,
     &                     t12,   t14,      tflx,  tgice,   topt,
     &                     val,   vis,      zbot,  snomin,  tem
!
cc
      PARAMETER (CHARNOCK=.014,CA=.4)!C CA IS THE VON KARMAN CONSTANT
      PARAMETER (G=grav,sigma=sbc)

      PARAMETER (ALPHA=5.,A0=-3.975,A1=12.32,B1=-7.755,B2=6.041)
      PARAMETER (A0P=-7.941,A1P=24.75,B1P=-8.705,B2P=7.899,VIS=1.4E-5)
      PARAMETER (AA1=-1.076,BB1=.7045,CC1=-.05808)
      PARAMETER (BB2=-.1954,CC2=.009999)
      PARAMETER (ELOCP=HVAP/CP,DFSNOW=.31,CH2O=4.2E6,CSOIL=1.26E6)
      PARAMETER (SCANOP=.5,CFACTR=.5,ZBOT=-3.,TGICE=271.2)
      PARAMETER (CICE=1880.*917.,topt=298.)
      PARAMETER (RHOH2O=1000.,CONVRAD=JCAL*1.E4/60.)
      PARAMETER (CTFIL1=.5,CTFIL2=1.-CTFIL1)
      PARAMETER (RNU=1.51E-5,ARNU=.135*RNU)
      parameter (snomin=1.0e-9)
!
      LOGICAL FLAG(IM), FLAGSNW(IM)
      real(kind=kind_phys) KT1(IM),       KT2(IM),      KTSOIL,
     &                     ET(IM,KM),
     &                     STSOIL(IM,KM), AI(IM,KM),    BI(IM,KM),
     &                     CI(IM,KM),     RHSTC(IM,KM)
      real(kind=kind_phys) rsmax(13), rgl(13),  rsmin(13), hs(13),
     &                     smmax(9),  smdry(9), smref(9),  smwlt(9)
c
c  the 13 vegetation types are:
c
c  1  ...  broadleave-evergreen trees (tropical forest)
c  2  ...  broadleave-deciduous trees
c  3  ...  broadleave and needle leave trees (mixed forest)
c  4  ...  needleleave-evergreen trees
c  5  ...  needleleave-deciduous trees (larch)
c  6  ...  broadleave trees with groundcover (savanna)
c  7  ...  groundcover only (perenial)
c  8  ...  broadleave shrubs with perenial groundcover
c  9  ...  broadleave shrubs with bare soil
c 10  ...  dwarf trees and shrubs with ground cover (trunda)
c 11  ...  bare soil
c 12  ...  cultivations (use parameters from type 7)
c 13  ...  glacial
c
      data rsmax/13*5000./
      data rsmin/150.,100.,125.,150.,100.,70.,40.,
     &           300.,400.,150.,999.,40.,999./
      data rgl/5*30.,65.,4*100.,999.,100.,999./
      data hs/41.69,54.53,51.93,47.35,47.35,54.53,36.35,
     &        3*42.00,999.,36.35,999./
      data smmax/.421,.464,.468,.434,.406,.465,.404,.439,.421/
      data smdry/.07,.14,.22,.08,.18,.16,.12,.10,.07/
      data smref/.283,.387,.412,.312,.338,.382,.315,.329,.283/
      data smwlt/.029,.119,.139,.047,.010,.103,.069,.066,.029/
!
      save rsmax, rsmin, rgl, hs, smmax, smdry, smref, smwlt
!
      DELT2 = DELT * 2.
C
C     ESTIMATE SIGMA ** K AT 2 M
C
      SIG2K = 1. - 4. * G * 2. / (CP * 280.)

C
C FLAG for land
C
      DO I = 1, IM
         FLAG(I) = SLIMSK(I).EQ. 1.
      ENDDO


C
C  save land-related prognostic fields for guess run
C
      do i=1, im
        if(FLAG(I) .AND. flag_guess(i)) then
          sheleg_old(i) = sheleg(i)
          tskin_old(i)  = tskin(i)
          canopy_old(i) = canopy(i)
          tprcp_old(i)  = tprcp(i)
          srflag_old(i) = srflag(i)
          do k=1, km
           stc_old(i,k) = stc(i,k)
          enddo
        endif
      enddo

C
CWei_FIX_3: you need to remove snow-rain detection here
C  For OSU LSM, the snow-rain detection is done inside gbphys routine !!
C
Clu_Rev6: snow-rain detection
C
C      DO I=1,IM
C        IF(FLAG(I).AND. flag_guess(i)) THEN
C          IF(SRFLAG(I) .EQ. 1.) THEN
C            SHELEG(i) = SHELEG(i) + 1.E3*TPRCP(i)
C            TPRCP(i)   = 0.
C          ENDIF
C        ENDIF
C      ENDDO
C
C  INITIALIZE VARIABLES. ALL UNITS ARE SUPPOSEDLY M.K.S. UNLESS SPECIFIE
C  PSURF IS IN PASCALS
C  WIND IS WIND SPEED, THETA1 IS ADIABATIC SURFACE TEMP FROM LEVEL 1
C  RHO IS DENSITY, QS1 IS SAT. HUM. AT LEVEL1 AND QSS IS SAT. HUM. AT
C  SURFACE
C  CONVERT SLRAD TO THE CIVILIZED UNIT FROM LANGLEY MINUTE-1 K-4
C  SURFACE ROUGHNESS LENGTH IS CONVERTED TO M FROM CM
C
!!
!     qs1 = fpvs(t1)
!     qss = fpvs(tskin)
      DO I=1,IM
        IF(flag_iter(i).and. FLAG(I)) THEN
        XRCL(I)  = SQRT(RCL(I))
        PSURF(I) = 1000. * PS(I)
        PS1(I)   = 1000. * PRSL1(I)
!       SLWD(I)  = SLRAD(I) * CONVRAD
        SLWD(I)  = SLRAD(I)
c
c  DLWFLX has been given a negative sign for downward longwave
c  snet is the net shortwave flux
c
        SNET(I) = -SLWD(I) - DLWFLX(I)
        WIND(I) = XRCL(I) * SQRT(U1(I) * U1(I) + V1(I) * V1(I))
     &              + MAX(0.0, MIN(DDVEL(I), 30.0))
        WIND(I) = MAX(WIND(I),1.)
        Q0(I) = MAX(Q1(I),1.E-8)
!       TSURF(I) = TSKIN(I)
        TSEA(I) = tsurf(I)                         !!! Cwei_q2m_iter
        THETA1(I) = T1(I) * PRSLKI(I)
        TV1(I) = T1(I) * (1. + RVRDM1 * Q0(I))
        THV1(I) = THETA1(I) * (1. + RVRDM1 * Q0(I))
        TVS(I) = TSEA(I) * (1. + RVRDM1 * Q0(I))
        RHO(I) = PS1(I) / (RD * TV1(I))
cjfe    QS1(I) = 1000. * FPVS(T1(I))
        qs1(i) = fpvs(t1(i))
        QS1(I) = EPS * QS1(I) / (PS1(I) + EPSM1 * QS1(I))
        QS1(I) = MAX(QS1(I), 1.E-8)
        Q0(I) = min(QS1(I),Q0(I))
cjfe    QSS(I) = 1000. * FPVS(TSEA(I))
        qss(i) = fpvs(tskin(i))                  !!! change to tsurf?
        QSS(I) = EPS * QSS(I) / (PSURF(I) + EPSM1 * QSS(I))
c       RS = PLANTR
        RS(I) = 0.
        if(VEGTYPE(I).gt.0.) RS(I) = rsmin(VEGTYPE(I))
        Z0(I) = .01 * Z0RL(i)
        CANOPY(I)= MAX(CANOPY(I),0.)
        DM(I) = 1.
        FACTSNW(I) = 10.
Clu     IF(SLIMSK(I).EQ.2.) FACTSNW(I) = 3.
C
C  SNOW DEPTH IN WATER EQUIVALENT IS CONVERTED FROM MM TO M UNIT
C
        SNOWD(I) = SHELEG(I) / 1000.
        FLAGSNW(I) = .FALSE.
C
C  WHEN SNOW DEPTH IS LESS THAN 1 MM, A PATCHY SNOW IS ASSUMED AND
C  SOIL IS ALLOWED TO INTERACT WITH THE ATMOSPHERE.
C  WE SHOULD EVENTUALLY MOVE TO A LINEAR COMBINATION OF SOIL AND
C  SNOW UNDER THE CONDITION OF PATCHY SNOW.
C
        IF(SNOWD(I).GT..001.OR.SLIMSK(I).EQ.2.) RS(I) = 0.
        IF(SNOWD(I).GT..001) FLAGSNW(I) = .TRUE.
C##DG  IF(LAT.EQ.LATD) THEN
C##DG    PRINT *, ' WIND,TV1,TVS,Q1,QS1,SNOW,SLIMSK=',
C##DG&   WIND,TV1,TVS,Q1,QS1,SNOWD,SLIMSK
C##DG    PRINT *, ' SNET, SLWD =', SNET, SLWD(I)
C##DG  ENDIF
       ENDIF
      ENDDO
!!
      DO I=1,IM
        IF(flag_iter(i).and. FLAG(I)) THEN
          ZSOIL(I,1) = -.10
       
          DO K = 2, KM
            ZSOIL(I,K) = ZSOIL(I,K-1)
     &                   + (-2. - ZSOIL(I,1)) / (KM - 1)
          ENDDO
CWei [+5] use the same soil layer structure as Noah if running with 4-layer
          if(km.gt.2)then
          ZSOIL(I,2) = -.40
          ZSOIL(I,3) = -1.0
          ZSOIL(I,4) = -2.0
          endif
        ENDIF
      ENDDO
!!
      DO I=1,IM
        IF(flag_iter(i).and. FLAG(I)) THEN
        Z1(I) = -RD * TV1(I) * LOG(PS1(I)/PSURF(I)) / G
        DRAIN(I) = 0.
        ENDIF
      ENDDO
!!
      DO K = 1, KM
        DO I=1,IM
          IF(flag_iter(i).and. FLAG(I)) THEN
          ET(I,K) = 0.
          RHSMC(I,K) = 0.
          AIM(I,K) = 0.
          BIM(I,K) = 1.
          CIM(I,K) = 0.
          STSOIL(I,K) = STC(I,K)
          ENDIF
        ENDDO
      ENDDO
      DO I=1,IM
        IF(flag_iter(i).and. FLAG(I)) THEN
        EDIR(I) = 0.
        EC(I) = 0.
        EVAP(I) = 0.
        HFLX(I) = 0.
        EP(I) = 0.
        SNOWMT(I) = 0.
        GFLUX(I) = 0.
        RHSCNPY(I) = 0.
        FX(I) = 0.
        ETPFAC(I) = 0.
        CANFAC(I) = 0.
Cwei added 10/24/2006
        EVBS(I)=0
        EVCW(I)=0
        TRANS(I)=0
        SBSNO(I)=0
        SNOWC(I)=0
        SNOHF(I)=0
        ENDIF
      ENDDO

C
C  RCP = RHO CP CH V
C
      DO I = 1, IM
        IF(flag_iter(i).and. FLAG(I)) THEN
        RCH(I) = RHO(I) * CP * CH(I) * WIND(I)
Cwei added 10/24/2006
        CMM(I)=CM(I)* WIND(I)
        CHH(I)=RHO(I)*CH(I)* WIND(I)
        ENDIF
      ENDDO

C
C  COMPUTE SOIL/SNOW/ICE HEAT FLUX IN PREPARATION FOR SURFACE ENERGY
C  BALANCE CALCULATION
C
      DO I = 1, IM
Clu     GFLUX(I) = 0.
        IF(flag_iter(i).and. FLAG(I)) THEN
          SMCZ(I) = .5 * (SMC(I,1) + .20)
          DFT0(I) = KTSOIL(SMCZ(I),SOILTYP(I))
Clu     ELSEIF(SLIMSK(I).EQ.2.) THEN
C  DF FOR ICE IS TAKEN FROM MAYKUT AND UNTERSTEINER
C  DF IS IN SI UNIT OF W K-1 M-1
Clu       DFT0(I) = 2.2
        ENDIF
      ENDDO
!!
      DO I=1,IM
        IF(flag_iter(i).and. FLAG(I)) THEN
C         IF(SNOWD(I).GT..001) THEN
          IF(FLAGSNW(I)) THEN
C
C  WHEN SNOW COVERED, GROUND HEAT FLUX COMES FROM SNOW
C
            TFLX = MIN(T1(I), TSEA(I))
            GFLUX(I) = -DFSNOW * (TFLX - STSOIL(I,1))
     &                 / (FACTSNW(I) * MAX(SNOWD(I),.001))
          ELSE
            GFLUX(I) = DFT0(I) * (STSOIL(I,1) - TSEA(I))
     &                 / (-.5 * ZSOIL(I,1))
          ENDIF
          GFLUX(I) = MAX(GFLUX(I),-200.)
          GFLUX(I) = MIN(GFLUX(I),+200.)
        ENDIF
      ENDDO
      DO I = 1, IM
        IF(flag_iter(i).and. FLAG(I)) THEN
        PARTLND(I) = 1.
        IF(SNOWD(I).GT.0..AND.SNOWD(I).LE..001) THEN
          PARTLND(I) = 1. - SNOWD(I) / .001
        ENDIF
        ENDIF
      ENDDO
      DO I = 1, IM
        IF(flag_iter(i).and. FLAG(I)) THEN
        SNOEVP(I) = 0.
        if(SNOWD(I).gt..001) PARTLND(I) = 0.
        ENDIF
      ENDDO
C
C  COMPUTE POTENTIAL EVAPORATION FOR LAND AND SEA ICE
C
      DO I = 1, IM
        IF(flag_iter(i).and. FLAG(I)) THEN
          T12 = T1(I) * T1(I)
          T14 = T12 * T12
C
C  RCAP = FNET - SIGMA T**4 + GFLX - RHO CP CH V (T1-THETA1)
C
          RCAP(I) = -SLWD(I) - SIGMA * T14 + GFLUX(I)
     &              - RCH(I) * (T1(I) - THETA1(I))
C
C  RSMALL = 4 SIGMA T**3 / RCH(I) + 1
C
          RSMALL(I) = 4. * SIGMA * T1(I) * T12 / RCH(I) + 1.
C
C  DELTA = L / CP * DQS/DT
C
          DELTA(I) = ELOCP * EPS * HVAP * QS1(I) / (RD * T12)
C
C  POTENTIAL EVAPOTRANSPIRATION ( WATTS / M**2 ) AND
C  POTENTIAL EVAPORATION
C
          TERM1(I) = ELOCP * RSMALL(I) * RCH(I)*(QS1(I)-Q0(I))
          TERM2(I) = RCAP(I) * DELTA(I)
          EP(I) = (ELOCP * RSMALL(I) * RCH(I) * (QS1(I) - Q0(I))
     &              + RCAP(I) * DELTA(I))
          EP(I) = EP(I) / (RSMALL(I) + DELTA(I))
        ENDIF
      ENDDO
C
C  ACTUAL EVAPORATION OVER LAND IN THREE PARTS : EDIR, ET, AND EC
C
C  DIRECT EVAPORATION FROM SOIL, THE UNIT GOES FROM M S-1 TO KG M-2 S-1
C
      DO I = 1, IM
        FLAG(I) = SLIMSK(I).EQ.1..AND.EP(I).GT.0.
      ENDDO
      DO I = 1, IM
        IF(flag_iter(i))THEN
        IF(FLAG(I)) THEN
          DF1(I) = FUNCDF(SMC(I,1),SOILTYP(I))
          KT1(I) = FUNCKT(SMC(I,1),SOILTYP(I))
        endif
        if(FLAG(I).and.STC(I,1).lt.t0c) then
          DF1(I) = 0.
          KT1(I) = 0.
        endif
        IF(FLAG(I)) THEN
c         TREF = .75 * THSAT(SOILTYP(I))
          TREF(I) = smref(SOILTYP(I))
c         TWILT = TWLT(SOILTYP(I))
          TWILT(I) = smwlt(SOILTYP(I))
          smcdry = smdry(SOILTYP(I))
c         FX(I) = -2. * DF1(I) * (SMC(I,1) - .23) / ZSOIL(I,1)
c    &            - KT1(I)
          FX(I) = -2. * DF1(I) * (SMC(I,1) - smcdry) / ZSOIL(I,1)
     &            - KT1(I)
          FX(I) = MIN(FX(I), EP(I)/HVAP)
          FX(I) = MAX(FX(I),0.)
C
C  SIGMAF IS THE FRACTION OF AREA COVERED BY VEGETATION
C
          EDIR(I) = FX(I) * (1. - SIGMAF(I)) * PARTLND(I)
        ENDIF
        endif
      ENDDO
c
c  calculate stomatal resistance
c
      DO I = 1, IM
        if(flag_iter(i).and.FLAG(I)) then
c
c  resistance due to PAR. We use net solar flux as proxy at the present time
c
          ff = .55 * 2. * SNET(I) / rgl(VEGTYPE(I))
          rcs = (ff + RS(I)/rsmax(VEGTYPE(I))) / (1. + ff)
          rcs = max(rcs,.0001)
          rct = 1.
          rcq = 1.
c
c  resistance due to thermal effect
c
c         rct = 1. - .0016 * (topt - theta1) ** 2
c         rct = max(rct,.0001)
c
c  resistance due to humidity
c
c         rcq = 1. / (1. + hs(VEGTYPE(I)) * (QS1(I) - Q0(I)))
c         rcq = max(rcq,.0001)
c
c  compute resistance without the effect of soil moisture
c
          RS(I) = RS(I) / (rcs * rct * rcq)
        endif
      ENDDO
C
C  TRANSPIRATION FROM ALL LEVELS OF THE SOIL
C
      DO I = 1, IM
        IF(flag_iter(i).and.FLAG(I)) THEN
          CANFAC(I) = (CANOPY(I) / SCANOP) ** CFACTR
          ETPFAC(I) = SIGMAF(I)
     &           * (1. - CANFAC(I)) / HVAP
          GX(I) = (SMC(I,1) - TWILT(I)) / (TREF(I) - TWILT(I))
          GX(I) = MAX(GX(I),0.)
          GX(I) = MIN(GX(I),1.)
c
c  resistance due to soil moisture deficit
c
          rss = GX(I) * (ZSOIL(I,1) / ZSOIL(I,km))
          rss = max(rss,.0001)
          RSI = RS(I) / rss
c
c  transpiration a la Monteith
c
          eth = (TERM1(I) + TERM2(I)) / 
     &          (DELTA(I) + RSMALL(I) * (1. + RSI * CH(I) * WIND(I)))
          ET(I,1) = ETPFAC(I) * eth
     &            * PARTLND(I)
        ENDIF
      ENDDO
!!
      DO K = 2, KM
        DO I=1,IM
          IF(flag_iter(i).and.FLAG(I)) THEN
            GX(I) = (SMC(I,K) - TWILT(I)) / (TREF(I) - TWILT(I))
            GX(I) = MAX(GX(I),0.)
            GX(I) = MIN(GX(I),1.)
c
c  resistance due to soil moisture deficit
c
          rss = GX(I) * ((ZSOIL(I,k) - ZSOIL(I,k-1))/ZSOIL(I,km))
          rss = max(rss,1.e-6)
          RSI = RS(I) / rss
c
c  transpiration a la Monteith
c
          eth = (TERM1(I) + TERM2(I)) / 
     &          (DELTA(I) + RSMALL(I) * (1. + RSI * CH(I) * WIND(I)))
            ET(I,K) = eth
     &               * ETPFAC(I) * PARTLND(I)
          ENDIF
        ENDDO
      ENDDO
!!
 400  CONTINUE
C
C  CANOPY RE-EVAPORATION
C
      DO I=1,IM
        IF(flag_iter(i).and.FLAG(I)) THEN
          EC(I) = SIGMAF(I) * CANFAC(I) * EP(I) / HVAP
          EC(I) = EC(I) * PARTLND(I)
          EC(I) = min(EC(I),CANOPY(I)/delt)
        ENDIF
      ENDDO
C
C  SUM UP TOTAL EVAPORATION
C
      DO I = 1, IM
        IF(flag_iter(i).and.FLAG(I)) THEN
         EVAP(I) = EDIR(I) + EC(I)
        ENDIF
      ENDDO
!!
      DO K = 1, KM
        DO I=1,IM
          IF(flag_iter(i).and.FLAG(I)) THEN
            EVAP(I) = EVAP(I) + ET(I,K)
          ENDIF
        ENDDO
      ENDDO
!!
C
C  RETURN EVAP UNIT FROM KG M-2 S-1 TO WATTS M-2
C
      DO I=1,IM
        IF(flag_iter(i).and.FLAG(I)) THEN
          EVAP(I) = MIN(EVAP(I)*HVAP,EP(I))
        ENDIF
      ENDDO
C##DG  IF(LAT.EQ.LATD) THEN
C##DG    PRINT *, 'FX(I), SIGMAF, EDIR(I), ETPFAC=', FX(I)*HVAP,SIGMAF,
C##DG&          EDIR(I)*HVAP,ETPFAC*HVAP
C##DG    PRINT *, ' ET =', (ET(K)*HVAP,K=1,KM)
C##DG    PRINT *, ' CANFAC(I), EC(I), EVAP', CANFAC(I),EC(I)*HVAP,EVAP
C##DG  ENDIF

C
C  TREAT DOWNWARD MOISTURE FLUX SITUATION
C  (EVAP WAS PRESET TO ZERO SO NO UPDATE NEEDED)
C  DEW IS CONVERTED FROM KG M-2 TO M TO CONFORM TO PRECIP UNIT
C
      DO I = 1, IM
        FLAG(I) = SLIMSK(I).EQ.1..AND.EP(I).LE.0.
        DEW(I) = 0.
      ENDDO
      DO I = 1, IM
        IF(flag_iter(i).and.FLAG(I)) THEN
          DEW(I) = -EP(I) * DELT / (HVAP * RHOH2O)
          EVAP(I) = EP(I)
          DEW(I) = DEW(I) * PARTLND(I)
          EVAP(I) = EVAP(I) * PARTLND(I)
          DM(I) = 1.
        ENDIF
      ENDDO
C
C  SNOW COVERED LAND 
C
      DO I = 1, IM
        FLAG(I) = SLIMSK(I).EQ.1..AND.SNOWD(I).GT.0.
      ENDDO
C
C  CHANGE OF SNOW DEPTH DUE TO EVAPORATION OR SUBLIMATION
C
C  CONVERT EVAP FROM KG M-2 S-1 TO M S-1 TO DETERMINE THE REDUCTION OF S
C
      DO I = 1, IM
        IF(flag_iter(i).and.FLAG(I)) THEN
          BFACT = SNOWD(I) / (DELT * EP(I) / (HVAP * RHOH2O))
          BFACT = MIN(BFACT,1.)
C
C  THE EVAPORATION OF SNOW
C
          IF(EP(I).LE.0.) BFACT = 1.
          IF(SNOWD(I).LE..001) THEN
c           EVAP = (SNOWD(I)/.001)*BFACT*EP(I) + EVAP
!           SNOEVP(I) = bfact * EP(I) * (1. - PARTLND(I))
!           EVAP = EVAP + SNOEVP(I)
            SNOEVP(I) = bfact * EP(I)
!           EVAP   = EVAP + SNOEVP(I) * (1. - PARTLND(I))
            EVAP(I)=EVAP(I)+SNOEVP(I)*(1.-PARTLND(I))
          ELSE
c           EVAP(I) = BFACT * EP(I)
            SNOEVP(I) = bfact * EP(I)
            EVAP(I) = SNOEVP(I)
          ENDIF
          TSEA(I) = T1(I) +
     &          (RCAP(I) - GFLUX(I) - DFSNOW * (T1(I) - STSOIL(I,1))
     &           /(FACTSNW(I) * MAX(SNOWD(I),.001))
c    &           + THETA1 - T1
c    &           - BFACT * EP(I)) / (RSMALL(I) * RCH(I)
     &           - SNOEVP(I)) / (RSMALL(I) * RCH(I)
     &           + DFSNOW / (FACTSNW(I)* MAX(SNOWD(I),.001)))
c         SNOWD(I) = SNOWD(I) - BFACT * EP(I) * DELT / (RHOH2O * HVAP)
          SNOWD(I) = SNOWD(I) - SNOEVP(I) * delt / (rhoh2o * hvap)
          SNOWD(I) = MAX(SNOWD(I),0.)
        ENDIF
      ENDDO
C
C  SNOW MELT (M)
C
 500  CONTINUE
      DO I = 1, IM
        FLAG(I) = SLIMSK(I).EQ.1.
     &            .AND.SNOWD(I).GT..0
      ENDDO
      DO I = 1, IM
        IF(flag_iter(i))THEN
        IF(FLAG(I).AND.TSEA(I).GT.T0C) THEN
          SNOWMT(I) = RCH(I) * RSMALL(I) * DELT
     &              * (TSEA(I) - T0C) / (RHOH2O * HFUS)
          SNOWMT(I) = min(SNOWMT(I),SNOWD(I))
          SNOWD(I) = SNOWD(I) - SNOWMT(I)
          SNOWD(I) = MAX(SNOWD(I),0.)
          TSEA(I) = MAX(T0C,TSEA(I)
     &             -HFUS*SNOWMT(I)*RHOH2O/(RCH(I)*RSMALL(I)*DELT))
        ENDIF
        ENDIF
      ENDDO
c
c  We need to re-evaluate evaporation because of snow melt
c    the skin temperature is now bounded to 0 deg C
c
!     qss = fpvs(TSEA)
      DO I = 1, IM
        FLAG(I) = SLIMSK(I).EQ. 1.
        IF(flag_iter(i).and.FLAG(I))THEN
!       IF (SNOWD(I) .GT. 0.0) THEN
        IF (SNOWD(I) .GT. snomin) THEN
cjfe      QSS(I) = 1000. * FPVS(TSEA(I))
          qss(i) = fpvs(TSEA(i))
          QSS(I) = EPS * QSS(I) / (PSURF(I) + EPSM1 * QSS(I))
          EVAP(I) = elocp * RCH(I) * (QSS(I) - Q0(I))
        ENDIF
        ENDIF
      ENDDO
C
C  PREPARE TENDENCY TERMS FOR THE SOIL MOISTURE FIELD WITHOUT PRECIPITAT
C  THE UNIT OF MOISTURE FLUX NEEDS TO BECOME M S-1 FOR SOIL MOISTURE
C   HENCE THE FACTOR OF RHOH2O
C
      DO I = 1, IM
       IF(flag_iter(i))THEN
        if(FLAG(I)) then
          DF1(I) = FUNCDF(SMCZ(I),SOILTYP(I))
          KT1(I) = FUNCKT(SMCZ(I),SOILTYP(I))
        endif
        if(FLAG(I).and.STC(I,1).lt.t0c) then
          DF1(I) = 0.
          KT1(I) = 0.
        endif
        IF(FLAG(I)) THEN
          RHSCNPY(I) = -EC(I) + SIGMAF(I) * RHOH2O * DEW(I) / DELT
          SMCZ(I) = MAX(SMC(I,1), SMC(I,2))
          DMDZ(I) = (SMC(I,1) - SMC(I,2)) / (-.5 * ZSOIL(I,2))
          RHSMC(I,1) = (DF1(I) * DMDZ(I) + KT1(I)
     &        + (EDIR(I) + ET(I,1))) / (ZSOIL(I,1) * RHOH2O)
          RHSMC(I,1) = RHSMC(I,1) - (1. - SIGMAF(I)) * DEW(I) /
     &                 ( ZSOIL(I,1) * delt)
          DDZ(I) = 1. / (-.5 * ZSOIL(I,2))
C
C  AIM, BIM, AND CIM ARE THE ELEMENTS OF THE TRIDIAGONAL MATRIX FOR THE
C  IMPLICIT UPDATE OF THE SOIL MOISTURE
C
          AIM(I,1) = 0.
          BIM(I,1) = DF1(I) * DDZ(I) / (-ZSOIL(I,1) * RHOH2O)
          CIM(I,1) = -BIM(I,1)
        ENDIF
       ENDIF
      ENDDO
!!
      DO K = 2, KM
        IF(K.LT.KM) THEN
          DO I=1,IM
           IF(flag_iter(i))THEN
            IF(FLAG(I)) THEN
              DF2 = FUNCDF(SMCZ(I),SOILTYP(I))
              KT2(I) = FUNCKT(SMCZ(I),SOILTYP(I))
            ENDIF
            IF(FLAG(I).and.STC(I,k).lt.t0c) THEN
              df2 = 0.
              KT2(I) = 0.
            ENDIF
            IF(FLAG(I)) THEN
              DMDZ2(I) = (SMC(I,K) - SMC(I,K+1))
     &                   / (.5 * (ZSOIL(I,K-1) - ZSOIL(I,K+1)))
              SMCZ(I) = MAX(SMC(I,K), SMC(I,K+1))
              RHSMC(I,K) = (DF2 * DMDZ2(I) + KT2(I)
     &             - DF1(I) * DMDZ(I) - KT1(I) + ET(I,K))
     &                     / (RHOH2O*(ZSOIL(I,K) - ZSOIL(I,K-1)))
              DDZ2(I) = 2. / (ZSOIL(I,K-1) - ZSOIL(I,K+1))
              CIM(I,K) = -DF2 * DDZ2(I)
     &                / ((ZSOIL(I,K-1) - ZSOIL(I,K))*RHOH2O)
            ENDIF
           ENDIF
          ENDDO
        ELSE
          DO I = 1, IM
           IF(flag_iter(i))THEN
            IF(FLAG(I)) THEN
              KT2(I) = FUNCKT(SMC(I,K),SOILTYP(I))
            ENDIF
            if(FLAG(I).and.STC(I,k).lt.t0c) KT2(I) = 0.
            IF(FLAG(I)) THEN
              RHSMC(I,K) = (KT2(I)
     &             - DF1(I) * DMDZ(I) - KT1(I) + ET(I,K))
     &                     / (RHOH2O*(ZSOIL(I,K) - ZSOIL(I,K-1)))
              DRAIN(I) = KT2(I)
              CIM(I,K) = 0.
            ENDIF
           ENDIF
          ENDDO
        ENDIF
        DO I = 1, IM
          IF(flag_iter(i).and.FLAG(I)) THEN
            AIM(I,K) = -DF1(I) * DDZ(I)
     &                / ((ZSOIL(I,K-1) - ZSOIL(I,K))*RHOH2O)
            BIM(I,K) = -(AIM(I,K) + CIM(I,K))
            DF1(I) = DF2
            KT1(I) = KT2(I)
            DMDZ(I) = DMDZ2(I)
            DDZ(I) = DDZ2(I)
          ENDIF
        ENDDO
      ENDDO
!!
 600  CONTINUE
C
C  UPDATE SOIL TEMPERATURE
C
      DO I=1,IM
Clu     FLAG(I) = SLIMSK(I).NE.0.
        FLAG(I) = SLIMSK(I).EQ.1.
      ENDDO
C
C  SURFACE TEMPERATURE IS PART OF THE UPDATE WHEN SNOW IS ABSENT
C
      DO I=1,IM
C       IF(FLAG(I).AND.SNOWD(I).LE..001) THEN
       IF(flag_iter(i))THEN
        IF(FLAG(I).AND..NOT.FLAGSNW(I)) THEN
          YY(I) = T1(I) +
c    &          (RCAP(I)-GFLUX(I) + THETA1 - T1(I)
     &          (RCAP(I)-GFLUX(I) 
     &           - EVAP(I)) / (RSMALL(I) * RCH(I))
          ZZ(I) = 1. + DFT0(I) / (-.5 * ZSOIL(I,1) * RCH(I) * RSMALL(I))
          XX(I) = DFT0(I) * (STSOIL(I,1) - YY(I)) /
     &            (.5 * ZSOIL(I,1) * ZZ(I))
        ENDIF
C       IF(FLAG(I).AND.SNOWD(I).GT..001) THEN
        IF(FLAG(I).AND.FLAGSNW(I)) THEN
          YY(I) = STSOIL(I,1)
C
C  HEAT FLUX FROM SNOW IS EXPLICIT IN TIME
C
          ZZ(I) = 1.
          XX(I) = DFSNOW * (STSOIL(I,1) - TSEA(I))
     &            / (-FACTSNW(I) * MAX(SNOWD(I),.001))
        ENDIF
       ENDIF
      ENDDO
C
C  COMPUTE THE FORCING AND THE IMPLICIT MATRIX ELEMENTS FOR UPDATE
C
C  CH2O IS THE HEAT CAPACITY OF WATER AND CSOIL IS THE HEAT CAPACITY OF
C
      DO I = 1, IM
        IF(flag_iter(i).and.FLAG(I)) THEN
          SMCZ(I) = MAX(SMC(I,1), SMC(I,2))
          DTDZ1(I) = (STSOIL(I,1) - STSOIL(I,2)) / (-.5 * ZSOIL(I,2))
          DFT1(I) = KTSOIL(SMCZ(I),SOILTYP(I))
          HCPCT(I) = SMC(I,1) * CH2O + (1. - SMC(I,1)) * CSOIL
          DFT2(I) = DFT1(I)
          DDZ(I) = 1. / (-.5 * ZSOIL(I,2))
C
C  AI, BI, AND CI ARE THE ELEMENTS OF THE TRIDIAGONAL MATRIX FOR THE
C  IMPLICIT UPDATE OF THE SOIL TEMPERATURE
C
          AI(I,1) = 0.
          BI(I,1) = DFT1(I) * DDZ(I) / (-ZSOIL(I,1) * HCPCT(I))
          CI(I,1) = -BI(I,1)
          BI(I,1) = BI(I,1)
     &            + DFT0(I) / (.5 * ZSOIL(I,1) **2 * HCPCT(I) * ZZ(I))
C         SS = DFT0(I) * (STSOIL(I,1) - YY(I))
C    &         / (.5 * ZSOIL(I,1) * ZZ(I))
C         RHSTC(1) = (DFT1(I) * DTDZ1(I) - SS)
          RHSTC(I,1) = (DFT1(I) * DTDZ1(I) - XX(I))
     &                 / (ZSOIL(I,1) * HCPCT(I))
        ENDIF
      ENDDO
!!
      DO K = 2, KM
        DO I=1,IM
          IF(flag_iter(i).and.FLAG(I)) THEN
            HCPCT(I) = SMC(I,K) * CH2O + (1. - SMC(I,K)) * CSOIL
          ENDIF
        ENDDO
        IF(K.LT.KM) THEN
          DO I = 1, IM
            IF(flag_iter(i).and.FLAG(I)) THEN
              DTDZ2(I) = (STSOIL(I,K) - STSOIL(I,K+1))
     &                   / (.5 * (ZSOIL(I,K-1) - ZSOIL(I,K+1)))
              SMCZ(I) = MAX(SMC(I,K), SMC(I,K+1))
              DFT2(I) = KTSOIL(SMCZ(I),SOILTYP(I))
              DDZ2(I) = 2. / (ZSOIL(I,K-1) - ZSOIL(I,K+1))
              CI(I,K) = -DFT2(I) * DDZ2(I)
     &                / ((ZSOIL(I,K-1) - ZSOIL(I,K)) * HCPCT(I))
            ENDIF
          ENDDO
        ELSE
C
C  AT THE BOTTOM, CLIMATOLOGY IS ASSUMED AT 2M DEPTH FOR LAND AND
C  FREEZING TEMPERATURE IS ASSUMED FOR SEA ICE AT Z(KM)
          DO I = 1, IM
            IF(flag_iter(i).and.FLAG(I)) THEN
              DTDZ2(I) = (STSOIL(I,K) - TG3(I))
     &              / (.5 * (ZSOIL(I,K-1) + ZSOIL(I,K)) - ZBOT)
              DFT2(I) = KTSOIL(SMC(I,K),SOILTYP(I))
              CI(I,K) = 0.
            ENDIF
          ENDDO
        ENDIF
        DO I = 1, IM
          IF(flag_iter(i).and.FLAG(I)) THEN
            RHSTC(I,K) = (DFT2(I) * DTDZ2(I) - DFT1(I) * DTDZ1(I))
     &                 / ((ZSOIL(I,K) - ZSOIL(I,K-1)) * HCPCT(I))
            AI(I,K) = -DFT1(I) * DDZ(I)
     &                / ((ZSOIL(I,K-1) - ZSOIL(I,K)) * HCPCT(I))
            BI(I,K) = -(AI(I,K) + CI(I,K))
            DFT1(I) = DFT2(I)
            DTDZ1(I) = DTDZ2(I)
            DDZ(I) = DDZ2(I)
          ENDIF
        ENDDO
      ENDDO
!!
 700  CONTINUE
C
C  SOLVE THE TRI-DIAGONAL MATRIX
C
      DO K = 1, KM
        DO I=1,IM
          IF(flag_iter(i).and.FLAG(I))  THEN
            RHSTC(I,K) = RHSTC(I,K) * DELT2
            AI(I,K) = AI(I,K) * DELT2
            BI(I,K) = 1. + BI(I,K) * DELT2
            CI(I,K) = CI(I,K) * DELT2
          ENDIF
        ENDDO
      ENDDO
C  FORWARD ELIMINATION
      DO I=1,IM
        IF(flag_iter(i).and.FLAG(I)) THEN
          CI(I,1) = -CI(I,1) / BI(I,1)
          RHSTC(I,1) = RHSTC(I,1) / BI(I,1)
        ENDIF
      ENDDO
!!
      DO K = 2, KM
        DO I=1,IM
          IF(flag_iter(i).and.FLAG(I)) THEN
            CC = 1. / (BI(I,K) + AI(I,K) * CI(I,K-1))
            CI(I,K) = -CI(I,K) * CC
            RHSTC(I,K) = (RHSTC(I,K) - AI(I,K) * RHSTC(I,K-1)) * CC
          ENDIF
        ENDDO
      ENDDO
!!
C  BACKWARD SUBSTITUTTION
      DO I=1,IM
        IF(flag_iter(i).and.FLAG(I)) THEN
          CI(I,KM) = RHSTC(I,KM)
        ENDIF
      ENDDO
!!
      DO K = KM-1, 1
        DO I=1,IM
          IF(flag_iter(i).and.FLAG(I)) THEN
            CI(I,K) = CI(I,K) * CI(I,K+1) + RHSTC(I,K)
          ENDIF
        ENDDO
      ENDDO
C
C  UPDATE SOIL AND ICE TEMPERATURE
C
      DO K = 1, KM
        DO I=1,IM
          IF(flag_iter(i).and.FLAG(I)) THEN
            STSOIL(I,K) = STSOIL(I,K) + CI(I,K)
          ENDIF
        ENDDO
      ENDDO
C
C  UPDATE SURFACE TEMPERATURE FOR SNOW FREE SURFACES
C
      DO I=1,IM
        IF(flag_iter(i))THEN
        IF(FLAG(I).AND..NOT.FLAGSNW(I)) THEN
          TSEA(I) = (YY(I) + (ZZ(I) - 1.) * STSOIL(I,1)) / ZZ(I)
        ENDIF
        ENDIF
      ENDDO
!!
C
C  TIME FILTER FOR SOIL AND SKIN TEMPERATURE
C
      DO I=1,IM
        IF(flag_iter(i).and. FLAG(I)) THEN
          TSURF(I) = CTFIL1 * TSEA(I) + CTFIL2 * TSURF(I)
        ENDIF
      ENDDO
      DO K = 1, KM
        DO I=1,IM
          IF(flag_iter(i).and. FLAG(I)) THEN
            STC(I,K) = CTFIL1 * STSOIL(I,K) + CTFIL2 * STC(I,K)
          ENDIF
        ENDDO
      ENDDO
C
C  GFLUX CALCULATION
C
c     DO I=1,IM
c       FLAG(I) = SLIMSK(I).EQ.1.
c    &            .AND.FLAGSNW(I)
c     ENDDO
      DO I = 1, IM
        IF(flag_iter(i).and.FLAG(I)) THEN
         if(FLAGSNW(I))then
          GFLUX(I) = -DFSNOW * (TSURF(I) - STC(I,1))
     &               / (FACTSNW(I) * MAX(SNOWD(I),.001))
         else
          GFLUX(I) = DFT0(I) * (STC(I,1) - TSURF(I))
     &               / (-.5 * ZSOIL(I,1))
         endif
        ENDIF
      ENDDO
c     DO I = 1, IM
c       FLAG(I) = SLIMSK(I).EQ.1.
c       IF(flag_iter(i))THEN
c       IF(FLAG(I).AND..NOT.FLAGSNW(I)) THEN
c         GFLUX(I) = DFT0(I) * (STC(I,1) - TSURF(I))
c    &               / (-.5 * ZSOIL(I,1))
c       ENDIF
c       ENDIF
c     ENDDO
C
C  CALCULATE SENSIBLE HEAT FLUX
C
      DO I = 1, IM
        IF(flag_iter(i).and. FLAG(I)) THEN
        HFLX(I) = RCH(I) * (TSURF(I) - THETA1(I))
        ENDIF
      ENDDO
C
C  THE REST OF THE OUTPUT
C
      DO I = 1, IM
        IF(flag_iter(i).and. FLAG(I)) THEN
        QSURF(I) = Q1(I) + EVAP(I) / (ELOCP * RCH(I))
        DM(I) = 1.
C
Cwei added 10/24/2006
        EVBS(I)=EDIR(I)
        EVCW(I)=EC(I)
        SBSNO(I)=SNOEVP(I)
        SNOWC(I)=1-PARTLND(I)
          STM(I)=-1.0*SMC(I,1)*ZSOIL(I,1)
          SNOHF(I)=DFSNOW * (T1(I) - STSOIL(I,1))
          TRANS(I)=ET(I,1)
         DO K=2,KM
          STM(I)=STM(I)+SMC(I,K)*(ZSOIL(I,K-1)-ZSOIL(I,K))
          TRANS(I)=TRANS(I)+ET(I,K)
         ENDDO 
C  CONVERT SNOW DEPTH BACK TO MM OF WATER EQUIVALENT
C
        SHELEG(I) = SNOWD(I) * 1000.
        ENDIF
      ENDDO
!
      do i=1,im
        IF(flag_iter(i).and. FLAG(I)) THEN
        tem     = 1.0 / rho(i)
        hflx(i) = hflx(i) * tem * cpinv
        evap(i) = evap(i) * tem * hvapi
        ENDIF
      enddo

Clu_q2m_iter [+17L]: restore land-related prognostic fields for guess run
      do i=1, im
      IF(FLAG(I)) THEN
        if(flag_guess(i)) then
          sheleg(i) = sheleg_old(i)
          tskin(i)  = tskin_old(i)
          canopy(i) = canopy_old(i)
          tprcp(i)  = tprcp_old(i)
          srflag(i) = srflag_old(i)
          do k=1, km
           stc(i,k) = stc_old(i,k)
          enddo
         else
          tskin(i) = tsurf(i)
        endif
      ENDIF
      enddo

!
C##DG  IF(LAT.EQ.LATD) THEN
CC       RBAL = -SLWD-SIGMA*TSKIN**4+GFLUX
CC    &         -EVAP - HFLX
C##DG    PRINT 6000,HFLX,EVAP,GFLUX,
C##DG&             STC(1), STC(2),TSKIN,RNET,SLWD
C##DG    PRINT *, ' T1 =', T1
 6000 FORMAT(8(F8.2,','))
CC     PRINT *, ' EP, ETP,T2M(I) =', EP, ETP,T2M(I)
CC     PRINT *, ' FH, FH2 =', FH, FH2
CC     PRINT *, ' PH, PH2 =', PH, PH2
CC     PRINT *, ' CH, RCH =', CH, RCH
CC     PRINT *, ' TERM1, TERM2 =', TERM1, TERM2
CC     PRINT *, ' RS(I), PLANTR =', RS(I), PLANTR
C##DG  ENDIF
      RETURN
      END
