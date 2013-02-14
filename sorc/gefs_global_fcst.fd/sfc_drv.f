
      SUBROUTINE SFC_DRV(IM,KM,PS,U1,V1,T1,Q1,
     &               SHELEG,SNCOVR1,SNWDPH,TSKIN,QSURF,TPRCP,SRFLAG,
     &               SMC,STC,SLC,DM,SOILTYP,SIGMAF,VEGTYPE,CANOPY,
     &               DLWFLX,DSWSFC,SLRAD,DELT,TG3,GFLUX,CM,CH,
     &               RCL,PRSL1,PRSLKI,SLIMSK,
     &               DRAIN,EVAP,HFLX,EP,DDVEL,
     +               RUNOFF,SLOPETYP,SHDMIN,SHDMAX,SNOALB,SFALB,
     +               CMM,CHH,ZF,EVBS,EVCW,TRANS,SBSNO,
     +               SNOWC,STM,SNOHF,SMCWLT2,SMCREF2,
     +               tsurf, flag_iter, flag_guess)
!
      USE MACHINE , ONLY : kind_phys
      USE FUNCPHYS, ONLY : fpvs
      USE PHYSCONS, grav => con_g, HVAP => con_HVAP
     &,             CP => con_CP,  JCAL => con_JCAL
     &,             EPS => con_eps, EPSM1 => con_epsm1
     &,             RVRDM1 => con_FVirt, RD => con_RD
      implicit none
!
!     include 'constant.h'
!
      integer              IM, KM
      real(kind=kind_phys), parameter :: cpinv=1.0/cp, HVAPI=1.0/HVAP
      real(kind=kind_phys) DELT
      INTEGER              SOILTYP(IM),  VEGTYPE(IM)
     &,                    SLOPETYP(IM)

      real(kind=kind_phys) PS(IM),       U1(IM),       V1(IM),
     &                     T1(IM),       Q1(IM),       SHELEG(IM),
     &                     SNWDPH(IM),   TSKIN(IM),    QSURF(IM),
     &                     TPRCP(IM),    SRFLAG(IM),
     &                     SMC(IM,KM),   STC(IM,KM),   SLC(IM,KM), 
     &                     DM(IM),       SIGMAF(IM),   CANOPY(IM),
     &                     DLWFLX(IM),   SLRAD(IM),    TG3(IM),
     &                     GFLUX(IM),    CM(IM),       CH(IM),
     &                     RCL(IM),      PRSL1(IM),    PRSLKI(IM),
     &                     SLIMSK(IM),   DRAIN(IM),    EVAP(IM),
     &                     HFLX(IM),     EP(IM),
     &                     DDVEL(IM),    RUNOFF(IM),
     &                     SHDMIN(IM),   SHDMAX(IM),
     &                     SNOALB(IM),   SFALB(IM)
     +,                    tsurf(im),   sncovr1(im),DSWSFC(im)  

Cwei added 10/24/2006
     &, CHH(IM),CMM(IM),ZF(IM),EVBS(IM),EVCW(IM)
     &, TRANS(IM),SBSNO(IM),SNOWC(IM),STM(IM)
     &, SNOHF(IM),SMCWLT2(IM),SMCREF2(IM)

      logical              flag_iter(im), flag_guess(im), FLAG(IM)
!
!     Locals -- GFS
!
      integer              k,i
      real(kind=kind_phys) PSURF(IM),    PS1(IM),    RCH(IM),   RHO(IM),
     &                     Q0(IM),       QS1(IM),    SLWD(IM),
     &                     THETA1(IM),   TV1(IM),    XRCL(IM),  
     &                     WIND(IM),     ZSOIL(IM,KM)
!
!     land-related prognostic fields
!
      real(kind=kind_phys) SHELEG_OLD(IM),SNWDPH_OLD(IM),
     +                     TPRCP_OLD(IM), SRFLAG_OLD(IM),
     +                     TSKIN_OLD(IM), CANOPY_OLD(IM),
     +                  SMC_OLD(IM,KM),STC_OLD(IM,KM),SLC_OLD(IM,KM)

      real(kind=kind_phys)  convrad, elocp, g,  rhoh2o, tem
!
!     Locals -- Noah
!
      integer              couple, ice, nsoil, nroot, slope, 
     +                     stype, vtype 
!
      real(kind=kind_phys) ET(KM),       ZSOIL_NOAH(4), SLDPTH(KM),
     &                     STSOIL(KM),   SMSOIL(KM),    SLSOIL(KM) 

      real(kind=kind_phys) A2, A3, A4, A23M4
!
      real(kind=kind_phys) alb, albedo, beta, chx, cmx, cmc,  
     &                     dew, drip, dqsdt2,
     &                     ec, edir, ett, eta, esnow, etp, 
     &                     flx1, flx2, flx3, ffrozp, lwdn,
     &                     pc, prcp,  ptu,
     &                     q2, q2sat, 
     &                     radflx, rc, rcs, rct, rcq, rcsoil, 
     &                     rsmin, runoff1, runoff2, runoff3, 
     &                     sfcspd, sfcprs, sfctmp, 
     &                     sheat, shdfac, shdmin1d, shdmax1d, 
     &                     smcwlt, smcdry, smcref, smcmax, 
     &                     sneqv, snoalb1d, snowh, snomlt, sncovr, 
     &                     soilw, soilm, ssoil, tsea,
     &                     th2, tbot, xlai, zlvl,swdn
!
cc
      PARAMETER (G=grav)
      PARAMETER (ELOCP=HVAP/CP)
      PARAMETER (RHOH2O=1000.,CONVRAD=JCAL*1.E4/60.)
CC    declare parameters needed for deriving noah variables
      PARAMETER (A2=17.2693882,A3=273.16,A4=35.86,
     &           A23M4=A2*(A3-A4) )
      DATA ZSOIL_NOAH/-0.1, -0.4, -1.0, -2.0/

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
          snwdph_old(i) = snwdph(i)
          tskin_old(i)  = tskin(i)
          canopy_old(i) = canopy(i)
          tprcp_old(i)  = tprcp(i)
          srflag_old(i) = srflag(i)
          do k=1, km
           smc_old(i,k) = smc(i,k)
           stc_old(i,k) = stc(i,k)
           slc_old(i,k) = slc(i,k)
          enddo
        endif
      enddo
C**
C**  INITIALIZATION BLOCK
C**
      DO I=1,IM
       IF(flag_iter(i).and. FLAG(I)) THEN
        DM(I) = 1.
        EP(I) = 0.
        EVAP(I) = 0.
        HFLX(I) = 0.
        GFLUX(I) = 0.
        DRAIN(I) = 0.
        CANOPY(I)= MAX(CANOPY(I),0.)
Cwei added 10/24/2006
        EVBS(I)=0
        EVCW(I)=0
        TRANS(I)=0
        SBSNO(I)=0
        SNOWC(I)=0
        SNOHF(I)=0
       ENDIF
      ENDDO

C**
C**   INITIALIZE VARIABLES 
C**
      DO I=1,IM
        IF(flag_iter(i).and. FLAG(I)) THEN
        XRCL(I)  = SQRT(RCL(I))
        WIND(I) = XRCL(I) * SQRT(U1(I) * U1(I) + V1(I) * V1(I))
     &            + MAX(0.0, MIN(DDVEL(I), 30.0))
        WIND(I) = MAX(WIND(I),1.)
        PSURF(I) = 1000. * PS(I)      !* convert sfc pressure from cb to Pa
        PS1(I)   = 1000. * PRSL1(I)
!       SLWD(I)  = SLRAD(I) * CONVRAD !*convert from cal cm-2 min-1 to w/m2
        SLWD(I)  = SLRAD(I)           !*rad flx in w/m2
        Q0(I) = MAX(Q1(I),1.E-8)      !* Q1=specific humidity at level 1  (Kg/Kg)
        THETA1(I) = T1(I) * PRSLKI(I) !* adiabatic temp at level 1 (K)
        TV1(I) = T1(I) * (1. + RVRDM1 * Q0(I))
        RHO(I) = PS1(I) / (RD * TV1(I))
        qs1(i) = fpvs(t1(i))         !* qs1=sat. humidity at level 1 (Kg/Kg)
        QS1(I) = EPS * QS1(I) / (PS1(I) + EPSM1 * QS1(I))
        QS1(I) = MAX(QS1(I), 1.E-8)
        Q0(I) = min(QS1(I),Q0(I))
       ENDIF
      ENDDO
!!
      DO I=1,IM
        IF(flag_iter(i).and. FLAG(I)) THEN
         DO K = 1, KM
           ZSOIL(I,K) = ZSOIL_NOAH(K)
         ENDDO
!CluX: skip QA(slope type)
!**      IF(SLOPETYP(I) .GT. 9) SLOPETYP(I) = 9   !<--- QA(SLOPE)
!**      IF(SLOPETYP(I) .EQ. 0) SLOPETYP(I) = 2   !<--- QA(SLOPE)
        ENDIF
      ENDDO

       
      DO I=1,IM
      IF(flag_iter(i).and. FLAG(I)) THEN
C**
C** GFS -> NOAH: PREPARE VARIABLES TO RUN NOAH LSM
C**
C   1. CONFIGURATION INFORMATION (C):
C   COUPLE     COUPLE-UNCOUPLE FLAG  (=1: COUPLED, =0: UNCOUPLED) 
C   FFROZP     FLAG FOR SNOW-RAIN DETECTION (1.=snow, 0.=rain)                
C   ICE        SEA-ICE FLAG  (=1: SEA-ICE, =0: LAND)
C   DT         TIMESTEP (SEC) (DT SHOULD NOT EXCEED 3600 SECS) = delt
C   ZLVL       HEIGHT (M) ABOVE GROUND OF ATMOSPHERIC FORCING VARIABLES
C   NSOIL      NUMBER OF SOIL LAYERS (AT LEAST 2)
C   SLDPTH     THE THICKNESS OF EACH SOIL LAYER (M)
        COUPLE = 1             !!<---  run noah lsm in 'couple' mode
        IF(SRFLAG(I) .EQ. 1.) FFROZP = 1.   !... snow phase
        IF(SRFLAG(I) .EQ. 0.) FFROZP = 0.   !... rain phase
        ICE = 0
        ZLVL = -RD * TV1(I) * LOG(PS1(I)/PSURF(I)) / G    !! Z1 for OSU
Cwei added 10/24/2006
        zf(i)=-RD * TV1(I) * LOG(PS1(I)/PSURF(I)) / G

        NSOIL = km
        SLDPTH(1) = - ZSOIL(I,1)
        DO K = 2,KM
         SLDPTH(K) = ZSOIL(I,K-1) - ZSOIL(I,K)
        END DO

C   2. FORCING DATA (F):
C   LWDN       LW DOWNWARD RADIATION (W M-2; POSITIVE, NOT NET LONGWAVE)
C   RADFLX     RADIATION FLUX (SOLDN or FDOWN)             
C   SFCPRS     PRESSURE AT HEIGHT ZLVL ABOVE GROUND (PASCALS)
C   PRCP       PRECIP RATE (KG M-2 S-1)
C   SFCTMP     AIR TEMPERATURE (K) AT HEIGHT ZLVL ABOVE GROUND
C   TH2        AIR POTENTIAL TEMPERATURE (K) AT HEIGHT ZLVL ABOVE GROUND
C   Q2         MIXING RATIO AT HEIGHT ZLVL ABOVE GROUND (KG KG-1)
       LWDN = dlwflx(I)           !..downward lw flux at sfc in w/m2
       SWDN = DSWSFC(I)           !..downward sw flux at sfc in w/m2
       RADFLX = -1.*SLWD(I)       !..net downward rad flx at sfc in w/m2
       SFCPRS = PS1(I) 
       PRCP = RHOH2O * TPRCP(I) / DELT
       SFCTMP = T1(I)  
       TH2 = THETA1(I)
       Q2 = Q0(I)

C   3. OTHER FORCING (INPUT) DATA (I):
C   SFCSPD     WIND SPEED (M S-1) AT HEIGHT ZLVL ABOVE GROUND
C   Q2SAT      SAT MIXING RATIO AT HEIGHT ZLVL ABOVE GROUND (KG KG-1)
C   DQSDT2     SLOPE OF SAT SPECIFIC HUMIDITY CURVE AT T=SFCTMP (KG KG-1 K-1)
       SFCSPD = WIND(I)
       Q2SAT =  QS1(I)
       DQSDT2 = Q2SAT * A23M4/(SFCTMP-A4)**2

C   4. CANOPY/SOIL CHARACTERISTICS (S):
C   VEGTYP     VEGETATION TYPE (INTEGER INDEX)                       -> VTYPE
C   SOILTYP    SOIL TYPE (INTEGER INDEX)                             -> STYPE
C   SLOPETYP   CLASS OF SFC SLOPE (INTEGER INDEX)                    -> SLOPE
C   SHDFAC     AREAL FRACTIONAL COVERAGE OF GREEN VEGETATION (0.0-1.0)
C   SHDMIN     MINIMUM AREAL FRACTIONAL COVERAGE OF GREEN VEGETATION -> SHDMIN1D
C   PTU        PHOTO THERMAL UNIT (PLANT PHENOLOGY FOR ANNUALS/CROPS)
C   ALB        BACKROUND SNOW-FREE SURFACE ALBEDO (FRACTION)
C   SNOALB     UPPER BOUND ON MAXIMUM ALBEDO OVER DEEP SNOW          -> SNOALB1D
C   TBOT       BOTTOM SOIL TEMPERATURE (LOCAL YEARLY-MEAN SFC AIR TEMP)
       VTYPE = VEGTYPE(I)
       STYPE = SOILTYP(I)
       SLOPE = SLOPETYP(I)
       SHDFAC = SIGMAF(I)
!CluX skip the scaling
!*     SHDMIN1D = 0.01 * SHDMIN(I)       !..convert from percent to fraction
!*     SHDMAX1D = 0.01 * SHDMAX(I)       !..convert from percent to fraction
!*     SNOALB1D = 0.01 * SNOALB(I)       !..convert from percent to fraction
       SHDMIN1D = SHDMIN(I)   
       SHDMAX1D = SHDMAX(I)     
       SNOALB1D = SNOALB(I)    
       PTU = 0.
       ALB = SFALB(I)
       TBOT = TG3(I)

C   5. HISTORY (STATE) VARIABLES (H):
C   CMC         CANOPY MOISTURE CONTENT (M)
C   T1          GROUND/CANOPY/SNOWPACK) EFFECTIVE SKIN TEMPERATURE (K) -> TSEA
C   STC(NSOIL)  SOIL TEMP (K)                                         -> STSOIL
C   SMC(NSOIL)  TOTAL SOIL MOISTURE CONTENT (VOLUMETRIC FRACTION)     -> SMSOIL
C   SH2O(NSOIL) UNFROZEN SOIL MOISTURE CONTENT (VOLUMETRIC FRACTION)  -> SLSOIL
C   SNOWH       ACTUAL SNOW DEPTH (M)
C   SNEQV       LIQUID WATER-EQUIVALENT SNOW DEPTH (M)
C   ALBEDO      SURFACE ALBEDO INCLUDING SNOW EFFECT (UNITLESS FRACTION)
C   CH          SURFACE EXCHANGE COEFFICIENT FOR HEAT AND MOISTURE (M S-1) -> CHX
C0  CM          SURFACE EXCHANGE COEFFICIENT FOR MOMENTUM (M S-1)          -> CMX
       CMC = CANOPY(I)/1000.              !.. convert from mm to m
!**    TSEA = TSKIN(I)
       TSEA = tsurf(I)                    !!! Clu_q2m_iter
       DO K = 1, KM
       STSOIL(K) = STC(I,K)
       SMSOIL(K) = SMC(I,K)
       SLSOIL(K) = SLC(I,K)
       END DO
       SNOWH = SNWDPH(I) / 1000.          !.. convert from mm to m
       SNEQV = SHELEG(I) / 1000.          !.. convert from mm to m
       IF(SNEQV .NE. 0. .AND. SNOWH .EQ. 0.) THEN   !<--- QA
          SNOWH = 10. * SNEQV
       ENDIF
! Added by Moorthi
!      IF(SNEQV .GT. 0. .AND. SNOWH .LT. SNEQV) THEN
!         SNOWH = 5. * SNEQV
!      ENDIF
!  up to here
       CHX = CH(I) * WIND(I)              ! compute conductance
       CMX = CM(I) * WIND(I)              ! compute conductance
Cwei added 10/24/2005
       CHH(I) = RHO(I)*CH(I) * WIND(I)              ! compute conductance
       CMM(I) = CM(I) * WIND(I)              ! compute conductance

C**
C** CALL NOAH LSM
C**

       CALL  SFLX (
     +  COUPLE,
!*   C  FFROZP,ICE,DT,ZLVL,NSOIL,SLDPTH,
     C  FFROZP,ICE,DELT,ZLVL,NSOIL,SLDPTH,
     F  LWDN,SWDN,RADFLX,SFCPRS,PRCP,SFCTMP,Q2,SFCSPD,
     I  TH2,Q2SAT,DQSDT2,
!*   S  VEGTYP,SOILTYP,SLOPETYP,SHDFAC,SHDMIN,PTU,ALB,SNOALB,TBOT,
     S  VTYPE,STYPE,SLOPE,SHDFAC,SHDMIN1D,PTU,ALB,SNOALB1D,TBOT,
!*   H  CMC,T1,STC,SMC,SH2O,SNOWH,SNEQV,ALBEDO,CH,CM,
     H  CMC,TSEA,STSOIL,SMSOIL,SLSOIL,SNOWH,SNEQV,
     H  ALBEDO,CHX,CMX,
     O  ETA,SHEAT,
     O  EC,EDIR,ET,ETT,ESNOW,DRIP,DEW,
     O  BETA,ETP,SSOIL,
     O  FLX1,FLX2,FLX3,
     O  SNOMLT,SNCOVR,
     O  RUNOFF1,RUNOFF2,RUNOFF3,
     O  RC,PC,RSMIN,XLAI,RCS,RCT,RCQ,RCSOIL,
     D  SOILW,SOILM,
     P  SMCWLT,SMCDRY,SMCREF,SMCMAX,NROOT)

C**
C**  NOAH -> GFS: PREPARE VARIABLES FOR RETURN TO PARENT MODE
C**
C   6. OUTPUT (O):
C   ETA        ACTUAL LATENT HEAT FLUX (W M-2: POSITIVE, IF UPWARD FROM SFC)
C   SHEAT      SENSIBLE HEAT FLUX (W M-2: POSITIVE, IF UPWARD FROM SFC)
C   BETA       RATIO OF ACTUAL/POTENTIAL EVAP (DIMENSIONLESS)
C   ETP        POTENTIAL EVAPORATION (W M-2)
C   SSOIL      SOIL HEAT FLUX (W M-2: NEGATIVE IF DOWNWARD FROM SURFACE)
C   RUNOFF1    SURFACE RUNOFF (M S-1), NOT INFILTRATING THE SURFACE
C   RUNOFF2    SUBSURFACE RUNOFF (M S-1), DRAINAGE OUT BOTTOM
       EVAP(I) = ETA
       HFLX(I) = SHEAT
       GFLUX(I) = SSOIL
Cwei added 10/24/2006
        EVBS(I)=EDIR
        EVCW(I)=EC
        TRANS(I)=ETT
        SBSNO(I)=ESNOW
        SNOWC(I)=SNCOVR
        STM(I)=SOILM
        SNOHF(I)=FLX1+FLX2+FLX3
        SMCWLT2(I)=SMCWLT
        SMCREF2(I)=SMCREF
        

       EP(I) = ETP
!*     TSKIN(I) = TSEA
       tsurf(I) = TSEA                           !!! Clu_q2m_iter
       DO K = 1, KM
       STC(I,K) = STSOIL(K) 
       SMC(I,K) = SMSOIL(K)
       SLC(I,K) = SLSOIL(K)
       END DO

C*     UNIT CONVERSION (FROM M S-1 to MM S-1)
       RUNOFF(I) = RUNOFF1 * 1000.   
       DRAIN(I) = RUNOFF2 * 1000.   

C*     UNIT CONVERSION (FROM M to MM)
       CANOPY(I) = CMC * 1000.
       SNWDPH(I) = SNOWH * 1000.
       SHELEG(I) = SNEQV * 1000.
       sncovr1(i)=sncovr

C   Do not return the following output fields to parent model
C   EC         CANOPY WATER EVAPORATION (M S-1)
C   EDIR       DIRECT SOIL EVAPORATION (M S-1)
C   ET(NSOIL)  PLANT TRANSPIRATION FROM A PARTICULAR ROOT LAYER (M S-1)
C   ETT        TOTAL PLANT TRANSPIRATION (M S-1)
C   ESNOW      SUBLIMATION FROM (OR DEPOSITION TO IF <0) SNOWPACK (M S-1)
C   DRIP       THROUGH-FALL OF PRECIP AND/OR DEW IN EXCESS OF CANOPY
C              WATER-HOLDING CAPACITY (M)
C   DEW        DEWFALL (OR FROSTFALL FOR T<273.15) (M)
C   BETA       RATIO OF ACTUAL/POTENTIAL EVAP (DIMENSIONLESS)
C   FLX1       PRECIP-SNOW SFC (W M-2)
C   FLX2       FREEZING RAIN LATENT HEAT FLUX (W M-2)
C   FLX3       PHASE-CHANGE HEAT FLUX FROM SNOWMELT (W M-2)
C   SNOMLT     SNOW MELT (M) (WATER EQUIVALENT)
C   SNCOVR     FRACTIONAL SNOW COVER (UNITLESS FRACTION, 0-1)
C   RUNOFF3    NUMERICAL TRUNCTATION IN EXCESS OF POROSITY (SMCMAX)
C              FOR A GIVEN SOIL LAYER AT THE END OF A TIME STEP
C   RC         CANOPY RESISTANCE (S M-1)
C   PC         PLANT COEFFICIENT (UNITLESS FRACTION, 0-1) WHERE PC*ETP
C             = ACTUAL TRANSP
C   XLAI       LEAF AREA INDEX (DIMENSIONLESS)
C   RSMIN      MINIMUM CANOPY RESISTANCE (S M-1)
C   RCS        INCOMING SOLAR RC FACTOR (DIMENSIONLESS)
C   RCT        AIR TEMPERATURE RC FACTOR (DIMENSIONLESS)
C   RCQ        ATMOS VAPOR PRESSURE DEFICIT RC FACTOR (DIMENSIONLESS)
C   RCSOIL     SOIL MOISTURE RC FACTOR (DIMENSIONLESS)
C   SOILW      AVAILABLE SOIL MOISTURE IN ROOT ZONE (UNITLESS FRACTION
C              BETWEEN SMCWLT AND SMCMAX)
C   SOILM      TOTAL SOIL COLUMN MOISTURE CONTENT (FROZEN+UNFROZEN) (M)
C   SMCWLT     WILTING POINT (VOLUMETRIC)
C   SMCDRY     DRY SOIL MOISTURE THRESHOLD WHERE DIRECT EVAP FRM TOP
C              LAYER ENDS (VOLUMETRIC)
C   SMCREF     SOIL MOISTURE THRESHOLD WHERE TRANSPIRATION BEGINS TO
C              STRESS (VOLUMETRIC)
C   SMCMAX     POROSITY, I.E. SATURATED VALUE OF SOIL MOISTURE
C             (VOLUMETRIC)
C   NROOT      NUMBER OF ROOT LAYERS, A FUNCTION OF VEG TYPE, DETERMINED
C              IN SUBROUTINE REDPRM.

      ENDIF
      ENDDO

C
C     COMPUTE QSURF (specific humidity at sfc)
C
      DO I = 1, IM
        IF(flag_iter(i).and. FLAG(I)) THEN
         RCH(I) = RHO(I) * CP * CH(I) * WIND(I)
         QSURF(I) = Q1(I) + EVAP(I) / (ELOCP * RCH(I))
         DM(I) = 1.
        ENDIF
      ENDDO
C
C
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
          snwdph(i) = snwdph_old(i)
          tskin(i)  = tskin_old(i)
          canopy(i) = canopy_old(i)
          tprcp(i)  = tprcp_old(i)
          srflag(i) = srflag_old(i)
          do k=1, km
           smc(i,k) = smc_old(i,k)
           stc(i,k) = stc_old(i,k)
           slc(i,k) = slc_old(i,k)
          enddo
         else
          tskin(i) = tsurf(i)    
        endif
      ENDIF
      enddo

      RETURN
      END

