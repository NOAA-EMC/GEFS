      MODULE HCON
 
      USE MACHINE
      IMPLICIT NONE
      SAVE
 
 
      real (kind=kind_rad) AMOLWT, CSUBP,    DIFFCTR,  G, GINV
     &,                     GRAVDR, O3DIFCTR, P0,       P0INV
     &,                     GP0INV, P0XZP2,   P0XZP8,   P0X2
     &,                     RADCON, RADCON1,  RATCO2MW, RATH2OMW
     &,                     RGASK,  RGASSP,   SECPDA
!
!***THE FOLLOWING DATA ARE LEVEL-INDEPENDENT
      DATA G/980.665/
!
      real (kind=kind_rad) HUNDRED, HNINETY, SIXTY, FIFTY, TEN, EIGHT
     &,                     FIVE,    FOUR,    THREE, TWO,   ONE, HAF
     &,                     QUARTR,  ZERO
!
      real (kind=kind_rad) H83E26,   H71E26,  H1E15 ,  H1E13,   H1E11
     &,                     H1E8,     H2E6,    H1E6,    H69766E5,H4E5
     &,                     H165E5,   H5725E4, H488E4,  H1E4,    H24E3
     &,                     H20788E3, H2075E3, H18E3,   H1224E3
     &,                     H67390E2, H5E2,    H3082E2, H3E2,    H2945E2
     &,                     H29316E2, H26E2,   H25E2,   H23E2,   H2E2
     &,                     H15E2,    H1386E2, H1036E2, H8121E1, H35E1
     &,                     H3116E1,  H28E1,   H181E1,  H18E1,   H161E1
     &,                     H16E1,    H1226E1, H9P94,   H6P08108,H3P6
     &,                     H3P5,     H2P9,    H2P8,    H2P5,    H1P8
     &,                     H1P4387,  H1P41819,H1P4,    H1P25892,H1P082
     &,                     HP816,    HP805,   HP8,     HP60241
     &,                     HP602409, HP6,     HP526315,HP518,   HP5048
     &,                     HP3795,   HP369,   HP26,    HP228,   HP219
     &,                     HP166666, HP144,   HP118666,HP1
       real (kind=kind_rad) H658M2,  H625M2,   H44871M2, H44194M2
     &,                     H42M2,   H41666M2, H28571M2, H2118M2
     &,                     H129M2,  H1M2,     H559M3,   H3M3
     &,                     H235M3,  H1M3,     H987M4,   H323M4
     &,                     H3M4,    H285M4,   H1M4,     H75826M4
     &,                     H6938M5, H394M5,   H37412M5, H15M5
     &,                     H1439M5, H128M5,   H102M5,   H1M5
     &,                     H7M6,    H4999M6,  H451M6,   H25452M6
     &,                     H1M6,    H391M7,   H1174M7,  H8725M8
     &,                     H327M8,  H257M8,   H1M8,     H23M10
     &,                     H14M10,  H11M10,   H1M10,    H83M11
     &,                     H82M11,  H8M11,    H77M11,   H72M11
     &,                     H53M11 , H48M11,   H44M11,   H42M11
     &,                     H37M11,  H35M11,   H32M11,   H3M11
     &,                     H28M11,  H24M11,   H23M11,   H2M11
     &,                     H18M11,  H15M11,   H14M11,   H114M11
     &,                     H11M11,  H1M11,    H96M12,   H93M12
     &,                     H77M12,  H74M12,   H65M12,   H62M12
     &,                     H6M12,   H45M12,   H44M12,   H4M12
     &,                     H38M12,  H37M12,   H3M12,    H29M12
     &,                     H28M12,  H24M12,   H21M12,   H16M12
     &,                     H14M12,  H12M12,   H8M13,    H46M13
     &,                     H36M13,  H135M13,  H12M13,   H1M13
     &,                     H3M14,   H15M14,   H14M14,   H101M16
     &,                     H1M16,   H1M17,    H1M18,    H1M19
     &,                     H1M20,   H1M21,    H1M22,    H1M23
     &,                     H1M24,   H26M30,   H14M30,   H25M31
     &,                     H21M31,  H12M31,   H9M32,    H55M32
     &,                     H45M32,  H4M33,    H62M34
!
      real (kind=kind_rad) HM2M2,    HM6666M2, HMP5,     HMP575
     &,                     HMP66667, HMP805,   HM1EZ,    HM13EZ
     &,                     HM19EZ,   HM1E1,    HM1597E1, HM161E1
     &,                     HM1797E1, HM181E1,  HM8E1,    HM1E2
!
      END MODULE HCON
 
      MODULE RNDDTA
 
      USE MACHINE
      IMPLICIT NONE
 
       SAVE
!
      integer nblw, nblx, nbly, nblm
!
      PARAMETER (NBLW=163,NBLX=47,NBLY=15)
      PARAMETER (NBLM=NBLY-1)
!
!    COMMON BLOCK BANDTA CONTAINS RANDOM BAND PARAMETERS FOR THE LW
!    CALCULATIONS USING 10 CM-1 WIDE BANDS.THE 15 UM CO2 COMPLEX
!    IS 2 BANDS,560-670 AND 670-800 CM-1. OZONE COEFFICIENTS ARE
!    IN 3 BANDS,670-800 (14.1 UM),990-1070 AND 1070-1200 (9.6 UM).
!    THE  (NBLW) BANDS NOW INCLUDE:
!                56 BANDS, 10  CM-1 WIDE    0  -   560  CM-1
!                 2 BANDS, 15 UM COMPLEX  560  -   670  CM-1
!                                         670  -   800  CM-1
!                 3 "CONTINUUM" BANDS     800  -   900  CM-1
!                                         900  -   990  CM-1
!                                        1070  -   1200 CM-1
!                 1 BAND FOR 9.6 UM BAND  990  -   1070 CM-1
!               100 BANDS, 10 CM-1 WIDE  1200  -   2200 CM-1
!                 1 BAND FOR 4.3 UM SRC  2270  -   2380 CM-1
!    THUS NBLW PRESENTLY EQUALS    163
!    ALL BANDS ARE ARRANGED IN ORDER OF INCREASING WAVENUMBER
!
!        ARNDM   =   RANDOM "A" PARAMETER FOR (NBLW) BANDS
!        BRNDM   =   RANDOM "B" PARAMETER FOR (NBLW) BANDS
!        BETAD   =   CONTINUUM COEFFICIENTS FOR (NBLW) BANDS
!        AP,BP   =   CAPPHI COEFFICIENTS FOR (NBLW) BANDS
!        ATP,BTP =   CAPPSI COEFFICIENTS FOR (NBLW) BANDS
!        BANDLO  =   LOWEST FREQUENCY IN EACH OF (NBLW) FREQ. BANDS
!        BANDHI  =   HIGHEST FREQUENCY IN EACH OF (NBLW) FREQ. BANDS
!        AO3RND  =   RANDOM "A" PARAMETER FOR OZONE IN (3) OZONE
!                    BANDS
!        BO3RND  =   RANDOM "B" PARAMETER FOR OZONE IN (3) OZONE
!                    BANDS
!        AB15    =   THE PRODUCT ARNDM*BRNDM FOR THE TWO BANDS
!                    REPRESENTING THE 15 UM BAND COMPLEX OF CO2
!     DATA FOR ARNDM,BRNDM,AP,BP,ATP,BTP,AO3RND,BO3RND ARE OBTAINED BY
!     USING THE AFGL 1982 CATALOG. CONTINUUM COEFFICIENTS ARE FROM
!     ROBERTS (1976).
      real (kind=kind_rad) ARNDM(NBLW),BRNDM(NBLW),BETAD(NBLW),AP(NBLW)
     &,                 BP(NBLW),ATP(NBLW),BTP(NBLW),BANDLO(NBLW)
     &,                 BANDHI(NBLW),AO3RND(3),BO3RND(3),AB15(2)
      DATA AO3RND /
     &   0.543368E+02,  0.234676E+04,  0.384881E+02/
      DATA BO3RND /
     &   0.526064E+01,  0.922424E+01,  0.496515E+01/
!
!    COMMON BLOCK BDWIDE CONTAINS RANDOM BAND PARAMETERS FOR SPECIFIC
!    WIDE BANDS. AT PRESENT,THE INFORMATION CONSISTS OF 1) RANDOM
!    MODEL PARAMETERS FOR THE 15 UM BAND,560-800 CM-1; 2) THE
!    CONTINUUM COEFFICIENT FOR THE 800-990,1070-1200 CM-1 BAND
!        SPECIFICALLY:
!        AWIDE       =   RANDOM "A" PARAMETER FOR  BAND
!        BWIDE       =   RANDOM "B" PARAMETER FOR  BAND
!        BETAWD      =   CONTINUUM COEFFICIENTS FOR BAND
!        APWD,BPWD   =   CAPPHI COEFFICIENTS FOR  BAND
!        ATPWD,BTPWD =   CAPPSI COEFFICIENTS FOR BAND
!        BDLOWD      =   LOWEST FREQUENCY IN EACH  FREQ  BAND
!        BDHIWD      =   HIGHEST FREQUENCY IN EACH FREQ  BAND
!        AB15WD      =   THE PRODUCT ARNDM*BRNDM FOR THE ONE BAND
!                        REPRESENTING THE 15 UM BAND COMPLEX OF CO2
!        BETINW      =   CONT.COEFFICIENT FOR A SPECIFIED WIDE
!                        FREQ.BAND (800-990 AND 1070-1200 CM-1).
!        SKO2D       =   1./BETINW, USED IN SPA88 FOR CONT. COEFFS
!        SKC1R       =   BETAWD/BETINW, USED FOR CONT. COEFF. FOR
!                        15 UM BAND IN FST88
!        SKO3R       =   RATIO OF CONT. COEFF. FOR 9.9 UM BAND TO
!                        BETINW, USED FOR 9.6 UM CONT COEFF IN FST88
!     DATA FOR AWIDE,BWIDE,APWD,BPWD,ATPWD,BTPWD,AO3WD,BO3WD ARE
!     OBTAINED BY USING THE AFGL 1982 CATALOG. CONTINUUM COEFFICIENTS
!     ARE FROM ROBERTS (1976).
      real (kind=kind_rad) AWIDE,BWIDE,BETAWD,APWD,BPWD,ATPWD,BTPWD,
     &                  BDLOWD,BDHIWD,BETINW,AB15WD,SKO2D,SKC1R,SKO3R
      DATA AWIDE  /
     &   0.309801E+01/
      DATA BWIDE  /
     &   0.495357E-01/
      DATA APWD   /
     &   0.177115E-01/
      DATA BPWD   /
     &  -0.545226E-04/
      DATA ATPWD  /
     &   0.187967E-01/
      DATA BTPWD  /
     &  -0.567449E-04/
      DATA BETAWD /
     &   0.347839E+02/
      DATA BETINW /
     &   0.766811E+01/
      DATA BDLOWD /
     &   0.560000E+03/
      DATA BDHIWD /
     &   0.800000E+03/
!
!    COMMON BLOCK BDCOMB CONTAINS RANDOM BAND PARAMETERS FOR THE LW
!    CALCULATIONS USING COMBINED WIDE FREQUENCY BANDS BETWEEN 160 AND
!    1200 CM-1,AS WELL AS THE 2270-2380 BAND FOR SOURCE CALC.
!        BANDS 1-8: COMBINED WIDE FREQUENCY BANDS FOR 160-560 CM-1
!        BANDS 9-14: FREQUENCY BANDS,AS IN BANDTA (NARROW BANDS)
!                    FOR 560-1200 CM-1
!        BAND  15:  FREQUENCY BAND 2270-2380 CM-1,USED FOR SOURCE
!                   CALCULATION ONLY
!        THUS NBLY PRESENTLY EQUALS   15
!
!        BANDS ARE ARRANGED IN ORDER OF INCREASING WAVENUMBER
!        ACOMB       =   RANDOM "A" PARAMETER FOR (NBLY) BANDS
!        BCOMB       =   RANDOM "B" PARAMETER FOR (NBLY) BANDS
!        BETACM      =   CONTINUUM COEFFICIENTS FOR (NBLY) BANDS
!        APCM,BPCM   =   CAPPHI COEFFICIENTS FOR (NBLY) BANDS
!        ATPCM,BTPCM =   CAPPSI COEFFICIENTS FOR (NBLY) BANDS
!        BDLOCM      =   LOWEST FREQUENCY IN EACH OF (NBLY) FREQ. BANDS
!        BDHICM      =   HIGHEST FREQUENCY IN EACH OF (NBLY) FREQ. BANDS
!        AO3CM       =   RANDOM "A" PARAMETER FOR OZONE IN (3) OZONE
!                        BANDS
!        BO3CM       =   RANDOM "B" PARAMETER FOR OZONE IN (3) OZONE
!                        BANDS
!        AB15CM      =   THE PRODUCT ARNDM*BRNDM FOR THE TWO BANDS
!                        REPRESENTING THE 15 UM BAND COMPLEX OF CO2
!        BETINC      =   CONT.COEFFICIENT FOR A SPECIFIED WIDE
!                        FREQ.BAND (800-990 AND 1070-1200 CM-1).
!        IBAND       =   INDEX NO OF THE 40 WIDE BANDS USED IN
!                        COMBINED WIDE BAND CALCULATIONS. IN OTHER
!                        WORDS,INDEX TELLING WHICH OF THE 40 WIDE
!                        BANDS BETWEEN 160-560 CM-1 ARE INCLUDED IN
!                        EACH OF THE FIRST 8 COMBINED WIDE BANDS
!     DATA FOR ACOMB,BCOMB,APCM,BPCM,ATPCM,BTPCM,AO3CM,BO3CM ARE
!     OBTAINED BY USING THE AFGL 1982 CATALOG. CONTINUUM COEFFICIENTS
!     ARE FROM ROBERTS (1976). IBAND INDEX VALUES ARE OBTAINED BY
!     EXPERIMENTATION.
      integer  IBAND(40)
      real (kind=kind_rad) ACOMB(NBLY),BCOMB(NBLY),
     &                  BETACM(NBLY),APCM(NBLY),BPCM(NBLY),ATPCM(NBLY),
     &                  BTPCM(NBLY),BDLOCM(NBLY),BDHICM(NBLY),BETINC,
     &                  AO3CM(3),BO3CM(3),AB15CM(2)
      DATA ACOMB  /
     &   0.152070E+05,  0.332194E+04,  0.527177E+03,  0.163124E+03,
     &   0.268808E+03,  0.534591E+02,  0.268071E+02,  0.123133E+02,
     &   0.600199E+01,  0.640803E+00,  0.501549E-01,  0.167961E-01,
     &   0.178110E-01,  0.170166E+00,  0.537083E-02/
      DATA BCOMB  /
     &   0.152538E+00,  0.118677E+00,  0.103660E+00,  0.100119E+00,
     &   0.127518E+00,  0.118409E+00,  0.904061E-01,  0.642011E-01,
     &   0.629660E-01,  0.643346E-01,  0.717082E-01,  0.629730E-01,
     &   0.875182E-01,  0.857907E-01,  0.214005E+00/
      DATA APCM   /
     &  -0.671879E-03,  0.654345E-02,  0.143657E-01,  0.923593E-02,
     &   0.117022E-01,  0.159596E-01,  0.181600E-01,  0.145013E-01,
     &   0.170062E-01,  0.233303E-01,  0.256735E-01,  0.274745E-01,
     &   0.279259E-01,  0.197002E-01,  0.349782E-01/
      DATA BPCM   /
     &  -0.113520E-04, -0.323965E-04, -0.448417E-04, -0.230779E-04,
     &  -0.361981E-04, -0.145117E-04,  0.198349E-04, -0.486529E-04,
     &  -0.550050E-04, -0.684057E-04, -0.447093E-04, -0.778390E-04,
     &  -0.982953E-04, -0.772497E-04, -0.748263E-04/
      DATA ATPCM  /
     &  -0.106346E-02,  0.641531E-02,  0.137362E-01,  0.922513E-02,
     &   0.136162E-01,  0.169791E-01,  0.206959E-01,  0.166223E-01,
     &   0.171776E-01,  0.229724E-01,  0.275530E-01,  0.302731E-01,
     &   0.281662E-01,  0.199525E-01,  0.370962E-01/
      DATA BTPCM  /
     &  -0.735731E-05, -0.294149E-04, -0.505592E-04, -0.280894E-04,
     &  -0.492972E-04, -0.341508E-04, -0.362947E-04, -0.250487E-04,
     &  -0.521369E-04, -0.746260E-04, -0.744124E-04, -0.881905E-04,
     &  -0.933645E-04, -0.664045E-04, -0.115290E-03/
      DATA BETACM /
     &   0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00,
     &   0.188625E+03,  0.144293E+03,  0.174098E+03,  0.909366E+02,
     &   0.497489E+02,  0.221212E+02,  0.113124E+02,  0.754174E+01,
     &   0.589554E+01,  0.495227E+01,  0.000000E+00/
      DATA IBAND  /
     &    2,   1,   2,   2,   1,   2,   1,   3,   2,   2,
     &    3,   2,   2,   4,   2,   4,   2,   3,   3,   2,
     &    4,   3,   4,   3,   7,   5,   6,   7,   6,   5,
     &    7,   6,   7,   8,   6,   6,   8,   8,   8,   8/
!
!***THE FOLLOWING DATA ARE LEVEL-INDEPENDENT
!CCCC DATA RCO2/3.3E-4/
!     DATA G/980.665/
!CCCC DATA CTAUDA/.5/
!CCCC DATA CSOLAR/1.96/
!CCCC DATA CCOSZ/.5/
!   B0,B1,B2,B3 ARE COEFFICIENTS USED TO CORRECT FOR THE USE OF 250K IN
!   THE PLANCK FUNCTION USED IN EVALUATING PLANCK-WEIGHTED CO2
!   TRANSMISSION FUNCTIONS. (SEE REF. 4)
!     DATA B0,B1,B2,B3/-.51926410E-4,-.18113332E-3,
!    1 -.10680132E-5,-.67303519E-7/
!    *******************************************************************
!    *                                                                 *
!    *                           B L C K F S  FROM G F D L             *
!    *  UNUSED DATA CLEANED OUT - NOV 86 AND MAR 89 ..K.A.CAMPANA....  *
!    &                                                                 *
!    *******************************************************************
!
!     FOR SEASONAL VARIATION
!        SEASON=1,2,3,4 FOR WINTER,SPRING,SUMMER,FALL ONLY (NOT ACTIVE)
!        SEASON=5 - SEASONAL VARIATION(I.E.INTERPOLATE TO DAY OF FCST)
!
!     INTEGER SEASON
!     COMMON/DIUCON/SEASON,FCSTDA,JTIME(5),DAZ(12),JDNMC,
!    &              FJDNMC,TSLAG,RLAG,TIMIN,TPI,HPI,YEAR,DAY,DHR,IXXXX
!     DATA SEASON/5/
!     DATA TSLAG/45.25/,  RLAG/14.8125/
!     DATA DAY/86400./,  YEAR/365.25/
!     DATA TPI/6.283185308/,  HPI/1.570796327/
!     DATA DAY/86400./,  YEAR/365.25/
!     DATA TPI/6.283185308/,  HPI/1.570796327/
!     DATA JTIME/0,1,0,0,0/
!     DATA DHR/2./
!     DATA DAZ/0.,31.,59.,90.,120.,151.,181.,212.,243.,273.,304.,334./
!
!     SEA SURFACE ALBEDO DATA
!
      real (kind=kind_rad) ALBD(21,20),ZA(20),TRN(21),DZA(19)
      DATA ZA/90.,88.,86.,84.,82.,80.,78.,76.,74.,70.,66.,62.,58.,54.,
     &        50.,40.,30.,20.,10.,0.0/
      DATA TRN/.00,.05,.10,.15,.20,.25,.30,.35,.40,.45,.50,.55,.60,.65,
     &         .70,.75,.80,.85,.90,.95,1.00/
      DATA DZA/8*2.0,6*4.0,5*10.0/
!
      real (kind=kind_rad)
     &      EM1V(5040),EM1VW(5040),T1(5041),
     &      T2(5040),T4(5040),EM3V(5040)
cjfe &      EM1(28,180),EM1WDE(28,180),TABLE1(28,180),
cjfe &      TABLE2(28,180),TABLE3(28,180),EM3(28,180)
!
      real (kind=kind_rad) DELCM(NBLY)
      DATA DELCM  /
     &   0.300000E+02,  0.110000E+03,  0.600000E+02,  0.400000E+02,
     &   0.200000E+02,  0.500000E+02,  0.400000E+02,  0.500000E+02,
     &   0.110000E+03,  0.130000E+03,  0.100000E+03,  0.900000E+02,
     &   0.800000E+02,  0.130000E+03,  0.110000E+03/
!
!      real (kind=kind_rad) SC
!      DATA SC/2.0/
!
      END MODULE RNDDTA
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      MODULE COMCD1
 
      USE MACHINE
      IMPLICIT NONE
 
      SAVE
 
cmy delete rocp alread in module physcons
      REAL (KIND=kind_rad) PTOPC(4,2),CVTOP,VVCLD(2),CLAPSE
      REAL (KIND=kind_rad) CRHRH,PSTRT,CLAPKC,DCLPS,CLPSE
      INTEGER KLOWT,KLOWB,LLYR
 
      END MODULE COMCD1
