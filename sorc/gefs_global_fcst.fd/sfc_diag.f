      SUBROUTINE SFC_DIAG(IM,KM,PS,U1,V1,T1,Q1,
     &                  TSKIN,QSURF,
     &                  F10M,U10M,V10M,T2M,Q2M,
     &                  RCL,PRSLKI,SLIMSK,
     &                  EVAP,FM,FH,FM10,FH2)
!
      USE MACHINE , ONLY : kind_phys
      USE FUNCPHYS, ONLY : fpvs
      USE PHYSCONS, grav => con_g, SBC => con_sbc, HVAP => con_HVAP
     &,             CP => con_CP, HFUS => con_HFUS, JCAL => con_JCAL
     &,             EPS => con_eps, EPSM1 => con_epsm1
     &,             RVRDM1 => con_FVirt, RD => con_RD
      implicit none
!
!     include 'constant.h'
!
      integer              IM, km
!
      real(kind=kind_phys) PS(IM),       U1(IM),      V1(IM),
     &                     T1(IM),       Q1(IM),  
     &                     TSKIN(IM),    QSURF(IM), 
     &                     F10M(IM),     U10M(IM),
     &                     V10M(IM),     T2M(IM),     Q2M(IM),
     &                     RCL(IM),      PRSL1(IM),   PRSLKI(IM),
     &                     SLIMSK(IM),   EVAP(IM),    
     &                     FM(IM),       FH(IM),
     &                     FM10(IM),     FH2(IM)
!
!     Locals
!
      real (kind=kind_phys), parameter :: qmin=1.0e-8
      integer              k,i
!
      real(kind=kind_phys) 
     &                     PSURF(IM),   QSS(IM),
     &                     THETA1(IM),  XRCL(IM)
!
      real(kind=kind_phys) g,    sig2k
!
cc
      PARAMETER (G=grav)
!
      LOGICAL FLAG(IM), FLAGSNW(IM)
      real(kind=kind_phys) KT1(IM),       KT2(IM),      KTSOIL,
     &                     ET(IM,KM),
     &                     STSOIL(IM,KM), AI(IM,KM),    BI(IM,KM),
     &                     CI(IM,KM),     RHSTC(IM,KM)
!
C
C     ESTIMATE SIGMA ** K AT 2 M
C
      SIG2K = 1. - 4. * G * 2. / (CP * 280.)
C
C  INITIALIZE VARIABLES. ALL UNITS ARE SUPPOSEDLY M.K.S. UNLESS SPECIFIE
C  PSURF IS IN PASCALS
C  THETA1 IS ADIABATIC SURFACE TEMP FROM LEVEL 1
C
!!
      DO I=1,IM
        XRCL(I)  = SQRT(RCL(I))
        PSURF(I) = 1000. * PS(I)
        THETA1(I) = T1(I) * PRSLKI(I)
      ENDDO
!!
!
      DO I = 1, IM
        F10M(I) = FM10(I) / FM(I)
        F10M(I) = min(F10M(I),1.)
        U10M(I) = F10M(I) * XRCL(I) * U1(I)
        V10M(I) = F10M(I) * XRCL(I) * V1(I)
         T2M(I) = TSKIN(I) * (1. - FH2(I) / FH(I))
     &          + THETA1(I) * FH2(I) / FH(I)
         T2M(I) = T2M(I) * SIG2K
C        Q2M(I) = QSURF(I) * (1. - FH2(I) / FH(I))
C    &         + Q1(I) * FH2(I) / FH(I)
C       T2M(I) = T1
C       Q2M(I) = Q1
        IF(EVAP(I).GE.0.) THEN
C
C  IN CASE OF EVAPORATION, USE THE INFERRED QSURF TO DEDUCE Q2M
C
          Q2M(I) = QSURF(I) * (1. - FH2(I) / FH(I))
     &         + max(qmin,Q1(I)) * FH2(I) / FH(I)      !  Moorthi
!!   &         + Q1(I) * FH2(I) / FH(I)
        ELSE
C
C  FOR DEW FORMATION SITUATION, USE SATURATED Q AT TSKIN
C
cjfe      QSS(I) = 1000. * FPVS(TSKIN(I))
          qss(I) = fpvs(tskin(I))
          QSS(I) = EPS * QSS(I) / (PSURF(I) + EPSM1 * QSS(I))
          Q2M(I) = QSS(I) * (1. - FH2(I) / FH(I))
     &         + max(qmin,Q1(I)) * FH2(I) / FH(I)      ! Moorthi
!!   &         + Q1(I) * FH2(I) / FH(I)
        ENDIF
cjfe    QSS(I) = 1000. * FPVS(T2M(I))
        QSS(I) = fpvs(t2m(I))
!       QSS(I) = 1000. * T2MO(I)
        QSS(I) = EPS * QSS(I) / (PSURF(I) + EPSM1 * QSS(I))
        Q2M(I) = MIN(Q2M(I),QSS(I))
      ENDDO

      RETURN
      END
