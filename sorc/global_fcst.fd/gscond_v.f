      SUBROUTINE GSCOND (IM,IX,KM,DT,PRSL,PS,Q,cwm,T
     &,                  tp, qp, psp, tp1, qp1, psp1, u, lprnt, ipr)
!
!     ******************************************************************
!     *                                                                *
!     *  SUBROUTINE FOR GRID-SCALE CONDENSATION & EVAPORATION          *
!     *  FOR THE MRF MODEL AT NCEP.                                    *
!     *                                                                *
!     ******************************************************************
!     *                                                                *
!     *  CREATED BY:   Q.  ZHAO         JAN. 1995                      *
!     *  MODIFIED BY:  H.-L. PAN        SEP. 1998                      *
!     *  MODIFIED BY:  S. MOORTHI       AUG. 1999, 2000                *
!     *                                                                *
!     *  REFERENCES:                                                   *
!     *                                                                *
!     ******************************************************************
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!
      USE MACHINE , ONLY : kind_phys
      USE FUNCPHYS , ONLY : fpvs
      USE PHYSCONS, PSAT => con_PSAT, HVAP => con_HVAP, grav => con_g
     &,             HFUS => con_HFUS, TTP => con_TTP, RD => con_RD
     &,             CP => con_CP, EPS => con_eps, EPSM1 => con_epsm1
     &,             RV => con_RV
      implicit none
!
      real (kind=kind_phys) G,    H1,   H2, H1000
     &,                     D00,  D125,  D5,   ELWV, ELIV
     &,                     EPSQ, TM10,  ELIW, ARCP
     &,                     A1,   R,     CPR,  RCPR, RCP
      PARAMETER (H1=1.E0,       H2=2.E0,               H1000=1000.0
     &,          D00=0.E0,      D125=.125E0,           D5=0.5E0
     &,          A1=PSAT  
     &,          ELWV=HVAP,     ELIV=HVAP+HFUS, G=grav
     &,          EPSQ=2.E-12,   TM10=TTP-10.,   R=RD
     &,          CPR=CP*R,      RCPR=H1/CPR,    RCP=H1/CP)
!
      real(kind=kind_phys), parameter :: cons_0=0.0, cons_m15=-15.0
!
      integer IM, IX, KM, ipr
      real (kind=kind_phys) Q(IX,KM),    T(IX,KM),    CWM(IX,KM)
     &,                     PRSL(IX,KM), PS(IM), DT
     &,                     TP(IX,KM),   QP(IX,KM),   PSP(IM)
     &,                     TP1(IX,KM),  QP1(IX,KM),  PSP1(IM)
!
      real (kind=kind_phys)  QI(IM), QINT(IM), U(IM,KM), CCRIK, E0
     &,                      COND,   rdt, us, cclimit, climit
     &,                      u00b,   u00t, tmt0, tmt15, qik, cwmik
     &,                      ai, bi, qw, u00ik, tik, pres, pp0, fi 
     &,                      at, aq, ap, fiw, elv, qc, rqik
     &,                      rqikk, tx1, tx2, tx3, es, qs
     &,                      tsq, delq, condi, cone0, us00, ccrik1
     &,                      aa, ab, ac, ad, ae, af, ag
     &,                      el2orc, albycp, vprs(im)
      INTEGER IW(IM,KM), i, k, iwik
      logical lprnt
!
!-----------------PREPARE CONSTANTS FOR LATER USES-----------------
!
      EL2ORC = HVAP*HVAP / (RV*CP)
      ALBYCP = HVAP / CP
!
      RDT     = H1/DT
      US      = H1
      CCLIMIT = 1.0E-3
      CLIMIT  = 1.0E-20
!
      DO  I = 1, IM
        IW(I,KM) = D00
      enddo
!
!  check for first time step
!
      if (tp(1,1) .lt. 1.) then
        do k = 1, km
          do i = 1, im
            tp(i,k) = t(i,k)
            qp(i,k) = max(q(i,k),epsq)
            tp1(i,k) = t(i,k)
            qp1(i,k) = max(q(i,k),epsq)
          enddo
        enddo
        do i = 1, im
          psp(i)  = ps(i)
          psp1(i) = ps(i)
        enddo
      endif
c
C*************************************************************
C*******BEGINING OF  GRID-SCALE CONDENSATION/EVAP. LOOP*******
C*************************************************************
C
!     DO K = KM-1,2,-1
      DO K = KM,1,-1
!       vprs(:) = 0.001 * fpvs(T(:,k))       ! fpvs in Pa
C-----------------------------------------------------------------------
C------------------QW, QI AND QINT--------------------------------------
        DO I = 1, IM                                    
          TMT0  = T(I,K)-273.16                                                
          TMT15 = MIN(TMT0,cons_m15)                                            
          QIK   = MAX(Q(I,K),EPSQ)
          CWMIK = MAX(CWM(I,K),CLIMIT)
!
!         AI    = 0.008855
!         BI    = 1.0
!         IF (TMT0 .LT. -20.0) THEN
!           AI = 0.007225
!           BI = 0.9674
!         END IF
!
!  the global qsat computation is done in cb
          pres    = prsl(i,k)
!
!         QW      = vprs(i)
          QW      = min(pres, 0.001 * fpvs(T(i,k)))
!
          QW      = EPS * QW / (PRES + EPSM1 * QW)
          QW      = MAX(QW,EPSQ)
!         QI(I)   = QW *(BI+AI*MIN(TMT0,cons_0))
!         QINT(I) = QW *(1.-0.00032*TMT15*(TMT15+15.))
          qi(i)   = qw
          qint(i) = qw
!         IF (TMT0 .LE. -40.) QINT(I) = QI(I)
C-------------------ICE-WATER ID NUMBER IW------------------------------
          IF(TMT0.LT.-15.0) THEN
            U00IK = U(I,K)
            FI    = QIK - U00IK*QI(I)    
            IF(FI.GT.D00.OR.CWMIK.GT.CLIMIT) THEN                    
               IW(I,K) = 1                                                   
            ELSE                                                           
              IW(I,K) = 0                                                   
            END IF                                                         
          END IF
C
          IF(TMT0.GE.0.0) THEN
            IW(I,K) = 0
          END IF
C
          IF (TMT0 .LT. 0.0 .AND. TMT0 .GE. -15.0) THEN
            IW(I,K) = 0
            if (k .lt. km) then
            IF (IW(I,K+1) .EQ. 1 .AND. CWMIK .GT. CLIMIT) IW(I,K) = 1
            endif
          END IF
        enddo
C--------------CONDENSATION AND EVAPORATION OF CLOUD--------------------
        DO I = 1, IM
C------------------------AT, AQ AND DP/DT-------------------------------
          QIK   = MAX(Q(I,K),EPSQ)
          CWMIK = MAX(CWM(I,K),CLIMIT)
          IWIK  = IW(I,K)
          U00IK = U(I,K)
          TIK   = T(I,K)
          PRES  = PRSL(I,K)   * H1000
          PP0   = (PRES / PS(I)) * PSP(I)
          AT    = (TIK-TP(I,K)) * RDT
          AQ    = (QIK-QP(I,K)) * RDT
          AP    = (PRES-PP0)    * RDT
C----------------THE SATUATION SPECIFIC HUMIDITY------------------------
          FIW   = FLOAT(IWIK)
          ELV   = (H1-FIW)*ELWV    + FIW*ELIV
          QC    = (H1-FIW)*QINT(I) + FIW*QI(I)
!     if (lprnt) print *,' qc=',qc,' qint=',qint(i),' qi=',qi(i)
C----------------THE RELATIVE HUMIDITY----------------------------------
          IF(QC.LE.1.0E-10) THEN
            RQIK=D00 
          ELSE
            RQIK = QIK/QC
          ENDIF
C----------------CLOUD COVER RATIO CCRIK--------------------------------
          IF (RQIK .LT. U00IK) THEN
             CCRIK = D00
          ELSEIF(RQIK.GE.US) THEN
             CCRIK = US
          ELSE
             RQIKK  = MIN(US,RQIK)
             CCRIK = H1-SQRT((US-RQIKK)/(US-U00IK))
          ENDIF
C-----------CORRECT CCR IF IT IS TOO SMALL IN LARGE CWM REGIONS--------
c         IF(CCRIK.GE.0.01.AND.CCRIK.LE.0.2.AND
c    &          .CWMIK.GE.0.2E-3) THEN
c          CCRIK=MIN(1.0,CWMIK*1.0E3)
c         END IF
C----------------------------------------------------------------------
!   If no cloud exists then evaporate any existing cloud condensate
C----------------EVAPORATION OF CLOUD WATER-----------------------------
          E0 = D00
          IF (CCRIK.LE.CCLIMIT.AND.CWMIK.GT.CLIMIT)  THEN 
!
!   First iteration - increment halved
!
            tx1 = tik
            tx3 = qik
!
            es   = min(pres, fpvs(tx1))
            qs   = u00ik * eps * ES / (pres + epsm1*es)
            tsq  = tx1 * tx1
            delq = 0.5 * (qs - tx3) * tsq / (tsq + EL2ORC * QS)
!
            tx2   = delq
            tx1   = tx1 - delq * ALBYCP
            tx3   = tx3 + delq
!
!   Second iteration
!
            es   = min(pres, fpvs(tx1))
            qs   = u00ik * eps * ES / (pres + epsm1*es)
            tsq  = tx1 * tx1
            delq = (qs - tx3) * tsq / (tsq + EL2ORC * QS)
!
            tx2  = tx2 + delq
            tx1  = tx1 - delq * ALBYCP
            tx3  = tx3 + delq
!
!   Third iteration
!
            es   = min(pres, fpvs(tx1))
            qs   = u00ik * eps * ES / (pres + epsm1*es)
            tsq  = tx1 * tx1
            delq = (qs - tx3) * tsq / (tsq + EL2ORC * QS)
            tx2  = tx2 + delq
!
            E0   = Max(tx2*RDT, cons_0)
!     if (lprnt .and. i .eq. ipr .and. k .eq. 34)
!    & print *,' tx2=',tx2,' qc=',qc,' u00ik=',u00ik,' rqik=',rqik
!    &,' cwmik=',cwmik,' e0',e0

!           E0 = MAX(QC*(U00IK-RQIK)*RDT, cons_0)
            E0 = MIN(CWMIK*RDT,   E0)
            E0 = MAX(cons_0,E0)
          END IF
!   If cloud cover > 0.2 condense water vapor in to cloud condensate
C-----------THE EQS. FOR COND. HAS BEEN REORGANIZED TO REDUCE CPU------
          COND = D00
!         IF (CCRIK .GT. 0.20 .AND. QC .GT. EPSQ) THEN
          IF (CCRIK .GT. CCLIMIT .AND. QC .GT. EPSQ) THEN
             US00   = US  - U00IK 
             CCRIK1 = 1.0 - CCRIK
             AA     = EPS*ELV*PRES*QIK
             AB     = CCRIK*CCRIK1*QC*US00
             AC     = AB + 0.5*CWMIK
             AD     = AB * CCRIK1
             AE     = CPR*TIK*TIK
             AF     = AE * PRES
             AG     = AA * ELV
             AI     = CP * AA
             COND   = (AC-AD)*(AF*AQ-AI*AT+AE*QIK*AP)/(AC*(AF+AG))
C-----------CHECK & CORRECT IF OVER CONDENSATION OCCURS-----------------
             CONDI  = (QIK   -U00IK   *QC*1.0)*RDT
             COND   = MIN(COND, CONDI)
C----------CHECK & CORRECT IF SUPERSATUATION IS TOO HIGH----------------
c             QTEMP=QIK-MAX(0.,(COND-E0))*DT
c             IF(QC.LE.1.0E-10) THEN
c               RQTMP=0.0
c             ELSE
c               RQTMP=QTEMP/QC
c             END IF
c             IF(RQTMP.GE.1.10) THEN
c               COND=(QIK-1.10*QC)*RDT
c             END IF
C-----------------------------------------------------------------------
             COND = MAX(COND, D00)
C-------------------UPDATE OF T, Q AND CWM------------------------------
          END IF
          CONE0    = (COND-E0) * DT
          CWM(I,K) = CWM(I,K) + CONE0
!     if (lprnt .and. i .eq. ipr) print *,' t=',t(i,k),' cone0',cone0
!    &,' cond=',cond,' e0=',e0,' elv=',elv,' rcp=',rcp,' k=',k
!    &,' cwm=',cwm(i,k)
          T(I,K)   = T(I,K)   + ELV*RCP*CONE0
          Q(I,K)   = Q(I,K)   - CONE0
        enddo                                  ! End of I-Loop!
      enddo                                    ! End of K-Loop!
C
C*********************************************************************
C****************END OF THE CONDENSATION/EVAPORATION LOOP*************
C*********************************************************************
C----------------store t, q, ps for next time step
      do k = 1, km
        do i = 1, im
          tp(i,k)  = tp1(i,k)
          qp(i,k)  = qp1(i,k)
!
          tp1(i,k) = t(i,k)
          qp1(i,k) = max(q(i,k),epsq)
        enddo
      enddo
      do i = 1, im
        psp(i)  = psp1(i)
        psp1(i) = ps(i)
      enddo
C-----------------------------------------------------------------------
      RETURN
      END
