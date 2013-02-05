!@PROCESS NOEXTCHK
!
!--- The 1st line is an inlined compiler directive that turns off -qextchk
!    during compilation, even if it's specified as a compiler option in the
!    makefile (Tuccillo, personal communication;  Ferrier, Feb '02).
!
!###############################################################################
!---------------------- Driver of the new microphysics -------------------------
!###############################################################################
!
      SUBROUTINE GSMDRIVE(IM, IX, LM,DT,PRSL,DEL,TIN,QIN,CCIN,slmsk,    &
     &                    F_ice, F_rain,  F_RimeF, APREC, SR, GRAV,     &
     &                    HVAP, HSUB, CP, RHC, XNCW, flgmin,            &
     &                    me, lprnt, ipr)
!    &                    HVAP, CP, RHC, XNCW, me, PRINT_diag)
!
!-------------------------------------------------------------------------------
!----- NOTE:  Code is currently set up w/o threading!  
!-------------------------------------------------------------------------------
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:  Grid-scale microphysical processes - condensation & precipitation
!   PRGRMMR: Ferrier         ORG: W/NP22     DATE: February 2001
!  2001-04-xx   Ferrier     - Beta-tested version
!  2001-05-21   Ferrier     - Added gradual latent heating to remove external waves
!  2001-05-30   Ferrier     - Changed default to uniform maritime conditions for testing
!  2001-11-09   Moorthi     - Modified for Global Spectral Model
!-------------------------------------------------------------------------------
! ABSTRACT:
!   * Merges original GSCOND & PRECPD subroutines.   
!   * Code has been substantially streamlined and restructured.
!   * Exchange between water vapor & small cloud condensate is calculated using
!     the original Asai (1965, J. Japan) algorithm.  See also references to
!     Yau and Austin (1979, JAS), Rutledge and Hobbs (1983, JAS), and Tao et al.
!     (1989, MWR).  This algorithm replaces the Sundqvist et al. (1989, MWR)
!     parameterization.  
!-------------------------------------------------------------------------------
! Prior PROGRAM HISTORY LOG:
!
! *** Heritage as Subroutine GSCOND:
!   94-~??  ZHAO         - ORIGINATOR
!   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
!   95-03-28  BLACK      - ADDED EXTERNAL EDGE
!   98-11-02  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
!
! *** Heritage as Subroutine PRECPD:
!   94-~??  ZHAO       - ORIGINATOR
!   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
!   95-11-20  ABELES     - PARALLEL OPTIMIZATION
!   96-03-29  BLACK      - REMOVED SCRCH COMMON
!   96-07-18  ZHAO       - NEW WMIN CALCULATION
!   96-09-25  BALDWIN    - NEW SR CALCULATION
!   98-11-02  BLACK      - MODIFICATION FOR DISTRIBUTED MEMORY
!-------------------------------------------------------------------------------
!     
! USAGE: CALL GSMDRIVE FROM gbphys
!
!   INPUT ARGUMENT LIST:
!       LM,DT,SL,DEL,PS,TIN,QIN,CCIN,slmsk,
!       F_ice, F_rain,  F_RimeF, APREC, SR, GRAV,
!       ilon, ilat, HVAP, CP, RHC, XNCW,me
!  
!   OUTPUT ARGUMENT LIST: 
!     TIN, QIN, CCIN, F_ice, F_rain,  F_RimeF, APREC
!     
!   OUTPUT FILES:
!     NONE
!     
! Subprograms & Functions called:
!   GSMCONST  - initialize rain & ice lookup tables, read from external file;
!               initialize constants
!   GSMCOLUMN - cloud microphysics calculations over vertical columns
!
! UNIQUE: NONE
!  
! LIBRARY: NONE
!  
!?--- COMMON BLOCKS (input for microphysics):
!?       CTLBLK, LOOPS, MASKS, PHYS, VRBLS, CLDWTR, PVRBLS, ACMCLH, PPTASM, C_FRACN
!
!--- COMMON BLOCKS ("triggers" for microphysics & statistics):
!       CMICRO_START, CMICRO_STATS
!   
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!
!------------------------------------------------------------------------
      USE MACHINE , ONLY : kind_phys
      use module_microphysics , only : gsmcolumn
      implicit none
!
      integer im, ix, lm, ilon, ilat, me, ipr
      real (kind=kind_phys) DT, GRAV, HVAP, HSUB, CP
      real (kind=kind_phys) TIN(IX,LM), QIN(IX,LM),  CCIN(IX,LM)        &
     &,                     DEL(IX,LM), PRSL(IX,LM), RHC(IM,LM)         &
     &,                     slmsk(IM),  APREC(IM),   SR(IM), XNCW(IM)   &
     &,                     RHC_col(LM), FLGMIN(im)
      logical lprnt
!
!----------------------------------------------------------------------
!-----  Key parameters passed to column microphysics (COLUMN_MICRO) ------
!------------------------------------------------------------------------- 
!
!--- Flag from INIT.F at start of model run, used in initiating statistics
!
!     COMMON /CMICRO_START/ MICRO_START
!     LOGICAL :: MICRO_START
!
!--- This variable is for debugging purposes (if .true.)
!
!     LOGICAL, PARAMETER :: PRINT_diag=.TRUE.
      LOGICAL PRINT_diag
!
!--- The following variables are for microphysical statistics (non-essential)
!
!     INTEGER, PARAMETER :: ITLO=-60, ITHI=40, ITHILO=ITHI-ITLO+1,
!    & ITHILO_N=ITHILO*4, ITHILO_QM=ITHILO*5, ITHILO_QT=ITHILO*22
!     COMMON /CMICRO_STATS/ NSTATS(ITLO:ITHI,4), QMAX(ITLO:ITHI,5),
!    & QTOT(ITLO:ITHI,22)
!     INTEGER :: NSTATS, NSTATS_0(ITLO:ITHI,4)
!     REAL :: QMAX, QTOT, QMAX_0(ITLO:ITHI,5),QTOT_0(ITLO:ITHI,22)
!     REAL, SAVE :: Thour_print, 
!    &  PRECmax(2),PRECtot(2),PRECmax_0(2),PRECtot_0(2)
!     REAL, PARAMETER :: DThour_print=3.     ! Print statistics every 3 h
!      REAL, PARAMETER :: DThour_print=0.     ! Print statistics every time step
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~ BEGIN section on hydrometeor fractions
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~ Saved values use REAL (REAL*4) arrays rather than INTEGER*2 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
      real (kind=kind_phys) F_ice(IX,LM), F_rain(IX,LM), F_RimeF(IX,LM),&
     &                      Fice, Frain, DUM
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~ END section on hydrometeor fractions
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!-----------------------------------------------------------------------
!-------------- Local arrays & parameters in GSMDRIVE -----------------
!-----------------------------------------------------------------------
!
!---- Comments on 14 March 2002
!    * EPSQ=1.E-12 is the universal lower limit for specific humidity and 
!      total condensate, and is consistent throughout the Eta code.
!
!     REAL, PARAMETER :: EPSQ=1.E-12,  RHOL=1000., T0C=273.15, 
      REAL, PARAMETER :: EPSQ=1.0E-20,  RHOL=1000., T0C=273.15,         &
     & T_ICE=-40., T_ICEK=T0C+T_ICE, RRHOL=1./RHOL, EPSQ1=1.001*EPSQ
!    & T_ICE=-10., T_ICEK=T0C+T_ICE, RRHOL=1./RHOL, EPSQ1=1.001*EPSQ
!
      REAL ARAIN, ASNOW, P_col(LM), QI_col(LM), QR_col(LM),             &
     & QV_col(LM), QW_col(LM), RimeF_col(LM), T_col(LM), THICK_col(LM), &
     & WC_col(LM), NCW(LM)
!
!
      real  Ps_Pa, QAUT0, tc, wc, qi, qr, qw, psfc
      integer L, LL, i
!
!------------------------------------------------------------------------
!
!#######################################################################
!########################## Begin Execution ############################
!#######################################################################
!
!------------------------------------------------------------------------
!---------------------- Microphysical constants -------------------------
!------------------------------------------------------------------------
!
!
!  move water from vapor to liquid should the liquid amount be negative
!
      do L = 1, LM
        do i=1,im
          if (CCIN(i,L) .lt. 0.0) then
            qin(i,L)  = qin(i,L) + CCIN(i,L)
            if (tin(i,l) .gt. t_icek) then
              tin(i,L)  = tin(i,L) - CCIN(i,L) * (HVAP/CP)
            else
              tin(i,L)  = tin(i,L) - CCIN(i,L) * (HSUB/CP)
            endif
            CCIN(i,L) = 0.
          endif
        enddo
      enddo
!
!------------------------------------------------------------------------
!--------------- Initialize constants for statistics --------------------
!------------------------------------------------------------------------
!
!       Thour_print=-DTPH/3600.+FLOAT(NTSD-1)*DT/3600.
!       IF (PRINT_diag) THEN
!
!-------- Total and maximum quantities
!
!         DO I=ITLO,ITHI
!--- Microphysical statistics dealing w/ grid-point counts
!           DO J=1,4
!             NSTATS(I,J)=0
!           ENDDO
!--- Microphysical statistics dealing w/ maxima of hydrometeor mass
!           DO J=1,5
!             QMAX(I,J)=0.
!           ENDDO
!--- Microphysical statistics dealing w/ total hydrometeor mass
!           DO J=1,22
!             QTOT(I,J)=0.
!           ENDDO
!         ENDDO
!         DO I=1,2
!           PRECmax(I)=0.    ! Maximum precip rates (rain, snow) at surface (mm/h)
!           PRECtot(I)=0.    ! Total precipitation (rain, snow) accumulation at surface
!         ENDDO
!       ENDIF
!     ENDIF
!
      do i=1,im              ! Begining of the I loop!
!
!       if (lprnt .and. i .eq. ipr) then
!          PRINT_diag = .true.
!       else
           PRINT_diag = .false.
!       endif
!       IF (PRINT_diag) THEN
!         print *,' printing for i=',i,' me=',me
!         print *,' ccin=',ccin(ipr,:)
!         print *,' qin=',qin(ipr,:)
!         print *,' F_rain=',F_rain(ipr,:)
!       endif
!
!--- Initialize column data (1D arrays)
!
      psfc = 0.0
      DO L=1,LM
        LL = LM + 1 - L
        P_col(L)     = PRSL(I,LL) * 1000.0       ! Level Pressure in Pa
        THICK_col(L) = DEL(I,LL) * (1000.0/GRAV) !--- Layer thickness = RHO*DZ
        T_col(L)     = TIN(I,LL)
        QV_col(L)    = max(EPSQ, QIN(I,LL))
        RHC_col(L)   = RHC(I,LL)
        WC_col(L)    = CCIN(I,LL)
!       NCW(L)       = XNCW(I) * (P_col(L)*0.001)
        NCW(L)       = XNCW(I)
        psfc         = psfc + del(I,LL) * 1000.0
      ENDDO
!     if (print_diag) print *,' wc_col=',wc_col
      DO L=1,LM
        LL = LM + 1 - L
        TC           = T_col(L)-T0C
        IF (WC_col(L) .LE. EPSQ1) THEN
          WC_col(L)  = 0.
          IF (TC .LT. T_ICE) THEN
            F_ice(I,LL) = 1.
          ELSE
            F_ice(I,LL) = 0.
          ENDIF
          F_rain(I,LL)  = 0.
          F_RimeF(I,LL) = 1.
        ENDIF
!
!--- Determine composition of condensate in terms of
!      cloud water, ice, & rain
!
        WC    = WC_col(L)
        QI    = 0.
        QR    = 0.
        QW    = 0.
        Fice  = F_ice(I,LL)
        Frain = F_rain(I,LL)
!
!--- REAL*4 array storage
!
!     if (print_diag) print *,' L=',L,' fice=',fice,' frain=',frain
!    &,' wc=',wc
        IF (Fice .GE. 1.) THEN
          QI = WC
        ELSE IF (Fice .LE. 0.) THEN
          QW = WC
        ELSE
          QI = Fice*WC
          QW = WC-QI
        ENDIF
        IF (QW.GT.0. .AND. Frain.GT.0.) THEN
          IF (Frain .GE. 1.) THEN
            QR = QW
            QW = 0.
          ELSE
            QR = Frain*QW
            QW = QW-QR
          ENDIF
        ENDIF
        RimeF_col(L) = F_RimeF(I,LL)              ! (real)
!
!     if (print_diag) print *,' qi=',qi,' qr=',qr,' qw=',qw,' wc=',wc
        QI_col(L) = QI
        QR_col(L) = QR
        QW_col(L) = QW
      ENDDO
!     if (PRINT_diag) then
!       print *,' QI_col=',qi_col
!       print *,' QR_col=',qr_col
!       print *,' QW_col=',qw_col
!     endif
!
!#######################################################################
!
!--- Perform the microphysical calculations in this column
!
      ilon = i
      ilat = 0
      CALL GSMCOLUMN ( ARAIN, ASNOW, DT, ilon, ilat, LM,                &
     & P_col, QI_col, QR_col, QV_col, QW_col, RimeF_col, T_col,         &
!    & THICK_col, WC_col, lm, RHC_col, NCW, .false., psfc)
     & THICK_col, WC_col, lm, RHC_col, NCW, flgmin(i), PRINT_diag, psfc)
!
!#######################################################################
!
!
!#######################################################################
!
!     if (PRINT_diag) then
!       print *,' arain=',arain,' asnow=',asnow
!       print *,' aQI_col=',qi_col
!       print *,' aQR_col=',qr_col
!       print *,' aQW_col=',qw_col
!     endif
!
!--- Update storage arrays
!
      DO L=1,LM
        LL = LM + 1 - L
        TIN(I,LL)  = T_col(L)
        IF (QIN(I,LL) .LT. EPSQ) THEN
          QIN(I,LL)  = QIN(I,LL) + QV_col(L)
        else
          QIN(I,LL)  = QV_col(L)
        endif
!     if (print_diag) print *,' ccin=',ccin(ipr,ll), wc_col(l)
        IF (CCIN(I,LL) .LT. EPSQ) THEN
          CCIN(I,LL) = CCIN(I,LL) + WC_col(L)
        else
          CCIN(I,LL) = WC_col(L)
        endif
!     if (print_diag) print *,' accin=',ccin(ipr,ll), wc_col(l)
!
!--- REAL*4 array storage
!
        F_RimeF(I,LL)=MAX(1., RimeF_col(L))
        IF (QI_col(L) .LE. EPSQ) THEN
          F_ice(I,LL)=0.
          IF (T_col(L) .LT. T_ICEK) F_ice(I,LL)=1.
        ELSE
          F_ice(I,LL)=MAX( 0., MIN(1., QI_col(L)/WC_col(L)) )
        ENDIF
        IF (QR_col(L) .LE. EPSQ) THEN
          DUM=0
        ELSE
          DUM=QR_col(L)/(QR_col(L)+QW_col(L))
        ENDIF
        F_rain(I,LL)=DUM
!
!
      ENDDO
!
!     IF (PRINT_diag) THEN
!       print *,' accin=',ccin(ipr,:)
!       print *,' aqin=',qin(ipr,:)
!       print *,' aF_rain=',F_rain(ipr,:)
!     endif
!
!--- Update accumulated precipitation statistics
!
!--- Surface precipitation statistics; SR is fraction of surface
!    precipitation (if >0) associated with snow
!
      APREC(I) = (ARAIN+ASNOW)*RRHOL    ! Accumulated surface precip (depth in m)
      IF(APREC(i) .LT. 1.E-8) THEN
        SR(I)  = 0.
      ELSE
        SR(I)  = RRHOL*ASNOW / APREC(I)
      ENDIF
!
!       IF (PRINT_diag) THEN
!         print *,' ccio=',ccin
!         print *,' qio=',qin
!         print *,' F_rain=',F_rain
!         print *,' aprec=',aprec,' arain=',arain,' asnow=',asnow
!       endif
!
!--- Debug statistics
!
!       IF (PRINT_diag) THEN
!         PRECtot(1)=PRECtot(1)+ARAIN
!         PRECtot(2)=PRECtot(2)+ASNOW
!         PRECmax(1)=MAX(PRECmax(1), ARAIN)
!         PRECmax(2)=MAX(PRECmax(2), ASNOW)
!       ENDIF
!#######################################################################
!#######################################################################
!
!-----------------------------------------------------------------------
!--------------------- END of main microphysics loop -------------------
!-----------------------------------------------------------------------
!
      ENDDO              ! End of the I loop
!
!     time_model=float(NTSD-1)*DT/3600.
!     IF (PRINT_diag .AND. time_model.GE.Thour_print) THEN
!       CALL MPI_REDUCE(NSTATS,NSTATS_0,ITHILO_N,MPI_INTEGER,MPI_SUM,0,
!    &                  MPI_COMM_COMP,IRTN)
!       CALL MPI_REDUCE(QMAX,QMAX_0,ITHILO_QM,MPI_REAL,MPI_MAX,0,
!    &                  MPI_COMM_COMP,IRTN)
!       CALL MPI_REDUCE(PRECmax,PRECmax_0,2,MPI_REAL,MPI_MAX,0,
!    &                  MPI_COMM_COMP,IRTN)
!       CALL MPI_REDUCE(QTOT,QTOT_0,ITHILO_QT,MPI_REAL,MPI_SUM,0,
!    &                  MPI_COMM_COMP,IRTN)
!       CALL MPI_REDUCE(PRECtot,PRECtot_0,2,MPI_REAL,MPI_SUM,0,
!    &                  MPI_COMM_COMP,IRTN)
!      IF (MYPE .EQ. 0) THEN
!       HDTPH=3600./DTPH            ! Convert precip rates to mm/h
!       DO K=ITLO,ITHI
!         QMAX_0(K,1)=1000.*QMAX_0(K,1)
!         QMAX_0(K,2)=1000.*QMAX_0(K,2)
!         QMAX_0(K,3)=1000.*QMAX_0(K,3)
!         QMAX_0(K,4)=HDTPH*QMAX_0(K,4)
!         QMAX_0(K,5)=HDTPH*QMAX_0(K,5)
!       ENDDO
!       PRECmax_0(1)=HDTPH*PRECmax_0(1)
!       PRECmax_0(2)=HDTPH*PRECmax_0(2)
!
!       WRITE(6,"(A,F5.2,4(A,G11.4))") '{ Time(h)=',time_model,
!    & '  TRAIN_sfc=',PRECtot_0(1),'  TSNOW_sfc=',PRECtot_0(2),
!    & '  RRmax_sfc(mm/h)=',PRECmax_0(1),
!    & '  SRmax_sfc(mm/h)=',PRECmax_0(2)
!
!       WRITE(6,"(3A)") '{ (C) <--------- Counts ----------> ',
!    & '<----------- g/kg ----------> <----- mm/h ------>',
!    & ' <---- kg/m**2 * # grids ---->'
!       WRITE(6,"(3A)") '{  T     NCICE  NCMIX  NCWAT NCRAIN  ',
!    & 'QIMAX     QWMAX     QRMAX     SRMAX     RRMAX     QITOT     ',
!    & 'QWTOT     QRTOT'
!       DO K=ITLO,ITHI
!         WRITE(6,"(A,I3,I9,3I7,8G10.4)") 
!    &      '{ ',K,(NSTATS_0(K,II), II=1,4),
!    &      (QMAX_0(K,JJ), JJ=1,5),(QTOT_0(K,KK), KK=1,3)
!       ENDDO
!
!       WRITE(6,"(3A)")
!    & '{  T   TCOND     TICND     TIEVP     TIDEP     TREVP     ',
!    & 'TRAUT     TRACW     TIMLT     TIACW     TIACWI    TIACWR    ',
!    & 'TIACR'
!       DO K=ITLO,ITHI
!         WRITE(6,"(A,I3,12G10.4)") '{ ',K,(QTOT_0(K,II), II=4,15)
!       ENDDO
!
!       WRITE(6,"(2A)")
!    & '{  T   DEL_QT   TVDIF   DEL_HYD        TWDIF  TIDIF       ',
!    & 'TRDIF    DARAIN   DASNOW    RimeF'
!       DO K=ITLO,ITHI
!         DEL_HYD=0.
!         DO II=17,19
!           DEL_HYD=DEL_HYD+QTOT_0(K,II)
!         ENDDO
!         DEL_QT=0.
!         DO II=16,21
!           DEL_QT=DEL_QT+QTOT_0(K,II)
!         ENDDO
!         IF (QTOT_0(K,22) .GT. 0.) THEN
!           RimeF_bulk=QTOT_0(K,1)/QTOT_0(K,22)
!           ELSE
!           RimeF_bulk=1.
!         ENDIF
!         WRITE(6,"(A,I3,9G10.4)") '{ ',K,DEL_QT,QTOT_0(K,16),
!    &      DEL_HYD,(QTOT_0(K,II), II=17,21),RimeF_bulk
!       ENDDO
!
!      ENDIF
!
!-------- Reset arrays storing total and maximum quantities
!
!      DO I=ITLO,ITHI
!--- Microphysical statistics dealing w/ grid-point counts
!        DO J=1,4
!          NSTATS(I,J)=0
!        ENDDO
!--- Microphysical statistics dealing w/ maxima of hydrometeor mass
!        DO J=1,5
!          QMAX(I,J)=0.
!        ENDDO
!--- Microphysical statistics dealing w/ total hydrometeor mass
!        DO J=1,22
!          QTOT(I,J)=0.
!        ENDDO
!      ENDDO
!      DO I=1,2
!        PRECmax(I)=0.    ! Maximum precip rates (rain, snow) at surface (mm/h)
!        PRECtot(I)=0.    ! Total precipitation (rain, snow) accumulation at surface
!      ENDDO
!      Thour_print=Thour_print+DThour_print
!     ENDIF
!
!-----------------------------------------------------------------------
!------------------------ Return to main program -----------------------
!-----------------------------------------------------------------------
!
      RETURN
!-----------------------------------------------------------------------
200   format(a2,i5,f6.2,4(1x,a10,g11.4))
210   format(a2,i5,f6.2,4(1x,a10,i7))
!-----------------------------------------------------------------------
      END
      SUBROUTINE MICRO_INIT(LM,LEN,F_ice,F_rain,F_RimeF,DT,FHOUR,me     &
     &,                     first)
!
!     This subroutine initializes the necessary constants and
!     tables for Brad Ferrier's cloud microphysics package
!
      USE MACHINE , ONLY : kind_phys
      use module_microphysics , only : gsmconst
      implicit none
!
      logical first
      integer LM, LEN, me
      real (kind=kind_phys) F_ice(LEN,LM), F_rain(LEN,LM),              &
     &                      F_RimeF(LEN,LM), DT, FHOUR
!
      if (fhour .lt. 0.1) then
        F_ice   = 0.              ! Initialize ice fraction array (real)
        F_rain  = 0.              ! Initialize rain fraction array (real)
        F_RimeF = 1.              ! Initialize rime factor array (real)
      endif
      CALL GSMCONST (DT,me,first) ! Initialize lookup tables & constants
!
      RETURN
      END
      SUBROUTINE INIT_MICRO(DT,levs,len,num_p3d,phy_f3d,fhour,me)
!
      USE MACHINE , ONLY : kind_phys
      implicit none
!
      integer levs, len, num_p3d, me,  nsphys
      real (kind=kind_phys)  dt, fhour, phy_f3d(len,levs,num_p3d)       &
     &,                      dtlast, dtp, dtphys
      logical first
      data first/.true./, dtlast/0.0/
      save first, dtlast
!
      dtphys=3600.
      NSPHYS=MAX(INT(2*dt/DTPHYS+0.9999),1)
      DTP=(dt+dt) / NSPHYS
!
      if (num_p3d .eq. 3 .and. dtp .ne. dtlast) then
!      Initialization and/or constant evaluation for Ferrier's microphysics
        call MICRO_INIT(LEVS, len, phy_f3d(1,1,1), phy_f3d(1,1,2)       &
     &,                 phy_f3d(1,1,3), DTP, FHOUR, me, first)
        dtlast = dtp
        first = .false.
      endif
   

      RETURN
      END
