      module module_ras
      USE MACHINE , ONLY : kind_phys
      use physcons, grav => con_g, cp => con_cp, alhl => con_hvap       &
     &,             alhf => con_hfus, rgas => con_rd, rkap => con_rocp  &
     &,             nu => con_FVirt
      implicit none
      SAVE
!     real, parameter :: nu=0.0
!
      integer, parameter :: nrcmax=12 ! Maximum # of random clouds per 1200s
!     integer, parameter :: nrcmax=15 ! Maximum # of random clouds per 1200s
!     integer, parameter :: nrcmax=20
      real (kind=kind_phys), parameter :: delt_c=1800.0
      logical,               parameter :: fix_ncld_hr=.true.
!
      real(kind=kind_phys) ZERO, HALF, ONE, TWO
      real(kind=kind_phys) FOUR_P2,FOUR
      real(kind=kind_phys) ONE_M1,ONE_M2,ONE_M5,ONE_M6,ONE_M10
      PARAMETER (ZERO=0.0, HALF=0.5,  ONE=1.0, TWO=2.0)
      PARAMETER (FOUR_P2=4.E2,FOUR=4.,ONE_M10=1.E-10,ONE_M6=1.E-6       &
     &,          ONE_M5=1.E-5,ONE_M2=1.E-2,ONE_M1=1.E-1)
!
      real(kind=kind_phys), parameter :: cmb2pa = 100.0  ! Conversion from MB to PA
      real(kind=kind_phys) onebg, gravcon, gravfac, elocp, elfocp,      &
     &                     rkapi, rkpp1i,  zfac
!
      parameter (ONEBG   = ONE / GRAV,    GRAVCON = cmb2pa * ONEBG      &
     &,          GRAVFAC = GRAV / CMB2PA, ELOCP   = ALHL / CP           &
     &,          ELFOCP  = (ALHL+ALHF) / CP                             &
     &,          RKAPI   = ONE / RKAP,    RKPP1I  = ONE / (ONE+RKAP)    &
     &,          zfac    = 0.28888889E-4 * ONEBG)
!
!     logical, parameter :: advcld=.false. advups=.true.
!     logical, parameter :: advcld=.true., advups=.true., advtvd=.false.
      logical, parameter :: advcld=.true., advups=.false., advtvd=.true.
!     logical, parameter :: advcld=.false., advups=.false.
!
      real(kind=kind_phys), allocatable  ::  RASAL(:)
      real(kind=kind_phys)  RHMAX,  qudfac, QUAD_LAM, RHRAM, TESTMB,    &
     &                      TSTMBI, HCRIT,  DD_DP,    RKNOB,  AFC, EKNOB

!     PARAMETER (DD_DP=1000.0, RKNOB=1.0, EKNOB=1.0)   ! No downdraft!
!     PARAMETER (DD_DP=100.0,  RKNOB=1.0, EKNOB=1.0)
!     PARAMETER (DD_DP=200.0,  RKNOB=1.0, EKNOB=1.0)
!     PARAMETER (DD_DP=250.0,  RKNOB=1.0, EKNOB=1.0)
!     PARAMETER (DD_DP=300.0,  RKNOB=1.0, EKNOB=1.0)
!     PARAMETER (DD_DP=450.0,  RKNOB=1.0, EKNOB=1.0)
!     PARAMETER (DD_DP=500.0,  RKNOB=0.5, EKNOB=1.0)
!     PARAMETER (DD_DP=500.0,  RKNOB=0.70, EKNOB=1.0)
!     PARAMETER (DD_DP=500.0,  RKNOB=0.75, EKNOB=1.0)
!     PARAMETER (DD_DP=500.0,  RKNOB=1.0, EKNOB=1.0)
      PARAMETER (DD_DP=500.0,  RKNOB=1.5, EKNOB=1.0)
!!!!! PARAMETER (DD_DP=450.0,  RKNOB=1.5, EKNOB=1.0)
!     PARAMETER (DD_DP=450.0,  RKNOB=2.0, EKNOB=1.0)
!     PARAMETER (DD_DP=450.0,  RKNOB=0.5, EKNOB=1.0)
!     PARAMETER (DD_DP=350.0,  RKNOB=0.5, EKNOB=1.0)
!     PARAMETER (DD_DP=350.0,  RKNOB=1.0, EKNOB=1.0)
!     PARAMETER (DD_DP=350.0,  RKNOB=2.0, EKNOB=1.0)
!     PARAMETER (DD_DP=350.0,  RKNOB=3.0, EKNOB=1.0)
!
      PARAMETER (RHMAX=1.0   )  !  MAX RELATIVE HUMIDITY
      PARAMETER (QUAD_LAM=1.0)  !  MASK FOR QUADRATIC LAMBDA
!     PARAMETER (RHRAM=0.15)    !  PBL RELATIVE HUMIDITY RAMP
      PARAMETER (RHRAM=0.05)    !  PBL RELATIVE HUMIDITY RAMP
!     PARAMETER (RHRAM=0.10)    !  PBL RELATIVE HUMIDITY RAMP
      PARAMETER (HCRIT=4000.0)  !  Critical Moist Static Energy
      parameter (qudfac=quad_lam*half)
!     parameter (qudfac=quad_lam*0.25)    ! Yogesh's
      parameter (testmb=0.1, tstmbi=one/testmb)
!
      real(kind=kind_phys) ALMIN1, ALMIN2, ALMAX
      real(kind=kind_phys) facdt
!
!     PARAMETER (ALMIN1=0.00E-6, ALMIN2=0.00E-5, ALMAX=1.0E-1)
!     PARAMETER (ALMIN1=0.00E-6, ALMIN2=0.00E-5, ALMAX=2.0E-2)
!     PARAMETER (ALMIN1=0.00E-6, ALMIN2=1.00E-6, ALMAX=2.0E-2)
!     PARAMETER (ALMIN1=5.00E-6, ALMIN2=2.50E-5, ALMAX=2.0E-2)
!     PARAMETER (ALMIN1=0.00E-6, ALMIN2=2.50E-5, ALMAX=2.0E-2)
!!!   PARAMETER (ALMIN1=0.00E-6, ALMIN2=2.50E-5, ALMAX=1.0E-2)
!!    PARAMETER (ALMIN1=0.00E-6, ALMIN2=2.50E-5, ALMAX=1.0E-3)
!     PARAMETER (ALMIN1=0.00E-6, ALMIN2=1.00E-5, ALMAX=1.0E-2)
!     PARAMETER (ALMIN1=0.00E-6, ALMIN2=2.00E-5, ALMAX=1.0E-2)
!     PARAMETER (ALMIN1=0.00E-6, ALMIN2=2.50E-5, ALMAX=1.0E-2)
      PARAMETER (ALMIN1=0.00E-6, ALMIN2=4.00E-5, ALMAX=1.0E-2)
!cnt  PARAMETER (ALMIN1=0.00E-6, ALMIN2=2.50E-5, ALMAX=5.0E-3)
!LL   PARAMETER (ALMIN1=0.00E-6, ALMIN2=2.50E-5, ALMAX=4.0E-3)
!     PARAMETER (ALMIN1=0.00E-6, ALMIN2=1.00E-5, ALMAX=2.0E-2)
!     PARAMETER (ALMIN1=0.00E-6, ALMIN2=5.00E-4, ALMAX=2.0E-2)
!     PARAMETER (ALMIN1=0.10E-4, ALMIN2=0.15E-4, ALMAX=1.0E-1)
!     PARAMETER (ALMIN1=0.00E-4, ALMIN2=0.40E-4, ALMAX=2.0E-2)
!     PARAMETER (ALMIN1=0.20E-4, ALMIN2=0.40E-4, ALMAX=2.0E-2)
!     PARAMETER (ALMIN1=0.25E-4, ALMIN2=0.50E-4, ALMAX=2.0E-2)
!     PARAMETER (ALMIN1=0.40E-4, ALMIN2=0.50E-4, ALMAX=2.0E-2)
!
      real(kind=kind_phys), parameter :: BLDMAX = 200.0
!
      INTEGER KBLMX
      real(kind=kind_phys) C0, C0I, QI0, QW0
!     PARAMETER (QI0=1.0E-4, QW0=1.0E-4)
!     PARAMETER (QI0=0.0E-5, QW0=0.0E-0)
      PARAMETER (QI0=1.0E-5, QW0=1.0E-5)
!     PARAMETER (QI0=1.0E-4, QW0=1.0E-5) ! 20050509
!     PARAMETER (QI0=1.0E-5, QW0=1.0E-6)
!     PARAMETER (QI0=0.0E-5, QW0=0.0E-5)
!!!   PARAMETER (QI0=5.0E-4, QW0=1.0E-5)
!     PARAMETER (QI0=5.0E-4, QW0=5.0E-4)
!     PARAMETER (QI0=2.0E-4, QW0=2.0E-5)
!     PARAMETER (QI0=2.0E-5, QW0=2.0E-5)
!     PARAMETER (QI0=2.0E-4, QW0=1.0E-4)
!     PARAMETER (QI0=2.0E-4, QW0=1.0E-5)
!     PARAMETER (QI0=1.0E-3, QW0=2.0E-5)
!     PARAMETER (QI0=1.0E-3, QW0=7.0E-4)
!     PARAMETER (C0I=5.0E-4)
!     PARAMETER (C0I=4.0E-4)
      PARAMETER (C0I=1.0E-3)
!     parameter (c0=1.0e-3)
!     parameter (c0=1.5e-3)
      parameter (c0=2.0e-3)
!     parameter (c0=1.0e-3, KBLMX=10, ERRMIN=0.0001, ERRMI2=0.1*ERRMIN)
!     parameter (c0=2.0e-3, KBLMX=10, ERRMIN=0.0001, ERRMI2=0.1*ERRMIN)
!
      real(kind=kind_phys) TF, TCR, TCRF, TCL
!     parameter (TF=130.16, TCR=160.16, TCRF=1.0/(TCR-TF),TCL=2.0)
!     parameter (TF=230.16, TCR=260.16, TCRF=1.0/(TCR-TF))
      parameter (TF=233.16, TCR=263.16, TCRF=1.0/(TCR-TF),TCL=2.0)
!
!     For Tilting Angle Specification
!
      real(kind=kind_phys) REFP(6), REFR(6), TLAC(8), PLAC(8), TLBPL(7) &
     &,                    drdp(5), VTP
!
      DATA PLAC/100.0, 200.0, 300.0, 400.0, 500.0, 600.0, 700.0, 800.0/
!     DATA TLAC/ 37.0,  25.0,  17.0,  12.0,  10.0,  8.0,  6.0,  5.0/
!     DATA TLAC/ 35.0,  24.0,  17.0,  12.0,  10.0,  8.0,  6.0,  5.0/
!     DATA TLAC/ 35.0,  25.0,  20.0,  17.5,  15.0,  12.5,  10.0,  5.0/
      DATA TLAC/ 35.0,  25.0,  20.0,  17.5,  15.0,  12.5,  10.0,  7.5/
!     DATA TLAC/ 37.0,  26.0,  18.0,  14.0,  10.0,  8.0,  6.0,  5.0/
!     DATA TLAC/ 25.0,  22.5,  20.0,  17.5,  15.0,  12.5,  10.0,  10.0/
      DATA REFP/500.0, 300.0, 250.0, 200.0, 150.0, 100.0/
!     DATA REFR/ 0.25,   0.5,  0.75,   1.0,   1.5,   2.0/
!     DATA REFR/ 0.5,   1.0,  1.5,   2.0,   3.0,   4.0/
      DATA REFR/ 1.0,   2.0,  3.0,   4.0,   6.0,   8.0/
!
      real(kind=kind_phys) AC(16), AD(16)
!
      integer, parameter :: nqrp=500001
      real(kind=kind_phys) C1XQRP, C2XQRP, TBQRP(NQRP), TBQRA(NQRP)     &
     &,                    TBQRB(NQRP)
!
      integer, parameter :: nvtp=10001
      real(kind=kind_phys) C1XVTP, C2XVTP, TBVTP(NVTP)
!
      contains
!
      subroutine set_ras_afc(dt)
      implicit none
      real(kind=kind_phys) DT
!     AFC = -(1.04E-4*DT)*(3600./DT)**0.578
      AFC = -(1.01097E-4*DT)*(3600./DT)**0.57777778
      end subroutine set_ras_afc

      subroutine ras_init(levs,  me)
!
      Implicit none
!
      integer levs
!
      real(kind=kind_phys) actp,   facm, tem,  actop
      real(kind=kind_phys) rasalf, tem1, tem2
      integer              i, l, me
!     PARAMETER (ACTP=1.7,   FACM=1.20)
      PARAMETER (ACTP=1.7,   FACM=1.00)
!     PARAMETER (ACTP=1.7,   FACM=0.90)
!     PARAMETER (ACTP=1.7,   FACM=0.75)
!     PARAMETER (ACTP=1.7,   FACM=0.60)
!     PARAMETER (ACTP=1.7,   FACM=0.5)   ! cnt
!     PARAMETER (ACTP=1.7,   FACM=0.4)
!     PARAMETER (ACTP=1.7,   FACM=0.0)
!
      real(kind=kind_phys) PH(15),    A(15)
!
      DATA PH/150.0, 200.0, 250.0, 300.0, 350.0, 400.0, 450.0, 500.0    &
     &,       550.0, 600.0, 650.0, 700.0, 750.0, 800.0, 850.0/
!
       DATA A/ 1.6851, 1.1686, 0.7663, 0.5255, 0.4100, 0.3677           &
     &,       0.3151, 0.2216, 0.1521, 0.1082, 0.0750, 0.0664            &
     &,       0.0553, 0.0445, 0.0633/
!
      logical first
      data first/.true./
!
      if (first) then
!
        allocate (rasal(levs))
!                                   set critical workfunction arrays
        ACTOP = ACTP*FACM
        DO L=1,15
          A(L) = A(L)*FACM
        ENDDO
        DO L=2,15
          TEM   = 1.0 / (PH(L) - PH(L-1))
          AC(L) = (PH(L)*A(L-1) - PH(L-1)*A(L)) * TEM
          AD(L) = (A(L) - A(L-1)) * TEM
        ENDDO
        AC(1)  = ACTOP
        AC(16) = A(15)
        AD(1)  = 0.0
        AD(16) = 0.0
!
!       CALL SETES
        CALL SETQRP
        CALL SETVTP
!
!       kblmx = levs / 2
!
!       RASALF  = 0.10
!       RASALF  = 0.20
        RASALF  = 0.30
!       RASALF  = 0.35
!
        DO L=1,LEVS
          RASAL(L) = RASALF
        ENDDO
!
!
        do i=1,7
          tlbpl(i) = (tlac(i)-tlac(i+1)) / (plac(i)-plac(i+1))
        enddo
        do i=1,5
          drdp(i)  = (REFR(i+1)-REFR(i)) / (REFP(i+1)-REFP(i))
        enddo
!
        VTP    = 36.34*SQRT(1.2)* (0.001)**0.1364
!
        if (me .eq. 0) print *,' NO DOWNDRAFT FOR CLOUD TYPES'          &
     &,        ' DETRAINING WITHIN THE BOTTOM ',DD_DP,' hPa LAYERS'
!
        first = .false.
      endif
!
      end subroutine ras_init
      end module module_ras
!
      module module_rascnv
!
      USE MACHINE , ONLY : kind_phys
      implicit none
      SAVE

!
      logical REVAP, CUMFRC
      LOGICAL WRKFUN, CALKBL, CRTFUN, UPDRET, BOTOP
      real(kind=kind_phys) FRAC, CRTMSF, MAX_NEG_BOUY, rhfacs, rhfacl   &
     &,                    FACE, DELX,   DDFAC
      parameter (frac=0.5, crtmsf=0.0)
!     PARAMETER (MAX_NEG_BOUY=0.25, REVAP=.TRUE., CUMFRC=.true.)
!     PARAMETER (MAX_NEG_BOUY=0.20, REVAP=.TRUE., CUMFRC=.true.)
!     PARAMETER (MAX_NEG_BOUY=0.15, REVAP=.true., CUMFRC=.true.)
!LL3  PARAMETER (MAX_NEG_BOUY=0.10, REVAP=.TRUE., CUMFRC=.true.)
!     PARAMETER (MAX_NEG_BOUY=0.15, REVAP=.true., CUMFRC=.false.)
      PARAMETER (MAX_NEG_BOUY=0.15, REVAP=.true., CUMFRC=.true.)
!     PARAMETER (MAX_NEG_BOUY=0.15, REVAP=.true., CUMFRC=.false.)
!     PARAMETER (MAX_NEG_BOUY=0.05, REVAP=.true., CUMFRC=.true.)
      PARAMETER (WRKFUN = .FALSE.,  UPDRET = .FALSE.)
      PARAMETER (CRTFUN = .TRUE.,   CALKBL = .true., BOTOP=.true.)
!
!     parameter (rhfacs=0.70, rhfacl=0.70)
!     parameter (rhfacs=0.75, rhfacl=0.75)
      parameter (rhfacs=0.80, rhfacl=0.80)
!     parameter (rhfacs=0.80, rhfacl=0.85)
      PARAMETER (FACE=5.0, DELX=10000.0, DDFAC=FACE*DELX*0.001)
!
!     real (kind=kind_phys), parameter :: pgftop=0.7, pgfbot=0.3        &
!     real (kind=kind_phys), parameter :: pgftop=0.75, pgfbot=0.35      &
      real (kind=kind_phys), parameter :: pgftop=0.80, pgfbot=0.30      &
     &,          pgfgrad=(pgfbot-pgftop)*0.001
!
      end module module_rascnv
!
!
      subroutine rascnv(IM,    IX,     k,      dt,    dtf,  rannum      &
     &,                 tin,   qin,    uin,    vin,   ccin,  trac       &
     &,                 prsi,  prsl,   prsik,  prslk, phil,  phii       &
     &,                 KPBL,  CDRAG,  RAINC,  kbot,  ktop,  kuo        &
     &,                 DDVEL, FLIPV,  facmb,  me,    garea, lmh, ccwfac&
     &,                 nrcm,  rhc,    ud_mf, dd_mf,  det_mf,lprnt, ipr)
!
!*********************************************************************
!*********************************************************************
!************         Relaxed Arakawa-Schubert      ******************
!************             Parameterization          ******************
!************          Plug Compatible Driver       ******************
!************               23 May 2002             ******************
!************                                       ******************
!************               Developed By            ******************
!************                                       ******************
!************             Shrinivas Moorthi         ******************
!************                                       ******************
!************                  EMC/NCEP             ******************
!*********************************************************************
!*********************************************************************
!
!
      USE MACHINE , ONLY : kind_phys
      use module_ras, DPD => DD_DP
      use module_rascnv
      Implicit none
!
      LOGICAL FLIPV, lprnt
!
!      input
!
      Integer IM, IX, k, ncrnd, me, trac, ipr, nrcm
      Integer kbot(im), ktop(im), kuo(im), KPBL(im), lmh(im)
!
      real(kind=kind_phys) tin(ix,k),     qin(ix,k),  uin(ix,k)         &
     &,                    vin(ix,k),     prsi(ix,k+1)                  &
     &,                    prsik(ix,k+1), prsl(ix,k), prslk(ix,k+1)     &
     &,                    phil(ix,k),    phii(ix,k+1),ccwfac(im)       &
     &,                    ccin(ix,k,trac+2)                            &
!    &,                    prsik(ix,k+1), clt(ix,k)                     &
     &,                    RAINC(im),     CDRAG(im),  DDVEL(im)         &
     &,                    rannum(ix,nrcm)                              &
     &,                    ud_mf(im,k), dd_mf(im,k), det_mf(im,k)
      real(kind=kind_phys) DT, facmb, garea(im), dtf, rhc(im,k)
!
!     locals
!
      real(kind=kind_phys) RAIN,     toi(k), qoi(k),  uvi(k,trac+2)     &
     &,                    TCU(k),   QCU(k), PCU(k),  clw(k), cli(k)    &
     &,                    QII(k), QLI(k),  PRS(k+1), PSJ(k+1)          &
     &,                    phi_l(k), phi_h(k+1)                         &
     &,                    RCU(k,2), wfnc,   flx(k+1), FLXD(K+1)
!    &,                    RCU(k,2), rkap,   rkapi,   rkpp1i,   wfnc
      real(kind=kind_phys) daylen,pfac,tla,pl,clwmin
      integer icm,irnd,ib

      PARAMETER (ICM=100, DAYLEN=86400.0, PFAC=1.0/450.0,clwmin=1.0e-10)
      Integer  IC(ICM)
!
      real(kind=kind_phys), allocatable ::  ALFINT(:,:)
!     real(kind=kind_phys) ALFINT(K), ALFINQ(K),   PRSM(K),  PSJM(K)
      real(kind=kind_phys)            ALFINQ(K),   PRSM(K),  PSJM(K)    &
     &,                    trcfac(trac+2,k)                             &
     &,                    alfind(K), rhc_l(k), dtvd(2,4)
!    &,                    DPI(K),    psjp(k+1)              
      real(kind=kind_phys) CFAC, TEM, dpi, sgc, ccwf, tem1, tem2
!
      Integer              KCR,  KFX, NCMX, NC,  KTEM, I,   L, lm1      &
     &,                    ntrc, ia,  ll,   km1, kp1,  ipt, lv, KBL, n  &
     &,                    lmhij, KRMIN, KRMAX, KFMAX
!
      LOGICAL  DNDRFT, lprint
!
      km1    = k - 1
      kp1    = k + 1
!
      ntrc = trac
      trcfac(:,:) = 1.0             !  For other tracers
      IF (CUMFRC) THEN
        ntrc = ntrc + 2
!       trcfac(trac+1) = 0.45       !  For press grad correction c=0.55
!       trcfac(trac+2) = 0.45       !  in momentum mixing calculations
      ENDIF
!
      if (.not. allocated(alfint)) allocate(alfint(k,ntrc+4))
!
      call set_ras_afc(dt)
!
      ccwf = 0.5
      DO IPT=1,IM
!
! Resolution dependent press grad correction momentum mixing
!
        IF (CUMFRC) THEN
!!!!      tem = max(0.0, min(1.0, sqrt(sqrt(garea(ipt)*0.25E-10))))
!         tem = max(0.0, min(1.0, sqrt(garea(ipt)*0.25E-10)))
!         tem = max(0.1, min(1.0, sqrt(garea(ipt)*0.25E-10)))
!         tem = max(0.25, min(1.0, sqrt(garea(ipt)*0.25E-10)))
!         tem = max(0.50, min(1.0, sqrt(garea(ipt)*0.25E-10)))
!         tem = max(0.45, min(1.0, sqrt(garea(ipt)*0.25E-10))) ! for r2 and rf exp
!         tem = 1.0                  ! for r1 exp
!         tem = 0.45                 ! for r6 exp

!         trcfac(trac+1,l) = tem       !  For press grad correction c=0.55
!         trcfac(trac+2,l) = tem       !  in momentum mixing calculations
!
          if (ccwfac(ipt) >= 0.0) ccwf = ccwfac(ipt)
        ENDIF
        do l=1,k
          ud_mf(ipt,l)  = 0.0
          dd_mf(ipt,l)  = 0.0
          det_mf(ipt,l) = 0.0
        enddo
!
!     Compute NCRND  : here LMH is the number of layers above the
!                      bottom surface.  For sigma coordinate LMH=K.
!
        LMHIJ = LMH(ipt)
        if (flipv) then
           ll  = kp1 - LMH(ipt)
           tem = 1.0 / prsi(ipt,ll)
        else
           ll  = LMH(ipt)
           tem = 1.0 / prsi(ipt,ll+1)
        endif
        KRMIN = 1
        KRMAX = km1
        KFMAX = KRMAX
        DO L=1,LMHIJ-1
          ll = l
          if (flipv) ll = kp1 -l ! Input variables are bottom to top!
          SGC = prsl(ipt,ll) * tem
          IF (SGC .LE. 0.050) KRMIN = L
!         IF (SGC .LE. 0.600) KRMAX = L
          IF (SGC .LE. 0.700) KRMAX = L
!         IF (SGC .LE. 0.800) KRMAX = L
!!        IF (SGC .LE. 0.760) KRMAX = L
!         IF (SGC .LE. 0.930) KFMAX = L
          IF (SGC .LE. 0.970) KFMAX = L    ! Commented on 20060202
!LL2      IF (SGC .LE. 0.950) KFMAX = L
        ENDDO
!     if (lprnt .and. ipt .eq. ipr) print *,' krmin=',krmin,' krmax=',
!    &krmax,' kfmax=',kfmax,' lmhij=',lmhij,' tem=',tem
!
        if (fix_ncld_hr) then
          NCRND = min(nrcmax, (KRMAX-KRMIN+1)) * (DTF/1200) + 0.50001
!         NCRND = min(nrcmax, (KRMAX-KRMIN+1)) * (DTF/1800) + 0.50001
          facdt = delt_c / dt
        else
          NCRND = min(nrcmax, (KRMAX-KRMIN+1))
          facdt = 1.0 
        endif
        IF (DT .GT. DTF) NCRND = (5*NCRND) / 4
        NCRND   = max(NCRND, 1)
!
        KCR    = MIN(LMHIJ,KRMAX)
        KTEM   = MIN(LMHIJ,KFMAX)
        KFX    = KTEM - KCR
!     if(lprnt)print*,' enter RASCNV k=',k,' ktem=',ktem,' LMHIJ='
!    &,                 LMHIJ
!    &,               ' krmax=',krmax,' kfmax=',kfmax
!    &,               ' kcr=',kcr, ' cdrag=',cdrag(ipr)
 
        IF (KFX .GT. 0) THEN
           IF (BOTOP) THEN
              DO NC=1,KFX
                IC(NC) = KTEM + 1 - NC
              ENDDO
           ELSE
              DO NC=KFX,1,-1
               IC(NC) = KTEM + 1 - NC
              ENDDO
           ENDIF
        ENDIF
!
        NCMX  = KFX + NCRND
        IF (NCRND .GT. 0) THEN
          DO I=1,NCRND
            IRND = (RANNUM(ipt,I)-0.0005)*(KCR-KRMIN+1)
            IC(KFX+I) = IRND + KRMIN
          ENDDO
        ENDIF
!
!     ia = 1
!
!     print *,' in rascnv: k=',k,'lat=',lat,' lprnt=',lprnt
!     if (lprnt) then
!        if (me .eq. 0) then
!        print *,' tin',(tin(ia,l),l=k,1,-1)
!        print *,' qin',(qin(ia,l),l=k,1,-1)
!     endif
!
!
        lprint = lprnt .and. ipt .eq. ipr
        lprint = lprnt
!       kuo(ipt)  = 0
        do l=1,k
          ll = l
          if (flipv) ll = kp1 -l ! Input variables are bottom to top!
          CLW(l)     = 0.0       ! Assumes initial value of Cloud water
          CLI(l)     = 0.0       ! Assumes initial value of Cloud ice
                                 ! to be zero i.e. no environmental condensate!!!
!         CLT(ipt,l) = 0.0
          QII(l)     = 0.0
          QLI(l)     = 0.0
!                          Initialize heating, drying, cloudiness etc.
          tcu(l)     = 0.0
          qcu(l)     = 0.0
          pcu(l)     = 0.0
          flx(l)     = 0.0
          flxd(l)    = 0.0
          rcu(l,1)   = 0.0
          rcu(l,2)   = 0.0
!                          Transfer input prognostic data into local variable
          toi(l)     = tin(ipt,ll)
          qoi(l)     = qin(ipt,ll)
          uvi(l,trac+1) = uin(ipt,ll)
          uvi(l,trac+2) = vin(ipt,ll)
!
          do n=1,trac
            uvi(l,n) = ccin(ipt,ll,n+2)
          enddo
!
        enddo
        flx(k+1)  = 0.0
        flxd(k+1) = 0.0
!
        if (ccin(ipt,1,2) .le. -999.0) then
          do l=1,k
            ll = l
            if (flipv) ll = kp1 -l ! Input variables are bottom to top!
              tem = ccin(ipt,ll,1)                                      &
     &            * MAX(ZERO, MIN(ONE, (TCR-toi(L))*TCRF))
              ccin(ipt,ll,2) = ccin(ipt,ll,1) - tem
              ccin(ipt,ll,1) = tem
          enddo
        endif
        if (advcld) then
          do l=1,k
            ll = l
            if (flipv) ll = kp1 -l ! Input variables are bottom to top!
            QII(L) = ccin(ipt,ll,1)
            QLI(L) = ccin(ipt,ll,2)
          enddo
        endif
!
        KBL  = KPBL(ipt)
        if (flipv) KBL  = MAX(MIN(k, kp1-KPBL(ipt)), k/2)
        rain = 0.0
!
        DO L=1,kp1
          ll = l
          if (flipv) ll = kp1 + 1 - l      ! Input variables are bottom to top!
          PRS(LL)   = prsi(ipt, L) * facmb ! facmb is for conversion to MB
          PSJ(LL)   = prsik(ipt,L)
          phi_h(LL) = phii(ipt,L)
        ENDDO
!
        DO L=1,k
          ll = l
          if (flipv) ll = kp1 - l          ! Input variables are bottom to top!
          PRSM(LL)  = prsl(ipt, L) * facmb ! facmb is for conversion to MB
          PSJM(LL)  = prslk(ipt,L)
          phi_l(LL) = phil(ipt,L)
          rhc_l(LL) = rhc(ipt,L)
!
!         rhc_l(ll) = 1.0
        ENDDO
!
!     if(lprint) print *,' PRS=',PRS
!     if(lprint) print *,' PRSM=',PRSM
!     if (lprint) then
!        print *,' qns=',qns(ia),' qoi=',qn0(ia,k),'qin=',qin(ia,1)
!        if (me .eq. 0) then
!        print *,' toi',(tn0(ia,l),l=1,k)
!        print *,' qoi',(qn0(ia,l),l=1,k),' kbl=',kbl
!     endif
!
!
!!      PSJP(KP1) = PSJ(KP1) * PRS(KP1) * RKPP1I
!       do l=k,kctop(1),-1
!         DPI     = RKPP1I / (PRS(L+1) - PRS(L))
!         PSJM(L) = (PSJ(L+1)*PRS(L+1) - PSJ(L)*PRS(L)) * DPI
!!        PSJP(L) = PSJ(L) * PRS(L) * RKPP1I
!!        DPI(L)  = 1.0 / (PRS(L+1) - PRS(L))
!!        PSJM(L) = (PSJP(L+1) - PSJP(L)) * DPI(L)
!         PRSM(L) = 1000.0 * PSJM(L) ** (1.0/rkap)
!!        PRSM(L) = 1000.0 * PSJM(L) ** rkapi
!!        PRSM(L) = 0.5 * (prs(L+1)+prs(L))
!       enddo
!
!
!     
!     print *,' ipt=',ipt
        alfint(:,:) = 0.5              ! For second order scheme
        alfind(:)   = 0.5
        if (advups) then               ! For first order upstream for updraft
          alfint(:,:) = 1.0
        elseif (advtvd) then           ! TVD flux limiter scheme for updraft
          alfint(:,:) = 1.0
          l   = krmin
          lm1 = l - 1
          dtvd(1,1) = cp*(toi(l)-toi(lm1)) + phi_l(l)-phi_l(lm1)        &
     &              + alhl*(qoi(l)-qoi(lm1))
          dtvd(1,2) = qoi(l) - qoi(lm1)
          dtvd(1,3) = qli(l) - qli(lm1)
          dtvd(1,4) = qii(l) - qii(lm1)
          do l=krmin+1,k
            lm1 = l - 1
!     print *,' toi=',toi(l),toi(lm1),' phi_l=',phi_l(l),phi_l(lm1)
!    &,' qoi=',qoi(l),qoi(lm1),' cp=',cp,' alhl=',alhl
            dtvd(2,1)   = cp*(toi(l)-toi(lm1)) + phi_l(l)-phi_l(lm1)    &
     &                  + alhl*(qoi(l)-qoi(lm1))
!     print *,' l=',l,' dtvd=',dtvd(:,1)
            if (abs(dtvd(2,1)) > 1.0e-10) then
              tem1        = dtvd(1,1) / dtvd(2,1)
              tem2        = abs(tem1)
              alfint(l,1) = 1.0 - 0.5*(tem1 + tem2)/(1.0 + tem2)   ! for h
            endif
!     print *,' alfint=',alfint(l,1),' l=',l,' ipt=',ipt
            dtvd(1,1)   = dtvd(2,1)
!
            dtvd(2,2)   = qoi(l) - qoi(lm1)
!     print *,' l=',l,' dtvd2=',dtvd(:,2)
            if (abs(dtvd(2,2)) > 1.0e-10) then
              tem1        = dtvd(1,2) / dtvd(2,2)
              tem2        = abs(tem1)
              alfint(l,2) = 1.0 - 0.5*(tem1 + tem2)/(1.0 + tem2)   ! for q
            endif
            dtvd(1,2)   = dtvd(2,2)
!
            dtvd(2,3)   = qli(l) - qli(lm1)
!     print *,' l=',l,' dtvd3=',dtvd(:,3)
            if (abs(dtvd(2,3)) > 1.0e-10) then
              tem1        = dtvd(1,3) / dtvd(2,3)
              tem2        = abs(tem1)
              alfint(l,3) = 1.0 - 0.5*(tem1 + tem2)/(1.0 + tem2)   ! for ql
            endif
            dtvd(1,3)   = dtvd(2,3)
!
            dtvd(2,4)   = qii(l) - qii(lm1)
!     print *,' l=',l,' dtvd4=',dtvd(:,4)
            if (abs(dtvd(2,4)) > 1.0e-10) then
              tem1        = dtvd(1,4) / dtvd(2,4)
              tem2        = abs(tem1)
              alfint(l,4) = 1.0 - 0.5*(tem1 + tem2)/(1.0 + tem2)   ! for qi
            endif
            dtvd(1,4)   = dtvd(2,4)
          enddo
!
          if (ntrc > 0) then
            do n=1,ntrc
              l = krmin
              dtvd(1,1)   = uvi(l,n) - uvi(l-1,n)
              do l=krmin+1,k
                dtvd(2,1)     = uvi(l,n) - uvi(l-1,n)
!     print *,' l=',l,' dtvdn=',dtvd(:,1),' n=',n,' l=',l
                if (abs(dtvd(2,1)) > 1.0e-10) then
                  tem1          = dtvd(1,1) / dtvd(2,1)
                  tem2          = abs(tem1)
                  alfint(l,n+4) = 1.0 - 0.5*(tem1 + tem2)/(1.0 + tem2) ! for tracers
                endif
                dtvd(1,1)     = dtvd(2,1)
              enddo
            enddo
          endif
        endif
!
!     print *,' after alfint for ipt=',ipt
        if (CUMFRC) then
          
          do l=krmin,k
            tem = 1.0 - max(pgfbot, min(pgftop, pgftop+pgfgrad*prsm(l)))
            trcfac(trac+1,l) = tem
            trcfac(trac+2,l) = tem
          enddo
        endif
!
        lprint = lprnt .and. ipt .eq. ipr
!     if (lprint) then
!       print *,' trcfac=',trcfac(1+trac,krmin:k)
!       print *,' alfint=',alfint(krmin:k,1)
!       print *,' alfinq=',alfint(krmin:k,2)
!       print *,' alfini=',alfint(krmin:k,4)
!       print *,' alfinu=',alfint(krmin:k,5)
!     endif
!
        if (calkbl) kbl = k
        DO NC=1,NCMX
!
          IB = IC(NC)
          if (ib .gt. kbl) cycle
!       lprint = lprnt .and. ipt .eq. ipr
!       lprint = lprnt .and. ipt .eq. ipr .and. ib .eq. 41
!
          DNDRFT = DPD .GT. 0.0
!
!     if (lprint) print *,' calling cloud type ib=',ib,' kbl=',kbl
!    *,' kpbl=',kpbl,' alfint=',alfint,' frac=',frac
!    *,' ntrc=',ntrc,' ipt=',ipt
!
!       if (advtvd) then           ! TVD flux limiter scheme for updraft
!         l   = ib
!         lm1 = l - 1
!         dtvd(1,1) = cp*(toi(l)-toi(lm1)) + phi_l(l)-phi_l(lm1)
!    &              + alhl*(qoi(l)-qoi(lm1))
!         dtvd(1,2) = qoi(l) - qoi(lm1)
!         dtvd(1,3) = qli(l) - qli(lm1)
!         dtvd(1,4) = qii(l) - qii(lm1)
!         do l=ib+1,k
!           lm1 = l - 1
!           dtvd(2,1)   = cp*(toi(l)-toi(lm1)) + phi_l(l)-phi_l(lm1)
!    &                  + alhl*(qoi(l)-qoi(lm1))
!           if (abs(dtvd(2,1)) > 1.0e-10) then
!             tem1        = dtvd(1,1) / dtvd(2,1)
!             tem2        = abs(tem1)
!             alfint(l,1) = 1.0 - 0.5*(tem1 + tem2)/(1.0 + tem2)   ! for h
!           endif
!           dtvd(1,1)   = dtvd(2,1)
!
!           dtvd(2,2)   = qoi(l) - qoi(lm1)
!           if (abs(dtvd(2,2)) > 1.0e-10) then
!             tem1        = dtvd(1,2) / dtvd(2,2)
!             tem2        = abs(tem1)
!             alfint(l,2) = 1.0 - 0.5*(tem1 + tem2)/(1.0 + tem2)   ! for q
!           endif
!           dtvd(1,2)   = dtvd(2,2)
!
!           dtvd(2,3)   = qli(l) - qli(lm1)
!           if (abs(dtvd(2,3)) > 1.0e-10) then
!             tem1        = dtvd(1,3) / dtvd(2,3)
!             tem2        = abs(tem1)
!             alfint(l,3) = 1.0 - 0.5*(tem1 + tem2)/(1.0 + tem2)   ! for ql
!           endif
!           dtvd(1,3)   = dtvd(2,3)
!
!           dtvd(2,4)   = qii(l) - qii(lm1)
!           if (abs(dtvd(2,4)) > 1.0e-10) then
!             tem1        = dtvd(1,4) / dtvd(2,4)
!             tem2        = abs(tem1)
!             alfint(l,4) = 1.0 - 0.5*(tem1 + tem2)/(1.0 + tem2)   ! for qi
!           endif
!           dtvd(1,4)   = dtvd(2,4)
!         enddo
!
!         if (ntrc > 0) then
!           do n=1,ntrc
!             l = ib
!             dtvd(1,1)   = uvi(l,n) - uvi(l-1,n)
!             do l=ib+1,k
!               dtvd(2,1)     = uvi(l,n) - uvi(l-1,n)
!               if (abs(dtvd(2,1)) > 1.0e-10) then
!                 tem1        = dtvd(1,1) / dtvd(2,1)
!                 tem2          = abs(tem1)
!                 alfint(l,n+4) = 1.0 - 0.5*(tem1 + tem2)/(1.0 + tem2) ! for tracers
!               endif
!               dtvd(1,1)     = dtvd(2,1)
!             enddo
!           enddo
!         endif
!       endif
!
!
!     if (lprint) then
!     ia = ipt
!     print *,' toi=',(toi(ia,l),l=1,K)
!     print *,' qoi=',(qoi(ia,l),l=1,K),' kbl=',kbl
!     print *,' toi=',(toi(l),l=1,K)
!     print *,' qoi=',(qoi(l),l=1,K),' kbl=',kbl
!     print *,' prs=',(prs(l),l=1,K)
!     endif
!
          WFNC = 0.0
          do L=IB,K+1
            FLX(L)    = 0.0
            FLXD(L)   = 0.0
          enddo
!
!
!     if (me .eq. 0) then
!     if(lprint)then
!     print *,' CALLING CLOUD TYPE IB= ', IB,' DT=',DT,' K=',K
!    &, 'ipt=',ipt
!     print *,' TOI=',(TOI(L),L=IB,K)
!     print *,' QOI=',(QOI(L),L=IB,K)
!     endif
!     print *,' alft=',alfint
!
          TLA = -10.0
!
!     if (lprint) print *,' qliin=',qli
!     if (lprint) print *,' qiiin=',qii
          CALL CLOUD(lmhij, IB, ntrc                                    &
     &,              RASAL(IB), FRAC,  MAX_NEG_BOUY                     &
     &,              ALFINT,         rhfacl, rhfacs, garea(ipt)         &
!    &,              ALFINT, ALFINQ, rhfacl, rhfacs, garea(ipt)         &
     &,              alfind, rhc_l                                      &
!
     &,              TOI, QOI, UVI, PRS, PRSM, phi_l, phi_h             &
!    &,              TOI, QOI, UVI, PRS, PRSM, PSJ, PSJM
!    &,              TOI, QOI, UVI, PRS, PRSM, PSJ, PSJM, DPI
!    &,              TOI, QOI, UVI, PRS, PSJ
     &,              QLI, QII, KBL, DDVEL(ipt)                          &
     &,              CDRAG(ipt),lprint, trcfac, ccwf                    &
!    &,              IDIAG, lprnt
     &,              TCU, QCU, RCU, PCU, FLX, FLXD                      &
     &,              RAIN, REVAP, DT                                    &
     &,              WFNC, WRKFUN, CALKBL, CRTFUN, TLA, DNDRFT, DPD)     
!    &,              WFNC, WRKFUN, CALKBL, CRTFUN, TLA, DNDRFT, UPDRET)
!     if (lprint) print *,' rain=',rain,' ipt=',ipt
!     if (me .eq. 0) then
!     if (lprint) then
!     print *,' after calling CLOUD TYPE IB= ', IB                      &
!    &,' rain=',rain,' prskd=',prs(ib),' qli=',qli(ib),' qii=',qii(ib)
!     print *,' TOI=',(TOI(L),L=1,K),' me=',me,' ib=',ib
!     print *,' QOI=',(QOI(L),L=1,K)
!     endif
!     if (lprint) print *,' qliou=',qli
!     if (lprint) print *,' qiiou=',qii
!
          do L=IB,K
            ll = l
            if (flipv) ll  = kp1 -l    ! Input variables are bottom to top!
            ud_mf(ipt,ll)  = ud_mf(ipt,ll)  + flx(l+1)
            dd_mf(ipt,ll)  = dd_mf(ipt,ll)  + flxd(l+1)
          enddo
          ll = ib
          if (flipv) ll  = kp1 - ib
          det_mf(ipt,ll) = det_mf(ipt,ll) + flx(ib)
! 
!     Compute cloud amounts for the Goddard radiation
!
!         IF (FLX(KBL) .GT. 0.0) THEN
!           PL   = 0.5 * (PRS(IB) + PRS(IB+1))
!           CFAC = MIN(1.0, MAX(0.0, (850.0-PL)*PFAC))
!         ELSE
!           CFAC = 0.0
!         ENDIF
!
!   Warining!!!!
!   ------------
!   By doing the following, CLOUD does not contain environmental
!   condensate!
!
          if (.not. advcld) then
            do l=1,K
!             clw(l ) = clw(l) + QLI(L) + QII(L)
              clw(l ) = clw(l) + QLI(L)
              cli(l ) = cli(l) + QII(L)
              QLI(L)  = 0.0
              QII(L)  = 0.0
            enddo
          endif
!
        ENDDO                      ! End of the NC loop!
!
        RAINC(ipt) = rain * 0.001    ! Output rain is in meters
!     if(lprint)print*,' convective precip=',rain*86400/dt,' mm/day'
!    1,               ' ipt=',ipt
!
!     if (lprint) then
!        print *,' toi',(tn0(imax,l),l=1,k)
!        print *,' qoi',(qn0(imax,l),l=1,k)
!     endif
!
        do l=1,k
          ll = l
          if (flipv) ll  = kp1 - l
          tin(ipt,ll)    = toi(l)                   ! Temperature
          qin(ipt,ll)    = qoi(l)                   ! Specific humidity
          uin(ipt,ll)    = uvi(l,trac+1)            ! U momentum
          vin(ipt,ll)    = uvi(l,trac+2)            ! V momentum
!         clw(l)         = clw(l) + qli(l) + qii(l) ! Cloud condensate
!         ccin(ipt,ll,1) = ccin(ipt,ll,1) + clw(l)
          do n=1,trac
            ccin(ipt,ll,n+2) = uvi(l,n)             ! Tracers
          enddo
        enddo
        if (advcld) then
          do l=1,k
            ll = l
            if (flipv) ll  = kp1 - l
!           ccin(ipt,ll,1) = qli(l) + qii(l) ! Cloud condensate
            ccin(ipt,ll,1) = qii(l)          ! Cloud ice
            ccin(ipt,ll,2) = qli(l)          ! Cloud water
          enddo
        else
          do l=1,k
            ll = l
            if (flipv) ll  = kp1 - l
!           ccin(ipt,ll,1) = ccin(ipt,ll,1) + clw(l)
            ccin(ipt,ll,1) = ccin(ipt,ll,1) + cli(l)
            ccin(ipt,ll,2) = ccin(ipt,ll,2) + clw(l)
          enddo
        endif
!
!       kuo(ipt)  = 0
!
        ktop(ipt) = kp1
        kbot(ipt) = 0

        do l=lmhij-1,1,-1
          if (prs(lmhij+1)-prs(l) .gt. 250.0 .and. tcu(l) .ne. 0.0) then ! for r1 &rf
!         if (prs(lmhij+1)-prs(l) .gt. 100.0 .and. tcu(l) .ne. 0.0) then ! for r1 &rf
!1        if (prs(lmhij+1)-prs(l) .gt. 300.0 .and. tcu(l) .ne. 0.0) then
!         if (prs(lmhij+1)-prs(l) .gt. 500.0 .and. tcu(l) .ne. 0.0) then
!         if (prs(lmhij+1)-prs(l) .gt. 400.0 .and. tcu(l) .ne. 0.0) then
!         if (prs(kp1)-prs(l) .gt. 500.0 .and. tcu(l) .ne. 0.0) then ! for r2 exp
!         if (prs(kp1)-prs(l) .gt. 400.0 .and. tcu(l) .ne. 0.0) then
!         if (prs(lmhij+1)-prs(l) .gt. 200.0 .and. tcu(l) .ne. 0.0) then
!         if (prsm(l) .lt. 900.0 .and. tcu(l) .ne. 0.0) then
!         if (phi_l(l) .gt. 10000.0 .and. tcu(l) .ne. 0.0) then
             kuo(ipt) = 1
          endif
!  New test for convective clouds ! added in 08/21/96
          if (clw(l)+cli(l) .gt. 0.0 .OR.                               &
     &        qli(l)+qii(l) .gt. clwmin) ktop(ipt) = l
        enddo
        do l=1,km1
          if (clw(l)+cli(l) .gt. 0.0 .OR.                               &
     &        qli(l)+qii(l) .gt. clwmin) kbot(ipt) = l
        enddo
        if (flipv) then
          ktop(ipt) = kp1 - ktop(ipt)
          kbot(ipt) = kp1 - kbot(ipt)
        endif
!
!     if (lprint) then
!        print *,' tin',(tin(ia,l),l=k,1,-1)
!        print *,' qin',(qin(ia,l),l=k,1,-1)
!     endif
!
!     Velocity scale from the downdraft!
!
        DDVEL(ipt) = DDVEL(ipt) * DDFAC * GRAV / (prs(K+1)-prs(k))
!
      ENDDO                            ! End of the IPT Loop!
      deallocate (alfint)
!
      RETURN
      END
      SUBROUTINE CRTWRK(PL, CCWF, ACR)
      USE MACHINE , ONLY : kind_phys
      use module_ras , only : ac, ad
      Implicit none
!
      real(kind=kind_phys) PL, CCWF, ACR
      INTEGER IWK
!
      IWK = PL * 0.02 - 0.999999999
      IWK = MAX(1, MIN(IWK,16))
      ACR = (AC(IWK) + PL * AD(IWK)) * CCWF
!
      RETURN
      END
      SUBROUTINE CLOUD(                                                 &
     &                  K, KD, M                                        &
     &,                 RASALF, FRACBL, MAX_NEG_BOUY                    &
     &,                 ALFINT,         RHFACL, RHFACS, garea           &
!    &,                 ALFINT, ALFINQ, RHFACL, RHFACS, garea           &
     &,                 alfind, rhc_ls                                  &

     &,                 TOI, QOI, ROI, PRS, PRSM, phil, phih            &
!    &,                 TOI, QOI, ROI, PRS, PRSM, PRJ, PRJM, DPI        &
!    &,                 TOI, QOI, ROI, PRS, PRJ                         &
     &,                 QLI, QII, KPBL, DSFC                            &
     &,                 CD,lprnt, trcfac,ccwf                           &
!    &,                 IDIAG, lprnt                                    &

     &,                 TCU, QCU, RCU, PCU, FLX, FLXD                   &
!    &,                 TCD, QCD                                        &
     &,                 CUP, REVAP, DT                                  &
     &,                 WFNC, WRKFUN, CALKBL, CRTFUN, TLA, DNDRFT, DPD)  

!
!***********************************************************************
!******************** Relaxed  Arakawa-Schubert ************************
!****************** Plug Compatible Scalar Version *********************
!************************ SUBROUTINE CLOUD  ****************************
!************************  October 2004     ****************************
!********************  VERSION 2.0  (modified) *************************
!************* Shrinivas.Moorthi@noaa.gov (301) 763 8000(X7233) ********
!***********************************************************************
!*Reference:
!-----------
!     NOAA Technical Report NWS/NCEP 99-01:
!     Documentation of Version 2 of Relaxed-Arakawa-Schubert
!     Cumulus Parameterization with Convective Downdrafts, June 1999.
!     by S. Moorthi and M. J. Suarez.
!
!***********************************************************************
!
!===>    UPDATES CLOUD TENDENCIES DUE TO A SINGLE CLOUD
!===>    DETRAINING AT LEVEL KD.
!
!***********************************************************************
!
!===>  TOI(K)     INOUT   TEMPERATURE             KELVIN
!===>  QOI(K)     INOUT   SPECIFIC HUMIDITY       NON-DIMENSIONAL
!===>  ROI(K,M)   INOUT   TRACER                  ARBITRARY
!===>  QLI(K)     INOUT   LIQUID WATER            NON-DIMENSIONAL
!===>  QII(K)     INOUT   ICE                     NON-DIMENSIONAL

!===>  PRS(K+1)   INPUT   PRESSURE @ EDGES        MB
!===>  PRSM(K)    INPUT   PRESSURE @ LAYERS       MB
!===>  PHIH(K+1)  INPUT   GEOPOTENTIAL @ EDGES  IN MKS units
!===>  PHIL(K)    INPUT   GEOPOTENTIAL @ LAYERS IN MKS units
!===>  PRJ(K+1)   INPUT   (P/P0)^KAPPA  @ EDGES   NON-DIMENSIONAL
!===>  PRJM(K)    INPUT   (P/P0)^KAPPA  @ LAYERS  NON-DIMENSIONAL

!===>  K      INPUT   THE RISE & THE INDEX OF THE SUBCLOUD LAYER
!===>  KD     INPUT   DETRAINMENT LEVEL ( 1<= KD < K )          
!===>  M      INPUT   NUMBER OF TRACERS. MAY BE ZERO.
!===>  DNDRFT INPUT   LOGICAL .TRUE. OR .FALSE.
!===>  DPD    INPUT   Minumum Cloud Depth for DOWNDRFAT Computation hPa
!
!===>  TCU(K  )   UPDATE  TEMPERATURE TENDENCY       DEG
!===>  QCU(K  )   UPDATE  WATER VAPOR TENDENCY       (G/G)
!===>  RCU(K,M)   UPDATE  TRACER TENDENCIES          ND
!===>  PCU(K-1)   UPDATE  PRECIP @ BASE OF LAYER     KG/M^2
!===>  FLX(K  )   UPDATE  MASS FLUX @ TOP OF LAYER   KG/M^2
!===>  CUP        UPDATE  PRECIPITATION AT THE SURFACE KG/M^2
!
      USE MACHINE , ONLY : kind_phys
      use module_ras
      IMPLICIT NONE
!
!  INPUT ARGUMENTS

      LOGICAL REVAP, DNDRFT, WRKFUN, CALKBL, CRTFUN, CALCUP
      logical lprnt
      INTEGER K, KD, M


      real(kind=kind_phys) TOI(K),    QOI(K ),  PRS(K+1), PRSM(K)       &
     &,                    QLI(K),    QII(K)                            &
     &,                    PHIH(K+1), ROI(K,M), PHIL(K)
!    &,                    PRJ(K+1),  ROI(K,M), PRJM(K)
!    &,                    PRJ(K+1),  ROI(K,M), PRJM(K),  DPI(K)
      real(kind=kind_phys) CD,        UFN,     DSFC
      INTEGER KPBL,   KBL,     KB1

!     real(kind=kind_phys) RASALF, FRACBL, MAX_NEG_BOUY, ALFINT(K),     &
      real(kind=kind_phys) RASALF, FRACBL, MAX_NEG_BOUY, ALFINT(K,M+4), &
     &                     RHFACL, RHFACS, garea, ccwf
      real(kind=kind_phys) DPD, alfind(k), rhc_ls(k)
!     real(kind=kind_phys) ALFINQ(K), DPD, alfind(k), rhc_ls(k)
      real(kind=kind_phys) trcfac(M,k)
 
!  UPDATE ARGUMENTS

      real(kind=kind_phys) TCU(K),   QCU(K),    RCU(K,M)                &
     &,                    TCD(K),   QCD(K),    PCU(K)                  &
     &,                    FLX(K+1), FLXD(K+1), CUP

!  TEMPORARY WORK SPACE

      real(kind=kind_phys) HOL(KD:K),  QOL(KD:K),   GAF(KD:K+1)         &
     &,                    HST(KD:K),  QST(KD:K),   TOL(KD:K)           &
     &,                    GMH(KD:K),  GMS(KD:K+1), GAM(KD:K+1)         &
     &,                    AKT(KD:K),  AKC(KD:K),   BKC(KD:K)           &
     &,                    LTL(KD:K),  RNN(KD:K),   FCO(KD:K)           &
     &,                                PRI(KD:K)                        &
!    &,                    PRH(KD:K),  PRI(KD:K)                        &
     &,                    QIL(KD:K),  QLL(KD:K)                        &
     &,                    ZET(KD:K),  XI(KD:K),    RNS(KD:K)           &
     &,                    Q0U(KD:K),  Q0D(KD:K),   vtf(KD:K)           &
     &,                    DLB(KD:K+1),DLT(KD:K+1), ETA(KD:K+1)         &
     &,                    PRL(KD:K+1)                                  &
     &,                    CIL(KD:K),  CLL(KD:K),   ETAI(KD:K)

      real(kind=kind_phys) ALM,   DET,    HCC,  CLP                     &
     &,                    HSU,   HSD,    QTL,  QTV                     &
     &,                    AKM,   WFN,    HOS,  QOS                     &
     &,                    AMB,   TX1,    TX2,  TX3                     &
     &,                    TX4,   TX5,    QIS,  QLS                     &
     &,                    HBL,   QBL,    RBL(M)                        &
     &,                    QLB,   QIB,    PRIS                          &
     &,                    WFNC,  TX6,    ACR                           &
     &,                    TX7,   TX8,    TX9,  RHC                     &
     &,                    hstkd, qstkd,  ltlkd, q0ukd, q0dkd, dlbkd    &
     &,                    qtp, qw00, qi00, qrbkd                       &
     &,                    hstold, rel_fac

!     INTEGER IA,  I1,  I2, ID1, ID2
!     INTEGER IB,  I3

      LOGICAL UNSAT, ep_wfn

      LOGICAL LOWEST, SKPDD

!     real(kind=kind_phys) TL, PL, QL, QS, DQS, ST1, SGN, C0, TAU,      &
!    &                     QTVP, HB, QB, TB, QQQ, C0I, QI0, QW0,        &
      real(kind=kind_phys) TL, PL, QL, QS, DQS, ST1, SGN, TAU,          &
     &                     QTVP, HB, QB, TB, QQQ,                       &
     &                     HCCP, DS, DH, AMBMAX, X00, EPP, QTLP,        &
!    &                     DPHIB, DPHIT, DEL_ETA, DETP, QUDFAC,         &
!    &                     DPI, DPHIB, DPHIT, DEL_ETA, DETP, QUDFAC,    &
     &                     DPI, DPHIB, DPHIT, DEL_ETA, DETP,            &
     &                     TEM, TEM1, TEM2, TEM3, TEM4,                 &
!    &                     TEM, TEM1, TEM2, TEM3, TEM4, ONEBG,          &
!    &                     TSTMBA, HCRIT,   RKPP1I, ST2,                &
     &                     ST2, ST3, ST4, ST5,                          &
     &                     ERRMIN, ERRMI2, ERRH, ERRW, ERRE, TEM5,      &
     &                     TEM6, HBD, QBD, st1s
      parameter (ERRMIN=0.0001, ERRMI2=0.1*ERRMIN)
!     parameter (c0=1.0e-3, KBLMX=20, ERRMIN=0.0001, ERRMI2=0.1*ERRMIN)
      INTEGER I, L,  N,  KD1, II                                        &
     &,       KP1, IT, KM1, KTEM, KK, KK1, LM1, LL, LP1, kbls, kmxh

      real avt, avq, avr, avh
!
!     REEVAPORATION
!
!     real(kind=kind_phys), parameter ::
!    &                   clfa = -0.452550814376093547E-03
!    &,                  clfb =  0.161398573159240791E-01
!    &,                  clfc = -0.163676268676807096
!    &,                  clfd =  0.447988962175259131
!    &,                  point3 = 0.3, point01=0.01

!     real(kind=kind_phys), parameter :: rainmin=1.0e-9
      real(kind=kind_phys), parameter :: rainmin=1.0e-8
      real(kind=kind_phys), parameter :: oneopt9=1.0/0.09
      real(kind=kind_phys), parameter :: oneopt4=1.0/0.04

      real(kind=kind_phys) CLFRAC, DT, clf, clvfr

      real(kind=kind_phys) ACTEVAP,AREARAT,DELTAQ,MASS,MASSINV,POTEVAP  &
     &,                    TEQ,QSTEQ,DQDT,QEQ
!    &,                    ELOCP,GRAVCON, GRAVFAC,  AFC, RKNOB, ELFOCP
!
!     Temporary workspace and parameters needed for downdraft
!
      real(kind=kind_phys) TLA, GMF
!
      real(kind=kind_phys) BUY(KD:K+1), QRB(KD:K),   QRT(KD:K)          &
     &,                    ETD(KD:K+1), HOD(KD:K+1), QOD(KD:K+1)        &
     &,                    GHD(KD:K),   GSD(KD:K),   EVP(KD:K)          &
     &,                    ETZ(KD:K),   CLDFR(KD:K)                     &
     &,                    TRAIN, DOF, CLDFRD                           &
     &,                    FAC, RSUM1, RSUM2, RSUM3, dpneg
      INTEGER IDH
      LOGICAL DDFT, UPDRET
!     real(kind=kind_phys) eps, epsm1, rvi, facw, faci, hsub, tmix, DEN
!     real(kind=kind_phys) eps, epsm1, rv, rd, depth
!     real(kind=kind_phys) eps, epsm1, rv, rd, fpvs, depth
!
!
!***********************************************************************
!
!CFPP$ EXPAND (QSATCN, CRTWRK)
!CFPP$ NOCONCUR R
!
      do l=1,K
        tcd(L) = 0.0
        qcd(L) = 0.0
      enddo
!
      KP1     = K  + 1
      KM1     = K  - 1
      KD1     = KD + 1
      kblmx   = k / 2
!
!     if (lprnt) print *,' IN CLOUD for KD=',kd
!     if (lprnt) print *,' prs=',prs(Kd:K+1)
!     if (lprnt) print *,' phil=',phil(KD:K)
!     if (lprnt) print *,' phih=',phih(KD:K+1)
!     if (lprnt) print *,' toi=',toi
!     if (lprnt) print *,' qoi=',qoi
!
!     do l=kd1,k
!       alfint(l) = (prjm(l)-prj(l)) / (prjm(l)-prjm(l-1))
!       alfinq(l) = alfint(l)
!     enddo
!
      CLDFRD   = 0.0
      DOF      = 0.0
      PRL(KP1) = PRS(KP1)
!
      DO L=KD,K
        RNN(L) = 0.0
        ZET(L) = 0.0
        XI(L)  = 0.0
!
        TOL(L) = TOI(L)
        QOL(L) = QOI(L)
        PRL(L) = PRS(L)
        BUY(L) = 0.0
        CLL(L) = QLI(L)
        CIL(L) = QII(L)
      ENDDO
!
      DO L=KD, K
        DPI    = ONE / (PRL(L+1) - PRL(L))
        PRI(L) = GRAVFAC * DPI
!
        PL     = PRSM(L)
        TL     = TOL(L)

!     if (lprnt) print *,' l=',l,' prl=',prl(l+1),prl(l),' pl=',pl,
!    &' dpi=',dpi,' prsm=',prsm(l)

        AKT(L) = (PRL(L+1) - PL) * DPI
!
!     if (lprnt) print *,' l=',l,' prl=',prl(l+1),prl(l),' pl=',pl,
!    &' dpi=',dpi,' prsm=',prsm(l),' akt=',akt(l)
!
        CALL QSATCN(TL, PL, QS, DQS,lprnt)
!
!     if(lprnt)print*,' qs=',qs,' tl=',tl,' pl=',pl
!    1,               ' dqs=',dqs,' qol=',qol(l)
!
!
        QST(L) = QS
        GAM(L) = DQS * ELOCP
        ST1    = ONE + GAM(L)
        GAF(L) = (ONE/ALHL) * (GAM(L)/(ONE + GAM(L)))
 
        QL     = MAX(MIN(QS*RHMAX,QOL(L)), ONE_M10)
        QOL(L) = QL
 
        TEM    = CP * TL
        LTL(L) = TEM * ST1 / (ONE+NU*(QST(L)+TL*DQS))
        vtf(L) = 1.0 + NU * QL
        ETA(L) = ONE / (LTL(L) * VTF(L))

        HOL(L) = TEM + QL * ALHL
        HST(L) = TEM + QS * ALHL
!
!     if(lprnt)print*,' l=',l,' hst=',hst(l),' tem=',tem
!    1,               ' qs=',qs,' alhl=',alhl
!     if (lprnt) print *,' L=',L,' tem=',tem,' ql=',ql,' alhl=',alhl
!    &,' alhf=',alhf,' qii=',qii(l),' cp=',cp,' tl=',tl
!    &,' qs=',qs,' qol=',qol(l),' rhmax=',rhmax,' hol=',hol(l)
!    &,' pl=',pl
  
      ENDDO
!
      ETA(K+1) = ZERO
      GMS(K)   = ZERO
!
      AKT(KD)  = HALF
      GMS(KD)  = ZERO
!
      CLP      = ZERO
!
      GAM(K+1) = GAM(K)
      GAF(K+1) = GAF(K)
!
      DO L=K,KD1,-1
!       TEM1   = CP * TOL(L) * VTF(L) / PRH(L)
 
        DPHIB  = PHIL(L) - PHIH(L+1)
        DPHIT  = PHIH(L) - PHIL(L)
!
        DLB(L) = DPHIB * ETA(L)
        DLT(L) = DPHIT * ETA(L)
!
        QRB(L) = DPHIB
        QRT(L) = DPHIT
!
        ETA(L) = ETA(L+1) + DPHIB

!     if (lprnt) print *,' L=',L,' dphib=',dphib,' dphit=',dphit
!    &,' eta=',eta(l),' hol_new=',hol(l)+eta(l)
!    &,' cp=',cp,' tol=',tol(l),' vtf=',vtf(l)
!
        HOL(L) = HOL(L) + ETA(L)
        hstold = hst(l)
        HST(L) = HST(L) + ETA(L)
!
!     if(lprnt)print*,' l=',l,' hst=',hst(l),' eta=',eta(l)
!    1,               ' hstold=',hstold

        ETA(L) = ETA(L) + DPHIT
      ENDDO
!
!     For the cloud top layer
!
      L = KD

      DPHIB  = PHIL(L) - PHIH(L+1)
!
      DLB(L) = DPHIB * ETA(L)
!
      QRB(L) = DPHIB
      QRT(L) = DPHIB
!
      ETA(L) = ETA(L+1) + DPHIB

      HOL(L) = HOL(L) + ETA(L)
      HST(L) = HST(L) + ETA(L)
!
!     if (kd .eq. 12) then
!     if (lprnt) print *,' IN CLOUD for KD=',KD,' K=',K
!     if (lprnt) print *,' l=',l,' hol=',hol(l),' hst=',hst(l)
!     if (lprnt) print *,' TOL=',tol
!     if (lprnt) print *,' qol=',qol
!     if (lprnt) print *,' hol=',hol
!     if (lprnt) print *,' hst=',hst
!     endif
!
!     To determine KBL internally -- If KBL is defined externally
!     the following two loop should be skipped
!
!     if (lprnt) print *,' calkbl=',calkbl

      IF (CALKBL) THEN
         KTEM = MAX(KD, K-KBLMX-2)
         kmxh = k

!        DO L=KM1,KTEM,-1
!     if(lprnt) print *,' l=',l,' kmxh=',kmxh,' prl=',prl(l)
!    &, prl(k),' hol=',hol(l),hol(kmxh)
!          if (prl(k) - prl(l) .gt. 100.0) exit
!          if (hol(l) .gt. hol(kmxh)) kmxh = l
!     if(lprnt) print *,' l=',l,' kmxh=',kmxh,' prl=',prl(l)
!        ENDDO

         DO L=kmxh,KTEM+1,-1
           kbls = l
           if (hst(l-1) .gt. hst(l)) exit
         ENDDO
         KBL   = Kmxh
         TX1   = ZERO
         UNSAT = .FALSE.
         DO L=kmxh-1,KTEM,-1
           TEM = HOL(K) - HOL(L)
           TX3 = (HOL(L) - HOL(L+1)) / (PRL(L+2) - PRL(L))

!     if (lprnt) print *,' l=',l,' kbl=',kbl,' tx3=',tx3,' tx1=',tx1
           IF (TX3 .LT. TX1 .AND. TEM .LT. HCRIT) THEN
             TX1   = TX3
             KBL   = L
!            KBL   = L+1
             UNSAT = .TRUE.
           ELSEIF (UNSAT .AND.                                          &
     &           ( ((KBL .LT. K-1) .AND. TX3 .GT. 0.5*TX1)              &
     &              .OR. TEM .GT. HCRIT) ) THEN
             TX1 = -1.0E20
           ENDIF
         ENDDO
!     if(lprnt) print *,' kbl=',kbl,' kbls=',kbls,' kmxh=',kmxh
!
!        ii = min(kbl,kbls)
         ii = kbl
         do l=ktem,kmxh-1
!          if (hol(kmxh) .gt. hst(l)) kbl = l
           if (hol(kmxh) .gt. hst(l)) kbl = l+1    ! Commented on 09/20/04
         enddo
!        if(lprnt) print *,' kblhst=',kbl,' ii=',ii

         if (prl(K+1) - prl(ii) .gt. 50.0 .and. ii .gt. kbl) kbl = ii
!        if(lprnt) print *,' kbl2=',kbl,' ii=',ii
         if (kbl .ne. ii) then
!          kbl = min(K, max(kbl+1, kd-1))
!!!        kbl = min(K, max(kbl, kd-1))
           if (PRL(K+1)-PRL(KBL) .gt. bldmax) kbl = max(kbl,ii)
         endif
!        if (ii .gt. kbl) then
!           if (hol(Kmxh)-hol(kbl) .gt. hcrit) kbl = ii
!        endif
!
!        ii = kbl
!        do l=ii,k
!          if (hol(k) .gt. hst(l)) kbl = l
!        enddo
!!!      kbl = min(K, max(kbl, kd-1))
!
         KBL  = min(k, MAX(KBL,K-KBLMX))
!!!!!    kbl = K - 2
!!!
!        tem1 = max(10.0, min(50.0,(prl(k+1) - prl(kd))*0.05))
!LL2     tem1 = max(10.0, min(50.0,(prl(k+1) - prl(kd))*0.066))
!        do l=k,k-kblmx,-1
!          tem = prl(k+1) - prl(l)
!LL        if (tem .gt. 20.0) then
!LL        if (tem .gt. 40.0) then
!!r1       if (tem .gt. 50.0) then
!          if (tem .gt. tem1) then
!            kbl = min(kbl,l)
!            exit
!          endif
!        enddo
! 
         tem1 = max(prl(k+1)-prl(k),                                    &
     &                     min((prl(kbl) - prl(kd))*0.05, 20.0))
!    &                     min((prl(kbl) - prl(kd))*0.05, 30.0))
         if (prl(k+1)-prl(kbl) .lt. tem1) then
           KTEM = MAX(KD+1, K-KBLMX)
           do l=k,KTEM,-1
             tem = prl(k+1) - prl(l)
             if (tem .gt. tem1) then
               kbl = min(kbl,l)
               exit
             endif
           enddo
         endif
!!!

         KPBL = KBL
!     if(lprnt)print*,' 1st kbl=',kbl,' kblmx=',kblmx,' kd=',kd
!     if(lprnt)print*,' tx3=',tx3,' tx1=',tx1,' tem=',tem
!    1,               ' hcrit=',hcrit
      ELSE
         KBL  = KPBL
!     if(lprnt)print*,' 2nd kbl=',kbl
      ENDIF
!     if(lprnt)print*,' after CALKBL l=',l,' hol=',hol(l)
!    1,               ' hst=',hst(l)
!
      KBL      = MAX(KBL,KD)
      KB1      = KBL - 1
!!
!!
!     do kbl=k,kd1,-1
!       st1  = 1.0 / (PRL(K+1) - PRL(KBL))
!       tem1 = (PRL(K+1)-PRL(K)) * st1
!       HBL = HOL(K) * tem1
!
!       DO L=KM1,KBL,-1
!         tem2 = (PRL(K+1)-PRL(L)) * st1
!         TEM  = tem2 - tem1
!         HBL  = HBL + HOL(L) * TEM
!         tem1 = tem2
!       enddo
!     if(lprnt) print *,' HBL=',HBL,' KBL=',KBL
!       KB1 = KBL - 1
!       st2 = 0.5 * (hst(kbl)+hst(kb1))
!       if (st2 .le. hbl) exit
!     ENDDO
!     if (hst(kbl) .le. hbl) kbl = kbl + 1
!     kbl = min(kbl+1, K)
!     KB1 = KBL - 1
!!    if (lprnt) print *,' HBL=',HBL,' HST=',st2,' KBL=',KBL,' kb1=',kb1
!     if(lprnt)print *,' HBL=',HBL,' HST=',hst(l),' KBL=',KBL
!    1,                ' kb1=',kb1
!     if (PRL(K+1)-PRL(KBL) .gt. bldmax) return
!!
!!
!
!     if (lprnt) print *,' kbl=',kbl,' prlkbl=',prl(kbl),prl(k+1)
      if(kb1 .le. kd)then
!       if(lprnt)print*,' kb1=',kb1,' kd=',kd,' EXIT CLOUD'
        return
      endif
      if(PRL(K+1)-PRL(KBL) .gt. bldmax)then
!       if(lprnt)print*,' prl(k+1)=',prl(k+1),' prl(kbl)=',prl(kbl)
!    1,               ' bldmax=',bldmax,' k+1=',k+1,' kbl=',kbl
!    2,               ' EXIT CLOUD'
        return
      endif
!
!     if (lprnt) print *,' kbl=',kbl
!
      PRIS     = ONE / (PRL(K+1)-PRL(KBL))
      TX1      = ETA(KBL)
!
      GMS(KBL) = 0.0
      XI(KBL)  = 0.0
      ZET(KBL) = 0.0
!     DEPTH    = ETA(KD) - ETA(KBL)
!
      DO L=K,KD,-1
        IF (L .GE. KBL) THEN
          ETA(L) = (PRL(K+1)-PRL(L)) * PRIS
        ELSE
          ZET(L) = (ETA(L) - TX1) * ONEBG
          XI(L)  =  ZET(L) * ZET(L) * QUDFAC
          ETA(L) =  ZET(L) - ZET(L+1)
          GMS(L) =  XI(L)  - XI(L+1)
        ENDIF
      ENDDO
!
      HBL = HOL(K) * ETA(K)
      QBL = QOL(K) * ETA(K)
      QLB = CLL(K) * ETA(K)
      QIB = CIL(K) * ETA(K)
!     TX1 = QOL(K) / QST(K) * ETA(K)
      TX1 = QST(K) * ETA(K)
!
      DO L=KM1,KBL,-1
         TEM = ETA(L) - ETA(L+1)
         HBL = HBL + HOL(L) * TEM
!     if(lprnt)print*,' l=',l,' qbl=',qbl,' qol=',qol(l)
!    1,               ' tem=',tem
         QBL = QBL + QOL(L) * TEM
         QLB = QLB + CLL(L) * TEM
         QIB = QIB + CIL(L) * TEM
!        TX1 = TX1 + QOL(L) / QST(L) * TEM
         TX1 = TX1 + QST(L) * TEM
      ENDDO
!     if (lprnt) print *,' hbl=',hbl,' qbl=',qbl
!                                   Find Min value of HOL in TX2
      TX2 = HOL(KD)
      IDH = KD1
      DO L=KD1,KB1
        IF (HOL(L) .LT. TX2) THEN
           TX2 = HOL(L)
           IDH = L             ! Level of minimum moist static energy!
        ENDIF
      ENDDO
      IDH = 1
      IDH = MAX(KD1, IDH)
!
      TEM1 = HBL - HOL(KD)
      TEM  = HBL - HST(KD1)                                             &
     &             - LTL(KD1) *( NU *(QOL(KD1)-QST(KD1)))
      LOWEST = KD .EQ. KB1
!

!     TX1   = QBL / TX1
      TX1   = RHFACS - QBL / TX1       !     Average RH
!     TX1   = RHFACS - TX1             !     Average of each layer RH
      UNSAT = (TEM .GT. ZERO .OR. (LOWEST .AND. TEM1 .GE. ZERO))        &
     &         .AND. (TX1 .LT. RHRAM)                                   &
     &         .AND. (KBL .GT. KD)

!     if(lprnt) print *,' unsat=',unsat,' tem=',tem,' tem1=',tem1
!    &,' tx1=',tx1,' rhram=',rhram,' kbl=',kbl,' kd=',kd,' lowest='
!    &,lowest,' rhfacs=',rhfacs,' ltl=',ltl(kd1),' qol=',qol(kd1)
!    &,' qst=',qst(kd1),' hst=',hst(kd1),' nu=',nu

!
!===>  IF NO SOUNDING MEETS FIRST CONDITION, RETURN
!     if(lprnt .and. (.not. unsat)) print *,' tx1=',tx1,' rhfacs='
!    &,rhfacs, ' tem=',tem,' hst=',hst(kd1)

      IF (.NOT. UNSAT) RETURN
!
!     TEM1   = TX1 - RHFACS
!     RHC    = MAX(ZERO, MIN(ONE, EXP(20.0*TEM1) ))
      RHC    = MAX(ZERO, MIN(ONE, EXP(-20.0*TX1) ))
!
      DO N=1,M
        RBL(N) = ROI(K,N) * ETA(K)
      ENDDO
      DO N=1,M
        DO L=KM1,KBL,-1
          RBL(N) = RBL(N) + ROI(L,N)*(ETA(L)-ETA(L+1))
        ENDDO
      ENDDO
!
      TX4    = 0.0
      TX5    = 0.0
!
      TX3      = QST(KBL) - GAF(KBL) * HST(KBL)
      QIL(KBL) = MAX(ZERO, MIN(ONE, (TCR-TCL-TOL(KBL))*TCRF))
!
      DO L=KB1,KD1,-1
        TEM      = QST(L) - GAF(L) * HST(L)
        TEM1     = (TX3 + TEM) * 0.5
        ST2      = (GAF(L)+GAF(L+1)) * 0.5
!
        FCO(L+1) =            TEM1 + ST2 * HBL

!     if(lprnt) print *,' fco=',fco(l+1),' tem1=',tem1,' st2=',st2
!    &,' hbl=',hbl,' tx3=',tx3,' tem=',tem,' gaf=',gaf(l),' l=',l

        RNN(L+1) = ZET(L+1) * TEM1 + ST2 * TX4
        GMH(L+1) = XI(L+1)  * TEM1 + ST2 * TX5
!
        TX3      = TEM
        TX4      = TX4 + ETA(L) * HOL(L)
        TX5      = TX5 + GMS(L) * HOL(L)
!
        QIL(L)   = MAX(ZERO, MIN(ONE, (TCR-TCL-TOL(L))*TCRF))
        QLL(L+1) = (0.5*ALHF) * ST2 * (QIL(L)+QIL(L+1)) + ONE
      ENDDO
!
!     FOR THE CLOUD TOP -- L=KD
!
      L = KD
!
      TEM      = QST(L) - GAF(L) * HST(L)
      TEM1     = (TX3 + TEM) * 0.5
      ST2      = (GAF(L)+GAF(L+1)) * 0.5
!
      FCO(L+1) =            TEM1 + ST2 * HBL
      RNN(L+1) = ZET(L+1) * TEM1 + ST2 * TX4
      GMH(L+1) = XI(L+1)  * TEM1 + ST2 * TX5
!
      FCO(L)   = TEM + GAF(L) * HBL
      RNN(L)   = TEM * ZET(L) + (TX4 + ETA(L)*HOL(L)) * GAF(L)
      GMH(L)   = TEM * XI(L)  + (TX5 + GMS(L)*HOL(L)) * GAF(L)
!
!   Replace FCO for the Bottom
!
      FCO(KBL) = QBL
      RNN(KBL) = 0.0
      GMH(KBL) = 0.0
!
      QIL(KD)  =  MAX(ZERO, MIN(ONE, (TCR-TCL-TOL(KD))*TCRF))
      QLL(KD1) = (0.5*ALHF) * ST2 * (QIL(KD) + QIL(KD1)) + ONE
      QLL(KD ) = ALHF * GAF(KD) * QIL(KD) + ONE
!
!     if (lprnt) print *,' fco=',fco(kd:kbl)
!     if (lprnt) print *,' qil=',qil(kd:kbl)
!     if (lprnt) print *,' qll=',qll(kd:kbl)
!
      st1  = qil(kd)
      st2  = c0i * st1
      tem  = c0  * (1.0-st1)
      tem2 = st2*qi0 + tem*qw0
!
      DO L=KD,KB1
         tx2    = akt(l) * eta(l)
         tx1    = tx2 * tem2
         q0u(l) = tx1
         FCO(L) = FCO(L+1) - FCO(L) + tx1
         RNN(L) = RNN(L+1) - RNN(L)                                     &
     &          + ETA(L)*(QOL(L)+CLL(L)+CIL(L)) + tx1*zet(l)
         GMH(L) = GMH(L+1) - GMH(L)                                     &
     &          + GMS(L)*(QOL(L)+CLL(L)+CIL(L)) + tx1*xi(l)
!
         tem1   = (1.0-akt(l)) * eta(l)

!     if(lprnt) print *,' qll=',qll(l),' st2=',st2,' tem=',tem
!    &,' tx2=',tx2,' akt=',akt(l),' eta=',eta(l)

         AKT(L) = QLL(L)   + (st2 + tem) * tx2

!     if(lprnt) print *,' akt==',akt(l),' l==',l

         AKC(L) = 1.0 / AKT(L)
!
         st1    = 0.5 * (qil(l)+qil(l+1))
         st2    = c0i * st1
         tem    = c0  * (1.0-st1)
         tem2   = st2*qi0 + tem*qw0
!
         BKC(L) = QLL(L+1) - (st2 + tem) * tem1
!
         tx1    = tem1*tem2
         q0d(l) = tx1
         FCO(L) = FCO(L) + tx1
         RNN(L) = RNN(L) + tx1*zet(l+1)
         GMH(L) = GMH(L) + tx1*xi(l+1)
      ENDDO

!     if(lprnt) print *,' akt=',akt(kd:kb1)
!     if(lprnt) print *,' akc=',akc(kd:kb1)

      qw00 = qw0
      qi00 = qi0
      ii = 0
  777 continue
!
!     if (lprnt) print *,' after 777 ii=',ii,' ep_wfn=',ep_wfn
!
      ep_wfn = .false.
      RNN(KBL) = 0.0
      TX3      = bkc(kb1) * (QIB + QLB)
      TX4      = 0.0
      TX5      = 0.0
      DO L=KB1,KD1,-1
        TEM    = BKC(L-1)       * AKC(L)
!     if (lprnt) print *,' tx3=',tx3,' fco=',fco(l),' akc=',akc(l)
!    &,' bkc=',bkc(l-1), ' l=',l
        TX3    = (TX3 + FCO(L)) * TEM
        TX4    = (TX4 + RNN(L)) * TEM
        TX5    = (TX5 + GMH(L)) * TEM
      ENDDO
      IF (KD .LT. KB1) THEN
         HSD   = HST(KD1)                                               &
     &         + LTL(KD1) *  NU *(QOL(KD1)-QST(KD1))
      ELSE
         HSD   = HBL
      ENDIF
!
!     if (lprnt) print *,' tx3=',tx3,' fco=',fco(kd),' akc=',akc(kd)
      TX3 = (TX3 + FCO(KD)) * AKC(KD)
      TX4 = (TX4 + RNN(KD)) * AKC(KD)
      TX5 = (TX5 + GMH(KD)) * AKC(KD)
      ALM = ALHF*QIL(KD) - LTL(KD) * VTF(KD)
!
      HSU = HST(KD) + LTL(KD) * NU * (QOL(KD)-QST(KD))

!     if (lprnt) print *,' hsu=',hsu,' hst=',hst(kd),
!    &' ltl=',ltl(kd),' qol=',qol(kd),' qst=',qst(kd)
!
!===> VERTICAL INTEGRALS NEEDED TO COMPUTE THE ENTRAINMENT PARAMETER
!
      TX1 = ALM * TX4
      TX2 = ALM * TX5

      DO L=KD,KB1
        TAU = HOL(L) - HSU
        TX1 = TX1 + TAU * ETA(L)
        TX2 = TX2 + TAU * GMS(L)
      ENDDO
!
!     MODIFY HSU TO INCLUDE CLOUD LIQUID WATER AND ICE TERMS
!
!     if (lprnt) print *,' hsu=',hsu,' alm=',alm,' tx3=',tx3

      HSU   = HSU - ALM * TX3
!
      CLP   = ZERO
      ALM   = -100.0
      HOS   = HOL(KD)
      QOS   = QOL(KD)
      QIS   = CIL(KD)
      QLS   = CLL(KD)
      UNSAT = HBL .GT. HSU .and. abs(tx1) .gt. 1.0e-4

!     if (lprnt) print *,' ii=',ii,' unsat=',unsat,' hsu=',hsu
!    &,' hbl=',hbl,' tx1=',tx1,' hsd=',hsd


!***********************************************************************


       ST1  = HALF*(HSU + HSD)
       IF (UNSAT) THEN
!
!  STANDARD CASE:
!   CLOUD CAN BE NEUTRALLY BOUYANT AT MIDDLE OF LEVEL KD W/ +VE LAMBDA.
!   EPP < .25 IS REQUIRED TO HAVE REAL ROOTS.
!
       clp = 1.0
       st2 = hbl - hsu

!     if(lprnt) print *,' tx2=',tx2,' tx1=',tx1,' st2=',st2
!
       if (tx2 .eq. 0.0) then
         alm = - st2 / tx1
         if (alm .gt. almax) alm = -100.0
       else
         x00 = tx2 + tx2
         epp = tx1 * tx1 - (x00+x00)*st2
         if (epp .gt. 0.0) then
           x00  = 1.0 / x00
           tem  = sqrt(epp)
           tem1 = (-tx1-tem)*x00
           tem2 = (-tx1+tem)*x00
           if (tem1 .gt. almax) tem1 = -100.0
           if (tem2 .gt. almax) tem2 = -100.0
           alm  = max(tem1,tem2)

!     if (lprnt) print *,' tem1=',tem1,' tem2=',tem2,' alm=',alm
!    &,' tx1=',tx1,' tem=',tem,' epp=',epp,' x00=',x00,' st2=',st2

         endif
       endif

!     if (lprnt) print *,' almF=',alm,' ii=',ii,' qw00=',qw00
!    &,' qi00=',qi00
!
!  CLIP CASE:
!   NON-ENTRAINIG CLOUD DETRAINS IN LOWER HALF OF TOP LAYER.
!   NO CLOUDS ARE ALLOWED TO DETRAIN BELOW THE TOP LAYER.
!
       ELSEIF ( (HBL .LE. HSU) .AND.                                    &
     &          (HBL .GT. ST1   )     ) THEN
         ALM = ZERO
         CLP = (HBL-ST1) / (HSU-ST1)
       ENDIF
!
      UNSAT = .TRUE.
      IF (ALMIN1 .GT. 0.0) THEN
        IF (ALM .GE. ALMIN1) UNSAT = .FALSE.
      ELSE
        LOWEST   = KD .EQ. KB1
        IF ( (ALM .GT. ZERO) .OR.                                       &
     &      (.NOT. LOWEST .AND. ALM .EQ. ZERO) ) UNSAT = .FALSE.
      ENDIF
!
!     if (alm*depth/grav .ge. 1.0) UNSAT = .TRUE.
!
!===>  IF NO SOUNDING MEETS SECOND CONDITION, RETURN
!
      IF (UNSAT) THEN
         IF (ii .gt. 0 .or. (qw00 .eq. 0.0 .and. qi00 .eq. 0.0)) RETURN
         CLP = 1.0
         ep_wfn = .true.
         GO TO 888
      ENDIF
!
!     if (lprnt) print *,' hstkd=',hst(kd),' qstkd=',qst(kd)
!    &,' ii=',ii,' clp=',clp

      st1s = ONE
      IF(CLP.GT.ZERO .AND. CLP.LT.ONE) THEN
        ST1     = HALF*(ONE+CLP)
        ST2     = ONE - ST1
        st1s    = st1
        hstkd   = hst(kd)
        qstkd   = qst(kd)
        ltlkd   = ltl(kd)
        q0ukd   = q0u(kd)
        q0dkd   = q0d(kd)
        dlbkd   = dlb(kd)
        qrbkd   = qrb(kd)
!
        HST(KD) = HST(KD)*ST1 + HST(KD1)*ST2
        HOS     = HOL(KD)*ST1 + HOL(KD1)*ST2
        QST(KD) = QST(KD)*ST1 + QST(KD1)*ST2
        QOS     = QOL(KD)*ST1 + QOL(KD1)*ST2
        QLS     = CLL(KD)*ST1 + CLL(KD1)*ST2
        QIS     = CIL(KD)*ST1 + CIL(KD1)*ST2
        LTL(KD) = LTL(KD)*ST1 + LTL(KD1)*ST2
!
        DLB(KD) = DLB(KD)*CLP
        qrb(KD) = qrb(KD)*CLP
        ETA(KD) = ETA(KD)*CLP
        GMS(KD) = GMS(KD)*CLP
        Q0U(KD) = Q0U(KD)*CLP
        Q0D(KD) = Q0D(KD)*CLP
      ENDIF
!
!
!***********************************************************************
!
!    Critical workfunction is included in this version
!
      ACR = 0.0
      TEM = PRL(KD1) - (PRL(KD1)-PRL(KD)) * CLP * HALF
      tx1 = PRL(KBL) - TEM
!!!   tx2 = min(700.0,max(tx1,100.0))
!!    tx2 = min(900.0,max(tx1,100.0))
!     tx2 = min(800.0,max(tx1,200.0))
!     rel_fac =  dt * 600.0 / (3600.0*((800.0-tx2)*0.5+(tx2-200.)*3.0))
!     rel_fac =  dt         / (6.0*((800.0-tx2)*0.5+(tx2-200.)*3.0))
!     rel_fac =  dt * facdt / (4.5*((900.0-tx2)*0.5+(tx2-100.)*3.0))
!!    rel_fac =  dt * facdt / (4.5*((900.0-tx2)*0.5+(tx2-100.)*6.0))
!!!   rel_fac =  dt * facdt / (6.0*((700.0-tx2)*1.0+(tx2-100.)*3.0))
!!!!  rel_fac =  dt * facdt / (6.0*((700.0-tx2)*0.5+(tx2-100.)*2.0))
!
!
!     tx2 = min(800.0,max(tx1,100.0))
!     tem1    = log(tx2*0.01) / log(8.0)
      tx2 = min(900.0,max(tx1,100.0))
      tem1    = log(tx2*0.01) / log(10.0)
!     rel_fac = (dt * facdt)  / (3600.0 * (tem1*4.0 + (1-tem1)*1.0))
      rel_fac = (dt * facdt)  / (3600.0 * (tem1*3.0 + (1-tem1)*1.0))
!     rel_fac = (dt * facdt)  / (3600.0 * (tem1*2.0 + (1-tem1)*1.0))
!cnt  rel_fac = (dt * facdt)  / (3600.0 * 1.5)
!     rel_fac = 0.3 
!
      rel_fac = max(zero, min(one,rel_fac))
      
      IF (CRTFUN) THEN
        CALL CRTWRK(TEM, CCWF, ST1)
!       ACR = (PRL(K) - TEM) * ST1
        ACR = TX1 * ST1
      ENDIF
!
!===>  NORMALIZED MASSFLUX
!
!  ETA IS THE THICKNESS COMING IN AND THE MASS FLUX GOING OUT.
!  GMS IS THE THICKNESS OF THE SQUARE; IT IS LATER REUSED FOR GAMMA_S
!
!     ETA(K) = ONE

      DO L=KB1,KD,-1
        ETA(L)  = ETA(L+1) + ALM * (ETA(L) + ALM * GMS(L))
      ENDDO
      DO L=KD,KBL
        ETAI(L) = 1.0 / ETA(L)
      ENDDO

!     if (lprnt) print *,' eta=',eta,' ii=',ii,' alm=',alm
!
!===>  CLOUD WORKFUNCTION
!
      WFN   = ZERO
      AKM   = ZERO
      DET   = ZERO
      HCC   = HBL
      UNSAT = .FALSE.
      QTL   = QST(KB1) - GAF(KB1)*HST(KB1)
      TX1   = HBL
!
!     tem   = qst(kbl) - gaf(kbl)*hst(kbl)
!     qtv   = 0.5 * ((tem+qtl) + (gaf(kbl)+gaf(kb1))*hbl)
!     det   = max(ZERO, qbl-qtv)
!     qtv   = qbl - det
!     det   = det + qlb + qib
!!
      qtv   = qbl
      det   = qlb + qib
!
      tx2   = 0.0
      dpneg = 0.0
!
      DO L=KB1,KD1,-1
         DEL_ETA = ETA(L) - ETA(L+1)
         HCCP = HCC + DEL_ETA*HOL(L)
!
         QTLP = QST(L-1) - GAF(L-1)*HST(L-1)
         QTVP = 0.5 * ((QTLP+QTL)*ETA(L)                                &
     &              + (GAF(L)+GAF(L-1))*HCCP)
         ST1  = ETA(L)*Q0U(L) + ETA(L+1)*Q0D(L)
         DETP = (BKC(L)*DET - (QTVP-QTV)                                &
     &        + DEL_ETA*(QOL(L)+CLL(L)+CIL(L)) + ST1)  * AKC(L)

!     if(lprnt) print *,' detp=',detp,' bkc=',bkc(l),' det=',det
!     if (lprnt .and. kd .eq. 15) 
!    &          print *,' detp=',detp,' bkc=',bkc(l),' det=',det
!    &,' qtvp=',qtvp,' qtv=',qtv,' del_eta=',del_eta,' qol='
!    &,qol(l),' st1=',st1,' akc=',akc(l)
!
         TEM1   = AKT(L)   - QLL(L)
         TEM2   = QLL(L+1) - BKC(L)
         RNS(L) = TEM1*DETP  + TEM2*DET - ST1

         qtp    = 0.5 * (qil(L)+qil(L-1))
         tem2   = min(qtp*(detp-eta(l)*qw00),                           &
     &               (1.0-qtp)*(detp-eta(l)*qi00))
         st1    = min(tx2,tem2)
         tx2    = tem2
!
         IF (rns(l) .lt. zero .or. st1 .lt. zero) ep_wfn = .TRUE.
         IF (DETP .LE. ZERO) UNSAT = .TRUE.
!        IF (DETP .LE. ZERO .or. rns(l) .lt. zero) UNSAT = .TRUE.

         ST1  = HST(L) - LTL(L)*NU*(QST(L)-QOL(L))


         TEM2 = HCCP   + DETP   * QTP * ALHF
!
!     if(lprnt) print *,' hst=',hst(l),' ltl=',ltl(l),' nu=',nu
!     if (lprnt .and. kd .eq. 15) 
!    &          print *,' hst=',hst(l),' ltl=',ltl(l),' nu=',nu
!    &,' qst=',qst(l),' qol=',qol(l),' hccp=',hccp,' detp=',detp
!    *,' qtp=',qtp,' alhf=',alhf,' vtf=',vtf(l)

         ST2  = LTL(L) * VTF(L)
         TEM5 = CLL(L) + CIL(L)
         TEM3 = (TX1  - ETA(L+1)*ST1 - ST2*(DET-TEM5*eta(l+1))) * DLB(L)
         TEM4 = (TEM2 - ETA(L  )*ST1 - ST2*(DETP-TEM5*eta(l)))  * DLT(L)
!
!     if (lprnt) then
!     if (lprnt .and. kd .eq. 12) then 
!       print *,' tem3=',tem3,' tx1=',tx1,' st1=',st1,' eta1=',eta(l+1)
!    &, ' st2=',st2,' det=',det,' tem5=',tem5,' dlb=',dlb(l)
!       print *,' tem4=',tem4,' tem2=',tem2,' detp=',detp
!    &, ' eta=',eta(l),' dlt=',dlt(l),' rns=',rns(l),' l=',l
!       print *,' bt1=',tem3/(eta(l+1)*qrb(l))
!    &,         ' bt2=',tem4/(eta(l)*qrt(l))
!      endif

         ST1  = TEM3 + TEM4

!     if (lprnt) print *,' wfn=',wfn,' st1=',st1,' l=',l,' ep_wfn=',
!    &ep_wfn,' akm=',akm

         WFN = WFN + ST1       
         AKM = AKM - min(ST1,ZERO)

!     if (lprnt) print *,' wfn=',wfn,' akm=',akm
!     if (lprnt .and. kd .eq. 12) print *,' wfn=',wfn,' akm=',akm

         if (st1 .lt. zero .and. wfn .lt. zero) then
           dpneg = dpneg + prl(l+1) - prl(l)
         endif

!        BUY(L) = 0.5 * (ETA(L+1) + ETA(L)) * ST1
!        BUY(L) = ETA(L+1)*tem3 + ETA(L)*tem4
!!       BUY(L) = tem3*ETAI(L+1) + tem4*ETAI(L)
         BUY(L) = 0.5 * (tem3/(eta(l+1)*qrb(l)) + tem4/(eta(l)*qrt(l)))
!        BUY(L) = 0.5 * st1 / ((eta(l)+eta(l+1))*(qrb(l)+qrt(l)))
!
         HCC = HCCP
         DET = DETP
         QTL = QTLP
         QTV = QTVP
         TX1 = TEM2

      ENDDO

      DEL_ETA = ETA(KD) - ETA(KD1)
      HCCP    = HCC + DEL_ETA*HOS
!
      QTLP = QST(KD) - GAF(KD)*HST(KD)
      QTVP = QTLP*ETA(KD) + GAF(KD)*HCCP
      ST1  = ETA(KD)*Q0U(KD) + ETA(KD1)*Q0D(KD)
      DETP = (BKC(KD)*DET - (QTVP-QTV)                                  &
     &     + DEL_ETA*(QOS+QLS+QIS) + ST1) * AKC(KD)
!
      TEM1    = AKT(KD)  - QLL(KD)
      TEM2    = QLL(KD1) - BKC(KD)
      RNS(KD) = TEM1*DETP  + TEM2*DET - ST1
!
      IF (rns(kd) .lt. zero) ep_wfn = .TRUE.
      IF (DETP.LE.ZERO) UNSAT = .TRUE.
!
  888 continue

!     if (lprnt) print *,' ep_wfn=',ep_wfn,' ii=',ii,' rns=',rns(kd)
!    &,' clp=',clp,' hst(kd)=',hst(kd)

      if (ep_wfn) then
        IF ((qw00 .eq. 0.0 .and. qi00 .eq. 0.0)) RETURN
        if (ii .eq. 0) then
          ii  = 1
          if (clp .gt. 0.0 .and. clp .lt. 1.0) then
            hst(kd) = hstkd
            qst(kd) = qstkd
            ltl(kd) = ltlkd
            q0u(kd) = q0ukd
            q0d(kd) = q0dkd
            dlb(kd) = dlbkd
            qrb(kd) = qrbkd
          endif
          do l=kd,kb1
            FCO(L) = FCO(L) - q0u(l) - q0d(l)
            RNN(L) = RNN(L) - q0u(l)*zet(l) - q0d(l)*zet(l+1)
            GMH(L) = GMH(L) - q0u(l)*xi(l)  - q0d(l)*zet(l+1)
            ETA(L) = ZET(L) - ZET(L+1)
            GMS(L) = XI(L)  - XI(L+1)
            Q0U(L) = 0.0
            Q0D(L) = 0.0
          ENDDO
          qw00 = 0.0
          qi00 = 0.0

!     if (lprnt) print *,' returning to 777 : ii=',ii,' qw00=',qw00,qi00
!    &,' clp=',clp,' hst(kd)=',hst(kd)

          go to 777
        else
          unsat = .true.
        endif
      endif
!
!
!     ST1 = 0.5 * (HST(KD)  - LTL(KD)*NU*(QST(KD)-QOS)
!    &          +  HST(KD1) - LTL(KD1)*NU*(QST(KD1)-QOL(KD1)))
!
      ST1 = HST(KD)  - LTL(KD)*NU*(QST(KD)-QOS)
      ST2 = LTL(KD)  * VTF(KD)
      TEM5 = (QLS + QIS) * eta(kd1)
      ST1  = HALF * (TX1-ETA(KD1)*ST1-ST2*(DET-TEM5))*DLB(KD)
!
!     if (lprnt) print *,' st1=',st1,' st2=',st2,' ltl=',ltl(kd)
!    *,ltl(kd1),' qos=',qos,qol(kd1)

      WFN = WFN + ST1
      AKM = AKM - min(ST1,ZERO)   ! Commented on 08/26/02 - does not include top
!

!     BUY(KD) = 0.5 * (ETA(KD1) + ETA(KD)) * ST1
!     BUY(KD) = ETA(KD1) * ST1
!!    BUY(KD) = ST1 * ETAI(KD1)
      BUY(KD) = ST1 / (ETA(KD1)*qrb(kd))
!     BUY(KD) = 0.5 * ST1 / (qrb(kd) * (eta(kd)+eta(kd1)))
!
!     if (lprnt) print *,' wfn=',wfn,' akm=',akm,' st1=',st1
!    &,' dpneg=',dpneg

      DET = DETP
      HCC = HCCP
      AKM = AKM / WFN


!***********************************************************************
!
!     If only to calculate workfunction save it and return
!
      IF (WRKFUN) THEN
        IF (WFN .GE. 0.0) WFNC = WFN
        RETURN
      ELSEIF (.NOT. CRTFUN) THEN
        ACR = WFNC
      ENDIF
!
!===>  THIRD CHECK BASED ON CLOUD WORKFUNCTION
!
      CALCUP = .FALSE.

!     TEM  =  MIN(CD*100.0, MAX_NEG_BOUY)
      TEM  =  MIN(CD*200.0, MAX_NEG_BOUY)
!LL2  tem  = max_neg_bouy
!     tem1 = dpneg / (prl(kbl)-prsm(kd))
      IF (WFN .GT. ACR .AND.  (.NOT. UNSAT)                             &
!    & .and. tem1 .le. 0.1  .AND. AKM .LE. TEM) THEN
!    & .and. dpneg .lt. 100.0  .AND. AKM .LE. TEM) THEN
     & .and. dpneg .lt. 150.0  .AND. AKM .LE. TEM) THEN
!    & .and. dpneg .lt. 200.0  .AND. AKM .LE. TEM) THEN
!
        CALCUP = .TRUE.
      ENDIF

!     if (lprnt) print *,' calcup=',calcup,' akm=',akm,' tem=',tem
!    *,' unsat=',unsat,' clp=',clp,' rhc=',rhc,' cd=',cd,' acr=',acr
!
!===>  IF NO SOUNDING MEETS THIRD CONDITION, RETURN
!
!     if (lprnt .and. kd .eq. 15) stop
      IF (.NOT. CALCUP) RETURN
!
! This is for not LL - 20050601
      IF (ALMIN2 .NE. 0.0) THEN
!       ST1 = 0.0
        IF (ALMIN1 .NE. ALMIN2) ST1 = 1.0 / max(ONE_M10,(ALMIN2-ALMIN1))
        IF (ALM .LT. ALMIN2) THEN
!!         CLP = CLP * (ALM - ALMIN1) * ST1
!!         CLP = CLP * (0.1 + 0.9*(ALM - ALMIN1) * ST1)
           CLP = CLP * max(0.0, min(1.0,(0.3 + 0.7*(ALM-ALMIN1)*ST1)))
!          CLP = CLP * max(0.0, min(1.0,(0.2 + 0.8*(ALM-ALMIN1)*ST1)))
!          CLP = CLP * max(0.0, min(1.0,(0.1 + 0.9*(ALM-ALMIN1)*ST1)))
        ENDIF
      ENDIF
!
!     if (lprnt) print *,' clp=',clp
!
      CLP = CLP * RHC
      do l=kd,kb1
        rnn(l) = rns(l)
      enddo
      DO L=KBL,K 
        RNN(L) = 0.0 
      ENDDO
!     if (lprnt) print *,' rnn=',rnn
!
!     If downdraft is to be invoked, do preliminary check to see
!     if enough rain is available and then call DDRFT.
!
      DDFT = .FALSE.
      IF (DNDRFT) THEN
!
        TRAIN = 0.0
        IF (CLP .GT. 0.0) THEN
          DO L=KD,KB1
            TRAIN = TRAIN + RNN(L)
          ENDDO
        ENDIF

        PL = (PRL(KD1) + PRL(KD))*HALF
        TEM = PRL(K+1)*(1.0-DPD*0.001)
!cnt    TEM = MIN(PRL(K+1)-DPD, PRL(KBL)-50.0)
!       TEM = MIN(PRL(K+1)-DPD, PRL(KBL)-300.0)
        IF (TRAIN .GT. 1.0E-4 .AND. PL .LE. TEM) DDFT  = .TRUE.
!
!       if (ddft) then
!!        DO L=KBL-3,KD,-1
!!        DO L=KBL-3,KD1,-1
!         DO L=KBL-3,KD1+1,-1
!           IF (BUY(L) .LT. 0.1) THEN
!             DDFT = .FALSE.
!             EXIT
!           ENDIF
!         ENDDO
!       endif
      ENDIF
!
!     if (lprnt) print *,' BEFORE CALLING DDRFT KD=',kd,' DDFT=',DDFT
!    &,                  ' PL=',PL,' TRAIN=',TRAIN
!     if (lprnt) print *,' buy=',(buy(l),l=kd,kb1)

      IF (DDFT) THEN
!
!     Call Downdraft scheme based on (Cheng and Arakawa, 1997)
!
        CALL DDRFT(                                                     &
     &              K, KD                                               &
     &,             TLA, ALFIND                                         &
     &,             TOL, QOL, HOL,   PRL, QST, HST, GAM, GAF, HBL, QBL  &
     &,             QRB, QRT, BUY,   KBL, IDH, ETA, RNN, ETAI           &
     &,             ALM, WFN, TRAIN, DDFT                               &
     &,             ETD, HOD, QOD,   EVP, DOF, CLDFR, ETZ               &
!    &,             ETD, HOD, QOD,   EVP, DOF, CLDFRD, ETZ              &
     &,             GMS, GSD, GHD,lprnt)               
!    &,             TX1, TX2, TX3, TX4, TX5, TX6, TX7, TX8, TX9)

      ENDIF
!
!  No Downdraft case (including case with no downdraft soln)
!  ---------------------------------------------------------
!
      IF (.NOT. DDFT) THEN
        DO L=KD,K+1
          ETD(L) = 0.0
          HOD(L) = 0.0
          QOD(L) = 0.0
        ENDDO
        DO L=KD,K
          EVP(L) = 0.0
          ETZ(L) = 0.0
        ENDDO

      ENDIF
!     if (lprnt) print *,' hod=',hod
!     if (lprnt) print *,' etd=',etd
!
!
!===> CALCULATE GAMMAS  i.e. TENDENCIES PER UNIT CLOUD BASE MASSFLUX
!           Includes downdraft terms!

      avh = 0.0

!
!     Fraction of detrained condensate evaporated
!
!     tem1 = max(ZERO, min(HALF, (prl(kd)-FOUR_P2)*ONE_M2))
!     tem1 = max(ZERO, min(HALF, (prl(kd)-300.0)*0.005))
      tem1 = 0.0
!     tem1 = 1.0
!     if (kd1 .eq. kbl) tem1 = 0.0
!
      tem2    = 1.0 - tem1
      TEM = DET * QIL(KD)

!     st1 = (HCC-ETA(KD)*HST(KD)) / (1.0+gam(KD))

      st1 = (HCC+ALHF*TEM-ETA(KD)*HST(KD)) / (1.0+gam(KD))
      DS  = ETA(KD1) * (HOS- HOL(KD)) - ALHL*(QOS - QOL(KD))
      DH  = ETA(KD1) * (HOS- HOL(KD))

!     GMS(KD) = (DS + st1 - tem1*det*alhl) * PRI(KD)
!     GMS(KD) = (DS + st1 - tem1*(det*alhl+tem*alhf)) * PRI(KD)

      GMS(KD) = (DS + st1 - tem1*det*alhl-tem*alhf) * PRI(KD)
!     GMS(KD) = (DS + st1 - det*alhl - tem*alhf) * PRI(KD)
      GMH(KD) = PRI(KD) * (HCC-ETA(KD)*HOS + DH)

!     GMH(KD) = PRI(KD) * (HCC-ETA(KD)*HOS + ALHF*tem + DH)
!     GMH(KD) = PRI(KD) * (HCC-ETA(KD)*HOS + ALHF*tem*tem1 + DH)

!     if (lprnt) print *,' gmhkd=',gmh(kd),' gmskd=',gms(kd)
!    &,' det=',det,' tem=',tem,' tem1=',tem1,' tem2=',tem2
!
!      TENDENCY FOR SUSPENDED ENVIRONMENTAL ICE AND/OR LIQUID WATER
!
!     tem2    = 1.0 - tem1
!     tem3    = tem2 * (1.0+alhf/alhl)
!     QIL(KD) =     (tem3*TEM + ETA(KD1)*(QIS-CIL(KD))                  &

      QIL(KD) =     (tem2*TEM + ETA(KD1)*(QIS-CIL(KD))                  &
     &                        - ETA(KD)*QIS ) * PRI(KD)
      QLL(KD) = (tem2*(DET-TEM) + ETA(KD1)*(QLS-CLL(KD))                &
     &                          - ETA(KD)*QLS ) * PRI(KD)
!
      GHD(KD) = 0.0
      GSD(KD) = 0.0
!
      DO L=KD1,K
       ST1 = ONE - ALFINT(L,1)
       ST2 = ONE - ALFINT(L,2)
       ST3 = ONE - ALFINT(L,3)
       ST4 = ONE - ALFINT(L,4)
       ST5 = ONE - ALFIND(L)
!      IF (L .LT. KBL) THEN
!      IF (L .LT. K) THEN
         HB       = ALFINT(L,1)*HOL(L-1) + ST1*HOL(L)
         QB       = ALFINT(L,2)*QOL(L-1) + ST2*QOL(L)

         TEM      = ALFINT(L,4)*CIL(L-1) + ST4*CIL(L)
         TEM2     = ALFINT(L,3)*CLL(L-1) + ST3*CLL(L)
 
         TEM1     = ETA(L) * (TEM - CIL(L))
         TEM3     = ETA(L) * (TEM2 - CLL(L))

         HBD      = ALFIND(L)*HOL(L-1) + ST5*HOL(L)
         QBD      = ALFIND(L)*QOL(L-1) + ST5*QOL(L)

         TEM5     = ETD(L) * (HOD(L) - HBD)
         TEM6     = ETD(L) * (QOD(L) - QBD)
!
         DH       = ETA(L) * (HB - HOL(L)) + TEM5
         DS       = DH - ALHL * (ETA(L) * (QB - QOL(L)) + TEM6)

         GMH(L)   = DH * PRI(L)
         GMS(L)   = DS * PRI(L)

!     if (lprnt) print *,' gmh=',gmh(l),' gms=',gms(l)
!    &,' dh=',dh,' ds=',ds,' qb=',qb,' qol=',qol(l),' eta=',eta(l)
!    &,' hb=',hb,' hol=',hol(l),' l=',l,' hod=',hod(l)
!    &,' etd=',etd(l),' qod=',qod(l),' tem5=',tem5,' tem6=',tem6
!
         GHD(L)   = TEM5 * PRI(L)
         GSD(L)   = (TEM5 - ALHL * TEM6) * PRI(L)
!
         QIL(L)   = TEM1 * PRI(L)
         QLL(L)   = TEM3 * PRI(L)

         TEM1     = ETA(L) * (CIL(L-1) - TEM)
         TEM3     = ETA(L) * (CLL(L-1) - TEM2)

         DH       = ETA(L) * (HOL(L-1) - HB) - TEM5
         DS       = DH - ALHL * ETA(L) * (QOL(L-1) - QB)                &
     &                 + ALHL * (TEM6 - EVP(L-1))

         GMH(L-1) = GMH(L-1) + DH * PRI(L-1)
         GMS(L-1) = GMS(L-1) + DS * PRI(L-1)
!
!     if (lprnt) print *,' gmh1=',gmh(l-1),' gms1=',gms(l-1)
!    &,' dh=',dh,' ds=',ds,' qb=',qb,' qol=',qol(l-1)
!    &,' hb=',hb,' hol=',hol(l-1),' evp=',evp(l-1)
!
         GHD(L-1) = GHD(L-1) - TEM5 * PRI(L-1)
         GSD(L-1) = GSD(L-1) - (TEM5-ALHL*(TEM6-EVP(L-1))) * PRI(L-1)

         QIL(L-1) = QIL(L-1) + TEM1 * PRI(L-1)
         QLL(L-1) = QLL(L-1) + TEM3 * PRI(L-1)
!      ELSEIF (L .EQ. KBL) THEN
!!       HB       = ALFINT(L)*HOL(L-1) + ST1*HBL
!!       QB       = ALFINT(L)*QOL(L-1) + ST1*QBL
!        HB       = ALFINT(L)*HOL(L-1) + ST1*HOL(L)
!        QB       = ALFINT(L)*QOL(L-1) + ST1*QOL(L)

!!       HB       = HBL
!!       QB       = QBL
!        HBD      = ALFINT(L)*HOL(L-1) + ST1*HOL(L)
!        QBD      = ALFINT(L)*QOL(L-1) + ST1*QOL(L)

!!       TEM      = ALFINQ(L)*CIL(L-1) + ST2*QIB
!!       TEM2     = ALFINQ(L)*CLL(L-1) + ST2*QLB
!        TEM      = ALFINQ(L)*CIL(L-1) + ST2*CIL(L)
!        TEM2     = ALFINQ(L)*CLL(L-1) + ST2*CLL(L)

!        TEM1     = ETA(L) * (TEM - QIB)
!        TEM3     = ETA(L) * (TEM2 - QLB)

!        TEM5     =  ETD(L) * (HOD(L) - HBD)
!        TEM6     =  ETD(L) * (QOD(L) - QBD)

!        tem4     = GRAVFAC * pris
!        TX1      = ETA(L) * (HB - HBL) * TEM4
!        TX2      = TX1 - ALHL * ETA(L) * (QB - QBL) * TEM4
!        DH       = TEM5

!        DS       =  DH - ALHL * (TEM6 + EVP(L))


!        GMH(L)   = TX1 + DH * PRI(L)
!        GMS(L)   = TX2 + DS * PRI(L)
!
!        GHD(L)   = TEM5 * PRI(L)
!        GSD(L)   = (TEM5 - ALHL * (TEM6+EVP(L))) * PRI(L)
!
!        QIL(L)   = TEM1 * tem4
!        QLL(L)   = TEM3 * tem4

!        TEM1     = ETA(L) * (CIL(L-1) - TEM)
!        TEM3     = ETA(L) * (CLL(L-1) - TEM2)

!        DH       = ETA(L) * (HOL(L-1) - HB) - TEM5
!        DS       = DH - ALHL * ETA(L) * (QOL(L-1) - QB)
!    *                 + ALHL * (TEM6 - EVP(L-1))

!        GMH(L-1) = GMH(L-1) + DH * PRI(L-1)
!        GMS(L-1) = GMS(L-1) + DS * PRI(L-1)
!
!        GHD(L-1) = GHD(L-1) - TEM5 * PRI(L-1)
!        GSD(L-1) = GSD(L-1) - (TEM5-ALHL*(TEM6-EVP(L-1)))
!    *                                  * PRI(L-1)

!        QIL(L-1) = QIL(L-1) + TEM1 * PRI(L-1)
!        QLL(L-1) = QLL(L-1) + TEM3 * PRI(L-1)
!      ELSE
!!       HB       = ALFINT(L)*HOL(L-1) + ST1*HOL(L)
!!       QB       = ALFINT(L)*QOL(L-1) + ST1*QOL(L)
!!                                                                                                                
!!       TEM      = ALFINQ(L)*CIL(L-1) + ST2*CIL(L)
!!       TEM2     = ALFINQ(L)*CLL(L-1) + ST2*CLL(L)
!!                                                                                                                
!!       TEM1     = ETA(L) * (TEM - CIL(L))
!!       TEM3     = ETA(L) * (TEM2 - CLL(L))
!!                                                                                                                
!!       TEM5     = ETD(L) * (HOD(L) - HB)
!!       TEM6     = ETD(L) * (QOD(L) - QB)
!
!!       DH       = ETA(L) * (HB - HOL(L)) + TEM5
!!       DS       = DH - ALHL * (ETA(L) * (QB - QOL(L)) + TEM6)
!                                                                                                                 
!!       GMH(L)   = DH * PRI(L)
!1       GMS(L)   = DS * PRI(L)

!     if (lprnt) print *,' gmh=',gmh(l),' gms=',gms(l)
!    &,' dh=',dh,' ds=',ds,' qb=',qb,' qol=',qol(l),' eta=',eta(l)
!    &,' hb=',hb,' hol=',hol(l),' l=',l
!
!!       GHD(L)   = TEM5 * PRI(L)
!!       GSD(L)   = (TEM5 - ALHL * TEM6) * PRI(L)
!
!!       QIL(L)   = TEM1 * PRI(L)
!!       QLL(L)   = TEM3 * PRI(L)
!
!
!
!        HBD      = ALFINT(L)*HOL(L-1) + ST1*HOL(L)
!        QBD      = ALFINT(L)*QOL(L-1) + ST1*QOL(L)
!        TEM5     =  ETD(L) * (HOD(L) - HBD)
!        TEM6     =  ETD(L) * (QOD(L) - QBD)
!        DH       =  TEM5
!        DS       =  DH - ALHL * (TEM6 + EVP(L))
!
!        GMH(L)   = TX1 + DH * PRI(L)
!        GMS(L)   = TX2 + DS * PRI(L)
!        GHD(L)   = DH * PRI(L)
!        GSD(L)   = DS * PRI(L)
!
!        DH       = - TEM5
!        DS       = DH  + ALHL * TEM6
!        GMH(L-1) = GMH(L-1) + DH * PRI(L-1)
!        GMS(L-1) = GMS(L-1) + DS * PRI(L-1)
!
!        GHD(L-1) = GHD(L-1) + DH * PRI(L-1)
!        GSD(L-1) = GSD(L-1) + DS * PRI(L-1)
!
!        QIL(L)   = QIL(L-1)
!        QLL(L)   = QLL(L-1)
!      ENDIF

        avh = avh + gmh(l-1)*(prs(l)-prs(l-1))

      ENDDO
!
!!    TEM2   =  - ALHL * EVP(K) * PRI(K)
!!    GMS(K) = GMS(K) + TEM2
!!    GSD(K) = GSD(K) + TEM2
!

      HBD  = HOL(K)
      QBD  = QOL(K)
      TEM5 =  ETD(K+1) * (HOD(K+1) - HBD)
      TEM6 =  ETD(K+1) * (QOD(K+1) - QBD)
      DH   = - TEM5
      DS   = DH  + ALHL * TEM6
      TEM1 = DH * PRI(K)
      TEM2 = (DS - ALHL * EVP(K)) * PRI(K)
!!    TEM2 = - ALHL * EVP(K) * PRI(K)
      GMH(K) = GMH(K) + TEM1
      GMS(K) = GMS(K) + TEM2
      GHD(K) = GHD(K) + TEM1
      GSD(K) = GSD(K) + TEM2

!     if (lprnt) print *,' gmhk=',gmh(k),' gmsk=',gms(k)
!    &,' tem1=',tem1,' tem2=',tem2,' dh=',dh,' ds=',ds
!
      avh = avh + gmh(K)*(prs(KP1)-prs(K))
!
      tem4   = - GRAVFAC * pris
      TX1    = DH * tem4
      TX2    = DS * tem4
!
      DO L=KBL,K
        GMH(L) = GMH(L) + TX1
        GMS(L) = GMS(L) + TX2
        GHD(L) = GHD(L) + TX1
        GSD(L) = GSD(L) + TX2
!
        avh = avh + tx1*(prs(l+1)-prs(l))
      ENDDO

!     DO L=KBL,K
!       tem = (eta(l+1) - eta(l)) * pri(l)
!       tx1 = dh * tem
!       tx2 = ds * tem
!       GMH(L) = GMH(L) + TX1
!       GMS(L) = GMS(L) + TX2
!       GHD(L) = GHD(L) + TX1
!       GSD(L) = GSD(L) + TX2
 
!       avh = avh + tx1*(prs(l+1)-prs(l))
!     ENDDO
!
!     if (lprnt) then
!        print *,' gmh=',gmh
!        print *,' gms=',gms(KD:K)
!     endif
!
!***********************************************************************
!***********************************************************************

!===>  KERNEL (AKM) CALCULATION BEGINS

!===>  MODIFY SOUNDING WITH UNIT MASS FLUX
!
!     TESTMB = 0.01

      DO L=KD,K

         TEM1   = GMH(L)
         TEM2   = GMS(L)
         HOL(L) = HOL(L) +  TEM1*TESTMB
         QOL(L) = QOL(L) + (TEM1-TEM2)  * (TESTMB/ALHL)
!    &                   +  ALHF*QIL(L))  * (TESTMB/ALHL)
         HST(L) = HST(L) +  TEM2*(ONE+GAM(L))*TESTMB
         QST(L) = QST(L) +  TEM2*GAM(L)*(TESTMB/ALHL)
         CLL(L) = CLL(L) + QLL(L) * TESTMB
         CIL(L) = CIL(L) + QIL(L) * TESTMB
      ENDDO
!

      if (alm .gt. 0.0) then
      HOS = HOS + GMH(KD)  * TESTMB
      QOS = QOS + (GMH(KD)-GMS(KD)) * (TESTMB/ALHL)
!    &          + ALHF*QIL(KD)) * (TESTMB/ALHL)

        QLS     = QLS + QLL(KD) * TESTMB
        QIS     = QIS + QIL(KD) * TESTMB
      else
        st2 = 1.0 - st1s
        HOS = HOS + (st1s*GMH(KD)+st2*GMH(KD1))  * TESTMB
        QOS = QOS + (st1s * (GMH(KD)-GMS(KD))                           &
     &            +  st2  * (GMH(KD1)-GMS(KD1))) * (TESTMB/ALHL)
!       QOS = QOS + (st1s * (GMH(KD)-GMS(KD)+ALHF*QIL(KD))              &
!    &            +  st2  * (GMH(KD1)-GMS(KD1)+ALHF*QIL(KD1)))          &
!    &            * (TESTMB/ALHL)
        HST(kd) = HST(kd) + (st1s*GMS(kd)*(ONE+GAM(kd))                 &
     &                    +  st2*gms(kd1)*(ONE+GAM(kd1))) * TESTMB
        QST(kd) = QST(kd) + (st1s*GMS(kd)*GAM(kd)                       &
     &                    +  st2*gms(kd1)*gam(kd1)) * (TESTMB/ALHL)

        QLS     = QLS + (st1s*QLL(KD)+st2*QLL(KD1)) * TESTMB
        QIS     = QIS + (st1s*QIL(KD)+st2*QIL(KD1)) * TESTMB
      endif

!
      TEM = PRL(K+1) - PRL(K)
      HBL = HOL(K) * TEM
      QBL = QOL(K) * TEM
      QLB = CLL(K) * TEM
      QIB = CIL(K) * TEM
      DO L=KM1,KBL,-1
        TEM = PRL(L+1) - PRL(L)
        HBL = HBL + HOL(L) * TEM
        QBL = QBL + QOL(L) * TEM
        QLB = QLB + CLL(L) * TEM
        QIB = QIB + CIL(L) * TEM
      ENDDO
      HBL = HBL * PRIS
      QBL = QBL * PRIS
      QLB = QLB * PRIS
      QIB = QIB * PRIS

!     if (lprnt) print *,' hbla=',hbl,' qbla=',qbl

!***********************************************************************

!===>  CLOUD WORKFUNCTION FOR MODIFIED SOUNDING, THEN KERNEL (AKM)
!
      AKM = ZERO
      TX1 = ZERO
      QTL = QST(KB1) - GAF(KB1)*HST(KB1)
      QTV = QBL
      HCC = HBL
      TX2 = HCC
      TX4 = (ALHF*0.5)*MAX(ZERO,MIN(ONE,(TCR-TCL-TOL(KB1))*TCRF))
!     TX4 = (ALHF*0.5)*MAX(0.0,MIN(1.0,(TCR-TOL(KB1))*TCRF))
!
!     tem   = qst(kbl) - gaf(kbl)*hst(kbl)
!     qtv   = 0.5 * ((tem+qtl) + (gaf(kbl)+gaf(kb1))*hbl)
!     tx1   = max(ZERO, qbl-qtv)
!     qtv   = qbl - tx1
!     tx1   = tx1 + qib + qlb
!
      qtv   = qbl
      tx1   = qib + qlb
!

      DO L=KB1,KD1,-1
         DEL_ETA = ETA(L) - ETA(L+1)
         HCCP = HCC + DEL_ETA*HOL(L)
!
         QTLP = QST(L-1) - GAF(L-1)*HST(L-1)
         QTVP = 0.5 * ((QTLP+QTL)*ETA(L)                                &
     &                +(GAF(L)+GAF(L-1))*HCCP)

         DETP = (BKC(L)*TX1 - (QTVP-QTV)                                &
     &        +  DEL_ETA*(QOL(L)+CLL(L)+CIL(L))                         &
     &        +  ETA(L)*Q0U(L) + ETA(L+1)*Q0D(L)) * AKC(L)
         IF (DETP .LE. ZERO) UNSAT = .TRUE.

         ST1  = HST(L) - LTL(L)*NU*(QST(L)-QOL(L))

         TEM2 = (ALHF*0.5)*MAX(ZERO,MIN(ONE,(TCR-TCL-TOL(L-1))*TCRF))
!        TEM2 = (ALHF*0.5)*MAX(0.0,MIN(1.0,(TCR-TOL(L-1))*TCRF))
         TEM1 = HCCP + DETP * (TEM2+TX4)

         ST2  = LTL(L) * VTF(L)
         TEM5 = CLL(L) + CIL(L)
         AKM  = AKM +                                                   &
     &     (  (TX2  -ETA(L+1)*ST1-ST2*(TX1-TEM5*eta(l+1))) * DLB(L)     &
     &      + (TEM1 -ETA(L  )*ST1-ST2*(DETP-TEM5*eta(l)))  * DLT(L) )
!
         HCC  = HCCP
         TX1  = DETP
         TX2  = TEM1
         QTL  = QTLP
         QTV  = QTVP
         TX4  = TEM2
      ENDDO
!
      if (unsat) return
!
!  Eventhough we ignore the change in lambda, we still assume
!  that the cLoud-top contribution is zero; as though we still
!  had non-bouyancy there.
!
!
      ST1 = HST(KD)  - LTL(KD)*NU*(QST(KD)-QOS)
      ST2 = LTL(KD)  * VTF(KD)
      TEM5 = (QLS + QIS) * eta(kd1)
      AKM  = AKM + HALF * (TX2-ETA(KD1)*ST1-ST2*(TX1-TEM5)) * DLB(KD)
!
      AKM = (AKM - WFN) * (ONE/TESTMB)


!***********************************************************************

!===>   MASS FLUX

!     if (acr .gt. 0.0) then
!       tem = max(0.01, min(0.05, (wfn-acr)/acr))
!     else
!       tem = max(0.01, 0.05*min(1.0, wfn))
!     endif
!!!   tem2 = (rasalf*(tem-0.01) + 0.05 - tem) * oneopt4
!     tem2 = (rel_fac*(tem-0.01) + 0.05 - tem) * oneopt4
      tem2 = rel_fac
!
      AMB = - (WFN-ACR) / AKM
!
!     if(lprnt) print *,' wfn=',wfn,' acr=',acr,' akm=',akm             &
!    &,' amb=',amb,' KD=',kd,' cldfrd=',cldfrd,' tem2=',tem2            &
!    &,' rel_fac=',rel_fac,' prskd=',prs(kd)

!===>   RELAXATION AND CLIPPING FACTORS
!
      AMB = AMB * CLP * tem2

!!!   if (DDFT) AMB = MIN(AMB, ONE/CLDFRD)
       
!===>   SUB-CLOUD LAYER DEPTH LIMIT ON MASS FLUX

      AMBMAX = (PRL(KP1)-PRL(KBL))*(FRACBL*GRAVCON)
      AMB    = MAX(MIN(AMB, AMBMAX),ZERO)


!     if(lprnt) print *,' AMB=',amb,' clp=',clp,' ambmax=',ambmax
!***********************************************************************
!*************************RESULTS***************************************
!***********************************************************************

!===>  PRECIPITATION AND CLW DETRAINMENT
!
      avt = 0.0
      avq = 0.0
      avr = dof

!
      DSFC = DSFC + AMB * ETD(K) * (1.0/DT)
!
!     DO L=KBL,KD,-1
      DO L=K,KD,-1
          PCU(L) = PCU(L) + AMB*RNN(L)      !  (A40)
          avr = avr + rnn(l)
!     if(lprnt) print *,' avr=',avr,' rnn=',rnn(l),' l=',l
      ENDDO
!
!===> TEMPARATURE AND Q CHANGE AND CLOUD MASS FLUX DUE TO CLOUD TYPE KD
!
      TX1  = AMB * (ONE/CP)
      TX2  = AMB * (ONE/ALHL)
      DO L=KD,K
        ST1    = GMS(L)*TX1
        TOI(L) = TOI(L) + ST1
        TCU(L) = TCU(L) + ST1
        TCD(L) = TCD(L) + GSD(L) * TX1
!
!       st1 = st1 - (alhf/cp) * QIL(L) * AMB
        st1 = st1 - (alhl/cp) * (QIL(L) + QLL(L)) * AMB

        avt = avt + st1 * (prs(l+1)-prs(l))

        FLX(L)  = FLX(L)  + ETA(L)*AMB
        FLXD(L) = FLXD(L) + ETD(L)*AMB
!
        QII(L)  = QII(L) + QIL(L) * AMB
        TEM     = 0.0

        QLI(L)  = QLI(L) + QLL(L) * AMB + TEM

        ST1     = (GMH(L)-GMS(L)) * TX2
!       ST1     = (GMH(L)-GMS(L)+ALHF*QIL(L)) * TX2

        QOI(L)  = QOI(L) + ST1
        QCU(L)  = QCU(L) + ST1
        QCD(L)  = QCD(L) + (GHD(L)-GSD(L)) * TX2
!
        avq = avq + (st1+(QLL(L)+QIL(L))*amb) * (prs(l+1)-prs(l))
!       avq = avq + st1 * (prs(l+1)-prs(l))
!       avr = avr + (QLL(L) + QIL(L)*(1+alhf/alhl))
!       avr = avr + (QLL(L) + QIL(L))
!    *                  * (prs(l+1)-prs(l)) * gravcon

!     if(lprnt) print *,' avr=',avr,' qll=',qll(l),' l=',l
!    &,' qil=',qil(l)

      ENDDO
      avr = avr * amb
!
!      Correction for negative condensate!
!     if (advcld) then
!       do l=kd,k
!         if (qli(l) .lt. 0.0) then
!           qoi(l) = qoi(l) + qli(l)
!           toi(l) = toi(l) - (alhl/cp) * qli(l)
!           qli(l) = 0.0
!         endif
!         if (qii(l) .lt. 0.0) then
!           qoi(l) = qoi(l) + qii(l)
!           toi(l) = toi(l) - ((alhl+alhf)/cp) * qii(l)
!           qii(l) = 0.0
!         endif
!       enddo
!     endif

!
!
!     if (lprnt) then
!       print *,' For KD=',KD
!       avt = avt * cp * 100.0*86400.0 / (alhl*DT*grav)
!       avq = avq *  100.0*86400.0 / (DT*grav)
!       avr = avr * 86400.0 / DT
!       print *,' avt=',avt,' avq=',avq,' avr=',avr,' avh='
!    *   ,avh,' alm=',alm,' DDFT=',DDFT,' KD=',KD
!    &,' TOIK-',toi(k),' TOIK-1=',toi(k-1),' TOIK-2=',toi(k-2)
!        if (kd .eq. 12 .and. .not. ddft) stop
!       if (avh .gt. 0.1 .or. abs(avt+avq) .gt. 1.0e-5 .or.
!    &      abs(avt-avr) .gt. 1.0e-5 .or. abs(avr+avq) .gt. 1.0e-5) stop
!
!     if (lprnt) then
!       print *,' For KD=',KD
!       print *,' TCU=',(tcu(l),l=kd,k)
!       print *,' QCU=',(Qcu(l),l=kd,k)
!     endif
!
      TX1 = 0.0
      TX2 = 0.0
!
!     REEVAPORATION OF FALLING CONVECTIVE RAIN
!
      IF (REVAP) THEN
!      AFC     = -(1.04E-4*DT)*(3600./DT)**0.578
!      rknob   = 5.0
!      rknob   = 3.0
!      rknob   = 0.0
!      rknob   = 1.9
!      rknob   = 2.0
!
       tem = 0.0
       do l=kd,kbl
!        tem = tem + pcu(l)
         IF (L .lt. IDH .or. (.not. DDFT)) THEN
           tem = tem + amb * rnn(l)
         endif
       enddo
       tem = tem + amb * dof
       tem = tem * (3600.0/dt)
!      tem1 = 4.0E10/max(garea,one) * sqrt((prl(kbl)-prl(kd))/prl(K+1))
!LLLL  tem1 = rknob * sqrt(sqrt(4.0E10/max(garea,one)))
!!     tem2 = sqrt(4.0E10/max(garea,one))
!!     tem1 = sqrt(tem2)
!!     tem1 = rknob * max(one, tem1*tem2)
       tem1 = max(1.0, min(100.0,sqrt((5.0E10/max(garea,one)))))
!Cntnewtem1 = max(1.0, min(100.0,(5.0E10/max(garea,one))))
!Cnt   tem1 = max(1.0, min(100.0,(4.0E10/max(garea,one))))
!      tem1 = rknob * sqrt(4.0E10/max(garea,one))
!      clfrac = ((clfa*tem + clfb)*tem + clfc)*tem + clfd
!      clfrac = min(point3, max(point01,clfrac))

!      if (lprnt) print *,' clfr0=',clf(tem),' tem=',tem,' tem1=',tem1

       clfrac = max(ZERO, min(ONE, rknob*clf(tem)*tem1))

!      if (lprnt) print *,' cldfrd=',cldfrd,' amb=',amb
!    &,' clfrac=',clfrac

!      TX3    = AMB*ETA(KD)*PRI(KD)
!      CLDFRD = MIN(AMB*CLDFRD, ONE)
!      if(lprnt) print *,' cldfrd=',cldfrd,' amb=',amb
!      CLDFRD = MIN(AMB*CLDFRD, 1.0)
!
!     if(lprnt) print *,' tx3=',tx3,' etakd=',eta(kd),' pri=',pri(kd)
!     if(lprnt) print *,' RNN=',RNN(kd:k)
!
!cnt   DO L=KD,K
       DO L=KD,KBL         ! Testing on 20070926
!        clvfr = (prl(l)+prl(l+1)) / (prl(k)+prl(k+1))
!!!      clvfr = 0.5 * (prl(l)+prl(l+1)) / prl(k+1)
!        clvfr = min(1.0, clvfr * clvfr)
!                                                 for L=KD,K
         IF (L .GE. IDH .AND. DDFT) THEN
           TX2    = TX2 + AMB * RNN(L)
           CLDFRD = MIN(AMB*CLDFR(L), clfrac)
!!!        CLDFRD = MIN(AMB*CLDFR(L), ONE)
!          if (l .eq. kbl) tx2 = tx2 + amb * dof
!          if (l .eq. kbl) tx1 = tx1 + amb * dof
         ELSE
           TX1 = TX1 + AMB * RNN(L)
         ENDIF
         tx4 = zfac * phil(l)
         tx4 = (one - tx4 * (one - half*tx4)) * afc
!
!        CLFRAC = MIN(TX3*rknob*1.1, ONE)
!        CLFRAC = ONE
!        CLFRAC = MIN(TX3*rknob, ONE)
!        CLFRAC = MIN(TX3*rknob, 1.0)

         IF (TX1 .GT. 0. .OR. TX2 .GT. 0.0) THEN
          TEQ     = TOI(L)
          QEQ     = QOI(L)
          PL      = 0.5 * (PRL(L+1)+PRL(L))

          ST1     = MAX(ZERO, MIN(ONE, (TCR-TEQ)*TCRF))
!         ST1     = MAX(0.0, MIN(1.0, (TCR-TEQ)*TCRF))
          ST2     = ST1*ELFOCP + (1.0-ST1)*ELOCP

          CALL QSATCN ( TEQ,PL,QSTEQ,DQDT,.false.)
!
!         tx8  = 10.0 * fpvs(teq)                  ! fpvs is in centibars!
!         tx9  = 1.0 / max(pl + epsm1 * tx8, 1.0e-10)
!         qsteq  = MIN(eps*tx8*tx9, 1.0)
!         dqdt = pl * qsteq * alhl *  tx9 / (teq*teq*rv)
!
          DELTAQ = 0.5 * (QSTEQ*rhc_ls(l)-QEQ) / (1.+ST2*DQDT)
!
          QEQ    = QEQ + DELTAQ
          TEQ    = TEQ - DELTAQ*ST2
!
          TEM1   = MAX(ZERO, MIN(ONE, (TCR-TEQ)*TCRF))
!         TEM1   = MAX(0.0, MIN(1.0, (TCR-TEQ)*TCRF))
          TEM2   = TEM1*ELFOCP + (1.0-TEM1)*ELOCP

          CALL QSATCN ( TEQ,PL,QSTEQ,DQDT,.false.)
!
!         tx8  = 10.0 * fpvs(teq)                  ! fpvs is in centibars!
!         tx9  = 1.0 / max(pl + epsm1 * tx8, 1.0e-10)
!         qsteq  = MIN(eps*tx8*tx9, 1.0)
!         dqdt = pl * qsteq * alhl *  tx9 / (teq*teq*rv)
!
          DELTAQ = (QSTEQ*rhc_ls(l)-QEQ) / (1.+TEM2*DQDT)
!
          QEQ    = QEQ + DELTAQ
          TEQ    = TEQ - DELTAQ*TEM2

          IF (QEQ .GT. QOI(L)) THEN
            POTEVAP = (QEQ-QOI(L))*(PRL(L+1)-PRL(L))*GRAVCON
!           POTEVAP = (QEQ-QOI(L))*(PRL(L+1)-PRL(L))*GRAVCON * 0.85

!           TEM3    = SQRT(PL*0.001)
            tem4    = 0.0
            if (tx1 .gt. 0.0)                                           &
     &      TEM4    = POTEVAP * (1. - EXP( tx4*TX1**0.57777778 ) )
!    &      TEM4    = POTEVAP * (1. - EXP( AFC*tx4*SQRT(TX1) ) )
!    &      TEM4    = POTEVAP * (1. - EXP( AFC*SQRT(TX1*TEM3) ) )
!    &      TEM4    = POTEVAP * (1. - EXP(-0.32*SQRT(DT*TX1*0.001) ) )
            ACTEVAP = MIN(TX1, TEM4*CLFRAC)
!           ACTEVAP = MIN(TX1, TEM4*CLFRAC*clvfr)
!     if(lprnt) print *,' L=',L,' actevap=',actevap,' tem4=',tem4,
!    &' clfrac='
!    &,clfrac,' potevap=',potevap,'efac=',AFC*SQRT(TX1*TEM3)
!    &,' tx1=',tx1
            if (tx1 .lt. rainmin*dt) actevap = min(tx1, potevap)
!
            tem4    = 0.0
            if (tx2 .gt. 0.0)                                           &
     &      TEM4    = POTEVAP * (1. - EXP( tx4*TX2**0.57777778 ) )
!    &      TEM4    = POTEVAP * (1. - EXP( AFC*tx4*SQRT(TX2) ) )
!    &      TEM4    = POTEVAP * (1. - EXP( AFC*SQRT(TX2*TEM3) ) )
!    &      TEM4    = POTEVAP * (1. - EXP(-0.32*SQRT(DT*TX2*0.001) ) )
            TEM4    = min(MIN(TX2, TEM4*CLDFRD), potevap-actevap)
            if (tx2 .lt. rainmin*dt) tem4 = min(tx2, potevap-actevap)
!
            TX1     = TX1 - ACTEVAP
            TX2     = TX2 - TEM4
            ST1     = (ACTEVAP+TEM4) * PRI(L)
            QOI(L)  = QOI(L) + ST1
            QCU(L)  = QCU(L) + ST1
!

            ST1     = ST1 * ELOCP
            TOI(L)  = TOI(L) - ST1 
            TCU(L)  = TCU(L) - ST1
          ENDIF
         ENDIF
       ENDDO
!
!!!    CUP = CUP + TX1 + TX2
       CUP = CUP + TX1 + TX2 + DOF * AMB
      ELSE
       DO L=KD,K
         TX1 = TX1 + AMB * RNN(L)
       ENDDO
       CUP = CUP + TX1 + DOF * AMB
      ENDIF

!     CUP = CUP + TX1 + TX2 + DOF * AMB
!     CUP = CUP + TX1 + TX2
!     if (lprnt) print *,' tx1=',tx1,' tx2=',tx2,' dof=',dof
!    &,' cup=',cup*86400/dt,' amb=',amb
!    &,' amb=',amb,' cup=',cup,' clfrac=',clfrac,' cldfrd=',cldfrd
!    &,' ddft=',ddft,' kd=',kd,' kbl=',kbl,' k=',k
!
!    MIXING OF PASSIVE TRACERS
!
      DO N=1,M

       DO L=KD,K
         HOL(L) = ROI(L,N)
       ENDDO
!
        HCC     = RBL(N)
        HOD(KD) = HOL(KD)
!      Compute downdraft properties for the tracer
       DO L=KD1,K
         ST1 = ONE - ALFIND(L)
         HB  = ALFIND(L)  * HOL(L-1) + ST1 * HOL(L)
         IF (ETZ(L-1) .NE. 0.0) THEN
           DEL_ETA = ETD(L) - ETD(L-1)
           TEM     = 1.0 / ETZ(L-1)
           IF (DEL_ETA .GT. 0.0) THEN
             HOD(L) = (ETD(L-1)*(HOD(L-1)-HOL(L-1))                     &
     &                +  ETD(L)  *(HOL(L-1)-HB)                         &
     &                +  ETZ(L-1)*HB) * TEM
           ELSE
             HOD(L) = (ETD(L-1)*(HOD(L-1)-HB) + ETZ(L-1)*HB) * TEM
           ENDIF
         ELSE
           HOD(L) = HB
         ENDIF
       ENDDO
             
       DO L=KB1,KD,-1
          HCC = HCC + (ETA(L)-ETA(L+1))*HOL(L)
       ENDDO
!
       GMH(KD) = PRI(KD) * (HCC-ETA(KD)*HOL(KD))
       DO L=KD1,K
        ST1 = ONE - ALFINT(L,N+4)
        ST2 = ONE - ALFIND(L)
!        IF (L .LT. KBL) THEN
           HB       = ALFINT(L,N+4) * HOL(L-1) + ST1 * HOL(L)
           HBD      = ALFIND(L) * HOL(L-1) + ST2 * HOL(L)
           TEM5     = ETD(L)    * (HOD(L) - HBD)
!!         DH       = ETA(L)    * (HB - HOL(L)) * trcfac(n) + TEM5
           DH       = ETA(L)    * (HB - HOL(L)) + TEM5
!!         GMH(L  ) = DH * PRI(L)
           GMH(L  ) = DH * PRI(L) * trcfac(n,l)
!!         DH       = ETA(L)    * (HOL(L-1) - HB) * trcfac(n) - TEM5
!!         GMH(L-1) = GMH(L-1)  + DH * PRI(L-1)
           DH       = ETA(L)    * (HOL(L-1) - HB) - TEM5
           GMH(L-1) = GMH(L-1)  + DH * PRI(L-1) * trcfac(n,l)
!        ELSEIF (L .EQ. KBL) THEN
!          HB       = ALFINT(L) * HOL(L-1) + ST1 * RBL(N)
!          HBD      = ALFINT(L) * HOL(L-1) + ST1 * HOL(L)
!          DH       = ETD(L)    * (HOD(L) - HBD)
!          tem4     = GRAVFAC * pris
!          TX1      = ETA(L)    * (HB - RBL(N)) * TEM4
!          GMH(L)   = (TX1      + DH * PRI(L)) * trcfac(n)
!          DH       = ETA(L)    * (HOL(L-1) - HB) - DH
!          GMH(L-1) = GMH(L-1)  + DH * PRI(L-1) * trcfac(n)
!        ELSE
!          HBD      = ALFINT(L) * HOL(L-1) + ST1 * HOL(L)
!          DH       = ETD(L)    * (HOD(L) - HBD)
!          GMH(L)   = (TX1      + DH * PRI(L)) * trcfac(n)
!          GMH(L-1) = GMH(L-1)  - DH * PRI(L-1) * trcfac(n)
!        ENDIF
       ENDDO
!
       DO L=KD,K
         ST1      = GMH(L)*AMB
         ROI(L,N) = HOL(L)   + ST1
         RCU(L,N) = RCU(L,N) + ST1
       ENDDO
      ENDDO                             ! Tracer loop M

!     if (lprnt) print *,' toio=',toi
!     if (lprnt) print *,' qoio=',qoi
!     if (lprnt .and. kd .eq. 41) stop
!     if (toi(K)-toi(k-1) .lt. 20.0) stop
!***********************************************************************
!***********************************************************************
!***********************************************************************

      RETURN
      END

      SUBROUTINE DDRFT(                                                 &
     &                  K, KD                                           &
     &,                 TLA, ALFIND                                     &
     &,                 TOL, QOL, HOL, PRL, QST, HST, GAM, GAF, HBL, QBL&
     &,                 QRB, QRT, BUY, KBL, IDH, ETA, RNN, ETAI         &
     &,                 ALM, WFN, TRAIN, DDFT                           &
     &,                 ETD, HOD, QOD, EVP, DOF, CLDFRD, WCB            &
     &,                 GMS, GSD, GHD,lprnt)                   
!    &,                 GMS, GSD, GHD,lprnt)
!    &,                 TX1, TX2, TX3, TX4, TX5, TX6, TX7, TX8, TX9)

!
!***********************************************************************
!******************** Cumulus Downdraft Subroutine *********************
!****************** Based on Cheng and Arakawa (1997)  ****** **********
!************************ SUBROUTINE DDRFT  ****************************
!*************************  October 2004  ******************************
!***********************************************************************
!***********************************************************************
!************* Shrinivas.Moorthi@noaa.gov (301) 763 8000(X7233) ********
!***********************************************************************
!***********************************************************************
!23456789012345678901234567890123456789012345678901234567890123456789012
!
!===>  TOL(K)     INPUT   TEMPERATURE            KELVIN
!===>  QOL(K)     INPUT   SPECIFIC HUMIDITY      NON-DIMENSIONAL

!===>  PRL(K+1)   INPUT   PRESSURE @ EDGES       MB

!===>  K     INPUT   THE RISE & THE INDEX OF THE SUBCLOUD LAYER
!===>  KD    INPUT   DETRAINMENT LEVEL ( 1<= KD < K )          
!     
      USE MACHINE , ONLY : kind_phys
      use module_ras
      IMPLICIT NONE
!
!  INPUT ARGUMENTS
!
      INTEGER K, KD
      real(kind=kind_phys) ALFIND(K)
      INTEGER KBL, KB1



      LOGICAL SKPDD, SKPUP

      real(kind=kind_phys) HOL(KD:K),   QOL(KD:K),   GAF(KD:K+1)        &
     &,                    HST(KD:K),   QST(KD:K),   TOL(KD:K)          &
     &,                    BUY(KD:K+1), QRB(KD:K),   QRT(KD:K)          &
     &,                    GAM(KD:K+1), RNN(KD:K),   RNS(KD:K)                       &
     &,                    ETA(KD:K+1), PRL(KD:K+1), ETAI(KD:K)
!
      real(kind=kind_phys)    HBL,     QBL,        PRIS                 &
     &,                       TRAIN,   WFN,        ALM
!
!     TEMPORARY WORK SPACE
!
      real(kind=kind_phys) GMS(KD:K+1)
      real(kind=kind_phys) TX1,    TX2,  TX3, TX4                       &
     &,                    TX5,    TX6,  TX7, TX8, TX9
      LOGICAL UNSAT

      real(kind=kind_phys) TL, PL, QL, QS, DQS, ST1,  HB, QB, TB        &
     &,                    QQQ, PICON, PIINV, DEL_ETA                   &
     &,                    TEM, TEM1, TEM2, TEM3, TEM4, ST2             &
     &,                    ERRMIN, ERRMI2, ERRH, ERRW, ERRE, TEM5       &
     &,                    TEM6, HBD, QBD
      INTEGER I, L,  N, IX, KD1, II                                     &
     &,       KP1, IT, KM1, KTEM, KK, KK1, LM1, LL, LP1                 &
     &,       IP1, JJ, ntla

!
      integer, parameter :: NUMTLA=2
!     integer, parameter :: NUMTLA=4
      parameter (ERRMIN=0.0001, ERRMI2=0.1*ERRMIN)
!     parameter (ERRMIN=0.00001, ERRMI2=0.1*ERRMIN)
!
      real(kind=kind_phys) TLA,    STLA,  CTL2, CTL3
      real(kind=kind_phys) GMF,    PI,    ONPG, CTLA, VTRM, VTPEXP      &
     &,    RPART,  QRMIN,  AA1,    BB1,   CC1,  DD1                     &
     &,    WC2MIN, WCMIN,  WCBASE, F2,    F3,   F5, GMF1, GMF5          &
     &,    QRAF,   QRBF,   CMPOR , del_tla               
!    &,    sialf
!
      parameter (ONPG=1.0+0.5, GMF=1.0/ONPG, RPART=0.0)
!     parameter (ONPG=1.0+0.5, GMF=1.0/ONPG, RPART=1.0)
!     parameter (ONPG=1.0+0.5, GMF=1.0/ONPG, RPART=0.5)
!     PARAMETER (AA1=1.0, BB1=1.5, CC1=1.1, DD1=0.85, F3=CC1, F5=2.5)
!     PARAMETER (AA1=2.0, BB1=1.5, CC1=1.1, DD1=0.85, F3=CC1, F5=2.5)
      PARAMETER (AA1=1.0, BB1=1.0, CC1=1.0, DD1=1.0, F3=CC1,  F5=1.0)
      parameter (QRMIN=1.0E-6, WC2MIN=0.01, GMF1=GMF/AA1, GMF5=GMF/F5)
!     parameter (QRMIN=1.0E-6, WC2MIN=1.00, GMF1=GMF/AA1, GMF5=GMF/F5)
!     parameter (sialf=0.5)
!
      PARAMETER (PI=3.1415926535897931, PIINV=1.0/PI)
      INTEGER ITR, ITRMU, ITRMD, KTPD, ITRMIN, ITRMND
!     PARAMETER (ITRMU=25, ITRMD=25, ITRMIN=7)
      PARAMETER (ITRMU=25, ITRMD=25, ITRMIN=12, ITRMND=12)
!     PARAMETER (ITRMU=25, ITRMD=25, ITRMIN=12)
!     PARAMETER (ITRMU=14, ITRMD=18, ITRMIN=7)
!     PARAMETER (ITRMU=10, ITRMD=10, ITRMIN=5)
      real(kind=kind_phys) QRP(KD:K+1), WVL(KD:K+1), AL2
      real(kind=kind_phys) WVLO(KD:K+1)
!
      real(kind=kind_phys) RNF(KD:K),   ETD(KD:K+1), WCB(KD:K)          &
     &,                    HOD(KD:K+1), QOD(KD:K+1), EVP(KD:K)          &
     &,                    ROR(KD:K+1), STLT(KD:K)                      &
     &,                    GHD(KD:K),   GSD(KD:K),   CLDFRD(KD:K)       &
     &,                    RNT,        RNB                              &
     &,                    ERRQ,       RNTP
      INTEGER IDW, IDH, IDN(K), idnm
      real(kind=kind_phys) ELM(K)
!     real(kind=kind_phys) EM(K*K), ELM(K)
      real(kind=kind_phys) EDZ, DDZ, CE, QHS, FAC, FACG, ASIN,          &
     &                     RSUM1, RSUM2, RSUM3, CEE
      LOGICAL DDFT, UPDRET, DDLGK
!
      real(kind=kind_phys) AA(KD:K,KD:K+1), QW(KD:K,KD:K)               &
     &,                    BUD(KD:K), VT(2), VRW(2), TRW(2)             &
     &,                    GQW(KD:K)                                    &
     &,                    QA(3),     WA(3),    DOF, DOFW               &
     &,                    QRPI(KD:K), QRPS(KD:K)
!    &,                    GQW(KD:K), WCB(KD:K)

!***********************************************************************

      real(kind=kind_phys) QRPF, VTPF
      logical lprnt
!CFPP$ EXPAND (QRPF, QRABF, VTPF)
!CFPP$ NOCONCUR R

!

!     if(lprnt) print *,' K=',K,' KD=',KD,' In Downdrft'

      KD1    = KD + 1
      KP1    = K  + 1
      KM1    = K  - 1
      KB1    = KBL - 1
!
      CMPOR  = CMB2PA / RGAS
!
!     VTP    = 36.34*SQRT(1.2)* (0.001)**0.1364
      VTPEXP = -0.3636
!     PIINV  = 1.0 / PI
      PICON  = PI * ONEBG * 0.5
!
!
!     Compute Rain Water Budget of the Updraft (Cheng and Arakawa, 1997)
!
      CLDFRD = 0.0
      RNTP   = 0.0
      DOF    = 0.0
      ERRQ   = 10.0
      RNB    = 0.0
      RNT    = 0.0
      TX2    = PRL(KBL)
!
      TX1      = (PRL(KD) + PRL(KD1)) * 0.5
      ROR(KD)  = CMPOR*TX1 / (TOL(KD)*(1.0+NU*QOL(KD)))
!     GMS(KD)  = VTP * ROR(KD) ** VTPEXP
      GMS(KD)  = VTP * VTPF(ROR(KD))
!
      QRP(KD)  = QRMIN
!
      TEM      = TOL(K) * (1.0 + NU * QOL(K))
      ROR(K+1) = 0.5 * CMPOR * (PRL(K+1)+PRL(K)) / TEM
      GMS(K+1) = VTP * VTPF(ROR(K+1))
      QRP(K+1) = QRMIN
!!    BUY(KD)  = MAX(BUY(KD),ONE_M1)
!     BUY(KD)  = MAX(BUY(KD), 0.1)
!     BUY(KD)  = MAX(BUY(KD), 0.0)
!
      kk = kbl
      DO L=KD1,K
        TEM = 0.5 * (TOL(L)+TOL(L-1))                                   &
     &      * (1.0 + (0.5*NU) * (QOL(L)+QOL(L-1)))
        ROR(L) = CMPOR * PRL(L) / TEM
!       GMS(L) = VTP * ROR(L) ** VTPEXP
        GMS(L) = VTP * VTPF(ROR(L))
        QRP(L) = QRMIN
!!      BUY(L) = MAX(BUY(L),ONE_M1)
!       BUY(L) = MAX(BUY(L), 0.1)
!       BUY(L) = MAX(BUY(L), 1.0E-5)
        if (buy(l) .le. 0.0 .and. kk .eq. KBL) then
          kk = l
!       if (buy(l) .le. 0.0) then
!         if (buy(l-1) .gt. 0.0 .and. buy(l+1) .gt. 0.0) then
!           buy(l) = 0.5 * (buy(l+1) + buy(l-1))
!         elseif (buy(l-1) .gt. 0.0) then
!           buy(l) = 0.5*buy(l-1)
!           buy(l) = 0.25 * buy(l-1)
!         else
!            BUY(L) = 1.0E-4
!            BUY(L) = 5.0E-4
!            BUY(L) = 1.0E-5
!         endif
        endif
!       BUY(L) = MAX(BUY(L), 1.0E-4)
!       BUY(L) = MAX(BUY(L), 1.0E-5)
!       BUY(L) = MAX(BUY(L), 5.0E-4)
      ENDDO
      if (kk .ne. kbl) then
        do l=kk,kbl
          buy(l) = 0.9 * buy(l-1)
        enddo
      endif
!
      do l=kd,k
        qrpi(l) = buy(l)
      enddo
      do l=kd1,kb1
        buy(l) = 0.25 * (qrpi(l-1)+qrpi(l)+qrpi(l)+qrpi(l+1))
!       tem = 0.5 * (eta(l)+eta(l+1))
!       buy(l) = buy(l) * tem * tem
      enddo
!     tem = 0.5 * (eta(KD)+eta(kd1))
!     buy(kd) = buy(kd) * tem * tem
      
!
!     CALL ANGRAD(TX1, ALM, STLA, CTL2, AL2, PI, TLA, TX2, WFN, TX3)
      tx1 = 1000.0 + tx1 - prl(k+1)
      CALL ANGRAD(TX1, ALM,  AL2, TLA, TX2, WFN, TX3)
!
!    Following Ucla approach for rain profile
!
      F2      = 2.0*BB1*ONEBG/(PI*0.2)
      WCMIN   = SQRT(WC2MIN)
      WCBASE  = WCMIN
!
!     del_tla = TLA * 0.2
!     del_tla = TLA * 0.25
      del_tla = TLA * 0.3
      TLA     = TLA - DEL_TLA
!
!     do ntla=1,numtla
!
!     if (errq .lt. 1.0 .or. tla .gt. 45.0) cycle
!
!     tla = tla + del_tla
!     STLA = SIN(TLA*PI/180.0)
!     CTL2 = 1.0 - STLA * STLA
!
!     if (lprnt) print *,' tla=',tla,' al2=',al2,' ptop='
!    &,0.5*(prl(kd)+prl(kd1)),' ntla=',ntla
!     if (lprnt) print *,' buy=',(buy(l),l=kd,kbl)
!
!     STLA = F2     * STLA * AL2
!     CTL2 = DD1    * CTL2
!     CTL3 = 0.1364 * CTL2
!
      DO L=KD,K
        RNF(L)   = 0.0
        RNS(L)   = 0.0
        WVL(L)   = 0.0
        STLT(L)  = 0.0
        GQW(L)   = 0.0
        QRP(L)   = QRMIN
        DO N=KD,K
          QW(N,L) = 0.0
        ENDDO
      ENDDO
!
!-----QW(N,L) = D(W(N)*W(N))/DQR(L)
!
      KK = KBL
!     WVL(KK)    = WCBASE
      QW(KD,KD)  = -QRB(KD)  * GMF1
      GHD(KD)    = ETA(KD)   * ETA(KD)
      GQW(KD)    = QW(KD,KD) * GHD(KD)
      GSD(KD)    = ETAI(KD)  * ETAI(KD)
!     GSD(KD)    = 1.0 / GHD(KD)
!
      GQW(KK)    = -  QRB(KK-1) * (GMF1+GMF1)
!
      WCB(KK)    = WCBASE * WCBASE
!     WVL(KK)    = WCBASE
!     STLT(KBL)  = 1.0 / WCBASE

      TX1        = WCB(KK)
      GSD(KK)    = 1.0
      GHD(KK)    = 1.0
!
      TEM        = GMF1 + GMF1
!!    TX1        = WCB(KK) + buy(kb1)*tem*qrb(kb1)
      DO L=KB1,KD1,-1
         GHD(L)  = ETA(L)  * ETA(L)
         GSD(L)  = ETAI(L) * ETAI(L)
!        GSD(L)  = 1.0 / GHD(L)
         GQW(L)  = - GHD(L) * (QRB(L-1)+QRT(L)) * TEM
         QW(L,L) = - QRT(L) * TEM
!
!        TX1     = TX1 + BUY(L) * TEM
!!       TX1     = TX1 + BUY(L) * TEM * (qrb(l-1)+qrt(l)) * ghd(l)
         st1     = 0.5 * (eta(l) + eta(l+1))
         TX1     = TX1 + BUY(L) * TEM * (qrb(l)+qrt(l)) * st1 * st1
         WCB(L)  = TX1 * GSD(L)
      ENDDO
!
      TEM1        = (QRB(KD) + QRT(KD1) + QRT(KD1)) * GMF1
      GQW(KD1)    = - GHD(KD1) * TEM1
!     QW(L,KD1)   = - QRT(KD1) * TEM
      QW(KD1,KD1) = - QRT(KD1) * TEM
!     WCB(KD)     = (TX1 + BUY(KD)*TEM) * GSD(KD)
      st1     = 0.5 * (eta(kd) + eta(kd1))
      WCB(KD)     = (TX1 + BUY(KD)*TEM*qrb(kd)*st1*st1) * GSD(KD)
!
      DO L=KD1,KBL
        DO N=KD,L-1
           QW(N,L) = GQW(L) * GSD(N)
        ENDDO
      ENDDO
      QW(KBL,KBL) = 0.0
!
!     WVL(KBL)    = WCBASE
!     STLT(KBL)   = 1.0 / WCBASE
!
!
      do ntla=1,numtla
!
!     if (errq .lt. 1.0 .or. tla .gt. 45.0) cycle
      if (errq .lt. 0.1 .or. tla .gt. 45.0) cycle
!
      tla = tla + del_tla
      STLA = SIN(TLA*PI/180.0)
      CTL2 = 1.0 - STLA * STLA
!
!     if (lprnt) print *,' tla=',tla,' al2=',al2,' ptop='
!    &,0.5*(prl(kd)+prl(kd1)),' ntla=',ntla,' f2=',f2,' stla=',stla
!     if (lprnt) print *,' buy=',(buy(l),l=kd,kbl)
!
      STLA = F2     * STLA * AL2
      CTL2 = DD1    * CTL2
      CTL3 = 0.1364 * CTL2
!
      DO L=KD,K
        RNF(L)   = 0.0
        WVL(L)   = 0.0
        STLT(L)  = 0.0
        QRP(L)   = QRMIN
      ENDDO
      WVL(KBL)    = WCBASE
      STLT(KBL)   = 1.0 / WCBASE
!
      DO L=KD,K+1
        DO N=KD,K
          AA(N,L) = 0.0
        ENDDO
      ENDDO
!
      SKPUP = .FALSE.
!
      DO ITR=1,ITRMU               ! Rain Profile Iteration starts!
        IF (.NOT. SKPUP) THEN
           wvlo = wvl
!
!-----CALCULATING THE VERTICAL VELOCITY
!
          TX1      = 0.0
          QRPI(KBL) = 1.0 / QRP(KBL)
          DO L=KB1,KD,-1
            TX1     = TX1    + QRP(L+1) * GQW(L+1)
            ST1     = WCB(L) + QW(L,L)  * QRP(L)                        &
     &                       + TX1      * GSD(L)
!           if (st1 .gt. 0.0) then
            if (st1 .gt. wc2min) then
!             WVL(L)  = SQRT(ST1)
              WVL(L)  = 0.5 * (SQRT(ST1) + WVL(L))
!             if (itr .eq. 1) wvl(l) = wvl(l) * 0.25
            else
!     if (lprnt)  print *,' l=',l,' st1=',st1,' wcb=',wcb(l),' qw='
!    &,qw(l,l),' qrp=',qrp(l),' tx1=',tx1,' gsd=',gsd(l),' ite=',itr
!             wvl(l) = 0.5*(wcmin+wvl(l))
              wvl(l) = 0.5 * (wvl(l) + wvl(l+1))
              qrp(l) = 0.5 * ((wvl(l)*wvl(l)-wcb(l)-tx1*gsd(l))/qw(l,l) &
     &                     + qrp(l))
!!            wvl(l) = 0.5 * (wvl(l) + wvl(l+1))
            endif
!           wvl(l)  = 0.5 * (wvl(l) + wvlo(l))
!           WVL(L)  = SQRT(MAX(ST1,WC2MIN))
            wvl(l)  = max(wvl(l), wcbase)
            STLT(L) = 1.0 / WVL(L)
            QRPI(L) = 1.0 / QRP(L)
          ENDDO
!         qrps = qrp
!         do l=kd1,kb1
!           qrp(l) = 0.25 * (qrps(l-1)+qrps(l)+qrps(l)+qrps(l+1))
!           qrpi(l) = 1.0 / qrp(l)
!         enddo
!         qrpi(kd) = 1.0 / qrp(kd)
!
!     if (lprnt) then
!     print *,' ITR=',ITR,' ITRMU=',ITRMU
!     print *,' WVL=',(WVL(L),L=KD,KBL)
!     print *,' qrp=',(qrp(L),L=KD,KBL)
!     print *,' qrpi=',(qrpi(L),L=KD,KBL)
!     print *,' rnf=',(rnf(L),L=KD,KBL)
!     endif
!
!-----CALCULATING TRW, VRW AND OF
!
!         VT(1)   = GMS(KD) * QRP(KD)**0.1364
          VT(1)   = GMS(KD) * QRPF(QRP(KD))
          TRW(1)  = ETA(KD) * QRP(KD) * STLT(KD)
          TX6     = TRW(1) * VT(1)
          VRW(1)  = F3*WVL(KD) - CTL2*VT(1)
          BUD(KD) = STLA * TX6 * QRB(KD) * 0.5
          RNF(KD) = BUD(KD)
          DOF     = 1.1364 * BUD(KD) * QRPI(KD)
          DOFW    = -BUD(KD) * STLT(KD)
!
          RNT     = TRW(1) * VRW(1)
          TX2     = 0.0
          TX4     = 0.0
          RNB     = RNT
          TX1     = 0.5
          TX8     = 0.0
!
          IF (RNT .GE. 0.0) THEN
            TX3 = (RNT-CTL3*TX6) * QRPI(KD)
            TX5 = CTL2 * TX6 * STLT(KD)
          ELSE
            TX3 = 0.0
            TX5 = 0.0
            RNT = 0.0
            RNB = 0.0
          ENDIF
!
          DO L=KD1,KB1
            KTEM    = MAX(L-2, KD)
            LL      = L - 1
!
!           VT(2)   = GMS(L) * QRP(L)**0.1364
            VT(2)   = GMS(L) * QRPF(QRP(L))
            TRW(2)  = ETA(L) * QRP(L) * STLT(L)
            VRW(2)  = F3*WVL(L) - CTL2*VT(2)
            QQQ     = STLA * TRW(2) * VT(2)
            ST1     = TX1  * QRB(LL)
            BUD(L)  = QQQ * (ST1 + QRT(L))
!
            QA(2)   = DOF
            WA(2)   = DOFW
            DOF     = 1.1364 * BUD(L) * QRPI(L)
            DOFW    = -BUD(L) * STLT(L)
!
            RNF(LL) = RNF(LL) + QQQ * ST1
            RNF(L)  =           QQQ * QRT(L)
!
            TEM3    = VRW(1) + VRW(2)
            TEM4    = TRW(1) + TRW(2)
!
            TX6     = .25 * TEM3 * TEM4
            TEM4    = TEM4 * CTL3
!
!-----BY QR ABOVE
!
!           TEM1    = .25*(TRW(1)*TEM3 - TEM4*VT(1))*TX7
            TEM1    = .25*(TRW(1)*TEM3 - TEM4*VT(1))*QRPI(LL)
            ST1     = .25*(TRW(1)*(CTL2*VT(1)-VRW(2))                   &
     &                  * STLT(LL) + F3*TRW(2))
!-----BY QR BELOW
            TEM2    = .25*(TRW(2)*TEM3 - TEM4*VT(2))*QRPI(L)
            ST2     = .25*(TRW(2)*(CTL2*VT(2)-VRW(1))                   &
     &                 * STLT(L)  + F3*TRW(1))
!
!      From top to  the KBL-2 layer
!
            QA(1)   = TX2
            QA(2)   = QA(2) + TX3 - TEM1
            QA(3)   = -TEM2
!
            WA(1)   = TX4
            WA(2)   = WA(2) + TX5 - ST1
            WA(3)   = -ST2
!
            TX2     = TEM1
            TX3     = TEM2
            TX4     = ST1
            TX5     = ST2
!
            VT(1)   = VT(2)
            TRW(1)  = TRW(2)
            VRW(1)  = VRW(2)
!
            IF (WVL(KTEM) .EQ. WCMIN) WA(1) = 0.0
            IF (WVL(LL)   .EQ. WCMIN) WA(2) = 0.0
            IF (WVL(L)    .EQ. WCMIN) WA(3) = 0.0
            DO N=KTEM,KBL
              AA(LL,N) = (WA(1)*QW(KTEM,N) * STLT(KTEM)                 &
     &                 +  WA(2)*QW(LL,N)   * STLT(LL)                   &
     &                 +  WA(3)*QW(L,N)    * STLT(L) ) * 0.5
            ENDDO
            AA(LL,KTEM) = AA(LL,KTEM) + QA(1)
            AA(LL,LL)   = AA(LL,LL)   + QA(2)
            AA(LL,L)    = AA(LL,L)    + QA(3)
            BUD(LL)     = (TX8 + RNN(LL)) * 0.5                         &
     &                    - RNB + TX6 - BUD(LL)
            AA(LL,KBL+1) = BUD(LL)
            RNB = TX6
            TX1 = 1.0
            TX8 = RNN(LL)
          ENDDO
          L  = KBL
          LL = L - 1
!         VT(2)   = GMS(L) * QRP(L)**0.1364
          VT(2)   = GMS(L) * QRPF(QRP(L))
          TRW(2)  = ETA(L) * QRP(L) * STLT(L)
          VRW(2)  = F3*WVL(L) - CTL2*VT(2)
          ST1     = STLA * TRW(2) * VT(2) * QRB(LL)
          BUD(L)  = ST1

          QA(2)   = DOF
          WA(2)   = DOFW
          DOF     = 1.1364 * BUD(L) * QRPI(L)
          DOFW    = -BUD(L) * STLT(L)
!
          RNF(LL) = RNF(LL) + ST1
!
          TEM3    = VRW(1) + VRW(2)
          TEM4    = TRW(1) + TRW(2)
!
          TX6     = .25 * TEM3 * TEM4
          TEM4    = TEM4 * CTL3
!
!-----BY QR ABOVE
!
          TEM1    = .25*(TRW(1)*TEM3 - TEM4*VT(1))*QRPI(LL)
          ST1     = .25*(TRW(1)*(CTL2*VT(1)-VRW(2))                     &
     &                * STLT(LL) + F3*TRW(2))
!-----BY QR BELOW
          TEM2    = .25*(TRW(2)*TEM3 - TEM4*VT(2))*QRPI(L)
          ST2     = .25*(TRW(2)*(CTL2*VT(2)-VRW(1))                     &
     &                 * STLT(L)  + F3*TRW(1))
!
!      For the layer next to the top of the boundary layer
!
          QA(1)   = TX2
          QA(2)   = QA(2) + TX3 - TEM1
          QA(3)   = -TEM2
!
          WA(1)   = TX4
          WA(2)   = WA(2) + TX5 - ST1
          WA(3)   = -ST2
!
          TX2     = TEM1
          TX3     = TEM2
          TX4     = ST1
          TX5     = ST2
!
          IDW     = MAX(L-2, KD)
!
          IF (WVL(IDW) .EQ. WCMIN) WA(1) = 0.0
          IF (WVL(LL)  .EQ. WCMIN) WA(2) = 0.0
          IF (WVL(L)   .EQ. WCMIN) WA(3) = 0.0
!
          KK = IDW
          DO N=KK,L
            AA(LL,N) = (WA(1)*QW(KK,N) * STLT(KK)                       &
     &               +  WA(2)*QW(LL,N) * STLT(LL)                       &
     &               +  WA(3)*QW(L,N)  * STLT(L) ) * 0.5

          ENDDO
!
          AA(LL,IDW) = AA(LL,IDW) + QA(1)
          AA(LL,LL)  = AA(LL,LL)  + QA(2)
          AA(LL,L)   = AA(LL,L)   + QA(3)
          BUD(LL)    = (TX8+RNN(LL)) * 0.5 - RNB + TX6 - BUD(LL)
!
          AA(LL,L+1) = BUD(LL)
!
          RNB        = TRW(2) * VRW(2)
!
!      For the top of the boundary layer
!
          IF (RNB .LT. 0.0) THEN
             KK    = KBL
             TEM   = VT(2) * TRW(2)
             QA(2) = (RNB - CTL3*TEM) * QRPI(KK)
             WA(2) = CTL2 * TEM * STLT(KK)
          ELSE
             RNB   = 0.0
             QA(2) = 0.0
             WA(2) = 0.0
          ENDIF
!
          QA(1) = TX2
          QA(2) = DOF + TX3 - QA(2)
          QA(3) = 0.0
!
          WA(1) = TX4
          WA(2) = DOFW + TX5 - WA(2)
          WA(3) = 0.0
!
          KK = KBL
          IF (WVL(KK-1) .EQ. WCMIN) WA(1) = 0.0
          IF (WVL(KK)   .EQ. WCMIN) WA(2) = 0.0
!
          DO II=1,2
             N = KK + II - 2
             AA(KK,N) = (WA(1)*QW(KK-1,N) * STLT(KK-1)                  &
     &                +  WA(2)*QW(KK,N)   * STLT(KK)) * 0.5
          ENDDO
          FAC = 0.5
          LL  = KBL
          L   = LL + 1
          LM1 = LL - 1
          AA(LL,LM1)  = AA(LL,LM1) + QA(1)
          AA(LL,LL)   = AA(LL,LL)  + QA(2)
          BUD(LL)     = 0.5*RNN(LM1) - TX6 + RNB - BUD(LL)
          AA(LL,LL+1) = BUD(LL)
!
!-----SOLVING THE BUDGET EQUATIONS FOR DQR
!
          DO L=KD1,KBL
            LM1  = L - 1
            UNSAT = ABS(AA(LM1,LM1)) .LT. ABS(AA(L,LM1))
            DO  N=LM1,KBL+1
               IF (UNSAT) THEN
                  TX1       = AA(LM1,N)
                  AA(LM1,N) = AA(L,N)
                  AA(L,N)   = TX1
               ENDIF
            ENDDO
            TX1 = AA(L,LM1) / AA(LM1,LM1)
            DO  N=L,KBL+1
               AA(L,N) = AA(L,N) - TX1 * AA(LM1,N)
            ENDDO
          ENDDO     
!
!-----BACK SUBSTITUTION AND CHECK IF THE SOLUTION CONVERGES
!
          KK = KBL
          KK1 = KK + 1
          AA(KK,KK1) = AA(KK,KK1) / AA(KK,KK)      !   Qr correction !
          TX2        = ABS(AA(KK,KK1)) * QRPI(KK)  !   Error Measure !
!     if (lprnt) print *,' tx2a=',tx2,' aa1=',aa(kk,kk1)
!    &,' qrpi=',qrpi(kk)
!
          KK = KBL + 1
          DO L=KB1,KD,-1
             LP1   = L + 1
             TX1  = 0.0
             DO N=LP1,KBL
               TX1  = TX1 + AA(L,N) * AA(N,KK)
             ENDDO
             AA(L,KK) = (AA(L,KK) - TX1) / AA(L,L)       ! Qr correction !
             TX2      = MAX(TX2, ABS(AA(L,KK))*QRPI(L))  ! Error Measure !
!     if (lprnt) print *,' tx2b=',tx2,' aa1=',aa(l,kk)
!    &,' qrpi=',qrpi(l),' L=',L
          ENDDO
!
!         tem = 0.5
          if (tx2 .gt. 1.0 .and. abs(errq-tx2) .gt. 0.1) then
            tem = 0.5
!!        elseif (tx2 .lt. 0.1) then
!!          tem = 1.2
          else
            tem = 1.0
          endif
!
          DO L=KD,KBL
!            QRP(L) = MAX(QRP(L)+AA(L,KBL+1), QRMIN)
             QRP(L) = MAX(QRP(L)+AA(L,KBL+1)*tem, QRMIN)
          ENDDO
!
!     if (lprnt) print *,' itr=',itr,' tx2=',tx2
          IF (ITR .LT. ITRMIN) THEN
             TEM = ABS(ERRQ-TX2) 
             IF (TEM .GE. ERRMI2 .AND. TX2 .GE. ERRMIN) THEN 
               ERRQ  = TX2                              ! Further iteration !
             ELSE 
               SKPUP = .TRUE.                           ! Converges      !
               ERRQ  = 0.0                              ! Rain profile exists!
!     print *,' here1',' tem=',tem,' tx2=',tx2,' errmi2=',
!    *errmi2,' errmin=',errmin
             ENDIF 
          ELSE
             TEM = ERRQ - TX2
!            IF (TEM .LT. ZERO .AND. ERRQ .GT. 0.1) THEN
             IF (TEM .LT. ZERO .AND. ERRQ .GT. 0.5) THEN
!            IF (TEM .LT. ZERO .and.                                    &
!    &          (ntla .lt. numtla .or. ERRQ .gt. 0.5)) THEN
!     if (lprnt) print *,' tx2=',tx2,' errq=',errq,' tem=',tem
               SKPUP = .TRUE.                           ! No convergence !
               ERRQ = 10.0                              ! No rain profile!
!!!!         ELSEIF (ABS(TEM).LT.ERRMI2 .OR. TX2.LT.ERRMIN) THEN
             ELSEIF (TX2.LT.ERRMIN) THEN
               SKPUP = .TRUE.                           ! Converges      !
               ERRQ = 0.0                               ! Rain profile exists!
!     print *,' here2'
             elseif (tem .lt. zero .and. errq .lt. 0.1) then
               skpup = .true.
!              if (ntla .eq. numtla .or. tem .gt. -0.003) then
                 errq  = 0.0
!              else
!                errq = 10.0
!              endif
             ELSE
               ERRQ = TX2                               ! Further iteration !
!     if (lprnt) print *,' itr=',itr,' errq=',errq
!              if (itr .eq. itrmu .and. ERRQ .GT. ERRMIN*10             &
!    &            .and. ntla .eq. 1) ERRQ = 10.0 
             ENDIF
          ENDIF
!
!         if (lprnt) print *,' ERRQ=',ERRQ

        ENDIF                                           ! SKPUP  ENDIF!
!
      ENDDO                                          ! End of the ITR Loop!!
!     enddo                                          ! End of ntla loop
!
!     if(lprnt) then
!       print *,' QRP=',(QRP(L),L=KD,KBL)
!       print *,'RNF=',(RNF(L),L=KD,KBL),' RNT=',RNT,' RNB=',RNB
!    &,' errq=',errq
!     endif
!
      IF (ERRQ .LT. 0.1) THEN
        DDFT = .TRUE.
        RNB  = - RNB
   !    do l=kd1,kb1-1
   !      if (wvl(l)-wcbase .lt. 1.0E-9) ddft = .false.
   !    enddo
      ELSE
        DDFT = .FALSE.
      ENDIF
!
!     Caution !! Below is an adjustment to rain flux to maintain
!                conservation of precip!
!
      IF (DDFT) THEN
        TX1 = 0.0
        DO L=KD,KB1
          TX1 = TX1 + RNF(L)
        ENDDO
!     if (lprnt) print *,' tx1+rnt+rnb=',tx1+rnt+rnb, ' train=',train
        TX1 = TRAIN / (TX1+RNT+RNB)
        IF (ABS(TX1-1.0) .LT. 0.2) THEN
           RNT = MAX(RNT*TX1,ZERO)
           RNB = RNB * TX1
        ELSE
           DDFT = .FALSE.
           ERRQ = 10.0
        ENDIF
      ENDIF
      enddo                                          ! End of ntla loop
!
      DOF = 0.0
      IF (.NOT. DDFT) RETURN     ! Rain profile did not converge!
!

      DO L=KD,KB1
         RNF(L) = RNF(L) * TX1

      ENDDO
!     if (lprnt) print *,' TRAIN=',TRAIN
!     if (lprnt) print *,' RNF=',RNF
!
!     Adjustment is over
!
!     Downdraft
!
      DO L=KD,K
        WCB(L) = 0.0
      ENDDO
!
      SKPDD = .NOT. DDFT
!
      ERRQ  = 10.0
      IF (.NOT. SKPDD) THEN
!
!     Calculate Downdraft Properties
!

        KK = MAX(KB1,KD1)
        DO L=KK,K
          STLT(L) = STLT(L-1)
        ENDDO
        TEM1 = 1.0 / BB1
!
        DO L=KD,K
          IF (L .LE. KBL) THEN
            TEM     = STLA * TEM1
            STLT(L) = ETA(L) * STLT(L) * TEM / ROR(L)
          ELSE
            STLT(L) = 0.0
          ENDIF
        ENDDO
!       if (lprnt) print *,' STLT=',stlt

        rsum1 = 0.0
        rsum2 = 0.0

!
        IDN      = 99
        DO L=KD,K+1
          ETD(L)  = 0.0
          WVL(L)  = 0.0
!         QRP(L)  = 0.0
        ENDDO
        DO L=KD,K
          EVP(L)   = 0.0
          BUY(L)   = 0.0
          QRP(L+1) = 0.0
        ENDDO
        HOD(KD)  = HOL(KD)
        QOD(KD)  = QOL(KD)
        TX1      = 0.0                               ! sigma at the top
!!!     TX1      = STLT(KD)*QRB(KD)*ONE              ! sigma at the top
!       TX1      = MIN(STLT(KD)*QRB(KD)*ONE, ONE)    ! sigma at the top
!       TX1      = MIN(STLT(KD)*QRB(KD)*0.5, ONE)    ! sigma at the top
        RNTP     = 0.0
        TX5      = TX1
        QA(1)    = 0.0
!     if(lprnt) print *,' stlt=',stlt(kd),' qrb=',qrb(kd)
!    *,' tx1=',tx1,' ror=',ror(kd),' gms=',gms(kd),' rpart=',rpart
!    *,' rnt=',rnt
!
!       Here we assume RPART of detrained rain RNT goes to Pd
!
        IF (RNT .GT. 0.0) THEN
          if (TX1 .gt. 0.0) THEN
            QRP(KD) = (RPART*RNT / (ROR(KD)*TX1*GMS(KD)))               &
     &                                          ** (1.0/1.1364)
           else
             tx1 = RPART*RNT / (ROR(KD)*GMS(KD)*QRP(KD)**1.1364)
           endif
            RNTP    = (1.0 - RPART) * RNT
            BUY(KD) = - ROR(KD) * TX1 * QRP(KD)
        ELSE
          QRP(KD) = 0.0
        ENDIF
!
!     L-loop for the downdraft iteration from KD1 to K+1 (bottom surface)
!
!     BUD(KD) = ROR(KD)
      idnm = 1
      DO L=KD1,K+1

          QA(1) = 0.0
          ddlgk = idn(idnm) .eq. 99
          if (.not. ddlgk) cycle
          IF (L .LE. K) THEN
            ST1   = 1.0 - ALFIND(L)
            WA(1) = ALFIND(L)*HOL(L-1) + ST1*HOL(L)
            WA(2) = ALFIND(L)*QOL(L-1) + ST1*QOL(L)
            WA(3) = ALFIND(L)*TOL(L-1) + ST1*TOL(L)
            QA(2) = ALFIND(L)*HST(L-1) + ST1*HST(L)
            QA(3) = ALFIND(L)*QST(L-1) + ST1*QST(L)
          ELSE
            WA(1) = HOL(K)
            WA(2) = QOL(K)
            WA(3) = TOL(K)
            QA(2) = HST(K)
            QA(3) = QST(K)
          ENDIF
!
          FAC = 2.0
          IF (L .EQ. KD1) FAC = 1.0

          FACG    = FAC * 0.5 * GMF5     !  12/17/97
!
!         DDLGK   =  IDN(idnm) .EQ. 99
          BUD(KD) = ROR(L)

!         IF (DDLGK) THEN
            TX1    = TX5
            WVL(L) = MAX(WVL(L-1),ONE_M1)

            QRP(L) = MAX(QRP(L-1),QRP(L))
!
!           VT(1)  = GMS(L-1) * QRP(L-1) ** 0.1364
            VT(1)  = GMS(L-1) * QRPF(QRP(L-1))
            RNT    = ROR(L-1) * (WVL(L-1)+VT(1))*QRP(L-1)
!     if(lprnt) print *,' l=',l,' qa=',qa(1), ' tx1RNT=',RNT*tx1,
!    *' wvl=',wvl(l-1)
!    *,' qrp=',qrp(l-1),' tx5=',tx5,' tx1=',tx1,' rnt=',rnt

!

!           TEM    = MAX(ALM, 2.5E-4) * MAX(ETA(L), 1.0)
            TEM    = MAX(ALM,ONE_M6) * MAX(ETA(L), ONE)
!           TEM    = MAX(ALM, 1.0E-5) * MAX(ETA(L), 1.0)
            TRW(1) = PICON*TEM*(QRB(L-1)+QRT(L-1))
            TRW(2) = 1.0 / TRW(1)
!
            VRW(1) = 0.5 * (GAM(L-1) + GAM(L))
            VRW(2) = 1.0 / (VRW(1) + VRW(1))
!
            TX4    =  (QRT(L-1)+QRB(L-1))*(ONEBG*FAC*500.00*EKNOB)
!
            DOFW   = 1.0 / (WA(3) * (1.0 + NU*WA(2)))      !  1.0 / TVbar!
!
            ETD(L) = ETD(L-1)
            HOD(L) = HOD(L-1)
            QOD(L) = QOD(L-1)
!
            ERRQ   = 10.0

!
            IF (L .LE. KBL) THEN
              TX3 = STLT(L-1) * QRT(L-1) * (0.5*FAC)
              TX8 = STLT(L)   * QRB(L-1) * (0.5*FAC)
              TX9 = TX8 + TX3
            ELSE
              TX3 = 0.0
              TX8 = 0.0
              TX9 = 0.0
            ENDIF
!
            TEM  = WVL(L-1) + VT(1)
            IF (TEM .GT. 0.0) THEN
              TEM1 = 1.0 / (TEM*ROR(L-1))
              TX3 = VT(1) * TEM1 * ROR(L-1) * TX3
              TX6 = TX1 * TEM1
            ELSE
              TX6 = 1.0
            ENDIF
!         ENDIF
!
          IF (L .EQ. KD1) THEN
            IF (RNT .GT. 0.0) THEN
              TEM    = MAX(QRP(L-1),QRP(L))
              WVL(L) = TX1 * TEM * QRB(L-1)*(FACG*5.0)
            ENDIF
            WVL(L) = MAX(ONE_M2, WVL(L))
            TRW(1) = TRW(1) * 0.5
            TRW(2) = TRW(2) + TRW(2)
          ELSE
            IF (DDLGK) EVP(L-1) = EVP(L-2)
          ENDIF
!
!       No downdraft above level IDH
!

          IF (L .LT. IDH) THEN

            ETD(L)   = 0.0
            HOD(L)   = WA(1)
            QOD(L)   = WA(2)
            EVP(L-1) = 0.0
            WVL(L)   = 0.0
            QRP(L)   = 0.0
            BUY(L)   = 0.0
            TX5      = TX9
            ERRQ     = 0.0
            RNTP     = RNTP + RNT * TX1
            RNT      = 0.0
            WCB(L-1) = 0.0
          ENDIF
!         BUD(KD) = ROR(L)
!
!       Iteration loop for a given level L begins
!
!         if (lprnt) print *,' tx8=',tx8,' tx9=',tx9,' tx5=',tx5
!    &,                      ' tx1=',tx1
          DO ITR=1,ITRMD
!
!           UNSAT =  DDLGK .AND. (ERRQ .GT. ERRMIN)
            UNSAT =  ERRQ .GT. ERRMIN
            IF (UNSAT) THEN
!
!             VT(1)  = GMS(L) * QRP(L) ** 0.1364
              VT(1)  = GMS(L) * QRPF(QRP(L))
              TEM    =  WVL(L) + VT(1)
!
              IF (TEM .GT. 0.0) THEN
                ST1    = ROR(L) * TEM * QRP(L) + RNT
                IF (ST1 .NE. 0.0) ST1 = 2.0 * EVP(L-1) / ST1
                TEM1   = 1.0 / (TEM*ROR(L))
                TEM2   = VT(1) * TEM1 * ROR(L) * TX8
              ELSE
                TEM1   = 0.0
                TEM2   = TX8
                ST1    = 0.0
              ENDIF
!     if (lprnt) print *,' st1=',st1,' tem=',tem,' ror=',ror(l)
!    &,' qrp=',qrp(l),' rnt=',rnt,' ror1=',ror(l-1),' wvl=',wvl(l)
!    &,' wvl1=',wvl(l-1),' tem2=',tem2,' vt=',vt(1),' tx3=',tx3
!
              st2 = tx5
              TEM = ROR(L)*WVL(L) - ROR(L-1)*WVL(L-1)
              if (tem .gt. 0.0) then
                TX5 = (TX1 - ST1 + TEM2 + TX3)/(1.0+tem*tem1)
              else
                TX5 = TX1 - tem*tx6 - ST1 + TEM2 + TX3
              endif
              TX5   = MAX(TX5,ZERO)
              tx5 = 0.5 * (tx5 + st2)
!
!             qqq = 1.0 + tem * tem1 * (1.0 - sialf)
!
!             if (qqq .gt. 0.0) then
!               TX5   = (TX1 - sialf*tem*tx6 - ST1 + TEM2 + TX3) / qqq
!             else
!               TX5   = (TX1 - tem*tx6 - ST1 + TEM2 + TX3)
!             endif
!
!     if(lprnt) print *,' tx51=',tx5,' tx1=',tx1,' st1=',st1,' tem2='
!     if(tx5 .le. 0.0 .and. l .gt. kd+2)
!    * print *,' tx51=',tx5,' tx1=',tx1,' st1=',st1,' tem2='
!    *,tem2,' tx3=',tx3,' tem=',tem,' tem1=',tem1,' wvl=',wvl(l-1),
!    &wvl(l),' l=',l,' itr=',itr,' evp=',evp(l-1),' vt=',vt(1)
!    *,' qrp=',qrp(l),' rnt=',rnt,' kd=',kd
!     if (lprnt) print *,' etd=',etd(l),' wvl=',wvl(l)
!    &,' trw=',trw(1),trw(2),' ror=',ror(l),' wa=',wa


!
              TEM1   = ETD(L)
              ETD(L) = ROR(L) * TX5 * MAX(WVL(L),ZERO)
!
              if (etd(l) .gt. 0.0) etd(l) = 0.5 * (etd(l) + tem1)
!

              DEL_ETA = ETD(L) - ETD(L-1)

!               TEM       = DEL_ETA * TRW(2)
!               TEM2      = MAX(MIN(TEM, 1.0), -1.0)
!               IF (ABS(TEM) .GT. 1.0 .AND. ETD(L) .GT. 0.0 ) THEN
!                 DEL_ETA = TEM2 * TRW(1)
!                 ETD(L)  = ETD(L-1) + DEL_ETA
!               ENDIF
!               IF (WVL(L) .GT. 0.0) TX5 = ETD(L) / (ROR(L)*WVL(L))
!
                ERRE  = ETD(L) - TEM1
!
                tem  = max(abs(del_eta), trw(1))
                tem2 = del_eta / tem
                TEM1 = SQRT(MAX((tem+DEL_ETA)*(tem-DEL_ETA),ZERO))
!               TEM1 = SQRT(MAX((TRW(1)+DEL_ETA)*(TRW(1)-DEL_ETA),0.0))

                EDZ  = (0.5 + ASIN(TEM2)*PIINV)*DEL_ETA + TEM1*PIINV

              DDZ   = EDZ - DEL_ETA
              WCB(L-1) = ETD(L) + DDZ
!
              TEM1  = HOD(L)
              IF (DEL_ETA .GT. 0.0) THEN
                QQQ    = 1.0 / (ETD(L) + DDZ)
                HOD(L) = (ETD(L-1)*HOD(L-1) + DEL_ETA*HOL(L-1)          &
     &                                            + DDZ*WA(1)) * QQQ
                QOD(L) = (ETD(L-1)*QOD(L-1) + DEL_ETA*QOL(L-1)          &
     &                                            + DDZ*WA(2)) * QQQ
              ELSEif((ETD(L-1) + EDZ) .gt. 0.0) then
                QQQ    = 1.0 / (ETD(L-1) + EDZ)
                HOD(L) = (ETD(L-1)*HOD(L-1) + EDZ*WA(1)) * QQQ
                QOD(L) = (ETD(L-1)*QOD(L-1) + EDZ*WA(2)) * QQQ
              ENDIF
              ERRH  = HOD(L) - TEM1
              ERRQ  = ABS(ERRH/HOD(L))  + ABS(ERRE/MAX(ETD(L),ONE_M5))
!     if (lprnt) print *,' ERRQP=',errq,' errh=',errh,' hod=',hod(l)
!    &,' erre=',erre,' etd=',etd(l),' del_eta=',del_eta
              DOF   = DDZ
              VT(2) = QQQ

!
              DDZ  = DOF
              TEM4 = QOD(L)
              TEM1 = VRW(1)
!
              QHS  = QA(3) + 0.5 * (GAF(L-1)+GAF(L))                    &
     &                           * (HOD(L)-QA(2))
!
!                                           First iteration       !
!
              ST2  = PRL(L) * (QHS + TEM1 * (QHS-QOD(L)))
              TEM2 = ROR(L) * QRP(L)
              CALL QRABF(TEM2,QRAF,QRBF)
              TEM6 = TX5 * (1.6 + 124.9 * QRAF) * QRBF * TX4
!
              CE   = TEM6 * ST2 / ((5.4E5*ST2 + 2.55E6)*(ETD(L)+DDZ))
!
              TEM2   = - ((1.0+TEM1)*(QHS+CE) + TEM1*QOD(L))
              TEM3   = (1.0 + TEM1) * QHS * (QOD(L)+CE)
              TEM    = MAX(TEM2*TEM2 - 4.0*TEM1*TEM3,ZERO)
              QOD(L) = MAX(TEM4, (- TEM2 - SQRT(TEM)) * VRW(2))
!

!
!                                            second iteration   !
!
              ST2  = PRL(L) * (QHS + TEM1 * (QHS-QOD(L)))
              CE   = TEM6 * ST2 / ((5.4E5*ST2 + 2.55E6)*(ETD(L)+DDZ))
!             CEE  = CE * (ETD(L)+DDZ)
!


              TEM2   = - ((1.0+TEM1)*(QHS+CE) + TEM1*tem4)
              TEM3   = (1.0 + TEM1) * QHS * (tem4+CE)
              TEM    = MAX(TEM2*TEM2 - 4.0*TEM1*TEM3,ZERO)
              QOD(L) = MAX(TEM4, (- TEM2 - SQRT(TEM)) * VRW(2))
!                                              Evaporation in Layer L-1
!

              EVP(L-1) = (QOD(L)-TEM4) * (ETD(L)+DDZ)
!                                              Calculate Pd (L+1/2)
              QA(1)    = TX1*RNT + RNF(L-1) - EVP(L-1)
!
!     if(lprnt) print *,' etd=',etd(l),' tx5=',tx5,' rnt=',rnt
!    *,' rnf=',rnf(l-1),' evp=',evp(l-1),' itr=',itr,' L=',L

!
              if (qa(1) .gt. 0.0) then
              IF (ETD(L) .GT. 0.0) THEN
                TEM    = QA(1) / (ETD(L)+ROR(L)*TX5*VT(1))
                QRP(L) = MAX(TEM,ZERO)
              ELSEIF (TX5 .GT. 0.0) THEN
                QRP(L) = (MAX(ZERO,QA(1)/(ROR(L)*TX5*GMS(L))))           &
     &                                          ** (1.0/1.1364)
              ELSE
                QRP(L) = 0.0
              ENDIF
              else
                qrp(l) = 0.5 * qrp(l)
              endif
!                                              Compute Buoyancy
              TEM1   = WA(3)+(HOD(L)-WA(1)-ALHL*(QOD(L)-WA(2)))         &
     &                                                  * (1.0/CP)
!             if (lprnt) print *,' tem1=',tem1,' wa3=',wa(3),' hod='
!    &,hod(l),' wa1=',wa(1),' qod=',qod(l),' wa2=',wa(2),' alhl=',alhl
!    &,' cmpor=',cmpor,' dofw=',dofw,' prl=',prl(l),' qrp=',qrp(l)
              TEM1   = TEM1 * (1.0 + NU*QOD(L))
              ROR(L) = CMPOR * PRL(L) / TEM1
              TEM1   = TEM1 * DOFW
!!!           TEM1   = TEM1 * (1.0 + NU*QOD(L)) * DOFW

              BUY(L) = (TEM1 - 1.0 - QRP(L)) * ROR(L) * TX5
!                                              Compute W (L+1/2)

              TEM1   = WVL(L)
!             IF (ETD(L) .GT. 0.0) THEN
              WVL(L) = VT(2) * (ETD(L-1)*WVL(L-1) - FACG                &
     &                 * (BUY(L-1)*QRT(L-1)+BUY(L)*QRB(L-1)))
!
!             if (lprnt) print *,' wvl=',wvl(l),'vt2=',vt(2),' buy1='
!    &,buy(l-1),' buy=',buy(l),' qrt1=',qrt(l-1),' qrb1=',qrb(l-1)
!    &,' etd1=',etd(l-1),' wvl1=',wvl(l-1)
!             ENDIF
!
              if (wvl(l) .lt. 0.0) then
!               WVL(L) = max(wvl(l), 0.1*tem1)
!               WVL(L) = 0.5*tem1
!               WVL(L) = 0.1*tem1
!               WVL(L) = 0.0
                WVL(L) = 1.0e-10
              else
                WVL(L) = 0.5*(WVL(L)+TEM1)
              endif

!
!             WVL(L) = max(0.5*(WVL(L)+TEM1), 0.0)

              ERRW   = WVL(L) - TEM1
!
              ERRQ   = ERRQ + ABS(ERRW/MAX(WVL(L),ONE_M5))

!     if (lprnt) print *,' errw=',errw,' wvl=',wvl(l)
!     if(lprnt .or. tx5 .eq. 0.0) then
!     if(tx5 .eq. 0.0 .and. l .gt. kbl) then
!        print *,' errq=',errq,' itr=',itr,' l=',l,' wvl=',wvl(l)
!    &,' tx5=',tx5,' idnm=',idnm,' etd1=',etd(l-1),' etd=',etd(l)
!    &,' kbl=',kbl
!     endif
!
!     if(lprnt) print *,' itr=',itr,' itrmnd=',itrmnd,' itrmd=',itrmd
!             IF (ITR .GE. MIN(ITRMIN,ITRMD/2)) THEN
              IF (ITR .GE. MIN(ITRMND,ITRMD/2)) THEN
!     if(lprnt) print *,' itr=',itr,' etd1=',etd(l-1),' errq=',errq
                IF (ETD(L-1) .EQ. 0.0 .AND. ERRQ .GT. 0.2) THEN
!     if(lprnt) print *,' bud=',bud(kd),' wa=',wa(1),wa(2)
                  ROR(L)   = BUD(KD)
                  ETD(L)   = 0.0
                  WVL(L)   = 0.0
                  ERRQ     = 0.0
                  HOD(L)   = WA(1)
                  QOD(L)   = WA(2)
!                 TX5      = TX1 + TX9
                  if (L .le. KBL) then
                    TX5      = TX9
                  else
                    TX5 = (STLT(KB1) * QRT(KB1)                         &
     &                  +  STLT(KBL) * QRB(KB1)) * (0.5*FAC)
                  endif

!     if(lprnt) print *,' tx1=',tx1,' rnt=',rnt,' rnf=',rnf(l-1)
!    *,' evp=',evp(l-1),' l=',l
                  EVP(L-1) = 0.0
                  TEM      = MAX(TX1*RNT+RNF(L-1),ZERO)
                  QA(1)    = TEM - EVP(L-1)
!                 IF (QA(1) .GT. 0.0) THEN
!     if(lprnt) print *,' ror=',ror(l),' tx5=',tx5,' tx1=',tx1
!    *,' tx9=',tx9,' gms=',gms(l),' qa=',qa(1)
!     if(lprnt) call mpi_quit(13)
!     if (tx5 .eq. 0.0 .or. gms(l) .eq. 0.0)
!     if (lprnt) 
!    *  print *,' Atx5=',tx5,' gms=',gms(l),' ror=',ror(l)
!    *,' L=',L,' QA=',QA(1),' tx1=',tx1,' tx9=',tx9
!    *,' kbl=',kbl,' etd1=',etd(l-1),' idnm=',idnm,' idn=',idn(idnm)
!    *,' errq=',errq
                  QRP(L)   = (QA(1) / (ROR(L)*TX5*GMS(L)))              &
     &                                            ** (1.0/1.1364)
!                 endif
                  BUY(L)   = - ROR(L) * TX5 * QRP(L)
                  WCB(L-1) = 0.0
                ENDIF
!
                DEL_ETA = ETD(L) - ETD(L-1)
                IF(DEL_ETA .LT. 0.0 .AND. ERRQ .GT. 0.1) THEN
                  ROR(L)   = BUD(KD)
                  ETD(L)   = 0.0
                  WVL(L)   = 0.0
!!!!!             TX5      = TX1 + TX9
                  CLDFRD(L-1) = TX5
!
                  DEL_ETA  = - ETD(L-1)
                  EDZ      = 0.0
                  DDZ      = -DEL_ETA
                  WCB(L-1) = DDZ

!
                  HOD(L)   = HOD(L-1)
                  QOD(L)   = QOD(L-1)

!
                  TEM4     = QOD(L)
                  TEM1     = VRW(1)
!
                  QHS      = QA(3) + 0.5 * (GAF(L-1)+GAF(L))            &
     &                                   * (HOD(L)-QA(2))

!
!                                           First iteration       !
!
                  ST2  = PRL(L) * (QHS + TEM1 * (QHS-QOD(L)))
                  TEM2 = ROR(L) * QRP(L-1)
                  CALL QRABF(TEM2,QRAF,QRBF)
                  TEM6 = TX5 * (1.6 + 124.9 * QRAF) * QRBF * TX4
!
                  CE   = TEM6*ST2/((5.4E5*ST2 + 2.55E6)*(ETD(L)+DDZ))
!

                  TEM2   = - ((1.0+TEM1)*(QHS+CE) + TEM1*QOD(L))
                  TEM3   = (1.0 + TEM1) * QHS * (QOD(L)+CE)
                  TEM    = MAX(TEM2*TEM2 -FOUR*TEM1*TEM3,ZERO)
                  QOD(L) = MAX(TEM4, (- TEM2 - SQRT(TEM)) * VRW(2))
!
!                                            second iteration   !
!
                  ST2  = PRL(L) * (QHS + TEM1 * (QHS-QOD(L)))
                  CE   = TEM6*ST2/((5.4E5*ST2 + 2.55E6)*(ETD(L)+DDZ))
!                 CEE  = CE * (ETD(L)+DDZ)
!


                  TEM2   = - ((1.0+TEM1)*(QHS+CE) + TEM1*tem4)
                  TEM3   = (1.0 + TEM1) * QHS * (tem4+CE)
                  TEM    = MAX(TEM2*TEM2 -FOUR*TEM1*TEM3,ZERO)
                  QOD(L) = MAX(TEM4, (- TEM2 - SQRT(TEM)) * VRW(2))

!                                              Evaporation in Layer L-1
!
                  EVP(L-1) = (QOD(L)-TEM4) * (ETD(L)+DDZ)

!                                               Calculate Pd (L+1/2)
!                 RNN(L-1) = TX1*RNT + RNF(L-1) - EVP(L-1)
                  QA(1)    = TX1*RNT + RNF(L-1)
                  EVP(L-1) = min(EVP(L-1), QA(1))
                  QA(1)    = QA(1) - EVP(L-1)
                  qrp(l)   = 0.0
!
!     if (tx5 .eq. 0.0 .or. gms(l) .eq. 0.0)
!     if (lprnt)
!    *  print *,' Btx5=',tx5,' gms=',gms(l),' ror=',ror(l)
!    *,' L=',L,' QA=',QA(1),' tx1=',tx1,' tx9=',tx9
!    *,' kbl=',kbl,' etd1=',etd(l-1),' DEL_ETA=',DEL_ETA
!    &,' evp=',evp(l-1)
!
!                 IF (QA(1) .GT. 0.0) THEN
!!                  RNS(L-1) = QA(1)
!!!                 tx5      = tx9
!                   QRP(L) = (QA(1) / (ROR(L)*TX5*GMS(L)))              &
!    &                                         ** (1.0/1.1364)
!                 endif
!                 ERRQ   = 0.0
!                                              Compute Buoyancy
!                 TEM1   = WA(3)+(HOD(L)-WA(1)-ALHL*(QOD(L)-WA(2)))     &
!    &                                                  * (1.0/CP)
!                 TEM1   = TEM1 * (1.0 + NU*QOD(L)) * DOFW

!                 BUY(L) = (TEM1 - 1.0 - QRP(L)) * ROR(L) * TX5
!
!                 IF (QA(1) .GT. 0.0) RNS(L) = QA(1)
                  IF (L .LE. K) THEN
                     RNS(L) = QA(1)
                     QA(1)  = 0.0
                  ENDIF
                  tx5      = tx9
                  ERRQ     = 0.0
                  QRP(L)   = 0.0
                  BUY(L)   = 0.0
                                                                              
!
                ENDIF
              ENDIF
            ENDIF
!

          ENDDO                ! End of the iteration loop  for a given L!
!       if (kd .eq. 13 .and. .not. ddft) stop
          IF (L .LE. K) THEN
            IF (ETD(L-1) .EQ. 0.0                                       &
     &         .AND. ERRQ .GT. 0.1 .and. l .le. kbl) THEN
!!!  &         .AND. ERRQ .GT. ERRMIN*10.0 .and. l .le. kbl) THEN
!    &         .AND. ERRQ .GT. ERRMIN*10.0) THEN
               ROR(L)   = BUD(KD)
               HOD(L)   = WA(1)
               QOD(L)   = WA(2)
               TX5      =       TX9     ! Does not make too much difference!
!              TX5      = TX1 + TX9
               EVP(L-1) = 0.0
!              EVP(L-1) = CEE * (1.0 - qod(l)/qa(3))
               QA(1)    = TX1*RNT + RNF(L-1)
               EVP(L-1) = min(EVP(L-1), QA(1))
               QA(1)    = QA(1) - EVP(L-1)
!              QRP(L)   = 0.0
      if (tx5 .eq. 0.0 .or. gms(l) .eq. 0.0) then
        print *,' Ctx5=',tx5,' gms=',gms(l),' ror=',ror(l)              &
     &,' L=',L,' QA=',QA(1),' tx1=',tx1,' tx9=',tx9                     &
     &,' kbl=',kbl,' etd1=',etd(l-1),' DEL_ETA=',DEL_ETA
      endif
!              IF (QA(1) .GT. 0.0) THEN
                 QRP(L) = (QA(1) / (ROR(L)*TX5*GMS(L)))                 &
     &                                         ** (1.0/1.1364)
!              ENDIF
               ETD(L)   = 0.0
               WVL(L)   = 0.0
               ST1      = 1.0 - ALFIND(L)

               ERRQ     = 0.0
               BUY(L)   = - ROR(L) * TX5 * QRP(L)
               WCB(L-1) = 0.0
            ENDIF
          ENDIF

!
!
          LL = MIN(IDN(idnm), K+1)
          IF (ERRQ .LT. 1.0 .AND. L .LE. LL) THEN
            IF (ETD(L-1) .GT. 0.0 .AND. ETD(L) .EQ. 0.0) THEN
             IDN(idnm) = L
             wvl(l)    = 0.0
             if (L .lt. KBL .or. tx5 .gt. 0.0) idnm  = idnm + 1
             errq      = 0.0
            ENDIF
            if (etd(l) .eq. 0.0 .and. l .gt. kbl) then
              idn(idnm) = l
              if (tx5 .gt. 0.0) idnm  = idnm + 1
            endif
          ENDIF

!       if (lprnt) then
!       print *,' ERRQ=',ERRQ,' IDN=',IDN(idnm),' idnm=',idnm
!       print *,' L=',L,' QRP=',QRP(L),' ETD=',ETD(L),' QA=',QA(1)
!    *,' evp=',evp(l-1),' rnf=',rnf(l-1)
!       endif

! 
!     If downdraft properties are not obtainable, (i.e.solution does
!      not converge) , no downdraft is assumed
!
!          IF (ERRQ .GT. ERRMIN*100.0 .AND. IDN(idnm) .EQ. 99)          &
           IF (ERRQ .GT. 0.1 .AND. IDN(idnm) .EQ. 99)                   &
     &                          DDFT = .FALSE.
!
!
        DOF = 0.0
        IF (.NOT. DDFT) RETURN
!
!     if (ddlgk .or. l .le. idn(idnm)) then
!     rsum2 = rsum2 + evp(l-1)
!     print *,' rsum1=',rsum1,' rsum2=',rsum2,' L=',L,' qa=',qa(1)
!    *,' evp=',evp(l-1)
!     else
!     rsum1 = rsum1 + rnf(l-1)
!     print *,' rsum1=',rsum1,' rsum2=',rsum2,' L=',L,' rnf=',rnf(l-1)
!     endif

      ENDDO                      ! End of the L Loop of downdraft !

        TX1 = 0.0

        DOF = QA(1)
!
!     print *,' dof=',dof,' rntp=',rntp,' rnb=',rnb
!     print *,' total=',(rsum1+dof+rntp+rnb)

      ENDIF                       ! SKPDD endif
!

       RNN(KD) = RNTP
       TX1     = EVP(KD)
       TX2     = RNTP + RNB + DOF

!     if (lprnt) print *,' tx2=',tx2
       II = IDH
       IF (II .GE. KD1+1) THEN
          RNN(KD)   = RNN(KD) + RNF(KD)
          TX2       = TX2 + RNF(KD)
          RNN(II-1) = 0.0
          TX1       = EVP(II-1)
        ENDIF
!     if (lprnt) print *,' tx2=',tx2,' idnm=',idnm,' idn=',idn(idnm)
        DO L=KD,K
          II = IDH

          IF (L .GT. KD1 .AND. L .LT. II) THEN
            RNN(L-1) = RNF(L-1)
            TX2      = TX2 + RNN(L-1)

          ELSEIF (L .GE. II .AND. L .LT. IDN(idnm)) THEN
!!!       ELSEIF (L .GE. II .AND. L .LE. IDN(idnm)) THEN

!           do jj=2,idnm
!             if (l .ge. idn(jj-1) .and. l .lt. idn(jj)) then
!!!             RNN(L)   = 0.0
!!!             TX1      = TX1 + EVP(L)
!             endif
!           enddo
!
            rnn(l) = rns(l)
            tx2    = tx2 + rnn(l)
            TX1    = TX1 + EVP(L)

          ELSEIF (L .GE. IDN(idnm)) THEN
            ETD(L+1) = 0.0
            HOD(L+1) = 0.0
            QOD(L+1) = 0.0
            EVP(L)   = 0.0
            RNN(L)   = RNF(L) + RNS(L)
            TX2      = TX2    + RNN(L)
          ENDIF
!     if (lprnt) print *,' tx2=',tx2,' L=',L,' rnn=',rnn(l)
        ENDDO
!       IF (K+1 .GT. IDN(idnm)) THEN
!         ETD(K+1) = 0.0
!         HOD(K+1) = 0.0
!         QOD(K+1) = 0.0
!         EVP(K)   = 0.0
!         RNN(K)   = RNF(K)
!         TX2      = TX2 + RNN(K)
!       ENDIF
!
!      For Downdraft case the rain is that falls thru the bottom

        L = KBL

        RNN(L)    = RNN(L) + RNB
        CLDFRD(L) = TX5

!
!     Caution !! Below is an adjustment to rain flux to maintain
!                conservation of precip!

!
!     if (lprnt) print *,' train=',train,' tx2=',tx2,' tx1=',tx1

        IF (TX1 .GT. 0.0) THEN
          TX1 = (TRAIN - TX2) / TX1
        ELSE
          TX1 = 0.0
        ENDIF

!       TX5      = EVP(KBL)

!!      EVP(KBL) = EVP(KBL) * TX1

!       TX3      = RNN(KBL) + EVP(KBL) + DOF
!       TX2      = RNN(KBL)
!       TX4      = EVP(KBL)

!       DO L=KD,KB1
        DO L=KD,K

!         TX5    = TX5 + EVP(L)
          EVP(L) = EVP(L) * TX1
!         TX3    = TX3 + EVP(L) + RNN(L)
!         TX2    = TX2 + RNN(L)
!         TX4    = TX4 + EVP(L)
        ENDDO
!
!     if (lprnt .and. kd .eq. 52) stop
!***********************************************************************
!***********************************************************************

      RETURN
      END

      SUBROUTINE QSATCN(TT,P,Q,DQDT,lprnt)

      USE MACHINE , ONLY : kind_phys
      USE FUNCPHYS , ONLY : fpvs
      USE PHYSCONS, RV => con_RV, CVAP => con_CVAP, CLIQ => con_CLIQ    &
     &,             CSOL => con_CSOL, TTP => con_TTP, HVAP => con_HVAP  &
     &,             HFUS => con_HFUS, EPS => con_eps, EPSM1 => con_epsm1
      implicit none
!     include 'constant.h'
!
      real(kind=kind_phys) TT, P, Q, DQDT
!
      real(kind=kind_phys) rvi, facw, faci, hsub, tmix, DEN
      real(kind=kind_phys) ZERO,ONE,ONE_M10
      PARAMETER (RVI=1.0/RV)
      PARAMETER (FACW=CVAP-CLIQ, FACI=CVAP-CSOL)
      PARAMETER (HSUB=HVAP+HFUS, tmix=TTP-20.0, DEN=1.0/(TTP-TMIX))
      PARAMETER (ZERO=0.,ONE=1.,ONE_M10=1.E-10)
      logical lprnt
!
!CFPP$ NOCONCUR R
      real(kind=kind_phys) es, d, hlorv, W
!
!     es    = 10.0 * fpvs(tt)                ! fpvs is in centibars!
      es    = 0.01 * fpvs(tt)                ! fpvs is in Pascals!
      D     = 1.0 / max(p+epsm1*es,ONE_M10)
!
      q     = MIN(eps*es*D, ONE)
!
      W     = max(ZERO, min(ONE, (TT - TMIX)*DEN))
      hlorv = ( W      * (HVAP + FACW * (tt-ttp))                       &
     &       + (1.0-W) * (HSUB + FACI * (tt-ttp)) ) * RVI
      dqdt  = p * q * hlorv *  D / (tt*tt)
!
      return
      end

      SUBROUTINE ANGRAD( PRES, ALM,  AL2, TLA, PRB, WFN, UFN)
!     SUBROUTINE ANGRAD( PRES, ALM, STLA, CTL2, AL2                     &
!    &,                  PI, TLA, PRB, WFN, UFN)
      USE MACHINE , ONLY : kind_phys
      use module_ras , only : refp, refr, tlac, plac, tlbpl, drdp, almax
      implicit none

!     real(kind=kind_phys) PRES, STLA, CTL2, pi,  pifac                 &
      real(kind=kind_phys) PRES                                         &
     &,                    ALM,  AL2,  TLA,  TEM, TEM1                  &
     &,                    PRB,  ACR,  WFN,  UFN
!
      integer i
!
!     pifac = pi / 180.0
!     print *,' pres=',pres
      IF (TLA .LT. 0.0) THEN
          IF (PRES .LE. PLAC(1)) THEN
            TLA = TLAC(1)
          ELSEIF (PRES .LE. PLAC(2)) THEN
            TLA = TLAC(2) + (PRES-PLAC(2))*tlbpl(1)
          ELSEIF (PRES .LE. PLAC(3)) THEN
            TLA = TLAC(3) + (PRES-PLAC(3))*tlbpl(2)
          ELSEIF (PRES .LE. PLAC(4)) THEN
            TLA = TLAC(4) + (PRES-PLAC(4))*tlbpl(3)
          ELSEIF (PRES .LE. PLAC(5)) THEN
            TLA = TLAC(5) + (PRES-PLAC(5))*tlbpl(4)
          ELSEIF (PRES .LE. PLAC(6)) THEN
            TLA = TLAC(6) + (PRES-PLAC(6))*tlbpl(5)
          ELSEIF (PRES .LE. PLAC(7)) THEN
            TLA = TLAC(7) + (PRES-PLAC(7))*tlbpl(6)
          ELSEIF (PRES .LE. PLAC(8)) THEN
            TLA = TLAC(8) + (PRES-PLAC(8))*tlbpl(7)
          ELSE
            TLA = TLAC(8)
          ENDIF
!         tla = tla * 1.5

!         STLA = SIN(TLA*PIFAC)
!         TEM1 = COS(TLA*PIFAC)
!         CTL2 = TEM1 * TEM1

      ELSE
!         STLA = SIN(TLA*PIFAC)
!         TEM1 = COS(TLA*PIFAC)
!         CTL2 = TEM1 * TEM1

      ENDIF
        IF (PRES .GE. REFP(1)) THEN
          TEM = REFR(1)
        ELSEIF (PRES .GE. REFP(2)) THEN
          TEM = REFR(1) + (PRES-REFP(1)) * drdp(1)
        ELSEIF (PRES .GE. REFP(3)) THEN
          TEM = REFR(2) + (PRES-REFP(2)) * drdp(2)
        ELSEIF (PRES .GE. REFP(4)) THEN
          TEM = REFR(3) + (PRES-REFP(3)) * drdp(3)
        ELSEIF (PRES .GE. REFP(5)) THEN
          TEM = REFR(4) + (PRES-REFP(4)) * drdp(4)
        ELSEIF (PRES .GE. REFP(6)) THEN
          TEM = REFR(5) + (PRES-REFP(5)) * drdp(5)
        ELSE
          TEM = REFR(6)
        ENDIF
!!      AL2 = min(ALMAX, MAX(ALM, 2.0E-4/TEM))
!       AL2 = min(2.0E-3, MAX(ALM, 2.0E-4/TEM))
!
        tem = 2.0E-4 / tem
        al2 = min(4.0*tem, max(alm, tem))
!
      RETURN
      END
      SUBROUTINE SETQRP
      USE MACHINE , ONLY : kind_phys
      use module_ras , only : NQRP,C1XQRP,C2XQRP,TBQRP,TBQRA,TBQRB
      implicit none

      real(kind=kind_phys) tem2,tem1,x,xinc,xmax,xmin
      integer jx
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!CFPP$ NOCONCUR R
!     XMIN=1.0E-6
      XMIN=0.0
      XMAX=5.0
      XINC=(XMAX-XMIN)/(NQRP-1)
      C1XQRP=1.-XMIN/XINC
      C2XQRP=1./XINC
      TEM1 = 0.001 ** 0.2046
      TEM2 = 0.001 ** 0.525
      DO JX=1,NQRP
        X         = XMIN + (JX-1)*XINC
        TBQRP(JX) =        X ** 0.1364
        TBQRA(JX) = TEM1 * X ** 0.2046
        TBQRB(JX) = TEM2 * X ** 0.525
      ENDDO    
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
      FUNCTION QRPF(QRP)
!
      USE MACHINE , ONLY : kind_phys
      use module_ras , only : NQRP,C1XQRP,C2XQRP,TBQRP,TBQRA,TBQRB
      implicit none

      real(kind=kind_phys) QRP, QRPF, XJ, REAL_NQRP, ONE
      PARAMETER (ONE=1.)
      INTEGER JX
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      REAL_NQRP=REAL(NQRP)
      XJ   = MIN(MAX(C1XQRP+C2XQRP*QRP,ONE),REAL_NQRP)
!     XJ   = MIN(MAX(C1XQRP+C2XQRP*QRP,ONE),FLOAT(NQRP))
      JX   = MIN(XJ,NQRP-ONE)
      QRPF = TBQRP(JX)  + (XJ-JX) * (TBQRP(JX+1)-TBQRP(JX))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
      SUBROUTINE QRABF(QRP,QRAF,QRBF)
      USE MACHINE , ONLY : kind_phys
      use module_ras , only : NQRP,C1XQRP,C2XQRP,TBQRP,TBQRA,TBQRB
      implicit none
!
      real(kind=kind_phys) QRP, QRAF, QRBF, XJ, REAL_NQRP, ONE
      PARAMETER (ONE=1.)
      INTEGER JX
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      REAL_NQRP=REAL(NQRP)
      XJ   = MIN(MAX(C1XQRP+C2XQRP*QRP,ONE),REAL_NQRP)
      JX   = MIN(XJ,NQRP-ONE)
      XJ   = XJ - JX
      QRAF = TBQRA(JX)  + XJ * (TBQRA(JX+1)-TBQRA(JX))
      QRBF = TBQRB(JX)  + XJ * (TBQRB(JX+1)-TBQRB(JX))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
      SUBROUTINE SETVTP
      USE MACHINE , ONLY : kind_phys
      use module_ras , only : NVTP,C1XVTP,C2XVTP,TBVTP
      implicit none

      real(kind=kind_phys) vtpexp,xinc,x,xmax,xmin
      integer jx
      PARAMETER(VTPEXP=-0.3636)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!CFPP$ NOCONCUR R
      XMIN=0.05
      XMAX=1.5
      XINC=(XMAX-XMIN)/(NVTP-1)
      C1XVTP=1.-XMIN/XINC
      C2XVTP=1./XINC
      DO JX=1,NVTP
        X         = XMIN + (JX-1)*XINC
        TBVTP(JX) =        X ** VTPEXP
      ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
      FUNCTION VTPF(ROR)
!
      USE MACHINE , ONLY : kind_phys
      use module_ras , only : NVTP,C1XVTP,C2XVTP,TBVTP
      implicit none
      real(kind=kind_phys) ROR, VTPF, XJ, REAL_NVTP, ONE
      PARAMETER (ONE=1.)
      INTEGER JX
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      REAL_NVTP=REAL(NVTP)
      XJ   = MIN(MAX(C1XVTP+C2XVTP*ROR,ONE),REAL_NVTP)
      JX   = MIN(XJ,NVTP-ONE)
      VTPF = TBVTP(JX)  + (XJ-JX) * (TBVTP(JX+1)-TBVTP(JX))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
      FUNCTION CLF(PRATE)
!
      USE MACHINE , ONLY : kind_phys
      implicit none
      real(kind=kind_phys) PRATE, CLF
!
      real (kind=kind_phys), parameter :: ccf1=0.30, ccf2=0.09          &
     &,                                   ccf3=0.04, ccf4=0.01          &
     &,                                   pr1=1.0,   pr2=5.0            &
     &,                                   pr3=20.0
!
      if (prate .lt. pr1) then
        clf = ccf1
      elseif (prate .lt. pr2) then
        clf = ccf2
      elseif (prate .lt. pr3) then
        clf = ccf3
      else
        clf = ccf4
      endif
!
      RETURN
      END
