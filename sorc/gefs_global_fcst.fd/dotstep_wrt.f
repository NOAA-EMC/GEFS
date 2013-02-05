      SUBROUTINE do_tstep_wrt(deltim,kdt,PHOUR,
CJFE &                 LAM,DLAM,LAMEXT,LATLOCAL,PHI,DPHI,DPHIBR,PHIBS,
CJFE &                 LBASIY,LATSINPE,LAMMP,PHIMP,SIGMP,
     &                 TRIE_LS,TRIO_LS,
     &                 LS_NODE,LS_NODES,MAX_LS_NODES,
     &                 LATS_NODES_A,GLOBAL_LATS_A,
     &                 LONSPERLAT,
     &                 LATS_NODES_R,GLOBAL_LATS_R,
     &                 LONSPERLAR,
     &                 LATS_NODES_EXT,GLOBAL_LATS_EXT,
     &                 EPSE,EPSO,EPSEDN,EPSODN,
     &                 SNNP1EV,SNNP1OD,NDEXEV,NDEXOD,
     &                 PLNEV_A,PLNOD_A,PDDEV_A,PDDOD_A,
     &                 PLNEW_A,PLNOW_A,
     &                 PLNEV_R,PLNOD_R,PDDEV_R,PDDOD_R,
     &                 PLNEW_R,PLNOW_R,
     &                 SYN_LS_A,DYN_LS_A,
     &                 SYN_GR_A_1,DYN_GR_A_1,ANL_GR_A_1,
     &                 SYN_GR_A_2,DYN_GR_A_2,ANL_GR_A_2,
     &                 LSLAG,
     &                 XLON,XLAT,COSZDG,COSZEN,
     &                 SLMSK,SHELEG,ZORL ,TSEA,
     &     HICE ,FICE  , TISFC,                                ! FOR SEA-ICE - XW Nov04
     &     ALVSF,ALNSF ,ALVWF ,ALNWF,FACSF ,FACWF,CV    ,CVT ,
     &     CVB  ,HPRIME,STC,SWH,HLW,ORO,SFCNSW,SFCDLW,
     &     TSFLW,FLUXR ,CLDCOV,TMPMAX,TMPMIN,BENGSH,PSMEAN,
     &     DVGWD,U10M,PSURF,V10M,RUNOFF,ULWSFC,DUGWD,CLDWRK,
     &     EP,GFLUX,DTSFC,DUSFC,DVSFC,DQSFC,DLWSFC,GESHEM,
     &     STYPE,UUSTAR,F10M,VTYPE,FFMM,FFHH,SMC,CANOPY,TG3,VFRAC,
Clu [+2L]: add (tprcp,srflag),(slc,snwdph,slope,shdmin,shdmax,snoalb)
     +     TPRCP,SRFLAG,
     +     SLC,SNWDPH,SLOPE,SHDMIN,SHDMAX,SNOALB,SFCDSW,SFALB, 
Cwei added 10/24/2006
     +     CHH,CMM,EPI,DLWSFCI,ULWSFCI,USWSFCI,DSWSFCI,DTSFCI,
     +     DQSFCI,GFLUXI,SRUNOFF,T1,Q1,U1,V1,ZLVL,EVBSA,EVCWA,
     +     TRANSA,SBSNOA,SNOWCA,SOILM,
     &     T2M,HPBL,Q2M,SLAG,SDEC,CDEC,
     &     OZPLIN,JINDX1,JINDX2,DDY,PDRYINI,
     &     DT3DT, DQ3DT, DU3DT, DV3DT,
     &     phy_f3d,  phy_f2d, NBLCK,
!    &     TPS,  QPS, PSP, TPS1, QPS1, PSP1,NBLCK,
     &     ZHOUR,N1,N4,LSOUT,COLAT1,CFHOUR1,
     &     SPS)
!    &     ZHOUR,N1,N4,LSOUT,COLAT1,CFHOUR1)
!DHOU 05/29/2008, added SPS as argument
cc
#include "f_hpm.h"
      use resol_def
      use layout1
      use gg_def
      use vert_def
!     use sig_io
      use date_def
      use namelist_def
      use mpi_def
      use ozne_def
      IMPLICIT NONE
!!     
      CHARACTER(16)                     :: CFHOUR1
      INTEGER,INTENT(IN):: LONSPERLAT(LATG),N1,N4
CJFE  INTEGER,INTENT(IN):: LATSINPE(LATS_NODE_A)
CJFE  INTEGER,INTENT(IN):: LATLOCAL(LATGD,0:NODES-1)
!!     
      REAL(KIND=KIND_EVOD),INTENT(IN):: deltim,PHOUR
      REAL(KIND=KIND_EVOD),INTENT(INOUT):: ZHOUR
CJFE  REAL(KIND=KIND_EVOD),INTENT(IN):: DPHIBR,PHIBS
CJFE  REAL(KIND=KIND_EVOD),INTENT(IN):: LBASIY(4,2,LATS_NODE_EXT)
CJFE  REAL(KIND=KIND_EVOD),INTENT(IN):: PHI(LATS_NODE_EXT)
CJFE  REAL(KIND=KIND_EVOD),INTENT(IN):: DPHI(LATS_NODE_EXT)
CJFE  REAL(KIND=KIND_EVOD),INTENT(IN):: DLAM(LATS_NODE_EXT)
CJFE  REAL(KIND=KIND_EVOD),INTENT(IN):: LAM(LONFX,LATS_NODE_A+1)
CJFE  REAL(KIND=KIND_EVOD),INTENT(IN):: LAMEXT(LONFX,LATS_NODE_EXT)
CJFE  REAL(KIND=KIND_EVOD),INTENT(IN):: LAMMP(LONF,LEVS,LATS_NODE_A)
CJFE  REAL(KIND=KIND_EVOD),INTENT(IN):: PHIMP(LONF,LEVS,LATS_NODE_A)
CJFE  REAL(KIND=KIND_EVOD),INTENT(IN):: SIGMP(LONF,LEVS,LATS_NODE_A)
!!     
      INTEGER NBLCK
      REAL(KIND=KIND_EVOD) TRIE_LS(LEN_TRIE_LS,2,11*LEVS+3*LEVH+6)
      REAL(KIND=KIND_EVOD) TRIO_LS(LEN_TRIO_LS,2,11*LEVS+3*LEVH+6)
cc
      integer              ls_node(ls_dim,3)
cc
!     ls_node(1,1) ... ls_node(ls_max_node,1) : values of L
!     ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!     ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
cc
      INTEGER              LS_NODES(LS_DIM,NODES)
      INTEGER          MAX_LS_NODES(NODES)
      INTEGER               LATS_NODES_A(NODES)
      INTEGER               LATS_NODES_EXT(NODES)
      INTEGER              GLOBAL_LATS_A(LATG)
      INTEGER        GLOBAL_LATS_EXT(LATG+2*JINTMX+2*NYPT*(NODES-1))
      INTEGER               LATS_NODES_R(NODES)
      INTEGER              GLOBAL_LATS_R(LATR)
      INTEGER                 LONSPERLAR(LATR)
c
      integer               lats_nodes_r_old(nodes)
      integer              global_lats_r_old(latr)
      logical ifshuff
!
      real(kind=kind_evod) colat1
      REAL(KIND=KIND_EVOD)    EPSE(LEN_TRIE_LS)
      REAL(KIND=KIND_EVOD)    EPSO(LEN_TRIO_LS)
      REAL(KIND=KIND_EVOD)  EPSEDN(LEN_TRIE_LS)
      REAL(KIND=KIND_EVOD)  EPSODN(LEN_TRIO_LS)
      REAL(KIND=KIND_EVOD) SNNP1EV(LEN_TRIE_LS)
      REAL(KIND=KIND_EVOD) SNNP1OD(LEN_TRIO_LS)
      INTEGER               NDEXEV(LEN_TRIE_LS)
      INTEGER               NDEXOD(LEN_TRIO_LS)
      REAL(KIND=KIND_EVOD)   PLNEV_A(LEN_TRIE_LS,LATG2)
      REAL(KIND=KIND_EVOD)   PLNOD_A(LEN_TRIO_LS,LATG2)
      REAL(KIND=KIND_EVOD)   PDDEV_A(LEN_TRIE_LS,LATG2)
      REAL(KIND=KIND_EVOD)   PDDOD_A(LEN_TRIO_LS,LATG2)
      REAL(KIND=KIND_EVOD)   PLNEW_A(LEN_TRIE_LS,LATG2)
      REAL(KIND=KIND_EVOD)   PLNOW_A(LEN_TRIO_LS,LATG2)
      REAL(KIND=KIND_EVOD)   PLNEV_R(LEN_TRIE_LS,LATR2)
      REAL(KIND=KIND_EVOD)   PLNOD_R(LEN_TRIO_LS,LATR2)
      REAL(KIND=KIND_EVOD)   PDDEV_R(LEN_TRIE_LS,LATR2)
      REAL(KIND=KIND_EVOD)   PDDOD_R(LEN_TRIO_LS,LATR2)
      REAL(KIND=KIND_EVOD)   PLNEW_R(LEN_TRIE_LS,LATR2)
      REAL(KIND=KIND_EVOD)   PLNOW_R(LEN_TRIO_LS,LATR2)
c$$$      INTEGER                LOTS,LOTD,LOTA
c$$$      PARAMETER            ( LOTS = 5*LEVS+1*LEVH+3 )
c$$$      PARAMETER            ( LOTD = 6*LEVS+2*LEVH+0 )
c$$$      PARAMETER            ( LOTA = 3*LEVS+1*LEVH+1 )
      REAL(KIND=KIND_EVOD) SYN_LS_A(4*LS_DIM,LOTS,LATG2)
      REAL(KIND=KIND_EVOD) DYN_LS_A(4*LS_DIM,LOTD,LATG2)
      REAL(KIND=KIND_EVOD) SYN_GR_A_1(LONFX*LOTS,LATS_DIM_EXT)
      REAL(KIND=KIND_EVOD) DYN_GR_A_1(LONFX*LOTD,LATS_DIM_EXT)
      REAL(KIND=KIND_EVOD) ANL_GR_A_1(LONFX*LOTA,LATS_DIM_EXT)
      REAL(KIND=KIND_EVOD) SYN_GR_A_2(LONFX*LOTS,LATS_DIM_EXT)
      REAL(KIND=KIND_EVOD) DYN_GR_A_2(LONFX*LOTD,LATS_DIM_EXT)
      REAL(KIND=KIND_EVOD) ANL_GR_A_2(LONFX*LOTA,LATS_DIM_EXT)
!!     
      REAL (KIND=KIND_RAD) ORO(LONR,LATS_NODE_R)
      REAL (KIND=KIND_RAD) XLON(LONR,LATS_NODE_R)
      REAL (KIND=KIND_RAD) XLAT(LONR,LATS_NODE_R)
      REAL (KIND=KIND_RAD) COSZDG(LONR,LATS_NODE_R)
      REAL (KIND=KIND_RAD) COSZEN(LONR,LATS_NODE_R)
      REAL (KIND=KIND_RAD) SLMSK (LONR,LATS_NODE_R),
     &     SHELEG(LONR,LATS_NODE_R),
     &     ZORL  (LONR,LATS_NODE_R),TSEA  (LONR,LATS_NODE_R),
     &     ALVSF (LONR,LATS_NODE_R),ALNSF (LONR,LATS_NODE_R),
     &     ALVWF (LONR,LATS_NODE_R),PWAT(lonr,lats_node_r),
     &     ALNWF (LONR,LATS_NODE_R),FACSF (LONR,LATS_NODE_R),
     &     FACWF (LONR,LATS_NODE_R),
     &     CV    (LONR,LATS_NODE_R),CVT   (LONR,LATS_NODE_R),
     &     CVB   (LONR,LATS_NODE_R),
     &     HPRIME(NMTVR,LONR,LATS_NODE_R),
     &     STC(LSOIL,LONR,LATS_NODE_R),FLUXR(nfxr,LONR,LATS_NODE_R),
     &     SFCNSW(LONR,LATS_NODE_R),SFCDLW(LONR,LATS_NODE_R),
     &     TSFLW (LONR,LATS_NODE_R),CLDCOV(LEVS,LONR,LATS_NODE_R),
     &     TMPMAX (LONR,LATS_NODE_R),TMPMIN(LONR,LATS_NODE_R),
     &     BENGSH (LONR,LATS_NODE_R),PSMEAN(LONR,LATS_NODE_R),
     &     DVGWD (LONR,LATS_NODE_R),U10M(LONR,LATS_NODE_R),
     &     PSURF (LONR,LATS_NODE_R),V10M(LONR,LATS_NODE_R),
     &     RUNOFF (LONR,LATS_NODE_R),ULWSFC(LONR,LATS_NODE_R),
     &     DUGWD (LONR,LATS_NODE_R),CLDWRK(LONR,LATS_NODE_R),
     &     EP (LONR,LATS_NODE_R),GFLUX(LONR,LATS_NODE_R),
     &     DTSFC (LONR,LATS_NODE_R),DUSFC(LONR,LATS_NODE_R),
     &     DVSFC (LONR,LATS_NODE_R),DQSFC(LONR,LATS_NODE_R),
     &     DLWSFC (LONR,LATS_NODE_R),GESHEM(LONR,LATS_NODE_R),
     &     T2M (LONR,LATS_NODE_R),HPBL(LONR,LATS_NODE_R),
     &     Q2M (LONR,LATS_NODE_R),STYPE(LONR,LATS_NODE_R),
     &     UUSTAR (LONR,LATS_NODE_R),F10M(LONR,LATS_NODE_R),
     &     VTYPE (LONR,LATS_NODE_R),FFMM(LONR,LATS_NODE_R),
     &     FFHH (LONR,LATS_NODE_R),CANOPY(LONR,LATS_NODE_R),
     &     TG3 (LONR,LATS_NODE_R),VFRAC(LONR,LATS_NODE_R),
     &     SMC (LSOIL,LONR,LATS_NODE_R)
Clu [+5L]: add (tprcp,srflag),(slc,snwdph,slope,snoalb,shdmin,shdmax)
     +,    TPRCP(LONR,LATS_NODE_R),SRFLAG(LONR,LATS_NODE_R)
     +,    SLC (LSOIL,LONR,LATS_NODE_R)
     +,    SNWDPH(LONR,LATS_NODE_R)
     +,    SLOPE (LONR,LATS_NODE_R), SNOALB(LONR,LATS_NODE_R)
     +,    SHDMIN(LONR,LATS_NODE_R), SHDMAX(LONR,LATS_NODE_R)
Cwei added 10/24/2006
     +,    CHH(LONR,LATS_NODE_R),CMM(LONR,LATS_NODE_R),
     +     EPI(LONR,LATS_NODE_R),DLWSFCI(LONR,LATS_NODE_R),
     +     ULWSFCI(LONR,LATS_NODE_R),USWSFCI(LONR,LATS_NODE_R),
     +     DSWSFCI(LONR,LATS_NODE_R),DTSFCI(LONR,LATS_NODE_R),
     +     DQSFCI(LONR,LATS_NODE_R),GFLUXI(LONR,LATS_NODE_R),
     +     SRUNOFF(LONR,LATS_NODE_R),
     +     T1(LONR,LATS_NODE_R),Q1(LONR,LATS_NODE_R),
     +     U1(LONR,LATS_NODE_R),V1(LONR,LATS_NODE_R),
     +     ZLVL(LONR,LATS_NODE_R),EVBSA(LONR,LATS_NODE_R),
     +     EVCWA(LONR,LATS_NODE_R),TRANSA(LONR,LATS_NODE_R),
     +     SBSNOA(LONR,LATS_NODE_R),SNOWCA(LONR,LATS_NODE_R),
     +     SOILM(LONR,LATS_NODE_R)
!c-- XW: FOR SEA-ICE Nov04
     &,    HICE(LONR,LATS_NODE_R),FICE(LONR,LATS_NODE_R)
     &,    SFCDSW(LONR,LATS_NODE_R), TISFC(LONR,LATS_NODE_R)
!c-- XW: END SEA-ICE
      REAL (KIND=KIND_RAD) SFALB(LONR,LATS_NODE_R)

      REAL (KIND=KIND_RAD)  SWH(NGPTC,LEVS,NBLCK,LATS_NODE_R)
      REAL (KIND=KIND_RAD)  HLW(NGPTC,LEVS,NBLCK,LATS_NODE_R)

      REAL (kind=kind_phys)
     &     phy_f3d(NGPTC,LEVS,NBLCK,lats_node_r,num_p3d),
     &     phy_f2d(lonr,lats_node_r,num_p2d),
!
!    &     TPS(NGPTC,LEVS,NBLCK,lats_node_r),
!    &     QPS(NGPTC,LEVS,NBLCK,lats_node_r),
!    &     TPS1(NGPTC,LEVS,NBLCK,lats_node_r),
!    &     QPS1(NGPTC,LEVS,NBLCK,lats_node_r),
!    &     PSP(lonr,lats_node_r),PSP1(lonr,lats_node_r),
!
     &     DDY(LATS_NODE_R),
     &     DT3DT(NGPTC,LEVS,6,NBLCK,lats_node_r),
     &     DQ3DT(NGPTC,LEVS,5+pl_coeff,NBLCK,lats_node_r),
     &     DU3DT(NGPTC,LEVS,3,NBLCK,lats_node_r),
     &     DV3DT(NGPTC,LEVS,3,NBLCK,lats_node_r)
      INTEGER JINDX1(LATS_NODE_R),JINDX2(LATS_NODE_R)
!!     
      INTEGER LEV,LEVMAX
      REAL OZPLIN(LATSOZP,LEVOZP,pl_coeff,timeoz) !OZONE PL Coeff
      REAL (KIND=KIND_PHYS) PDRYINI
      REAL(KIND=KIND_EVOD) SLAG,SDEC,CDEC
c$$$      INTEGER   P_GZ,P_ZEM,P_DIM,P_TEM,P_RM,P_QM
c$$$      INTEGER   P_ZE,P_DI,P_TE,P_RQ,P_Q,P_DLAM,P_DPHI,P_ULN,P_VLN
c$$$      INTEGER   P_W,P_X,P_Y,P_RT,P_ZQ
c$$$      PARAMETER(P_GZ  = 0*LEVS+0*LEVH+1,  !      GZE/O(LNTE/OD,2),
c$$$     X          P_ZEM = 0*LEVS+0*LEVH+2,  !     ZEME/O(LNTE/OD,2,LEVS),
c$$$     X          P_DIM = 1*LEVS+0*LEVH+2,  !     DIME/O(LNTE/OD,2,LEVS),
c$$$     X          P_TEM = 2*LEVS+0*LEVH+2,  !     TEME/O(LNTE/OD,2,LEVS),
c$$$     X          P_RM  = 3*LEVS+0*LEVH+2,  !      RME/O(LNTE/OD,2,LEVH),
c$$$     X          P_QM  = 3*LEVS+1*LEVH+2,  !      QME/O(LNTE/OD,2),
c$$$     X          P_ZE  = 3*LEVS+1*LEVH+3,  !      ZEE/O(LNTE/OD,2,LEVS),
c$$$     X          P_DI  = 4*LEVS+1*LEVH+3,  !      DIE/O(LNTE/OD,2,LEVS),
c$$$     X          P_TE  = 5*LEVS+1*LEVH+3,  !      TEE/O(LNTE/OD,2,LEVS),
c$$$     X          P_RQ  = 6*LEVS+1*LEVH+3,  !      RQE/O(LNTE/OD,2,LEVH),
c$$$     X          P_Q   = 6*LEVS+2*LEVH+3,  !       QE/O(LNTE/OD,2),
c$$$     X          P_DLAM= 6*LEVS+2*LEVH+4,  !  DPDLAME/O(LNTE/OD,2),
c$$$     X          P_DPHI= 6*LEVS+2*LEVH+5,  !  DPDPHIE/O(LNTE/OD,2),
c$$$     X          P_ULN = 6*LEVS+2*LEVH+6,  !     ULNE/O(LNTE/OD,2,LEVS),
c$$$     X          P_VLN = 7*LEVS+2*LEVH+6,  !     VLNE/O(LNTE/OD,2,LEVS),
c$$$     X          P_W   = 8*LEVS+2*LEVH+6,  !       WE/O(LNTE/OD,2,LEVS),
c$$$     X          P_X   = 9*LEVS+2*LEVH+6,  !       XE/O(LNTE/OD,2,LEVS),
c$$$     X          P_Y   =10*LEVS+2*LEVH+6,  !       YE/O(LNTE/OD,2,LEVS),
c$$$     X          P_RT  =11*LEVS+2*LEVH+6,  !      RTE/O(LNTE/OD,2,LEVH),
c$$$     X          P_ZQ  =11*LEVS+3*LEVH+6)  !      ZQE/O(LNTE/OD,2)
      INTEGER   kdt,       IERR,J,K,L,LOCL,N
      real(kind=kind_evod)  ye1(levs)
      REAL(KIND=kind_mpi)  coef00m(LEVS,ntrac) ! temp. ozone clwater  
      REAL(KIND=kind_evod) coef00(LEVS,ntrac) ! temp. ozone clwater  
      INTEGER              INDLSEV,JBASEV
      INTEGER              INDLSOD,JBASOD
      integer iprint
      include 'function2'
      LOGICAL LSLAG,LSOUT,ex_out
      LOGICAL SPS

c
c timings
      real(kind=kind_evod) global_times_a(latg,nodes)
      real(kind=kind_evod) global_times_b(latr,nodes)
      real(kind=kind_evod) global_times_r(latr,nodes)
      integer tag,ireq1,ireq2
      real*8 rtc ,timer1,timer2
!
!     real(kind=kind_evod) cons0p92,filta 
!
!
!     cons0p92 =   0.92d0
!     filta= cons0p92 
 
!sela if (me.eq.0) PRINT 102,KDT
  102 FORMAT(1H ,'KDT IN do_tstep  =',I6)
!     SHOUR=SHOUR+deltim
!
!DHOU 05-29-2008, This subroutine is adapted from do_tstep 
!      Only the output calls are retained with modified control (if)
!      This routine is called in at begining of GFS_Run only if SPS=.true.
!----------------------------------------------------------
      IF (lsout) THEN
CC
      CALL f_hpmstart(32,"TWRITEEO")
CC
      CALL countperf(0,18,0.)
c
      CALL WRTOUT(PHOUR,FHOUR,ZHOUR,IDATE,
     X              TRIE_LS,TRIO_LS,
     X              SL,SI,
     X              LS_NODES,MAX_LS_NODES,
     &    geshem,TSEA,SMC,SHELEG,STC,TG3,ZORL,CV,CVB,
     &    CVT,ALVSF,ALVWF,ALNSF,ALNWF,SLMSK,VFRAC,CANOPY,F10M,
     &    VTYPE,STYPE,FACSF,FACWF,UUSTAR,FFMM,FFHH,PWAT,
     &    HICE ,FICE , TISFC,                           ! FOR SEA-ICE - XW Nov04
Clu [+2L]: add (tprcp,srflag),(slc,snwdph,slope,shdmin,shdmax,snoalb)
     +    TPRCP,SRFLAG,
     +    SLC,SNWDPH,SLOPE,SHDMIN,SHDMAX,SNOALB, 
Cwei added 10/24/2006
     +     CHH,CMM,EPI,DLWSFCI,ULWSFCI,USWSFCI,DSWSFCI,DTSFCI,
     +     DQSFCI,GFLUXI,SRUNOFF,T1,Q1,U1,V1,ZLVL,EVBSA,EVCWA,
     +     TRANSA,SBSNOA,SNOWCA,SOILM,
     +    ORO,
     &   DT3DT, DQ3DT, CLDCOV, DU3DT, DV3DT,
     & tmpmax,tmpmin,runoff,t2m,q2m,psurf,dvgwd,hpbl,ep,
     & dugwd,v10m,dtsfc,dqsfc,gflux,dusfc,dvsfc,bengsh,
     & cldwrk,u10m,fluxr,dlwsfc,ulwsfc,pdryini,
C DHOU, 02-29-2008 Added file names as argument
C    & global_lats_r,lonsperlar,nblck,COLAT1,CFHOUR1,pl_coeff)
     & global_lats_r,lonsperlar,nblck,COLAT1,CFHOUR1,pl_coeff,
     &        'SIG.S','SFC.S','FLX.S')
!
      CALL f_hpmstop(32)
        ENDIF ! if ls_out
CC
      CALL countperf(1,18,0.)
CC
!!
        IF (mod(kdt,nsres).eq.0) THEN
!!
!     if(.not.adiab)then
          CALL wrt_restart(TRIE_LS,TRIO_LS,
     &        TSEA,SMC,SHELEG,STC,TG3,ZORL,CV,CVB,
     &        CVT,ALVSF,ALVWF,ALNSF,ALNWF,SLMSK,VFRAC,CANOPY,F10M,
     &        VTYPE,STYPE,FACSF,FACWF,UUSTAR,FFMM,FFHH,
     &        HICE ,FICE , TISFC,                       ! FOR SEA-ICE - XW Nov04
Clu [+2L]: add (tprcp,srflag),(slc,snwdph,slope,shdmin,shdmax,snoalb)
     +        TPRCP,SRFLAG,
     +        SLC,SNWDPH,SLOPE,SHDMIN,SHDMAX,SNOALB,
     +        ORO,T2M,Q2M,
     &        SI,SL,fhour,idate,
     &        igen,pdryini,
     x        ls_node,ls_nodes,max_ls_nodes,
     &        global_lats_r,lonsperlar,SNNP1EV,SNNP1OD,
C    &        phy_f3d, phy_f2d, ngptc, nblck, adiab, ens_nam)
C DHOU, 02-29-2008 Added file names as argument
     &        phy_f3d, phy_f2d, ngptc, nblck, adiab, ens_nam,
     &        'SIGS1','SIGS2','SFCS')

c
!       endif !not.adiab
        ENDIF
!
!DHOU added return, 02-21-2008
!DHOU remove lines after RETURN, 05-29-2008
        RETURN
      END
