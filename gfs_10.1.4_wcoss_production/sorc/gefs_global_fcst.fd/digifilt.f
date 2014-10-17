      SUBROUTINE tldfi(deltim,kdt,PHOUR,
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
     &                 XLON,XLAT,COSZDG,sfc_fld,flx_fld,nsst_fld,
     &                 HPRIME,SWH,HLW,FLUXR,
     &                 SFALB,SLAG,SDEC,CDEC,
     &                 OZPLIN,JINDX1,JINDX2,
     &                 DDY,PDRYINI,
     &                 phy_f3d,  phy_f2d, NBLCK,
     &                 ZHOUR,N1,N4,LSOUT,COLAT1,
     &                 CFHOUR1)
       
      use resol_def
      use layout1
      use gg_def
      use vert_def
!     use sig_io
      use date_def
      use namelist_def
      use ozne_def
      use Sfc_Flx_ESMFMod
      use Nsstm_ESMFMod
!
      IMPLICIT NONE
      include 'mpif.h'      

!!     
      TYPE(Sfc_Var_Data)        :: sfc_fld
      TYPE(Flx_Var_Data)        :: flx_fld
      TYPE(Nsst_Var_Data)       :: nsst_fld
!
      CHARACTER(16)                     :: CFHOUR1
      INTEGER,INTENT(IN):: LONSPERLAT(LATG),N1,N4
CJFE  INTEGER,INTENT(IN):: LATSINPE(LATS_NODE_A)
CJFE  INTEGER,INTENT(IN):: LATLOCAL(LATGD,0:NODES-1)
!!     
      REAL(KIND=KIND_EVOD),INTENT(INOUT):: deltim,PHOUR,ZHOUR
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
      INTEGER NBLCK,I
      REAL(KIND=KIND_EVOD) TRIE_LS(LEN_TRIE_LS,2,11*LEVS+3*LEVH+6)
      REAL(KIND=KIND_EVOD) TRIO_LS(LEN_TRIO_LS,2,11*LEVS+3*LEVH+6)
      INTEGER              LS_NODE (LS_DIM)
      INTEGER              LS_NODES(LS_DIM,NODES)
      INTEGER          MAX_LS_NODES(NODES)
      INTEGER               LATS_NODES_A(NODES)
      INTEGER               LATS_NODES_EXT(NODES)
      INTEGER              GLOBAL_LATS_A(LATG)
      INTEGER        GLOBAL_LATS_EXT(LATG+2*JINTMX+2*NYPT*(NODES-1))
      INTEGER               LATS_NODES_R(NODES)
      INTEGER              GLOBAL_LATS_R(LATR)
      INTEGER                 LONSPERLAR(LATR)
!!
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
      REAL (KIND=KIND_RAD) XLON(LONR,LATS_NODE_R)
      REAL (KIND=KIND_RAD) XLAT(LONR,LATS_NODE_R)
      REAL (KIND=KIND_RAD) COSZDG(LONR,LATS_NODE_R),
     &                     HPRIME(NMTVR,LONR,LATS_NODE_R),
     &                     FLUXR(nfxr,LONR,LATS_NODE_R),
     &                     SFALB(LONR,LATS_NODE_R),
     &                     SWH(NGPTC,LEVS,NBLCK,LATS_NODE_R),
     &                     HLW(NGPTC,LEVS,NBLCK,LATS_NODE_R)

      REAL (kind=kind_phys)
     &     phy_f3d(NGPTC,LEVS,NBLCK,lats_node_r,num_p3d),
     &     phy_f2d(lonr,lats_node_r,num_p2d),
!
     &     DDY(LATS_NODE_R)

      INTEGER JINDX1(LATS_NODE_R),JINDX2(LATS_NODE_R)
!!     
      INTEGER LEV,LEVMAX
      REAL OZPLIN(LATSOZP,LEVOZP,pl_coeff,timeoz) !OZONE Coeff
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
      INTEGER   kdt,IERR,J,K,L,LOCL,N
      integer  idt,mdt,jdt,kdtdfi
      logical lsout
      REAL(KIND=KIND_EVOD) TEE1(LEVS)
cjfe
      real(kind=kind_evod)  ye1(levs)
      REAL(KIND=KIND_EVOD)  coef00(LEVS,ntrac) ! temp. ozone clwater  
      INTEGER              INDLSEV,JBASEV
      INTEGER              INDLSOD,JBASOD
      include 'function2'
      LOGICAL LSLAG
      REAL QSE(lnte,2),DISE(lnte,2,LEVS),
     &     ZES(lnte,2,LEVS),
     &     TES(lnte,2,LEVS),RQSE(lnte,2,levh)
      REAL QSO(lnto,2),DISO(lnto,2,LEVS),
     &     ZOS(lnto,2,LEVS),
     &     TOS(lnto,2,LEVS),RQSO(lnto,2,levh)
      real totsum

!     print *,' enter tldfi ' 					! hmhj
!
C  INCLUDE FIRST TWO TIME LEVELS
!!
      KDTDFI=KDT+NSDFI
      CALL DFINI(-NSDFI-1  ,NSDFI,
     & trie_ls(1,1,P_q),trie_ls(1,1,P_di),
     & trie_ls(1,1,P_ze),trie_ls(1,1,P_te),trie_ls(1,1,P_rq),
     & trio_ls(1,1,P_q),trio_ls(1,1,P_di),
     & trio_ls(1,1,P_ze),trio_ls(1,1,P_te),trio_ls(1,1,P_rq),
     & TOTSUM,QSE,DISE,ZES,TES,RQSE,QSO,DISO,ZOS,TOS,RQSO)
!!
      CALL DFINI(KDT-KDTDFI,NSDFI,
     & trie_ls(1,1,P_q),trie_ls(1,1,P_di),
     & trie_ls(1,1,P_ze),trie_ls(1,1,P_te),trie_ls(1,1,P_rq),
     & trio_ls(1,1,P_q),trio_ls(1,1,P_di),
     & trio_ls(1,1,P_ze),trio_ls(1,1,P_te),trio_ls(1,1,P_rq),
     & TOTSUM,QSE,DISE,ZES,TES,RQSE,QSO,DISO,ZOS,TOS,RQSO)
      KDT=KDT+1
      FHOUR=KDT*DELTIM/3600
ccccccccccccccccccccccccccccccccccc
      lssav=.true. !always true, except in digital filter
      lsswr=.true. !ex short wave radaition, used in gloopr(astronomy)
      lslwr=.true. !ex long  wave radaition, used in gloopr(astronomy)
      lsfwd=.true. !true only during forward step
      lscca=.false.!get clouds from precp.(first step use fixio_R clds)
      lsout=MOD(KDT,NSOUT).EQ.0.OR.PHOUR.EQ.0.
      if (nsout_hf > 0 .and. phour <= fhmax_hf)                         &
     &   lsout = MOD(kdt ,NSOUT_hf) == 0 .OR. lsout
ccccccccccccccccccccccccccccccccccc
!!
      if(hybrid)then
       call get_cd_hyb(deltim/2.)
      else if( gen_coord_hybrid ) then				! hmhj
       call get_cd_hyb_gc(deltim/2.)				! hmhj
      else
       call get_cd_sig(am,bm,deltim/2.,tov,sv)
      endif
CC
      CALL do_tstep(deltim/2.,kdt,PHOUR,
CJFE &           LAM,DLAM,LAMEXT,LATLOCAL,PHI,DPHI,DPHIBR,PHIBS,
CJFE &           LBASIY,LATSINPE,LAMMP,PHIMP,SIGMP,
     &           TRIE_LS,TRIO_LS,
     &           LS_NODE,LS_NODES,MAX_LS_NODES,
     &           LATS_NODES_A,GLOBAL_LATS_A,
     &           LONSPERLAT,
     &           LATS_NODES_R,GLOBAL_LATS_R,
     &           LONSPERLAR,
     &           LATS_NODES_EXT,GLOBAL_LATS_EXT,
     &           EPSE,EPSO,EPSEDN,EPSODN,
     &           SNNP1EV,SNNP1OD,NDEXEV,NDEXOD,
     X           PLNEV_A,PLNOD_A,PDDEV_A,PDDOD_A,
     X           PLNEW_A,PLNOW_A,
     X           PLNEV_R,PLNOD_R,PDDEV_R,PDDOD_R,
     X           PLNEW_R,PLNOW_R,
     X           SYN_LS_A,DYN_LS_A,
     X           SYN_GR_A_1,DYN_GR_A_1,ANL_GR_A_1,
     X           SYN_GR_A_2,DYN_GR_A_2,ANL_GR_A_2,
     &           LSLAG,
     &           XLON,XLAT,COSZDG,sfc_fld,flx_fld,nsst_fld,
     &           HPRIME,SWH,HLW,FLUXR,
     &           SFALB,SLAG,SDEC,CDEC,
     &           OZPLIN,JINDX1,JINDX2,
     &           DDY,PDRYINI,
     &           phy_f3d,  phy_f2d, NBLCK,
     &           ZHOUR,N1,N4,LSOUT,COLAT1,
     &           CFHOUR1,.false.)
CC
      if(me.eq.0) print*,'kdt after forward step in digifilter=',kdt
!!
      PHOUR=FHOUR
      CALL DFINI(KDT-KDTDFI,NSDFI,
     & trie_ls(1,1,P_q),trie_ls(1,1,P_di),
     & trie_ls(1,1,P_ze),trie_ls(1,1,P_te),trie_ls(1,1,P_rq),
     & trio_ls(1,1,P_q),trio_ls(1,1,P_di),
     & trio_ls(1,1,P_ze),trio_ls(1,1,P_te),trio_ls(1,1,P_rq),
     & TOTSUM,QSE,DISE,ZES,TES,RQSE,QSO,DISO,ZOS,TOS,RQSO)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  INCLUDE TIME LEVELS UP TO HOUR FHOUR+FHDFI
c.....
      if(hybrid)then
       call get_cd_hyb(deltim)
      else if( gen_coord_hybrid ) then				! hmhj
       call get_cd_hyb_gc(deltim)				! hmhj
      else
       call get_cd_sig(am,bm,deltim,tov,sv)
      endif
c.....

      LSFWD=.FALSE.
      LSSAV=.TRUE.
      IDT=KDT+1
      MDT=KDTDFI
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DO JDT=IDT,MDT
        KDT=KDT+1
        FHOUR=KDT*DELTIM/3600
        LSOUT=MOD(KDT,NSOUT).EQ.0
        if (nsout_hf > 0 .and. fhour <= fhmax_hf)                         &
     &  lsout = MOD(kdt ,NSOUT_hf) == 0
        LSCCA=MOD(KDT,NSSWR).EQ.0
        LSSWR=MOD(KDT,NSSWR).EQ.1
        LSLWR=MOD(KDT,NSLWR).EQ.1
cccccccccccccccccccccccccccc
      CALL do_tstep(deltim,kdt,PHOUR,
CJFE &           LAM,DLAM,LAMEXT,LATLOCAL,PHI,DPHI,DPHIBR,PHIBS,
CJFE &           LBASIY,LATSINPE,LAMMP,PHIMP,SIGMP,
     &           TRIE_LS,TRIO_LS,
     &           LS_NODE,LS_NODES,MAX_LS_NODES,
     &           LATS_NODES_A,GLOBAL_LATS_A,
     &           LONSPERLAT,
     &           LATS_NODES_R,GLOBAL_LATS_R,
     &           LONSPERLAR,
     &           LATS_NODES_EXT,GLOBAL_LATS_EXT,
     &           EPSE,EPSO,EPSEDN,EPSODN,
     &           SNNP1EV,SNNP1OD,NDEXEV,NDEXOD,
     X           PLNEV_A,PLNOD_A,PDDEV_A,PDDOD_A,
     X           PLNEW_A,PLNOW_A,
     X           PLNEV_R,PLNOD_R,PDDEV_R,PDDOD_R,
     X           PLNEW_R,PLNOW_R,
     X           SYN_LS_A,DYN_LS_A,
     X           SYN_GR_A_1,DYN_GR_A_1,ANL_GR_A_1,
     X           SYN_GR_A_2,DYN_GR_A_2,ANL_GR_A_2,
     &           LSLAG,
     &           XLON,XLAT,COSZDG,sfc_fld,flx_fld,nsst_fld,
     &           HPRIME,SWH,HLW,FLUXR,
     &           SFALB,SLAG,SDEC,CDEC,
     &           OZPLIN,JINDX1,JINDX2,
     &           DDY,PDRYINI,
     &           phy_f3d,  phy_f2d, NBLCK,
     &           ZHOUR,N1,N4,LSOUT,COLAT1,
     &           CFHOUR1,.false.)
!!
      CALL DFINI(KDT-KDTDFI,NSDFI,
     & trie_ls(1,1,P_q),trie_ls(1,1,P_di),
     & trie_ls(1,1,P_ze),trie_ls(1,1,P_te),trie_ls(1,1,P_rq),
     & trio_ls(1,1,P_q),trio_ls(1,1,P_di),
     & trio_ls(1,1,P_ze),trio_ls(1,1,P_te),trio_ls(1,1,P_rq),
     & TOTSUM,QSE,DISE,ZES,TES,RQSE,QSO,DISO,ZOS,TOS,RQSO)
        PHOUR=FHOUR
      ENDDO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  SAVE SURFACE CONDITIONS
!!
      CALL synchro()
!!
!  SAVE SURFACE CONDITIONS
      call fixwr(1,

     & nsst_fld%ifd,     nsst_fld%time_old, nsst_fld%time_ins,
     & nsst_fld%I_Sw,    nsst_fld%I_Q,      nsst_fld%I_Qrain,
     & nsst_fld%I_M,     nsst_fld%I_Tau,    nsst_fld%I_Sw_Zw,
     & nsst_fld%I_Q_Ts,  nsst_fld%I_M_Ts,   nsst_fld%Tref,
     & nsst_fld%dt_cool, nsst_fld%z_c,      nsst_fld%dt_warm,
     & nsst_fld%z_w,     nsst_fld%c_0,      nsst_fld%c_d,
     & nsst_fld%w_0,     nsst_fld%w_d,

     & sfc_fld%hice,   sfc_fld%fice,   sfc_fld%tisfc, sfc_fld%tsea,  
     & sfc_fld%smc,    sfc_fld%sheleg, sfc_fld%stc,   sfc_fld%tg3,
     & sfc_fld%zorl,   sfc_fld%cv,     sfc_fld%cvb,   sfc_fld%cvt,
     & sfc_fld%alvsf,  sfc_fld%alvwf,  sfc_fld%alnsf, sfc_fld%alnwf,
     & sfc_fld%vfrac,  sfc_fld%canopy, sfc_fld%f10m,  sfc_fld%vtype,
     & sfc_fld%stype,  sfc_fld%facsf,  sfc_fld%facwf, sfc_fld%uustar,
     & sfc_fld%ffmm,   sfc_fld%ffhh,   sfc_fld%tprcp, sfc_fld%srflag,
     + sfc_fld%slc,    sfc_fld%snwdph, sfc_fld%slope, sfc_fld%shdmin,
     & sfc_fld%shdmax, sfc_fld%snoalb, sfc_fld%sncovr)
   
c......................................................................
C  INCLUDE TIME LEVELS UP TO HOUR FHOUR+2*FHDFI
C  BUT DO NOT SAVE DIAGNOSTICS FOR THIS TIME
      LSSAV=.FALSE.
      LSOUT=.FALSE.
      IDT=KDT+1
      MDT=KDT+NSDFI
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DO JDT=IDT,MDT
        KDT=KDT+1
        FHOUR=KDT*DELTIM/3600
        LSCCA=MOD(KDT,NSSWR).EQ.0
        LSSWR=MOD(KDT,NSSWR).EQ.1
        LSLWR=MOD(KDT,NSLWR).EQ.1
cccccccccccccccccccccccccccc
      CALL do_tstep(deltim,kdt,PHOUR,
CJFE &           LAM,DLAM,LAMEXT,LATLOCAL,PHI,DPHI,DPHIBR,PHIBS,
CJFE &           LBASIY,LATSINPE,LAMMP,PHIMP,SIGMP,
     &           TRIE_LS,TRIO_LS,
     &           LS_NODE,LS_NODES,MAX_LS_NODES,
     &           LATS_NODES_A,GLOBAL_LATS_A,
     &           LONSPERLAT,
     &           LATS_NODES_R,GLOBAL_LATS_R,
     &           LONSPERLAR,
     &           LATS_NODES_EXT,GLOBAL_LATS_EXT,
     &           EPSE,EPSO,EPSEDN,EPSODN,
     &           SNNP1EV,SNNP1OD,NDEXEV,NDEXOD,
     X           PLNEV_A,PLNOD_A,PDDEV_A,PDDOD_A,
     X           PLNEW_A,PLNOW_A,
     X           PLNEV_R,PLNOD_R,PDDEV_R,PDDOD_R,
     X           PLNEW_R,PLNOW_R,
     X           SYN_LS_A,DYN_LS_A,
     X           SYN_GR_A_1,DYN_GR_A_1,ANL_GR_A_1,
     X           SYN_GR_A_2,DYN_GR_A_2,ANL_GR_A_2,
     &           LSLAG,
     &           XLON,XLAT,COSZDG,sfc_fld,flx_fld,nsst_fld,
     &           HPRIME,SWH,HLW,FLUXR,
     &           SFALB,SLAG,SDEC,CDEC,
     &           OZPLIN,JINDX1,JINDX2,
     &           DDY,PDRYINI,
     &           phy_f3d,  phy_f2d, NBLCK,
     &           ZHOUR,N1,N4,LSOUT,COLAT1,
     &           CFHOUR1,.false.)
!!
!!
      CALL DFINI(KDT-KDTDFI,NSDFI,
     & trie_ls(1,1,P_q),trie_ls(1,1,P_di),
     & trie_ls(1,1,P_ze),trie_ls(1,1,P_te),trie_ls(1,1,P_rq),
     & trio_ls(1,1,P_q),trio_ls(1,1,P_di),
     & trio_ls(1,1,P_ze),trio_ls(1,1,P_te),trio_ls(1,1,P_rq),
     & TOTSUM,QSE,DISE,ZES,TES,RQSE,QSO,DISO,ZOS,TOS,RQSO)
       PHOUR=FHOUR
      ENDDO
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  DO FINAL DIGITAL FILTER, set (n-1)=(n), and run forward step in main
      CALL DFINI(NSDFI+1,NSDFI,
     & trie_ls(1,1,P_q),trie_ls(1,1,P_di),
     & trie_ls(1,1,P_ze),trie_ls(1,1,P_te),trie_ls(1,1,P_rq),
     & trio_ls(1,1,P_q),trio_ls(1,1,P_di),
     & trio_ls(1,1,P_ze),trio_ls(1,1,P_te),trio_ls(1,1,P_rq),
     & TOTSUM,QSE,DISE,ZES,TES,RQSE,QSO,DISO,ZOS,TOS,RQSO)
      if(me.eq.0) print*,'kdt after last dfini in digifilter=',kdt
!!
      do i=1,len_trie_ls
        trie_ls(i,1,P_qm)=trie_ls(i,1,P_q)
        trie_ls(i,2,P_qm)=trie_ls(i,2,P_q)
      enddo
      do k=1,levs
        do i=1,len_trie_ls
          trie_ls(i,1,P_tem+k-1)=trie_ls(i,1,P_te+k-1)
          trie_ls(i,1,P_dim+k-1)=trie_ls(i,1,P_di+k-1)
          trie_ls(i,1,P_zem+k-1)=trie_ls(i,1,P_ze+k-1)
          trie_ls(i,2,P_tem+k-1)=trie_ls(i,2,P_te+k-1)
          trie_ls(i,2,P_dim+k-1)=trie_ls(i,2,P_di+k-1)
          trie_ls(i,2,P_zem+k-1)=trie_ls(i,2,P_ze+k-1)
        enddo
      enddo
      do k=1,levh
        do i=1,len_trie_ls
          trie_ls(i,1,P_rm+k-1)=trie_ls(i,1,P_rq+k-1)
          trie_ls(i,2,P_rm+k-1)=trie_ls(i,2,P_rq+k-1)
        enddo
      enddo
!!
      do i=1,len_trio_ls
        trio_ls(i,1,P_qm)=trio_ls(i,1,P_q)
        trio_ls(i,2,P_qm)=trio_ls(i,2,P_q)
      enddo
      do k=1,levs
        do i=1,len_trio_ls
          trio_ls(i,1,P_tem+k-1)=trio_ls(i,1,P_te+k-1)
          trio_ls(i,1,P_dim+k-1)=trio_ls(i,1,P_di+k-1)
          trio_ls(i,1,P_zem+k-1)=trio_ls(i,1,P_ze+k-1)
          trio_ls(i,2,P_tem+k-1)=trio_ls(i,2,P_te+k-1)
          trio_ls(i,2,P_dim+k-1)=trio_ls(i,2,P_di+k-1)
          trio_ls(i,2,P_zem+k-1)=trio_ls(i,2,P_ze+k-1)
        enddo
      enddo
      do k=1,levh
        do i=1,len_trio_ls
          trio_ls(i,1,P_rm+k-1)=trio_ls(i,1,P_rq+k-1)
          trio_ls(i,2,P_rm+k-1)=trio_ls(i,2,P_rq+k-1)
        enddo
      enddo
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  REPLACE SURFACE CONDITIONS with conditions written at mid ini segment.
c  forward step in main begins with values in the middle of the filter span
c  forward step in main begins with values at time fhdfi
      call fixwr(2,

     & nsst_fld%ifd,     nsst_fld%time_old, nsst_fld%time_ins,
     & nsst_fld%I_Sw,    nsst_fld%I_Q,      nsst_fld%I_Qrain,
     & nsst_fld%I_M,     nsst_fld%I_Tau,    nsst_fld%I_Sw_Zw,
     & nsst_fld%I_Q_Ts,  nsst_fld%I_M_Ts,   nsst_fld%Tref,
     & nsst_fld%dt_cool, nsst_fld%z_c,      nsst_fld%dt_warm,
     & nsst_fld%z_w,     nsst_fld%c_0,      nsst_fld%c_d,
     & nsst_fld%w_0,     nsst_fld%w_d,

     & sfc_fld%hice,   sfc_fld%fice,   sfc_fld%tisfc, sfc_fld%tsea,
     & sfc_fld%smc,    sfc_fld%sheleg, sfc_fld%stc,   sfc_fld%tg3,
     & sfc_fld%zorl,   sfc_fld%cv,     sfc_fld%cvb,   sfc_fld%cvt,
     & sfc_fld%alvsf,  sfc_fld%alvwf,  sfc_fld%alnsf, sfc_fld%alnwf,
     & sfc_fld%vfrac,  sfc_fld%canopy, sfc_fld%f10m,  sfc_fld%vtype,
     & sfc_fld%stype,  sfc_fld%facsf,  sfc_fld%facwf, sfc_fld%uustar,
     & sfc_fld%ffmm,   sfc_fld%ffhh,   sfc_fld%tprcp, sfc_fld%srflag,
     + sfc_fld%slc,    sfc_fld%snwdph, sfc_fld%slope, sfc_fld%shdmin,
     & sfc_fld%shdmax, sfc_fld%snoalb, sfc_fld%sncovr)
!!
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  RESET CLOCK AND OUTPUT initialized fields
      KDT=KDTDFI
      FHOUR=KDT*DELTIM/3600 ! note that fhour also comes from last fixio
      if(me.eq.0) print*,'fhour after reset clock digifilter=',fhour,kdt
      LSOUT=MOD(KDT,NSOUT).EQ.0
      if (nsout_hf > 0 .and. fhour <= fhmax_hf)                         &
     &  lsout = MOD(kdt ,NSOUT_hf) == 0
!!
      IF (me.eq.0) THEN
        write(*,*)'Initialized values in digifilter'
        write(*,*)'************'
c$$$        CALL bar3(trie_ls(1,1,P_ze),trio_ls(1,1,P_ze),'ze ',levs)
c$$$        CALL bar3(trie_ls(1,1,P_di),trio_ls(1,1,P_di),'di ',levs)
c$$$        CALL bar3(trie_ls(1,1,P_te),trio_ls(1,1,P_te),'te ',levs)
c$$$        CALL bar3(trie_ls(1,1,P_rq),trio_ls(1,1,P_rq),'rq ',levs)
c$$$        CALL bar3(trie_ls(1,1,P_rq+levs),trio_ls(1,1,P_rq+levs),
c$$$     &            'oz1 ',levs)
c$$$        CALL bar3(trie_ls(1,1,P_rq+2*levs),trio_ls(1,1,P_rq+2*levs),
c$$$     &            'oz2 ',levs)
c$$$        CALL bar3(trie_ls(1,1,P_q),trio_ls(1,1,P_q),'q ',1)
c$$$        CALL bar3(trie_ls(1,1,P_gz),trio_ls(1,1,P_gz),'gz ',1)
!       print*,'P_qm =',P_qm ,' P_rm =',P_rm 
!sela if (.NOT.LIOPE.or.icolor.ne.2) then
c$$$        CALL RMS_spect(TRIE_LS(1,1,P_QM ), TRIE_LS(1,1,P_DIM),
c$$$     X             TRIE_LS(1,1,P_TEM), TRIE_LS(1,1,P_ZEM),
c$$$     X             TRIE_LS(1,1,P_RM ),
c$$$     X             TRIO_LS(1,1,P_QM ), TRIO_LS(1,1,P_DIM),
c$$$     X             TRIO_LS(1,1,P_TEM), TRIO_LS(1,1,P_ZEM),
c$$$     X             TRIO_LS(1,1,P_RM ),
c$$$     X             LS_NODES,MAX_LS_NODES)
!sela endif
!---------------------------------------------------------------

      ENDIF
!!
      PHOUR=FHOUR
!--------------------------------------------
cmy reset digifilter switch to zero for activiation of reshuffling lats loopa
      fhdfi = 0

!     print *,' leave tldfi ' 					! hmhj

      RETURN
      END
!!
      SUBROUTINE DFINI(KSTEP,NSTEP,QE,DIE,ZE,TE,RQE,
     &                QO,DIO,ZO,TO,RQO,
     & TOTSUM,QSE,DISE,ZES,TES,RQSE,QSO,DISO,ZOS,TOS,RQSO)
cc
      use resol_def
      use layout1
      implicit none
cc
!!
      REAL QE(len_trie_ls,2),DIE(len_trie_ls,2,levs),
     &     ZE(len_trie_ls,2,levs),
     &     TE(len_trie_ls,2,levs),RQE(len_trie_ls,2,levh)
      REAL QO(len_trio_ls,2),DIO(len_trio_ls,2,levs),
     &     ZO(len_trio_ls,2,levs),
     &     TO(len_trio_ls,2,levs),RQO(len_trio_ls,2,levh)
!!
      INTEGER len_trie,len_trio
      REAL digfil,sx,wx,totsumi
      REAL QSE(lnte,2),DISE(lnte,2,LEVS),
     &     ZES(lnte,2,LEVS),
     &     TES(lnte,2,LEVS),RQSE(lnte,2,levh)
      REAL QSO(lnto,2),DISO(lnto,2,LEVS),
     &     ZOS(lnto,2,LEVS),
     &     TOS(lnto,2,LEVS),RQSO(lnto,2,levh)
      integer levl,kl
ccc      save TOTSUM,QSE,DISE,ZES,TES,RQSE,QSO,DISO,ZOS,TOS,RQSO
C
      real totsum
      integer i,k,nstep,kstep

!     print *,' enter DFINI ' 					! hmhj

      IF(KSTEP.LT.-NSTEP) THEN !++++++++++++++++++++++++++++++++++
        TOTSUM = 0
        QSE=0.
        QSO=0.
        DISE=0.
        ZES=0.
        TES=0.
        RQSE=0.
        DISO=0.
        ZOS=0.
        TOS=0.
        RQSO=0.
C
      ELSEIF(KSTEP.LE.NSTEP) THEN !++++++++++++++++++++++++++++++
csela  print*,'arrived at ELSEIF(KSTEP.LE.NSTEP)',ktstep
        IF(KSTEP.NE.0) THEN  !--------------------------------
          SX     = ACOS(-1.)*KSTEP/NSTEP
          WX     = ACOS(-1.)*KSTEP/(NSTEP+1)
          DIGFIL = SIN(WX)/WX*SIN(SX)/SX
          if(me.eq.0)then
          print*,'in dfini sx=',sx,'wx=',wx,'digfil=',digfil,
     &    'at kstep=',kstep
          endif
        ELSE                 !--------------------------------
csela   print*,'arrived at IF(KSTEP.NE.0) in ELSEIF(KSTEP.LE.NSTEP),
csela&                ktstep= ntstep=',ktstep,ntstep
          DIGFIL=1
        ENDIF                !--------------------------------

        TOTSUM = TOTSUM + DIGFIL
        DO K=1,2
          DO I=1,len_trie_ls
           QSE(I,k)=QSE(I,k)+DIGFIL*QE(I,k)
          ENDDO
        ENDDO
        DO K=1,2
          DO I=1,len_trio_ls
           QSO(I,k)=QSO(I,k)+DIGFIL*QO(I,k)
          ENDDO
        ENDDO
        DO KL=1,levs
        DO K=1,2
          DO I=1,len_trie_ls
            DISE(I,K,kl)=DISE(I,K,kl)+DIGFIL*DIE(I,K,kl)
            ZES(I,K,kl)=ZES(I,K,kl)+DIGFIL*ZE(I,K,kl)
            TES(I,K,kl)=TES(I,K,kl)+DIGFIL*TE(I,K,kl)
          ENDDO
          DO I=1,len_trio_ls
            DISO(I,K,kl)=DISO(I,K,kl)+DIGFIL*DIO(I,K,kl)
            ZOS(I,K,kl)=ZOS(I,K,kl)+DIGFIL*ZO(I,K,kl)
            TOS(I,K,kl)=TOS(I,K,kl)+DIGFIL*TO(I,K,kl)
          ENDDO
        ENDDO
        ENDDO
        DO KL=1,levh
        DO K=1,2
          DO I=1,len_trie_ls
            RQSE(I,K,kl)=RQSE(I,K,kl)+DIGFIL*RQE(I,K,kl)
          ENDDO
          DO I=1,len_trio_ls
            RQSO(I,K,kl)=RQSO(I,K,kl)+DIGFIL*RQO(I,K,kl)
          ENDDO
        ENDDO
        ENDDO
C
      ELSE  !++++++++++++++++++++++++++++++++++++++++++++++++++++
csela   print*,'arrived at (KSTEP.LT.-NSTEP) in dfini
csela& ktstep= ntstep=',ktstep,ntstep
        TOTSUMI = 1.0 / TOTSUM
        DO K=1,2
          DO I=1,len_trie_ls
            QE(I,k)  = QSE(I,k)  * TOTSUMI
          ENDDO
          DO I=1,len_trio_ls
            QO(I,k)  = QSO(I,k)  * TOTSUMI
          ENDDO
        ENDDO
        DO KL=1,levs
        DO K=1,2
          DO I=1,len_trie_ls
            DIE(I,K,kl) = DISE(I,K,kl) * TOTSUMI
            ZE(I,K,kl) = ZES(I,K,kl) * TOTSUMI
            TE(I,K,kl) = TES(I,K,kl) * TOTSUMI
          ENDDO
          DO I=1,len_trio_ls
            DIO(I,K,kl) = DISO(I,K,kl) * TOTSUMI
            ZO(I,K,kl) = ZOS(I,K,kl) * TOTSUMI
            TO(I,K,kl) = TOS(I,K,kl) * TOTSUMI
          ENDDO
        ENDDO
        ENDDO
        DO KL=1,levh
        DO K=1,2
          DO I=1,len_trie_ls
            RQE(I,K,kl) = RQSE(I,K,kl) * TOTSUMI
          ENDDO
          DO I=1,len_trio_ls
            RQO(I,K,kl) = RQSO(I,K,kl) * TOTSUMI
          ENDDO
        ENDDO
        ENDDO
      ENDIF !+++++++++++++++++++++++++++++++++++++++++++++++++++++
C
!     print *,' leave DFINI ' 					! hmhj
      END
      SUBROUTINE fixwr(iflag,

     + ifd,time_old,time_ins,I_Sw,I_Q,I_Qrain,         ! FOR Ocean   - XL Dec07
     + I_M,I_Tau,I_Sw_Zw,I_Q_Ts,I_M_Ts,
     + Tref,dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d,

     & hice,fice,tisfc,                                ! FOR SEA-ICE - XW Nov04
     & tsea,smc,sheleg,stc,tg3,zorl,cv,cvb,cvt,
     & alvsf,alvwf,alnsf,alnwf,vfrac,canopy,f10m,vtype,stype,
Clu [-1L/+2L]: add (tprcp,srflag),(slc,snwdph,slope,shdmin,shdmax,snoalb)
Clu  & facsf,facwf,uustar,ffmm,ffhh)
     + facsf,facwf,uustar,ffmm,ffhh,tprcp,srflag,
     + slc,snwdph,slope,shdmin,shdmax,snoalb,sncovr)

c
c***********************************************************************
c     PURPOSE:
c      save or retrieve fixed fields in digifilt
c
c***********************************************************************
c
      use resol_def
      use layout1
      implicit none
      integer iflag
      real SMC(LSOIL,lonr,lats_node_r),STC(LSOIL,lonr,lats_node_r),
     &     HICE(lonr,lats_node_r),FICE(lonr,lats_node_r),  ! FOR SEA-ICE - NOV04
     &     TISFC(lonr,lats_node_r),

     &     ifd(lonr,lats_node_r),                          ! FOR O
cean - Dec07
     &     time_old(lonr,lats_node_r),time_ins(lonr,lats_node_r),
     &     I_Sw(lonr,lats_node_r),I_Q(lonr,lats_node_r),
     &     I_Qrain(lonr,lats_node_r),I_M(lonr,lats_node_r),
     &     I_Tau(lonr,lats_node_r),I_Sw_Zw(lonr,lats_node_r),
     &     I_Q_Ts(lonr,lats_node_r),I_M_Ts(lonr,lats_node_r),
     &     Tref(lonr,lats_node_r),dt_cool(lonr,lats_node_r),
     &     z_c(lonr,lats_node_r),dt_warm(lonr,lats_node_r),
     &     z_w(lonr,lats_node_r),c_0(lonr,lats_node_r),
     &     c_d(lonr,lats_node_r),w_0(lonr,lats_node_r),
     &     w_d(lonr,lats_node_r),

     &     TSEA  (lonr,lats_node_r),SHELEG(lonr,lats_node_r),
     &     TG3   (lonr,lats_node_r),
     &     ZORL  (lonr,lats_node_r),CV    (lonr,lats_node_r),
     &     CVB   (lonr,lats_node_r),
     &     CVT   (lonr,lats_node_r),ALVSF (lonr,lats_node_r),
     &     ALVWF (lonr,lats_node_r),
     &     ALNSF (lonr,lats_node_r),ALNWF (lonr,lats_node_r),
     &     SLMSK (lonr,lats_node_r),
     &     VFRAC (lonr,lats_node_r),CANOPY(lonr,lats_node_r),
     &     F10M  (lonr,lats_node_r),
     &     VTYPE (lonr,lats_node_r),STYPE (lonr,lats_node_r),
     &     FACSF (lonr,lats_node_r),
     &     FACWF (lonr,lats_node_r),UUSTAR(lonr,lats_node_r),
     &     FFMM  (lonr,lats_node_r),
     &     FFHH  (lonr,lats_node_r)
Clu [+5L]: add (tprcp,srflag),(slc,snwdph,snoalb,slope,shdmin,shdmax)
     +,    TPRCP (lonr,lats_node_r),SRFLAG(lonr,lats_node_r)
     +,    SLC    (LSOIL,lonr,lats_node_r)
     +,    SNWDPH (lonr,lats_node_r)
     +,    SNOALB (lonr,lats_node_r),SLOPE (lonr,lats_node_r)
     +,    SHDMIN (lonr,lats_node_r),SHDMAX(lonr,lats_node_r)
     +,    SNCOVR (lonr,lats_node_r)

      real , allocatable :: SMC1(:,:,:),STC1(:,:,:),
     &  HICE1(:,:),FICE1(:,:),TISFC1(:,:),                   ! FOR SEA-ICE - XW Nov04

     &  ifd1(:,:),time_old1(:,:),                            ! FOR SEA-ICE - XW Nov04
     &  time_ins1(:,:),I_Sw1(:,:),I_Q1(:,:),I_Qrain1(:,:),
     &  I_M1(:,:),
     &  I_Tau1(:,:),I_Sw_Zw1(:,:),I_Q_Ts1(:,:),I_M_Ts1(:,:),
     &  Tref1(:,:),dt_cool1(:,:),z_c1(:,:),dt_warm1(:,:),
     &  z_w1(:,:),c_01(:,:),c_d1(:,:),w_01(:,:),w_d1(:,:),

     &  TSEA1(:,:),SHELEG1(:,:),TG31(:,:),
     &  ZORL1(:,:),CV1(:,:),CVB1(:,:),
     &  CVT1(:,:),ALVSF1(:,:),ALVWF1(:,:),
     &  ALNSF1(:,:),ALNWF1(:,:),SLMSK1(:,:),
     &  VFRAC1(:,:),CANOPY1(:,:),F10M1(:,:),
     &  VTYPE1(:,:),STYPE1(:,:),FACSF1(:,:),
     &  FACWF1(:,:),UUSTAR1(:,:),FFMM1(:,:),
     &  FFHH1(:,:)
Clu [+3L]: add (tprcp1,srflag1),(slc1,snwdph1,slope1,shdmin1,shdmax1,snoalb1)
     +, TPRCP1(:,:),SRFLAG1(:,:)
     +, SLC1(:,:,:),SNWDPH1(:,:),SLOPE1(:,:)
     +, SHDMIN1(:,:),SHDMAX1(:,:),SNOALB1(:,:), SNCOVR1(:,:)

      logical first
      data first/.true./
      save   first,SMC1,STC1,TSEA1,SHELEG1,TG31,ZORL1,CV1,CVB1,CVT1
      save   HICE1,FICE1,TISFC1                          ! FOR SEA-ICE - XW Nov04
!                                                        ! FOR Ocean   - XL Dec07
      save   ifd1,time_old1,time_ins1,I_Sw1,I_Q1,I_Qrain1,I_M1
      save   I_Tau1,I_Sw_Zw1,I_Q_Ts1,I_M_Ts1,Tref1,dt_cool1,z_c1
      save   dt_warm1,z_w1,c_01,c_d1,w_01,w_d1
      save   ALVSF1,ALVWF1,ALNSF1,ALNWF1,SLMSK1,VFRAC1,CANOPY1,F10M1
      save   VTYPE1,STYPE1,FACSF1,FACWF1,UUSTAR1,FFMM1,FFHH1
Clu [+2L]: save (tprcp1,srflag1),(slc1,snwdph1,slope1,shdmin1,shdmax1,snoalb1)
      save   TPRCP1,SRFLAG1
      save   SLC1,SNWDPH1,SLOPE1,SHDMIN1,SHDMAX1,SNOALB1,SNCOVR1
  
      integer i,j,k

c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
!     print *,' enter fixwr ' 					! hmhj
      if (first) then
        allocate (SMC1(LSOIL,lonr,lats_node_r))
        allocate (STC1(LSOIL,lonr,lats_node_r))
        allocate (HICE1(lonr,lats_node_r))              ! FOR SEA-ICE - XW Nov04
        allocate (FICE1(lonr,lats_node_r))              ! FOR SEA-ICE - XW Nov04
        allocate (TISFC1(lonr,lats_node_r))             ! FOR SEA-ICE - XW Nov04
                                                        ! FOR Ocean   - XL Dec07
        allocate (ifd1(lonr,lats_node_r))
        allocate (time_old1(lonr,lats_node_r))
        allocate (time_ins1(lonr,lats_node_r))
        allocate (I_Sw1(lonr,lats_node_r))
        allocate (I_Q1(lonr,lats_node_r))
        allocate (I_Qrain1(lonr,lats_node_r))
        allocate (I_M1(lonr,lats_node_r))
        allocate (I_Tau1(lonr,lats_node_r))
        allocate (I_Sw_Zw1(lonr,lats_node_r))
        allocate (I_Q_Ts1(lonr,lats_node_r))
        allocate (I_M_Ts1(lonr,lats_node_r))

        allocate (Tref1(lonr,lats_node_r))
        allocate (dt_cool1(lonr,lats_node_r))
        allocate (z_c1(lonr,lats_node_r))
        allocate (dt_warm1(lonr,lats_node_r))
        allocate (z_w1(lonr,lats_node_r))
        allocate (c_01(lonr,lats_node_r))
        allocate (c_d1(lonr,lats_node_r))
        allocate (w_01(lonr,lats_node_r))
        allocate (w_d1(lonr,lats_node_r))

        allocate (TSEA1(lonr,lats_node_r))
        allocate (SHELEG1(lonr,lats_node_r))
        allocate (TG31(lonr,lats_node_r))
        allocate (ZORL1(lonr,lats_node_r))
        allocate (CV1(lonr,lats_node_r))
        allocate (CVB1(lonr,lats_node_r))
        allocate (CVT1(lonr,lats_node_r))
        allocate (ALVSF1(lonr,lats_node_r))
        allocate (ALVWF1(lonr,lats_node_r))
        allocate (ALNSF1(lonr,lats_node_r))
        allocate (ALNWF1(lonr,lats_node_r))
        allocate (SLMSK1(lonr,lats_node_r))
        allocate (VFRAC1(lonr,lats_node_r))
        allocate (CANOPY1(lonr,lats_node_r))
        allocate (F10M1(lonr,lats_node_r))
        allocate (VTYPE1(lonr,lats_node_r))
        allocate (STYPE1(lonr,lats_node_r))
        allocate (FACSF1(lonr,lats_node_r))
        allocate (FACWF1(lonr,lats_node_r))
        allocate (UUSTAR1(lonr,lats_node_r))
        allocate (FFMM1(lonr,lats_node_r))
        allocate (FFHH1(lonr,lats_node_r))
Clu [+8L]: allocate (tprcp,srflag),(slc,snwdph,slope,shdmin,shdmax,snoalb)
        allocate (TPRCP1(lonr,lats_node_r))
        allocate (SRFLAG1(lonr,lats_node_r))
        allocate (SLC1(LSOIL,lonr,lats_node_r))
        allocate (SNWDPH1(lonr,lats_node_r))
        allocate (SLOPE1(lonr,lats_node_r))
        allocate (SHDMIN1(lonr,lats_node_r))
        allocate (SHDMAX1(lonr,lats_node_r))
        allocate (SNOALB1(lonr,lats_node_r))
        allocate (SNCOVR1(lonr,lats_node_r))
        first = .false.
      endif
!
      if(iflag.eq.1) then
        do k=1,lsoil
          do j=1,lats_node_r
            do i=1,lonr
              smc1(k,i,j)=smc(k,i,j)
              stc1(k,i,j)=stc(k,i,j)
              slc1(k,i,j)=slc(k,i,j)        !! Clu [+1L]: slc -> slc1
            enddo
          enddo
        enddo
        do j=1,lats_node_r
          do i=1,lonr
            hice1(i,j)=hice(i,j)                        ! FOR SEA-ICE - XW Nov04
            fice1(i,j)=fice(i,j)                        ! FOR SEA-ICE - XW Nov04
            tisfc1(i,j)=tisfc(i,j)                      ! FOR SEA-ICE - XW Nov04

            I_Sw1(i,j)    = I_Sw(i,j)                   ! FOR Ocean   - XL Dec07
            I_Q1(i,j)     = I_Q(i,j)                    ! FOR Ocean   - XL Dec07
            I_Qrain1(i,j) = I_Qrain(i,j)                ! FOR Ocean   - XL Dec07
            I_M1(i,j)     = I_M(i,j)                    ! FOR Ocean   - XL Dec07
            I_Tau1(i,j)   = I_Tau(i,j)                  ! FOR Ocean   - XL Dec07
            I_Sw_Zw1(i,j) = I_Sw_Zw(i,j)                ! FOR Ocean   - XL Dec07
            I_Q_Ts1(i,j)  = I_Q_Ts(i,j)                 ! FOR Ocean   - XL Dec07
            I_M_Ts1(i,j)  = I_M_Ts(i,j)                 ! FOR Ocean   - XL Dec07
            Tref1(i,j)    = Tref(i,j)                   ! FOR Ocean   - XL Dec07
            dt_cool1(i,j) = dt_cool(i,j)                ! FOR Ocean   - XL Dec07
            z_c1(i,j)     = z_c(i,j)                    ! FOR Ocean   - XL Dec07
            dt_warm1(i,j) = dt_warm(i,j)                ! FOR Ocean   - XL Dec07
            z_w1(i,j)     = z_w(i,j)                    ! FOR Ocean   - XL Dec07
            c_01(i,j)     = c_0(i,j)                    ! FOR Ocean   - XL Dec07
            c_d1(i,j)     = c_d(i,j)                    ! FOR Ocean   - XL Dec07
            w_01(i,j)     = w_0(i,j)                    ! FOR Ocean   - XL Dec07
            w_d1(i,j)     = w_d(i,j)                    ! FOR Ocean   - XL Dec07

            tsea1(i,j)=tsea(i,j)
            sheleg1(i,j)=sheleg(i,j)
            tg31(i,j)=tg3(i,j)
            zorl1(i,j)=zorl(i,j)
            cv1(i,j)=cv(i,j)
            cvb1(i,j)=cvb(i,j)
            cvt1(i,j)=cvt(i,j)
            alvsf1(i,j)=alvsf(i,j)
            alvwf1(i,j)=alvwf(i,j)
            alnsf1(i,j)=alnsf(i,j)
            alnwf1(i,j)=alnwf(i,j)
            slmsk1(i,j)=slmsk(i,j)
            vfrac1(i,j)=vfrac(i,j)
            canopy1(i,j)=canopy(i,j)
            f10m1(i,j)=f10m(i,j)
            vtype1(i,j)=vtype(i,j)
            stype1(i,j)=stype(i,j)
            facsf1(i,j)=facsf(i,j)
            facwf1(i,j)=facwf(i,j)
            uustar1(i,j)=uustar(i,j)
            ffmm1(i,j)=ffmm(i,j)
            ffhh1(i,j)=ffhh(i,j)
Clu [+7L]: add (tprcp,srflag),(snwdph,slope,shdmin,shdmax,snoalb)
            tprcp1(i,j)=tprcp(i,j)
            srflag1(i,j)=srflag(i,j)
            snwdph1(i,j)=snwdph(i,j)
            slope1(i,j)=slope(i,j)
            shdmin1(i,j)=shdmin(i,j)
            shdmax1(i,j)=shdmax(i,j)
            snoalb1(i,j)=snoalb(i,j)
            sncovr1(i,j)=sncovr(i,j)
          enddo
        enddo
      elseif(iflag.eq.2) then
        do k=1,lsoil
          do j=1,lats_node_r
            do i=1,lonr
              smc(k,i,j)=smc1(k,i,j)
              stc(k,i,j)=stc1(k,i,j)
              slc(k,i,j)=slc1(k,i,j)          !! Clu [+1L]: slc1 -> slc
            enddo
          enddo
        enddo
        do j=1,lats_node_r
          do i=1,lonr
            hice(i,j)=hice1(i,j)                        ! FOR SEA-ICE - XW Nov04
            fice(i,j)=fice1(i,j)                        ! FOR SEA-ICE - XW Nov04
            tisfc(i,j)=tisfc1(i,j)                      ! FOR SEA-ICE - XW Nov04

            I_Sw(i,j)    = I_Sw1(i,j)                   ! FOR Ocean   - XL Dec07
            I_Q(i,j)     = I_Q1(i,j)                    ! FOR Ocean   - XL Dec07
            I_Qrain(i,j) = I_Qrain1(i,j)                ! FOR Ocean   - XL Dec07
            I_M(i,j)     = I_M1(i,j)                    ! FOR Ocean   - XL Dec07
            I_Tau(i,j)   = I_Tau1(i,j)                  ! FOR Ocean   - XL Dec07
            I_Sw_Zw(i,j) = I_Sw_Zw1(i,j)                ! FOR Ocean   - XL Dec07
            I_Q_Ts(i,j)  = I_Q_Ts1(i,j)                 ! FOR Ocean   - XL Dec07
            I_M_Ts(i,j)  = I_M_Ts1(i,j)                 ! FOR Ocean   - XL Dec07

            Tref(i,j)    = Tref1(i,j)                   ! FOR Ocean   - XL Dec07
            dt_cool(i,j) = dt_cool1(i,j)                ! FOR Ocean   - XL Dec07
            z_c(i,j)     = z_c1(i,j)                    ! FOR Ocean   - XL Dec07
            dt_warm(i,j) = dt_warm1(i,j)                ! FOR Ocean   - XL Dec07
            z_w(i,j)     = z_w1(i,j)                    ! FOR Ocean   - XL Dec07
            c_0(i,j)     = c_01(i,j)                    ! FOR Ocean   - XL Dec07
            c_d(i,j)     = c_d1(i,j)                    ! FOR Ocean   - XL Dec07
            w_0(i,j)     = w_01(i,j)                    ! FOR Ocean   - XL Dec07
            w_d(i,j)     = w_d1(i,j)                    ! FOR Ocean   - XL Dec07

            tsea(i,j)=tsea1(i,j)
            sheleg(i,j)=sheleg1(i,j)
            tg3(i,j)=tg31(i,j)
            zorl(i,j)=zorl1(i,j)
            cv(i,j)=cv1(i,j)
            cvb(i,j)=cvb1(i,j)
            cvt(i,j)=cvt1(i,j)
            alvsf(i,j)=alvsf1(i,j)
            alvwf(i,j)=alvwf1(i,j)
            alnsf(i,j)=alnsf1(i,j)
            alnwf(i,j)=alnwf1(i,j)
            slmsk(i,j)=slmsk1(i,j)
            vfrac(i,j)=vfrac1(i,j)
            canopy(i,j)=canopy1(i,j)
            f10m(i,j)=f10m1(i,j)
            vtype(i,j)=vtype1(i,j)
            stype(i,j)=stype1(i,j)
            facsf(i,j)=facsf1(i,j)
            facwf(i,j)=facwf1(i,j)
            uustar(i,j)=uustar1(i,j)
            ffmm(i,j)=ffmm1(i,j)
            ffhh(i,j)=ffhh1(i,j)
Clu [+7L]: add (tprcp,srflag),(snwdph,slope,shdmin,shdmax,snoalb)
            tprcp(i,j)=tprcp1(i,j)
            srflag(i,j)=srflag1(i,j)
            snwdph(i,j)=snwdph1(i,j)
            slope(i,j)=slope1(i,j)
            shdmin(i,j)=shdmin1(i,j)
            shdmax(i,j)=shdmax1(i,j)
            snoalb(i,j)=snoalb1(i,j)
            sncovr(i,j)=sncovr1(i,j)
          enddo
        enddo
      endif
!     print *,' leave fixwr ' 					! hmhj
      return
      end

