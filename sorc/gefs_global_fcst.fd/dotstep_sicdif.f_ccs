      SUBROUTINE do_tstep(deltim,kdt,PHOUR,
!JFE &                 LAM,DLAM,LAMEXT,LATLOCAL,PHI,DPHI,DPHIBR,PHIBS,
!JFE &                 LBASIY,LATSINPE,LAMMP,PHIMP,SIGMP,
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
     &                 XLON,XLAT,COSZDG, sfc_fld, flx_fld, nsst_fld,
     &                 HPRIME,SWH,HLW,FLUXR,SFALB,SLAG,SDEC,CDEC,
     &                 OZPLIN,JINDX1,JINDX2,DDY,PDRYINI,
     &                 phy_f3d,  phy_f2d, NBLCK,
     &                 ZHOUR,N1,N4,LSOUT,COLAT1,CFHOUR1,SPS)
!!
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
      use Sfc_Flx_ESMFMod
      use Nsstm_ESMFMod
      use d3d_def
      IMPLICIT NONE
!!     
      TYPE(Sfc_Var_Data)        :: sfc_fld
      TYPE(Flx_Var_Data)        :: flx_fld
      TYPE(Nsst_Var_Data)       :: nsst_fld
      CHARACTER(16)             :: CFHOUR1
      INTEGER,INTENT(IN)        :: LONSPERLAT(LATG),N1,N4
!JFE  INTEGER,INTENT(IN):: LATSINPE(LATS_NODE_A)
!JFE  INTEGER,INTENT(IN):: LATLOCAL(LATGD,0:NODES-1)
!!     
      REAL(KIND=KIND_EVOD),INTENT(IN):: deltim,PHOUR
      REAL(KIND=KIND_EVOD),INTENT(INOUT):: ZHOUR
!JFE  REAL(KIND=KIND_EVOD),INTENT(IN):: DPHIBR,PHIBS
!JFE  REAL(KIND=KIND_EVOD),INTENT(IN):: LBASIY(4,2,LATS_NODE_EXT)
!JFE  REAL(KIND=KIND_EVOD),INTENT(IN):: PHI(LATS_NODE_EXT)
!JFE  REAL(KIND=KIND_EVOD),INTENT(IN):: DPHI(LATS_NODE_EXT)
!JFE  REAL(KIND=KIND_EVOD),INTENT(IN):: DLAM(LATS_NODE_EXT)
!JFE  REAL(KIND=KIND_EVOD),INTENT(IN):: LAM(LONFX,LATS_NODE_A+1)
!JFE  REAL(KIND=KIND_EVOD),INTENT(IN):: LAMEXT(LONFX,LATS_NODE_EXT)
!JFE  REAL(KIND=KIND_EVOD),INTENT(IN):: LAMMP(LONF,LEVS,LATS_NODE_A)
!JFE  REAL(KIND=KIND_EVOD),INTENT(IN):: PHIMP(LONF,LEVS,LATS_NODE_A)
!JFE  REAL(KIND=KIND_EVOD),INTENT(IN):: SIGMP(LONF,LEVS,LATS_NODE_A)
!!     
      INTEGER NBLCK
      REAL(KIND=KIND_EVOD) TRIE_LS(LEN_TRIE_LS,2,11*LEVS+3*LEVH+6)
      REAL(KIND=KIND_EVOD) TRIO_LS(LEN_TRIO_LS,2,11*LEVS+3*LEVH+6)
!!
      integer              ls_node(ls_dim,3)
!!
!     ls_node(1,1) ... ls_node(ls_max_node,1) : values of L
!     ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!     ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
!!
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
!$$$      INTEGER                LOTS,LOTD,LOTA
!$$$      PARAMETER            ( LOTS = 5*LEVS+1*LEVH+3 )
!$$$      PARAMETER            ( LOTD = 6*LEVS+2*LEVH+0 )
!$$$      PARAMETER            ( LOTA = 3*LEVS+1*LEVH+1 )
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
      REAL OZPLIN(LATSOZP,LEVOZP,pl_coeff,timeoz) !OZONE PL Coeff
      REAL (KIND=KIND_PHYS) PDRYINI
      REAL(KIND=KIND_EVOD) SLAG,SDEC,CDEC
!$$$      INTEGER   P_GZ,P_ZEM,P_DIM,P_TEM,P_RM,P_QM
!$$$      INTEGER   P_ZE,P_DI,P_TE,P_RQ,P_Q,P_DLAM,P_DPHI,P_ULN,P_VLN
!$$$      INTEGER   P_W,P_X,P_Y,P_RT,P_ZQ
!$$$      PARAMETER(P_GZ  = 0*LEVS+0*LEVH+1,  !      GZE/O(LNTE/OD,2),
!$$$     X          P_ZEM = 0*LEVS+0*LEVH+2,  !     ZEME/O(LNTE/OD,2,LEVS),
!$$$     X          P_DIM = 1*LEVS+0*LEVH+2,  !     DIME/O(LNTE/OD,2,LEVS),
!$$$     X          P_TEM = 2*LEVS+0*LEVH+2,  !     TEME/O(LNTE/OD,2,LEVS),
!$$$     X          P_RM  = 3*LEVS+0*LEVH+2,  !      RME/O(LNTE/OD,2,LEVH),
!$$$     X          P_QM  = 3*LEVS+1*LEVH+2,  !      QME/O(LNTE/OD,2),
!$$$     X          P_ZE  = 3*LEVS+1*LEVH+3,  !      ZEE/O(LNTE/OD,2,LEVS),
!$$$     X          P_DI  = 4*LEVS+1*LEVH+3,  !      DIE/O(LNTE/OD,2,LEVS),
!$$$     X          P_TE  = 5*LEVS+1*LEVH+3,  !      TEE/O(LNTE/OD,2,LEVS),
!$$$     X          P_RQ  = 6*LEVS+1*LEVH+3,  !      RQE/O(LNTE/OD,2,LEVH),
!$$$     X          P_Q   = 6*LEVS+2*LEVH+3,  !       QE/O(LNTE/OD,2),
!$$$     X          P_DLAM= 6*LEVS+2*LEVH+4,  !  DPDLAME/O(LNTE/OD,2),
!$$$     X          P_DPHI= 6*LEVS+2*LEVH+5,  !  DPDPHIE/O(LNTE/OD,2),
!$$$     X          P_ULN = 6*LEVS+2*LEVH+6,  !     ULNE/O(LNTE/OD,2,LEVS),
!$$$     X          P_VLN = 7*LEVS+2*LEVH+6,  !     VLNE/O(LNTE/OD,2,LEVS),
!$$$     X          P_W   = 8*LEVS+2*LEVH+6,  !       WE/O(LNTE/OD,2,LEVS),
!$$$     X          P_X   = 9*LEVS+2*LEVH+6,  !       XE/O(LNTE/OD,2,LEVS),
!$$$     X          P_Y   =10*LEVS+2*LEVH+6,  !       YE/O(LNTE/OD,2,LEVS),
!$$$     X          P_RT  =11*LEVS+2*LEVH+6,  !      RTE/O(LNTE/OD,2,LEVH),
!$$$     X          P_ZQ  =11*LEVS+3*LEVH+6)  !      ZQE/O(LNTE/OD,2)
      INTEGER   kdt,       IERR,J,K,L,LOCL,N
      real(kind=kind_evod)  ye1(levs)
      REAL(KIND=kind_mpi)  coef00m(LEVS,ntrac) ! temp. ozone clwater  
      REAL(KIND=kind_evod) coef00(LEVS,ntrac) ! temp. ozone clwater  
      INTEGER              INDLSEV,JBASEV
      INTEGER              INDLSOD,JBASOD
      integer iprint
      include 'function2'
      LOGICAL LSLAG,LSOUT,ex_out,wrt_g3d
      LOGICAL SPS

!
! timings
      real(kind=kind_evod) global_times_a(latg,nodes)
      real(kind=kind_evod) global_times_b(latr,nodes)
      real(kind=kind_evod) global_times_r(latr,nodes)
      integer tag,ireq1,ireq2
      real*8 rtc ,timer1,timer2
      integer i
!
!sela if (me.eq.0) PRINT 102,KDT
! 102 FORMAT(1H ,'KDT IN do_tstep  =',I6)

      SHOUR=SHOUR+deltim

!-> Coupling insertion
      call ATM_DBG2(kdt,PHOUR,ZHOUR,SHOUR,3)
      CALL ATM_TSTEP_INIT(kdt)
!<- Coupling insertion

!
!----------------------------------------------------------
      if (.NOT.LIOPE.or.icolor.ne.2) then
!!
      IF (LSLAG) THEN
!JFE    CALL GLOOPA_SL
!JFE &    (deltim,LAM,DLAM,LAMEXT,LATLOCAL,PHI,DPHI,DPHIBR,PHIBS,
!JFE &     LBASIY,LATSINPE,LAMMP,PHIMP,SIGMP,
!JFE &     TRIE_LS,TRIO_LS,
!JFE &     LS_NODE,LS_NODES,MAX_LS_NODES,
!JFE &     LATS_NODES_A,GLOBAL_LATS_A,
!JFE &     LONSPERLAT,
!JFE &     LATS_NODES_EXT,GLOBAL_LATS_EXT,
!JFE &     EPSE,EPSO,EPSEDN,EPSODN,
!JFE &     SNNP1EV,SNNP1OD,NDEXEV,NDEXOD,
!JFE &     PLNEV_A,PLNOD_A,PDDEV_A,PDDOD_A,PLNEW_A,PLNOW_A,
!JFE &     SYN_LS_A,DYN_LS_A,
!JFE &     SYN_GR_A_1,DYN_GR_A_1,ANL_GR_A_1,
!JFE &     SYN_GR_A_2,DYN_GR_A_2,ANL_GR_A_2)
      ELSE
      global_times_a=0.


        CALL GLOOPA
     X    (deltim,TRIE_LS,TRIO_LS,
     X     LS_NODE,LS_NODES,MAX_LS_NODES,
     X     LATS_NODES_A,GLOBAL_LATS_A,
     X     LONSPERLAT,
     X     EPSE,EPSO,EPSEDN,EPSODN,
     X     SNNP1EV,SNNP1OD,NDEXEV,NDEXOD,
     X     PLNEV_A,PLNOD_A,PDDEV_A,PDDOD_A,PLNEW_A,PLNOW_A,
     X     global_times_a,kdt)
       
!
!
         iprint = 0
          if (iprint .eq. 1) print*,' fin gloopa kdt = ',kdt
!
!my gather lat timings for load balancing
!sela    if (reshuff_lats_a .and. kdt .eq. 5) then
!sela    call redist_lats_a(kdt,global_times_a,
!selax                  lats_nodes_a,global_lats_a,
!selax                  lonsperlat,
!selax                  lats_nodes_ext,global_lats_ext,iprint)
!sela    endif
      ENDIF
!----------------------------------------------------------

!my set to zero for every timestep
       global_times_r = 0.0
!
!
      if(.not.adiab) then
        if (nscyc .gt. 0) then
          IF (mod(kdt,nscyc).eq.1) THEN
             CALL gcycle(me,LATS_NODE_R,LONSPERLAR,global_lats_r,
     &                  ipt_lats_node_r,idate,fhour,fhcyc,
     &                  XLON ,XLAT  , sfc_fld, ialb)
          ENDIF
        endif
!
        if (num_p3d .eq. 3) then        ! Ferrier Microphysics initialization
          call INIT_MICRO(DELTIM, levs, NGPTC*NBLCK*lats_node_r, num_p3d
     &,                   phy_f3d(1,1,1,1,1), fhour, me)
        endif
      endif
!
!-> Coupling insertion

  ! lgetSSTICE_cc must be defined by this moment. It used to be an argument
  ! to ATM_GETSST, accessible here via USE SURFACE_cc. Now it is defined in
  ! ATM_TSTEP_INIT called above, and the USE is removed. (Even in the earlier
  ! version lgetSSTICE_cc did not have to be an actual argumnent, since
  ! it is in the module SURFACE_cc USEd by ATM_GETSST.)

!     call ATM_GETSST(sfc_fld%TSEA,sfc_fld%SLMSK,sfc_fld%ORO)
      call ATM_GETSSTICE(sfc_fld%TSEA,sfc_fld%TISFC,sfc_fld%FICE,
     &                   sfc_fld%HICE,sfc_fld%SHELEG,sfc_fld%SLMSK,
     &                   sfc_fld%ORO,kdt)

!<- Coupling insertion


!
! XU LI: Update Tref after gcycle is called
!      when GFS is run seperately (not parallel run)
!
!      if ( nsst_active .and. .not. tr_analysis ) then
       if ( nsst_active) then
         do j = 1, lats_node_r
           do i = 1, lonr
             if (sfc_fld%slmsk(i,j) == 0 ) then
               sfc_fld%TSEA(i,j) = sfc_fld%TSEA(i,j)
     &               + nsst_fld%dt_warm(i,j) - nsst_fld%dt_cool(i,j)
             endif
           enddo
         enddo
       endif


!sela  if (me.eq.0) PRINT*,'COMPLETED GLOOPA IN do_tstep'
      if (lsswr .or. lslwr) then         ! Radiation Call!
       if(.not.adiab) then
        CALL GLOOPR
     X    (TRIE_LS,TRIO_LS,
     X     LS_NODE,LS_NODES,MAX_LS_NODES,
     X     LATS_NODES_R,GLOBAL_LATS_R,
     X     LONSPERLAR,
     X     EPSE,EPSO,EPSEDN,EPSODN,
     X     SNNP1EV,SNNP1OD,PLNEV_R,PLNOD_R,
     X     PDDEV_R,PDDOD_R,
!    X     SNNP1EV,SNNP1OD,NDEXEV,NDEXOD,
!    X     PLNEV_R,PLNOD_R,PDDEV_R,PDDOD_R,PLNEW_R,PLNOW_R,
     X     PHOUR,
     &     XLON,XLAT,COSZDG,flx_fld%COSZEN,
     &     sfc_fld%SLMSK,sfc_fld%SHELEG,sfc_fld%SNCOVR,sfc_fld%SNOALB,
     &     sfc_fld%ZORL,sfc_fld%TSEA, HPRIME,SFALB,
     &     sfc_fld%ALVSF,sfc_fld%ALNSF,sfc_fld%ALVWF ,sfc_fld%ALNWF,
     &     sfc_fld%FACSF,sfc_fld%FACWF,sfc_fld%CV,sfc_fld%CVT ,
     &     sfc_fld%CVB,SWH,HLW,flx_fld%SFCNSW,flx_fld%SFCDLW,
     &     sfc_fld%FICE,sfc_fld%TISFC,flx_fld%SFCDSW,
     &     flx_fld%TSFLW,FLUXR,phy_f3d,SLAG,SDEC,CDEC,NBLCK,KDT,
     &     global_times_r)
           if (iprint .eq. 1) print*,' me = fin gloopr ',me
!     print *,' after gloopr sdec=',sdec,' cdec=',cdec
       ENDIF !not.adiab
      ENDIF
!!
      if( .not. explicit ) then						! hmhj
!
      if( gen_coord_hybrid ) then                                       ! hmhj


!$omp parallel do private(locl)
      do locl=1,ls_max_node                                             ! hmhj
         call sicdife_hyb_gc(trie_ls(1,1,P_dim), trie_ls(1,1,P_tem),    ! hmhj
     x                       trie_ls(1,1,P_qm ), trie_ls(1,1,P_x  ),    ! hmhj
     x                       trie_ls(1,1,P_y  ), trie_ls(1,1,P_zq ),    ! hmhj
     x                       trie_ls(1,1,P_di ), trie_ls(1,1,P_te ),    ! hmhj
     x                       trie_ls(1,1,P_q  ),deltim,                 ! hmhj
     x                       trie_ls(1,1,P_uln), trie_ls(1,1,P_vln),    ! hmhj
     x                       ls_node,snnp1ev,ndexev,locl)               ! hmhj

         call sicdifo_hyb_gc(trio_ls(1,1,P_dim), trio_ls(1,1,P_tem),    ! hmhj
     x                       trio_ls(1,1,P_qm ), trio_ls(1,1,P_x  ),    ! hmhj
     x                       trio_ls(1,1,P_y  ), trio_ls(1,1,P_zq ),    ! hmhj
     x                       trio_ls(1,1,P_di ), trio_ls(1,1,P_te ),    ! hmhj
     x                       trio_ls(1,1,P_q  ),deltim,                 ! hmhj
     x                       trio_ls(1,1,P_uln), trio_ls(1,1,P_vln),    ! hmhj
     x                       ls_node,snnp1od,ndexod,locl)               ! hmhj
      enddo                                                             ! hmhj

      else if(hybrid)then                                               ! hmhj

!$omp parallel do private(locl)
      do locl=1,ls_max_node
         call sicdife_hyb(trie_ls(1,1,P_dim), trie_ls(1,1,P_tem),
     x                    trie_ls(1,1,P_qm ), trie_ls(1,1,P_x  ),
     x                    trie_ls(1,1,P_y  ), trie_ls(1,1,P_zq ),
     x                    trie_ls(1,1,P_di ), trie_ls(1,1,P_te ),
     x                    trie_ls(1,1,P_q  ),deltim,
     x                    trie_ls(1,1,P_uln), trie_ls(1,1,P_vln),
     x                    ls_node,snnp1ev,ndexev,locl)

         call sicdifo_hyb(trio_ls(1,1,P_dim), trio_ls(1,1,P_tem),
     x                    trio_ls(1,1,P_qm ), trio_ls(1,1,P_x  ),
     x                    trio_ls(1,1,P_y  ), trio_ls(1,1,P_zq ),
     x                    trio_ls(1,1,P_di ), trio_ls(1,1,P_te ),
     x                    trio_ls(1,1,P_q  ),deltim,
     x                    trio_ls(1,1,P_uln), trio_ls(1,1,P_vln),
     x                    ls_node,snnp1od,ndexod,locl)
      enddo

      else ! hybrid

!$omp parallel do private(locl)
      do locl=1,ls_max_node
         CALL SICDIFE_sig(TRIE_LS(1,1,P_DIM), TRIE_LS(1,1,P_TEM),
     X                    TRIE_LS(1,1,P_QM ), TRIE_LS(1,1,P_X  ),
     X                    TRIE_LS(1,1,P_Y  ), TRIE_LS(1,1,P_ZQ ),
     X                    AM,BM,TOV,SV,deltim,
     X                    TRIE_LS(1,1,P_ULN), TRIE_LS(1,1,P_VLN),
     X                    LS_NODE,SNNP1EV,NDEXEV,locl,TRIE_LS(1,1,P_DI))

         CALL SICDIFO_sig(TRIO_LS(1,1,P_DIM), TRIO_LS(1,1,P_TEM),
     X                    TRIO_LS(1,1,P_QM ), TRIO_LS(1,1,P_X  ),  
     X                    TRIO_LS(1,1,P_Y  ), TRIO_LS(1,1,P_ZQ ),
     X                    AM,BM,TOV,SV,deltim,
     X                    TRIO_LS(1,1,P_ULN), TRIO_LS(1,1,P_VLN),
     X                    LS_NODE,SNNP1OD,NDEXOD,locl,TRIO_LS(1,1,P_DI))
      enddo
      endif ! hybrid

      endif 		! not explicit					! hmhj

!
!----------------------------------------------------------
!sela if (.NOT.LIOPE.or.icolor.ne.2) then
!sela   print*,'liope=',liope,' icolor=',icolor,' after loopa'
!sela   CALL RMS_spect(TRIE_LS(1,1,P_zq ), TRIE_LS(1,1,P_x  ),
!selaX             TRIE_LS(1,1,P_y  ), TRIE_LS(1,1,P_w  ),
!selaX             TRIE_LS(1,1,P_Rt ),
!selaX             TRIO_LS(1,1,P_zq ), TRIO_LS(1,1,P_x  ),
!selaX             TRIO_LS(1,1,P_y  ), TRIO_LS(1,1,P_w  ),
!selaX             TRIO_LS(1,1,P_Rt ),
!selaX             LS_NODES,MAX_LS_NODES)
!sela endif
!----------------------------------------------------------
! hmhj compute coef00 for all, even for hybrid mode
      coef00(:,:) = 0.0
      IF ( ME .EQ. ME_L_0 ) THEN
        DO LOCL=1,LS_MAX_NODE
          l=ls_node(locl,1)
          jbasev=ls_node(locl,2)
          IF ( L .EQ. 0 ) THEN
            N=0
! 1 Corresponds to temperature,  2 corresponds to ozon, 3 to clwater
            DO K=1,LEVS
              coef00(K,1) = TRIE_LS(INDLSEV(N,L),1,P_Y +K-1)
!             if (ntoz .gt. 1) then
              if (ntoz .gt. 1 .and.                                     ! hmhj
     &            .not. (hybrid.or.gen_coord_hybrid)) then              ! hmhj
                coef00(K,ntoz) = TRIE_LS(INDLSEV(N,L),1,
     &                                   (ntoz-1)*levs+P_rt+K-1)
              endif
            ENDDO
          ENDIF
        END DO
      END IF
      coef00m = coef00
      CALL MPI_BCAST(coef00m,levs*ntrac,MPI_R_MPI,ME_L_0,MC_COMP,IERR)
      coef00=coef00m
      if( gen_coord_hybrid ) then                                       ! hmhj
        call updown_gc(sl,coef00(1,1))                                  ! hmhj
      else                                                              ! hmhj
        call updown(sl,coef00(1,1))
      endif                                                             ! hmhj
!     if (ntoz .gt. 1) call updown(sl,coef00(1,ntoz))
      if (ntoz .gt. 1 .and. .not. (hybrid.or.gen_coord_hybrid)) then    ! hmhj
               call updown(sl,coef00(1,ntoz))
      endif

!
!$omp parallel do shared(TRIE_LS,TRIO_LS)
!$omp+shared(deltim,SL,LS_NODE,coef00,hybrid)
      do k=1,levs
         CALL deldifs(TRIE_LS(1,1,P_RT+k-1), TRIE_LS(1,1,P_W+k-1),
     X                TRIE_LS(1,1,P_QM    ), TRIE_LS(1,1,P_X+k-1),
     X                TRIE_LS(1,1,P_Y +k-1), TRIE_LS(1,1,P_TEM+k-1),    ! hmhj
     X                TRIO_LS(1,1,P_RT+k-1), TRIO_LS(1,1,P_W+k-1),
     X                TRIO_LS(1,1,P_QM    ), TRIO_LS(1,1,P_X+k-1),
     X                TRIO_LS(1,1,P_Y +k-1), TRIO_LS(1,1,P_TEM+k-1),    ! hmhj
     X                deltim,SL,LS_NODE,coef00,k,hybrid,                ! hmhj
     &                gen_coord_hybrid)                                 ! hmhj
      enddo
!
!
!-------------------------------------------
      if(.not.lsfwd)then
!-------------------------------------------
      CALL FILTR1EO(TRIE_LS(1,1,P_TEM), TRIE_LS(1,1,P_TE ),
     X              TRIE_LS(1,1,P_Y  ), TRIE_LS(1,1,P_DIM),
     X              TRIE_LS(1,1,P_DI ), TRIE_LS(1,1,P_X  ),
     X              TRIE_LS(1,1,P_ZEM), TRIE_LS(1,1,P_ZE ),
     X              TRIE_LS(1,1,P_W  ), TRIE_LS(1,1,P_RM ),
     X              TRIE_LS(1,1,P_RQ ), TRIE_LS(1,1,P_RT ),
     X              TRIO_LS(1,1,P_TEM), TRIO_LS(1,1,P_TE ),
     X              TRIO_LS(1,1,P_Y  ), TRIO_LS(1,1,P_DIM),
     X              TRIO_LS(1,1,P_DI ), TRIO_LS(1,1,P_X  ),
     X              TRIO_LS(1,1,P_ZEM), TRIO_LS(1,1,P_ZE ),
     X              TRIO_LS(1,1,P_W  ), TRIO_LS(1,1,P_RM ),
     X              TRIO_LS(1,1,P_RQ ), TRIO_LS(1,1,P_RT ),
     X              FILTA,LS_NODE)

      CALL countperf(0,13,0.)
      DO J=1,LEN_TRIE_LS
         TRIE_LS(J,1,P_QM)=TRIE_LS(J,1,P_Q )
         TRIE_LS(J,2,P_QM)=TRIE_LS(J,2,P_Q )
         TRIE_LS(J,1,P_Q )=TRIE_LS(J,1,P_ZQ)
         TRIE_LS(J,2,P_Q )=TRIE_LS(J,2,P_ZQ)
      ENDDO
      DO J=1,LEN_TRIO_LS
         TRIO_LS(J,1,P_QM)=TRIO_LS(J,1,P_Q )
         TRIO_LS(J,2,P_QM)=TRIO_LS(J,2,P_Q )
         TRIO_LS(J,1,P_Q )=TRIO_LS(J,1,P_ZQ)
         TRIO_LS(J,2,P_Q )=TRIO_LS(J,2,P_ZQ)
      ENDDO
      CALL countperf(1,13,0.)

!-------------------------------------------
      else
!-------------------------------------------
      CALL countperf(0,13,0.)
      DO J=1,LEN_TRIE_LS
         TRIE_LS(J,1,P_Q)=TRIE_LS(J,1,P_ZQ)
         TRIE_LS(J,2,P_Q)=TRIE_LS(J,2,P_ZQ)
      ENDDO
      DO J=1,LEN_TRIO_LS
         TRIO_LS(J,1,P_Q)=TRIO_LS(J,1,P_ZQ)
         TRIO_LS(J,2,P_Q)=TRIO_LS(J,2,P_ZQ)
      ENDDO
      CALL countperf(1,13,0.)
!-------------------------------------------
      endif
!
!-------------------------------------------
           if (iprint .eq. 1) print*,' me = beg gloopb ',me
!my set to zero for every timestep
       global_times_b = 0.0

      if(.not.adiab) then

      CALL GLOOPB
     X    (TRIE_LS,TRIO_LS,
     X     LS_NODE,LS_NODES,MAX_LS_NODES,
     X     LATS_NODES_R,GLOBAL_LATS_R,
     X     LONSPERLAR,
     X     EPSE,EPSO,EPSEDN,EPSODN,
     X     SNNP1EV,SNNP1OD,NDEXEV,NDEXOD,
     X     PLNEV_R,PLNOD_R,PDDEV_R,PDDOD_R,PLNEW_R,PLNOW_R,
     &     deltim,PHOUR, sfc_fld, flx_fld, nsst_fld, SFALB,
     &     XLON  ,
     &     SWH,HLW,HPRIME,SLAG,SDEC,CDEC,
     &     OZPLIN,JINDX1,JINDX2,DDY,PDRYINI,
     &     phy_f3d,  phy_f2d, XLAT,NBLCK,KDT,
     &     global_times_b)
!
!     if (kdt .eq. 1) call mpi_quit(222)
!sela if (iprint .eq. 1) stop
!!
!       if (nscyc .gt. 0) then
!         IF (mod(kdt,nscyc).eq.0) THEN
!            CALL gcycle(me,LATS_NODE_R,LONSPERLAR,global_lats_r,
!    &                  ipt_lats_node_r,idate,fhour,fhcyc,
!    &                  XLON ,XLAT  , sfc_fld)
!         ENDIF
!       endif
       endif ! not.adiab
!
!$omp parallel do shared(TRIE_LS,NDEXEV,TRIO_LS,NDEXOD)
!$omp+shared(SL,SPDMAX,deltim,LS_NODE)
      do k=1,levs
         CALL damp_speed(TRIE_LS(1,1,P_X+k-1), TRIE_LS(1,1,P_W +k-1),
     X                   TRIE_LS(1,1,P_Y+k-1), TRIE_LS(1,1,P_RT+k-1),
     X                   NDEXEV,
     X                   TRIO_LS(1,1,P_X+k-1), TRIO_LS(1,1,P_W +k-1),
     X                   TRIO_LS(1,1,P_Y+k-1), TRIO_LS(1,1,P_RT+k-1),
     X                   NDEXOD,
     X                   SL,SPDMAX(k),deltim,LS_NODE)
      enddo
!
!--------------------------------------------
      if(.not.lsfwd)then
!--------------------------------------------
      CALL FILTR2EO(TRIE_LS(1,1,P_TEM), TRIE_LS(1,1,P_TE ),
     X              TRIE_LS(1,1,P_Y  ), TRIE_LS(1,1,P_DIM),
     X              TRIE_LS(1,1,P_DI ), TRIE_LS(1,1,P_X  ),
     X              TRIE_LS(1,1,P_ZEM), TRIE_LS(1,1,P_ZE ),
     X              TRIE_LS(1,1,P_W  ), TRIE_LS(1,1,P_RM ),
     X              TRIE_LS(1,1,P_RQ ), TRIE_LS(1,1,P_RT ),
     X              TRIO_LS(1,1,P_TEM), TRIO_LS(1,1,P_TE ),
     X              TRIO_LS(1,1,P_Y  ), TRIO_LS(1,1,P_DIM),
     X              TRIO_LS(1,1,P_DI ), TRIO_LS(1,1,P_X  ),
     X              TRIO_LS(1,1,P_ZEM), TRIO_LS(1,1,P_ZE ),
     X              TRIO_LS(1,1,P_W  ), TRIO_LS(1,1,P_RM ),
     X              TRIO_LS(1,1,P_RQ ), TRIO_LS(1,1,P_RT ),
     X              FILTA,LS_NODE)
!--------------------------------------------
      else
!--------------------------------------------
      CALL countperf(0,13,0.)
      DO K=1,LEVS
      DO J=1,LEN_TRIE_LS
         TRIE_LS(J,1,P_DI+K-1)=TRIE_LS(J,1,P_X+K-1)
         TRIE_LS(J,2,P_DI+K-1)=TRIE_LS(J,2,P_X+K-1)
         TRIE_LS(J,1,P_ZE+K-1)=TRIE_LS(J,1,P_W+K-1)
         TRIE_LS(J,2,P_ZE+K-1)=TRIE_LS(J,2,P_W+K-1)
         TRIE_LS(J,1,P_TE+K-1)=TRIE_LS(J,1,P_Y+K-1)
         TRIE_LS(J,2,P_TE+K-1)=TRIE_LS(J,2,P_Y+K-1)
      ENDDO
      ENDDO
      DO K=1,LEVS
      DO J=1,LEN_TRIO_LS
         TRIO_LS(J,1,P_DI+K-1)=TRIO_LS(J,1,P_X+K-1)
         TRIO_LS(J,2,P_DI+K-1)=TRIO_LS(J,2,P_X+K-1)
         TRIO_LS(J,1,P_ZE+K-1)=TRIO_LS(J,1,P_W+K-1)
         TRIO_LS(J,2,P_ZE+K-1)=TRIO_LS(J,2,P_W+K-1)
         TRIO_LS(J,1,P_TE+K-1)=TRIO_LS(J,1,P_Y+K-1)
         TRIO_LS(J,2,P_TE+K-1)=TRIO_LS(J,2,P_Y+K-1)
      ENDDO
      ENDDO
      DO K=1,LEVH
      DO J=1,LEN_TRIE_LS
         TRIE_LS(J,1,P_RQ+K-1)=TRIE_LS(J,1,P_RT+K-1)
         TRIE_LS(J,2,P_RQ+K-1)=TRIE_LS(J,2,P_RT+K-1)
      ENDDO
      ENDDO
      DO K=1,LEVH
      DO J=1,LEN_TRIO_LS
         TRIO_LS(J,1,P_RQ+K-1)=TRIO_LS(J,1,P_RT+K-1)
         TRIO_LS(J,2,P_RQ+K-1)=TRIO_LS(J,2,P_RT+K-1)
      ENDDO
      ENDDO
      CALL countperf(1,13,0.)
!--------------------------------------------
      endif
!     if (kdt .eq. 2) call mpi_quit(444)
!!
      endif !.NOT.LIOPE.or.icolor.ne.2
!--------------------------------------------
!--------------------------------------------
!$$$      ex_out=.true.     !    output
!$$$      if(ex_out) then    
!--------------------------------------------
!--------------------------------------------
      IF (lsout) THEN
!!
      CALL f_hpmstart(32,"TWRITEEO")
!!
      CALL countperf(0,18,0.)
!
      wrt_g3d = MOD(kdt ,nsout) == 0 .or. phour == 0.
      CALL WRTOUT(PHOUR,FHOUR,ZHOUR,IDATE,
     X            TRIE_LS,TRIO_LS,
     X            SL,SI,
     X            ls_node,LS_NODES,MAX_LS_NODES,
     &            sfc_fld, flx_fld, nsst_fld,
     &            fluxr,pdryini,
     &            lats_nodes_r,global_lats_r,lonsperlar,nblck,
     &            COLAT1,CFHOUR1,pl_coeff,
     &            epsedn,epsodn,snnp1ev,snnp1od,plnev_r,plnod_r,
     &            plnew_r,plnow_r,'SIG.F','SFC.F','FLX.F',wrt_g3d)


!     endif
!
      CALL f_hpmstop(32)
!!
      CALL countperf(1,18,0.)
!!
!!
        IF (mod(kdt,nsres).eq.0 .and. (.not. SPS)) THEN
!!
!     if(.not.adiab)then
          CALL wrt_restart(TRIE_LS,TRIO_LS,
     &       sfc_fld, nsst_fld,
     &       SI,SL,fhour,idate,
     &       igen,pdryini,
     x       ls_node,ls_nodes,max_ls_nodes,
     &       global_lats_r,lonsperlar,SNNP1EV,SNNP1OD,
     &       phy_f3d, phy_f2d, ngptc, nblck, adiab, ens_nam,
     &       nsst_active,'SIGR1','SIGR2','SFCR','NSSTR')
!
!       endif !not.adiab
        ENDIF
        ENDIF ! if ls_out
!my
      if (reshuff_lats_r) then  
      if (.NOT.LIOPE.or.icolor.ne.2) then

!my gather lat timings for load balancing
       ifshuff = .false.

!sela    if (fhdfi .eq. 0 .and. lsout) then
!sela  call redist_lats_r_b(kdt,global_times_r,global_times_b,
!selax                  lats_nodes_r,global_lats_r,
!selax                  lats_nodes_r_old,global_lats_r_old,
!selax                  lonsperlar,ifshuff,iprint)
!sela  if (iprint .eq. 1) print*,' after redist_lats_r_b, ifshuff = ',
!sela.     ifshuff
!sela  endif ! fhdfi
!
!sela  if (ifshuff) then

!$$$      CALL LONLAT_PARA(GLOBAL_LATS_R,XLON,XLAT,LONSPERLAR)

!sela  timer1=rtc()
!sela call shuffle_surface (
!sela&                 XLON,XLAT,COSZDG,COSZEN,
!sela&                 SLMSK,SHELEG,ZORL ,TSEA,
!sela&     ALVSF,ALNSF ,ALVWF ,ALNWF,FACSF ,FACWF,CV    ,CVT ,
!sela&     CVB  ,HPRIME,STC,SWH,HLW,ORO,SFCNSW,SFCDLW,
!sela&     TSFLW,FLUXR ,CLDCOV,TMPMAX,TMPMIN,BENGSH,PSMEAN,
!sela&     DVGWD,U10M,PSURF,V10M,RUNOFF,ULWSFC,DUGWD,CLDWRK,
!sela&     EP,GFLUX,DTSFC,DUSFC,DVSFC,DQSFC,DLWSFC,GESHEM,
!sela&     STYPE,UUSTAR,F10M,VTYPE,FFMM,FFHH,SMC,CANOPY,TG3,VFRAC,
!sela&     T2M,HPBL,Q2M,
!sela&     POZ,OZPRDIN,OZDISIN,JINDX1,JINDX2,DDY,PDRYINI,
!sela&     DT3DT, DQ3DT, DU3DT, DV3DT,
!sela&     TPS,  QPS, PSP, TPS1, QPS1, PSP1,NBLCK,
!selax     global_lats_r_old, global_lats_r,
!selax     lats_nodes_r_old,  lats_nodes_r)
!sela
!sela CALL SETINDXOZ(LATS_NODE_R,LATS_NODE_R,GLOBAL_LATS_R,
!sela&               JINDX1,JINDX2,DDY)
!sela     timer2=rtc()
!sela   print*,' overhead for reshuffling = ',timer2-timer1          

!sela  print*,' after redist_lats_r_b, ifshuff = ',
!sela.     ifshuff
!sela  write(6,*) ' after shuffle_surface new global_lats_r = '
!sela  write(6,830)     global_lats_r
 830   format(15(i3,1x))
!sela
!sela  endif ! ifshuff
      ENDIF ! .NOT.LIOPE.or.icolor.ne.2
!
!$$$       if (me .eq. 0 .and. ifshuff .eq. true.) 
!my could send ifshuff to io node and check whether to send global_lats_r array 
!
!my broadcast new global_lats_r to io node
!
        tag = kdt
        print*,' me, nodes,kdt = ',me,nodes,kdt
        if (me .eq. 0) then
         print*,' before mpi_isend ****sending global_lats_r = ',
     .      global_lats_r
      CALL MPI_isend(global_lats_r,latr,MPI_INTEGER,
     X               nodes,tag,MPI_COMM_ALL,ireq1,IERR)
           elseif (liope .and. icolor .eq. 2) then
         print*, ' before mpi_irecv global_lats_r for io node = ',
     .     global_lats_r
      CALL MPI_irecv(global_lats_r,latr,MPI_INTEGER,
     X               0,tag,MPI_COMM_ALL,ireq2,IERR)     

        endif
        call mpi_barrier(MPI_COMM_ALL,ierr)
         if (liope .and. icolor .eq. 2) 
     .    print*,' after mpi_irecv global_lats_r for io node = ',
     .     global_lats_r

      endif ! reshuff_lats_r
!
      IF (mod(kdt,nszer).eq.0 .and. lsout) THEN
        call flx_init(flx_fld,ierr)
        zhour = fhour
        FLUXR = 0.
!
        if (ldiag3d .or. lggfs3d) then
          call d3d_zero(ldiag3d,lggfs3d)
          if (fhour >= fhgoc3d) lggfs3d = .false.
        endif
      ENDIF
!
!$$$      ENDIF ! ex out

! Coupling insertion->
      CALL ATM_SENDFLUXES(sfc_fld%SLMSK)
!<- Coupling insertion

      RETURN
      END
