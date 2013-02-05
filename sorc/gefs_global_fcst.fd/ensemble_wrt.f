      SUBROUTINE ensemble_wrt(kdt,PHOUR,
     &                 TRIE_LS,TRIO_LS,
     &                 LS_NODE,LS_NODES,MAX_LS_NODES,
     &                 LATS_NODES_R,GLOBAL_LATS_R,LONSPERLAR,
     &                 EPSEDN,EPSODN,SNNP1EV,SNNP1OD,
     &                 PLNEV_R,PLNOD_R,PDDEV_R,PDDOD_R,
     &                 PLNEW_R,PLNOW_R,
     &                 XLON,XLAT,sfc_fld, flx_fld, nsst_fld,
     &                 FLUXR,PDRYINI, phy_f3d,  phy_f2d, NBLCK,
     &                 ZHOUR,LSOUT,COLAT1,CFHOUR1)
!
!#include "f_hpm.h"
      use resol_def
      use layout1
      use gg_def
      use vert_def
      use date_def
      use namelist_def
      use mpi_def
      use ozne_def
      use Sfc_Flx_ESMFMod
      use Nsstm_ESMFMod
      use d3d_def
!
      IMPLICIT NONE
!!     
      TYPE(Sfc_Var_Data)                :: sfc_fld
      TYPE(Flx_Var_Data)                :: flx_fld
      TYPE(Nsst_Var_Data)               :: nsst_fld
      CHARACTER(16)                     :: CFHOUR1
!!     
      REAL(KIND=KIND_EVOD),INTENT(IN)   :: PHOUR
      REAL(KIND=KIND_EVOD),INTENT(INOUT):: ZHOUR
!!     
      INTEGER NBLCK
      REAL(KIND=KIND_EVOD) TRIE_LS(LEN_TRIE_LS,2,11*LEVS+3*LEVH+6)
      REAL(KIND=KIND_EVOD) TRIO_LS(LEN_TRIO_LS,2,11*LEVS+3*LEVH+6)
!
      integer              ls_node(ls_dim,3)
!
!     ls_node(1,1) ... ls_node(ls_max_node,1) : values of L
!     ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!     ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
!
      INTEGER          LS_NODES(LS_DIM,NODES)
      INTEGER          MAX_LS_NODES(NODES),      LATS_NODES_R(NODES)
      INTEGER          GLOBAL_LATS_R(LATR),  LONSPERLAR(LATR)
!
      real(kind=kind_evod) colat1
      REAL(KIND=KIND_EVOD)  EPSEDN(LEN_TRIE_LS)
      REAL(KIND=KIND_EVOD)  EPSODN(LEN_TRIO_LS)
      REAL(KIND=KIND_EVOD) SNNP1EV(LEN_TRIE_LS)
      REAL(KIND=KIND_EVOD) SNNP1OD(LEN_TRIO_LS)
      REAL(KIND=KIND_EVOD)   PLNEV_R(LEN_TRIE_LS,LATR2)
      REAL(KIND=KIND_EVOD)   PLNOD_R(LEN_TRIO_LS,LATR2)
      REAL(KIND=KIND_EVOD)   PDDEV_R(LEN_TRIE_LS,LATR2)
      REAL(KIND=KIND_EVOD)   PDDOD_R(LEN_TRIO_LS,LATR2)
      REAL(KIND=KIND_EVOD)   PLNEW_R(LEN_TRIE_LS,LATR2)
      REAL(KIND=KIND_EVOD)   PLNOW_R(LEN_TRIO_LS,LATR2)
!!     
      REAL (KIND=KIND_RAD) XLON(LONR,LATS_NODE_R)
      REAL (KIND=KIND_RAD) XLAT(LONR,LATS_NODE_R)

      REAL (kind=kind_phys)
     &     phy_f3d(NGPTC,LEVS,NBLCK,lats_node_r,num_p3d),
     &     phy_f2d(lonr,lats_node_r,num_p2d)
      REAL (KIND=KIND_RAD) FLUXR(nfxr,LONR,LATS_NODE_R)
!!     
      REAL (KIND=KIND_PHYS) PDRYINI
      INTEGER   kdt,       J,K,L,N
      real(kind=kind_evod)  ye1(levs)
      integer iprint
      LOGICAL LSOUT

!
!DHOU 05-29-2008, This subroutine is adapted from do_tstep 
!      Only the output calls are retained with modified control (if)
!      This routine is called in at begining of GFS_Run only if SPS=.true.
!----------------------------------------------------------
      IF (lsout) THEN
!
      CALL f_hpmstart(32,"TWRITEEO")
!
      CALL countperf(0,18,0.)
!
      CALL WRTOUT(PHOUR,FHOUR,ZHOUR,IDATE,
     X            TRIE_LS,TRIO_LS,
     X            SL,SI,
     X            ls_node,LS_NODES,MAX_LS_NODES,
     &            sfc_fld, flx_fld, nsst_fld,
     &            fluxr,pdryini,
     &            lats_nodes_r,global_lats_r,lonsperlar,nblck,
     &            COLAT1,CFHOUR1,pl_coeff,
     &            epsedn,epsodn,snnp1ev,snnp1od,plnev_r,plnod_r,
     &            plnew_r,plnow_r,
     &           'SIG.S','SFB.S','FLX.S') ! DHOU, 02-29-2008 Added file names as argument
!
      CALL f_hpmstop(32)
      ENDIF ! if ls_out
!
      CALL countperf(1,18,0.)
!!
      IF (mod(kdt,nsres).eq.0) THEN
        CALL wrt_restart(TRIE_LS,TRIO_LS,
     &     sfc_fld, nsst_fld,
     &     SI,SL,fhour,idate,
     &     igen,pdryini,
     x     ls_node,ls_nodes,max_ls_nodes,
     &     global_lats_r,lonsperlar,SNNP1EV,SNNP1OD,
     &     phy_f3d, phy_f2d, ngptc, nblck, adiab, ens_nam,
     &     nsst_active,
     &     'SIGS1','SIGS2','SFCS','NSSTS')  ! DHOU, 02-29-2008 Added file names as argument

      ENDIF
!
      RETURN
      END
