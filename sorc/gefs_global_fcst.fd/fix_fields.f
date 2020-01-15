
      SUBROUTINE fix_fields(
     &                  LONSPERLAR,GLOBAL_LATS_R,XLON,XLAT,sfc_fld,
     &                  nsst_fld,HPRIME,JINDX1,JINDX2,DDY,OZPLIN,
     &                  CREAD,CREAD_NSST)
!!     
      use machine , only : kind_rad
      use funcphys                         
      use module_progtm             
      use resol_def
      use namelist_def
      use layout1
      use ozne_def
      use Sfc_Flx_ESMFMod
      use Nsstm_ESMFMod
      IMPLICIT NONE
!!     
      TYPE(Sfc_Var_Data)        :: sfc_fld
      TYPE(Nsst_Var_Data)       :: nsst_fld
      INTEGER NREAD, NREAD_NSST
      CHARACTER (len=*)   :: CREAD
      CHARACTER (len=*)   :: CREAD_NSST
      INTEGER JINDX1(LATS_NODE_R),JINDX2(LATS_NODE_R)
      REAL (KIND=KIND_RAD) DDY(LATS_NODE_R)
      REAL (KIND=KIND_RAD) HPRIME(NMTVR,LONR,LATS_NODE_R)

      INTEGER IOZONDP
      REAL (kind=kind_rad) OZPLIN(LATSOZP,LEVOZP,pl_coeff,timeoz)
     &,                    XLON(LONR,LATS_NODE_R)
     &,                    XLAT(LONR,LATS_NODE_R)
       
      INTEGER              GLOBAL_LATS_R(LATR)
      INTEGER                 LONSPERLAR(LATR)
      real, PARAMETER:: RLAPSE=0.65E-2
      integer needoro, i, j
!!     
      call gfuncphys
      if (lsm == 0) then ! For OSU LSM
         CALL GRDDF
         CALL GRDKT
      endif
!!     
      IOZONDP = 0
      if (ntoz .gt. 0) IOZONDP = 1
      NREAD   = 14
!     CREAD   = 'fort.14'
      sfc_fld%ORO     = 0.
      NEEDORO = 0
      if(.not.adiab)then
        if (fhini .eq. fhrot) then
          if (me .eq. 0) print *,' call read_sfc CREAD=',cread
          CALL read_sfc(sfc_fld,NEEDORO,NREAD,
     &                  CREAD,GLOBAL_LATS_R,LONSPERLAR)

          if ( nsst_active ) then
            NREAD_NSST   = 15
            if (me .eq. 0) print *,' call read_nsst nsst_restart : ',
     &                               nsst_restart
            nsst_fld%slmsk = sfc_fld%slmsk
            if ( nsst_restart ) then
              CALL read_nsst(nsst_fld,NREAD_NSST,CREAD_NSST,
     &                      GLOBAL_LATS_R,LONSPERLAR)
              do j = 1, lats_node_r
                do i = 1, lonr
                  if ( sfc_fld%SLMSK(i,j) == 0.0 ) then
                    sfc_fld%TSEA(i,j) = nsst_fld%Tref(i,j)
     &                  + nsst_fld%dt_warm(i,j) - nsst_fld%dt_cool(i,j)
     &                  - sfc_fld%oro(i,j) * rlapse
                  endif
                enddo
              enddo
            elseif ( .not. nsst_restart ) then
              CALL set_nsst(sfc_fld%tsea,nsst_fld)
            endif                        ! if ( nsst_restart ) then
          endif
        else
          if (me .eq. 0) print *,' call read_sfc_r CREAD=',cread
          CALL read_sfc_r(sfc_fld,NEEDORO,NREAD,
     &                    CREAD,GLOBAL_LATS_R,LONSPERLAR)

          if ( nsst_active ) then
            nsst_fld%slmsk = sfc_fld%slmsk
            NREAD_NSST   = 15
            if (me .eq. 0) print *,' call read_nsst_r CREAD=',cread_nsst
            CALL read_nsst_r(nsst_fld,NREAD_NSST,CREAD_NSST,
     &                      GLOBAL_LATS_R,LONSPERLAR)
          endif
        endif
      endif
      NEEDORO=1
      CALL read_mtn_hprim_oz(sfc_fld%SLMSK,HPRIME,NEEDORO,sfc_fld%ORO,
     &     IOZONDP,OZPLIN, GLOBAL_LATS_R,LONSPERLAR)
!      
      CALL SETINDXOZ(LATS_NODE_R,LATS_NODE_R,GLOBAL_LATS_R,
     &               JINDX1,JINDX2,DDY)
!      
      CALL LONLAT_PARA(GLOBAL_LATS_R,XLON,XLAT,LONSPERLAR)
!!     
      RETURN
      END
