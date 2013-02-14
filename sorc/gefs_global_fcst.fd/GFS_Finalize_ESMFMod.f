!
! !MODULE: GFS_Finalize_ESMFMod --- Finalize module of the ESMF grided
!                                   component of the GFS system.
!
! !DESCRIPTION: GFS finalize module.
!
! !REVISION HISTORY:
!
!  November 2004      Weiyu Yang Initial code.
!  February 2006      Shrinivas Moorthi - removed some comments
!
!
! !INTERFACE:
!
 MODULE GFS_Finalize_ESMFMod
!
!!USES:
!
 USE GFS_InternalState_ESMFMod
 use gfsio_module , only : gfsio_finalize

 IMPLICIT none

 CONTAINS

 SUBROUTINE GFS_Finalize(gcGFS, gis, rc)

 TYPE(ESMF_GridComp),               INTENT(inout) :: gcGFS
 TYPE(GFS_InternalState)                          :: gis
 INTEGER, OPTIONAL,                 INTENT(out)   :: rc

 INTEGER                           :: rc1 = ESMF_SUCCESS
!
!***********************************************************************
!
      IF (me.eq.0) THEN
      CALL countperf(0,17,0.)
        write(*,*)'Final values'
        write(*,*)'************'
        CALL bar3(gis%trie_ls(1,1,gis%P_ze),                   &
                  gis%trio_ls(1,1,gis%P_ze),'ze ',levs)
        CALL bar3(gis%trie_ls(1,1,gis%P_di),                   &
                  gis%trio_ls(1,1,gis%P_di),'di ',levs)
        CALL bar3(gis%trie_ls(1,1,gis%P_te),                   &
                  gis%trio_ls(1,1,gis%P_te),'te ',levs)
        CALL bar3(gis%trie_ls(1,1,gis%P_rq),                   &
                  gis%trio_ls(1,1,gis%P_rq),'rq ',levs)
        CALL bar3(gis%trie_ls(1,1,gis%P_rq+levs),              &
                  gis%trio_ls(1,1,gis%P_rq+levs),'oz1 ',levs)
        CALL bar3(gis%trie_ls(1,1,gis%P_rq+2*levs),            &
                  gis%trio_ls(1,1,gis%P_rq+2*levs),'oz2 ',levs)
        CALL bar3(gis%trie_ls(1,1,gis%P_q),                    &
                  gis%trio_ls(1,1,gis%P_q),'q  ',1)
        CALL bar3(gis%trie_ls(1,1,gis%P_gz),                   &
                  gis%trio_ls(1,1,gis%P_gz),'gz ',1)
        CALL countperf(1,17,0.)
      ENDIF

      CALL synchro
      CALL countperf(1,15,0.)
      CALL countperf(0,15,0.)
      CALL synchro
      CALL countperf(1,15,0.)
!!
      CALL end_countperf()
      CALL write_countperf(NODES,me,fhour)
!!
      if(me.eq.0) then
        call w3tage('gsm     ')
      endif
!!
!C    JJT TERMINATE HPM ... THIS IS NEEDED
      CALL f_hpmterminate(ME)
!
      if (gfsio_out .or. gfsio_in) then
        call gfsio_finalize()
      endif

! Release the ESMF states, clock and grided component.
!-----------------------------------------------------
 CALL ESMF_GridCompDestroy(gcGFS, rc = rc1)

 IF(PRESENT(rc)) THEN
      rc = rc1
 END IF

 END SUBROUTINE GFS_Finalize

 END MODULE GFS_Finalize_ESMFMod
