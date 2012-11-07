! !MODULE: GFS_ErrMsgMod --- Initialize module of the ESMF
!                                     gridded component of the GFS system.
!
! !DESCRIPTION: GFS gridded component Error Messages
!
! !REVISION HISTORY:
!
!  March 2006 S. Moorthi
!
!
! !INTERFACE:
!
 MODULE GFS_ErrMsgMod

!
!!USES:
!
 USE ESMF_Mod                 ! The ESMF library.

 IMPLICIT none
 CONTAINS
 SUBROUTINE ERR_MSG(rc1,msg,var,rcfinal)
!
 integer, intent(inout)        :: rc1
 integer, intent(out)          :: rcfinal
 character (len=*), intent(in) :: msg
 character (len=*), intent(in) :: var
 IF(ESMF_LogMsgFoundError(rc1, msg)) THEN
     rcfinal = ESMF_FAILURE
     PRINT*, 'Error Happened When Running the ESMF_ConfigGetAttribute-', &
     var,' rc = ', rc1
     rc1     = ESMF_SUCCESS
 END IF
 END SUBROUTINE ERR_MSG
 SUBROUTINE ERR_MSG1(rc1,msg,rcfinal)
!
 integer, intent(inout)          :: rc1
 integer, intent(out)            :: rcfinal
 character (len=*), intent(in)   :: msg
 IF(ESMF_LogMsgFoundError(rc1, msg)) THEN
     rcfinal = ESMF_FAILURE
     PRINT*, 'Error Happened When Running the ESMF_ConfigGetAttribute-', &
     'rc = ', rc1,' msg=',msg
     rc1     = ESMF_SUCCESS
 END IF
 END SUBROUTINE ERR_MSG1
 SUBROUTINE ERR_MSG3(rc1,msg,rc)
 integer, intent(inout)        :: rc1
 integer, intent(out)          :: rc
 character (len=*), intent(in) :: msg
 IF(ESMF_LogMsgFoundError(rc1, msg)) THEN
     rc  = ESMF_FAILURE
     PRINT*, 'Error Happened for ',msg, ' rc = ', rc1
     rc1 = ESMF_SUCCESS
 END IF
 END SUBROUTINE ERR_MSG3
 END MODULE GFS_ErrMsgMod
