 SUBROUTINE StartTimeGet(yy, mm, dd, hh, mns, sec, kfhour, n1, n2,    &
                         grib_inp, fhrot, cfile, cfile2, rc)

! This subroutine gets and calculates the start time from reading the
! sigma file information.

! !REVISION HISTORY:
!
!  March 2005      Weiyu Yang Initial code.
!
!USES:
!
 USE ESMF_Mod,     ONLY: ESMF_SUCCESS
 USE machine,      ONLY: kind_io4, kind_evod
 USE date_def,     ONLY: idate
!USE resol_def,    ONLY: Ensemble_Id, Total_member
 use sigio_module
 use sigio_r_module
 use gfsio_module
 use gfsio_def

 IMPLICIT none

!
! ARGUMENTS:
!-----------

 INTEGER,                INTENT(IN)  :: grib_inp
 INTEGER,                INTENT(out) :: yy, mm, dd, hh, mns, sec
 INTEGER,                INTENT(out) :: n1, n2
 INTEGER,                INTENT(out) :: kfhour
 !LOGICAL,                INTENT(in)  :: gfsio_in
 REAL(KIND = kind_evod), INTENT(in)  :: fhrot
 INTEGER,                INTENT(out) :: rc     ! return code

 INTEGER                        :: rc1 = ESMF_SUCCESS
 REAL(KIND = kind_evod)         :: fhour
 REAL(KIND = kind_io4)          :: fhour4
 type(sigio_head) head
 character (len=*)              :: cfile, cfile2
 integer iret, khour

 n1    = 11
 n2    = 12
 
 print *,' grib_inp=',grib_inp,' n1=',n1
 if (grib_inp .le. 0) then
   call sigio_rropen(n1,cfile,iret)
   call sigio_rrhead(n1,head,iret)

   IF(head%fhour /= fhrot) THEN
      call sigio_rropen(n2,cfile2,iret)
      call sigio_rrhead(n2,head,iret)
   END IF
 
   fhour  = head%fhour
   idate  = head%idate
 else
   print *,' grib_inp=',grib_inp,' cfile=',cfile
   call gfsio_open(gfile_in,trim(cfile),'read',iret)
   call gfsio_getfilehead(gfile_in,iret=iret,idate=idate,fhour=fhour4)
   fhour = fhour4

   print *,' fhour4=',fhour4,' idate=',idate,' iret=',iret
   if (iret .ne. 0) call mpi_quit(5555)
 endif
 yy     = idate(4)
 mm     = idate(2)
 dd     = idate(3)
 hh     = idate(1)
 mns    = 0
 sec    = 0
 kfhour = NINT(fhour)
 print *,' idate=',idate,' fhour=',fhour,' kfhour=',kfhour

 rc = rc1

 END SUBROUTINE StartTimeGet
