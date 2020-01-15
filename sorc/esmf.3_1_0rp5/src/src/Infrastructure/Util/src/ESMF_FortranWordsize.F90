! $Id: ESMF_FortranWordsize.cppF90,v 1.1.2.10 2010/02/01 20:52:29 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_FortranWordsize.F90"
! ESMF FortranWordsize module
module ESMF_FortranWordsizeMod
!==============================================================================
!
! This file contains wordsize functions that are automatically
! generated from macros to handle the type/kind overloading.
!
!------------------------------------------------------------------------------
! INCLUDES
! < ignore blank lines below. they are created by the files which
! define various macros. >
#include "ESMF.h"
!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_LogErrMod
  implicit none
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
!------------------------------------------------------------------------------
! !PUBLIC FUNCTION:
  public ESMF_FortranUDTPointerSize
  public ESMF_FortranWordsize
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FortranWordsize -- Generic interface to find Fortran data sizes
!
! !INTERFACE:
  interface ESMF_FortranWordsize
    !------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
#ifndef ESMF_NO_INTEGER_1_BYTE 
 module procedure ESMF_FortranWordsizeI1 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
 module procedure ESMF_FortranWordsizeI2 
#endif 
 module procedure ESMF_FortranWordsizeI4 
 module procedure ESMF_FortranWordsizeI8 
 module procedure ESMF_FortranWordsizeR4 
 module procedure ESMF_FortranWordsizeR8 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!EOPI
  end interface
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_FortranUDTPointerSize()"
!BOPI
! !IROUTINE: ESMF_FortranUDTPointerSize - Get upper limit on size of Fortran UDT Pointer in bytes
! !INTERFACE:
  subroutine ESMF_FortranUDTPointerSize(size)
!
! !ARGUMENTS:
    integer, intent(out) :: size
!EOPI
!------------------------------------------------------------------------------
    type simple_udt
      sequence
      real :: a, b, c
      integer :: i, j, k
    end type
    type(simple_udt), pointer :: udt_ptr
    character :: udt_endchar
    common /udtcom/ udt_ptr, udt_endchar
    call ESMF_FortranUDTPointerSizeInit()
    size = ichar(udt_endchar)
    ! because of compiler introduced padding size may actually overestimate the
    ! size of the (UDT, pointer) structure
    if (size<4) then
      ! A size smaller than 4 bytes is suspicious and may indicate that the
      ! size was not determined correctly by the above code!
      size = 64 ! bytes - large enough to work on all current platforms
    endif
  end subroutine ESMF_FortranUDTPointerSize
!------------------------------------------------------------------------------
!==============================================================================
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!BOP 
! !IROUTINE: ESMF_FortranWordsize - Return the size in byte units of a scalar 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FortranWordsize() 
! function ESMF_FortranWordsize<typekind>(var, rc) 
! 
! !RETURN VALUE: 
! integer :: ESMF_FortranWordsize<typekind> 
! 
! !ARGUMENTS: 
! <type>(ESMF_KIND_<typekind>), intent(in) :: var 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return the size in units of bytes of a scalar (var) argument. 
! Valid types and kinds supported by the framework are: 
! integers of 1-byte, 2-byte, 4-byte, and 8-byte size, and 
! reals of 4-byte and 8-bytes size. 
! 
! The arguments are: 
! \begin{description} 
! \item [var] 
! Scalar of any supported type and kind 
! \item [rc] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!------------------------------------------------------------------------------ 

#undef ESMF_METHOD 
!define ESMF_METHOD to "ESMF_FortranWordsize##mtypekind" 
#define ESMF_METHOD "ESMF_FortranWordsize" 
 integer function ESMF_FortranWordsizeI1(var, rc) 

 integer(ESMF_KIND_I1), intent(in) :: var 
 integer, intent(out), optional :: rc 

 ! local data 
 integer :: size 
 integer(ESMF_KIND_I1) :: varTK(2) ! varTK is same TK as var 
 logical :: rcpresent ! Return code present 

 ! Initialize return code; assume routine not implemented 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 

 call ESMF_PointerDifference(ESMC_POINTER_SIZE, varTK(1), varTK(2), size) 
 ESMF_FortranWordsizeI1 = size 

 if (rcpresent) rc = ESMF_SUCCESS 

 end function ESMF_FortranWordsizeI1 

! < end macro - do not edit directly > 
!---------------------------------------------------------------------------- 
 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!------------------------------------------------------------------------------ 

#undef ESMF_METHOD 
!define ESMF_METHOD to "ESMF_FortranWordsize##mtypekind" 
#define ESMF_METHOD "ESMF_FortranWordsize" 
 integer function ESMF_FortranWordsizeI2(var, rc) 

 integer(ESMF_KIND_I2), intent(in) :: var 
 integer, intent(out), optional :: rc 

 ! local data 
 integer :: size 
 integer(ESMF_KIND_I2) :: varTK(2) ! varTK is same TK as var 
 logical :: rcpresent ! Return code present 

 ! Initialize return code; assume routine not implemented 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 

 call ESMF_PointerDifference(ESMC_POINTER_SIZE, varTK(1), varTK(2), size) 
 ESMF_FortranWordsizeI2 = size 

 if (rcpresent) rc = ESMF_SUCCESS 

 end function ESMF_FortranWordsizeI2 

! < end macro - do not edit directly > 
!---------------------------------------------------------------------------- 
 
#endif 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!------------------------------------------------------------------------------ 

#undef ESMF_METHOD 
!define ESMF_METHOD to "ESMF_FortranWordsize##mtypekind" 
#define ESMF_METHOD "ESMF_FortranWordsize" 
 integer function ESMF_FortranWordsizeI4(var, rc) 

 integer(ESMF_KIND_I4), intent(in) :: var 
 integer, intent(out), optional :: rc 

 ! local data 
 integer :: size 
 integer(ESMF_KIND_I4) :: varTK(2) ! varTK is same TK as var 
 logical :: rcpresent ! Return code present 

 ! Initialize return code; assume routine not implemented 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 

 call ESMF_PointerDifference(ESMC_POINTER_SIZE, varTK(1), varTK(2), size) 
 ESMF_FortranWordsizeI4 = size 

 if (rcpresent) rc = ESMF_SUCCESS 

 end function ESMF_FortranWordsizeI4 

! < end macro - do not edit directly > 
!---------------------------------------------------------------------------- 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!------------------------------------------------------------------------------ 

#undef ESMF_METHOD 
!define ESMF_METHOD to "ESMF_FortranWordsize##mtypekind" 
#define ESMF_METHOD "ESMF_FortranWordsize" 
 integer function ESMF_FortranWordsizeI8(var, rc) 

 integer(ESMF_KIND_I8), intent(in) :: var 
 integer, intent(out), optional :: rc 

 ! local data 
 integer :: size 
 integer(ESMF_KIND_I8) :: varTK(2) ! varTK is same TK as var 
 logical :: rcpresent ! Return code present 

 ! Initialize return code; assume routine not implemented 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 

 call ESMF_PointerDifference(ESMC_POINTER_SIZE, varTK(1), varTK(2), size) 
 ESMF_FortranWordsizeI8 = size 

 if (rcpresent) rc = ESMF_SUCCESS 

 end function ESMF_FortranWordsizeI8 

! < end macro - do not edit directly > 
!---------------------------------------------------------------------------- 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!------------------------------------------------------------------------------ 

#undef ESMF_METHOD 
!define ESMF_METHOD to "ESMF_FortranWordsize##mtypekind" 
#define ESMF_METHOD "ESMF_FortranWordsize" 
 integer function ESMF_FortranWordsizeR4(var, rc) 

 real(ESMF_KIND_R4), intent(in) :: var 
 integer, intent(out), optional :: rc 

 ! local data 
 integer :: size 
 real(ESMF_KIND_R4) :: varTK(2) ! varTK is same TK as var 
 logical :: rcpresent ! Return code present 

 ! Initialize return code; assume routine not implemented 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 

 call ESMF_PointerDifference(ESMC_POINTER_SIZE, varTK(1), varTK(2), size) 
 ESMF_FortranWordsizeR4 = size 

 if (rcpresent) rc = ESMF_SUCCESS 

 end function ESMF_FortranWordsizeR4 

! < end macro - do not edit directly > 
!---------------------------------------------------------------------------- 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!------------------------------------------------------------------------------ 

#undef ESMF_METHOD 
!define ESMF_METHOD to "ESMF_FortranWordsize##mtypekind" 
#define ESMF_METHOD "ESMF_FortranWordsize" 
 integer function ESMF_FortranWordsizeR8(var, rc) 

 real(ESMF_KIND_R8), intent(in) :: var 
 integer, intent(out), optional :: rc 

 ! local data 
 integer :: size 
 real(ESMF_KIND_R8) :: varTK(2) ! varTK is same TK as var 
 logical :: rcpresent ! Return code present 

 ! Initialize return code; assume routine not implemented 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 

 call ESMF_PointerDifference(ESMC_POINTER_SIZE, varTK(1), varTK(2), size) 
 ESMF_FortranWordsizeR8 = size 

 if (rcpresent) rc = ESMF_SUCCESS 

 end function ESMF_FortranWordsizeR8 

! < end macro - do not edit directly > 
!---------------------------------------------------------------------------- 
 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
end module ESMF_FortranWordsizeMod
subroutine f_esmf_fortranudtpointersize(size)
  ! C callable interface to ESMF_FortranUDTPointerSize()
  use ESMF_FortranWordsizeMod
  integer::size
  call ESMF_FortranUDTPointerSize(size)
end subroutine
subroutine f_esmf_fortranudtpointercopy(dst, src)
  ! C callable routine that makes a copy of the internal structure of a
  ! Fortran pointer to a user derived type (UDT). ESMF uses this call internally
  ! from the InternalState code in order be able to store and return the pointer
  ! to a UDT passed in by the user.
  !
  ! The implemented scheme rests on a very fundamental assumption, that the
  ! memory footprint of a Fortran pointer to a UDT is UDT-independent!
  ! Internally ESMF does not have access to the actual UDT that is defined in
  ! the user code. Instead this routine defines a dummy UDT called "simple_udt"
  ! below. The arguments are defined as wrappers that hold pointers to this
  ! dummy UDT. With the assumption that the size of the pointer to a UDT is
  ! UDT-independent the pointer assignment below "dst%udt => src%udt" will also
  ! be UDT-independent and essentially copy all of the bytes necessary from the
  ! src to the dst pointer.
  !
  ! The associated unit test src/prologue/tests/ESMF_F95PtrUTest.F90 verifies
  ! that the above assumption holds. If this test starts failing on a platform
  ! we will need to reconsider the entire approach!
  type simple_udt
    sequence
    real :: a, b, c
    integer :: i, j, k
  end type
  type wrapper
    sequence
    type(simple_udt), pointer:: udt
  end type
  type (wrapper):: dst
  type (wrapper):: src
  dst%udt => src%udt
end subroutine
