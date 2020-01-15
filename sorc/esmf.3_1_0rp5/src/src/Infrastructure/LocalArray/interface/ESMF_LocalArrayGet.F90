! $Id: ESMF_LocalArrayGet.cppF90,v 1.4.2.4 2010/02/01 20:51:28 svasquez Exp $
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
#define ESMF_FILENAME "ESMF_LocalArrayGet.F90"
!==============================================================================
!
! ESMF LocalArrayCreate module
module ESMF_LocalArrayGetMod
!
!==============================================================================
!
! This file contains the LocalArray class definition and all LocalArray
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
! < ignore blank lines below. they are created by the files which
! define various macros. >
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_LocalArrayGetMod - Manage data uniformly between F90 and C++
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_LocalArray} class and
! associated functions and subroutines.
!
! C and C++ arrays are simple pointers to memory.
! Fortran arrays contain shape and stride definitions and are strongly
! typed. To enable interoperability between the languages the C++ code
! must be able to obtain this information from the Fortran description
! (which is called the "dope vector" in Fortran), either through a priori
! knowledge or through query.
!EOPI
!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod ! ESMF utility types
  use ESMF_InitMacrosMod ! ESMF initializer macros
  use ESMF_BaseMod ! ESMF base class
  use ESMF_LogErrMod ! ESMF error handling
  use ESMF_IOSpecMod
  use ESMF_ArraySpecMod
  ! class sub modules
  use ESMF_LocalArrayCreateMod ! contains the ESMF_LocalArray derived type
  implicit none
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
character(*), parameter, private :: version = &
  '$Id: ESMF_LocalArrayGet.cppF90,v 1.4.2.4 2010/02/01 20:51:28 svasquez Exp $'
!------------------------------------------------------------------------------
! ! Internal wrapper structures for passing f90 pointers to C++ and
! ! guaranteeing they are passed by reference on all compilers and all
! ! platforms. These are never seen outside this module.
!
      ! < these expand into defined type declarations >

#ifndef ESMF_NO_INTEGER_1_BYTE 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap1DI1 
 integer (ESMF_KIND_I1),dimension(:),pointer :: ptr1DI1 
 end type ESMF_ArrWrap1DI1 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap2DI1 
 integer (ESMF_KIND_I1),dimension(:,:),pointer :: ptr2DI1 
 end type ESMF_ArrWrap2DI1 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap3DI1 
 integer (ESMF_KIND_I1),dimension(:,:,:),pointer :: ptr3DI1 
 end type ESMF_ArrWrap3DI1 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap4DI1 
 integer (ESMF_KIND_I1),dimension(:,:,:,:),pointer :: ptr4DI1 
 end type ESMF_ArrWrap4DI1 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap5DI1 
 integer (ESMF_KIND_I1),dimension(:,:,:,:,:),pointer :: ptr5DI1 
 end type ESMF_ArrWrap5DI1 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap6DI1 
 integer (ESMF_KIND_I1),dimension(:,:,:,:,:,:),pointer :: ptr6DI1 
 end type ESMF_ArrWrap6DI1 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap7DI1 
 integer (ESMF_KIND_I1),dimension(:,:,:,:,:,:,:),pointer :: ptr7DI1 
 end type ESMF_ArrWrap7DI1 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap1DI2 
 integer (ESMF_KIND_I2),dimension(:),pointer :: ptr1DI2 
 end type ESMF_ArrWrap1DI2 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap2DI2 
 integer (ESMF_KIND_I2),dimension(:,:),pointer :: ptr2DI2 
 end type ESMF_ArrWrap2DI2 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap3DI2 
 integer (ESMF_KIND_I2),dimension(:,:,:),pointer :: ptr3DI2 
 end type ESMF_ArrWrap3DI2 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap4DI2 
 integer (ESMF_KIND_I2),dimension(:,:,:,:),pointer :: ptr4DI2 
 end type ESMF_ArrWrap4DI2 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap5DI2 
 integer (ESMF_KIND_I2),dimension(:,:,:,:,:),pointer :: ptr5DI2 
 end type ESMF_ArrWrap5DI2 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap6DI2 
 integer (ESMF_KIND_I2),dimension(:,:,:,:,:,:),pointer :: ptr6DI2 
 end type ESMF_ArrWrap6DI2 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap7DI2 
 integer (ESMF_KIND_I2),dimension(:,:,:,:,:,:,:),pointer :: ptr7DI2 
 end type ESMF_ArrWrap7DI2 
 
#endif 
#endif 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap1DI4 
 integer (ESMF_KIND_I4),dimension(:),pointer :: ptr1DI4 
 end type ESMF_ArrWrap1DI4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap1DI8 
 integer (ESMF_KIND_I8),dimension(:),pointer :: ptr1DI8 
 end type ESMF_ArrWrap1DI8 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap1DR4 
 real (ESMF_KIND_R4),dimension(:),pointer :: ptr1DR4 
 end type ESMF_ArrWrap1DR4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap1DR8 
 real (ESMF_KIND_R8),dimension(:),pointer :: ptr1DR8 
 end type ESMF_ArrWrap1DR8 
 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap2DI4 
 integer (ESMF_KIND_I4),dimension(:,:),pointer :: ptr2DI4 
 end type ESMF_ArrWrap2DI4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap2DI8 
 integer (ESMF_KIND_I8),dimension(:,:),pointer :: ptr2DI8 
 end type ESMF_ArrWrap2DI8 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap2DR4 
 real (ESMF_KIND_R4),dimension(:,:),pointer :: ptr2DR4 
 end type ESMF_ArrWrap2DR4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap2DR8 
 real (ESMF_KIND_R8),dimension(:,:),pointer :: ptr2DR8 
 end type ESMF_ArrWrap2DR8 
 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap3DI4 
 integer (ESMF_KIND_I4),dimension(:,:,:),pointer :: ptr3DI4 
 end type ESMF_ArrWrap3DI4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap3DI8 
 integer (ESMF_KIND_I8),dimension(:,:,:),pointer :: ptr3DI8 
 end type ESMF_ArrWrap3DI8 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap3DR4 
 real (ESMF_KIND_R4),dimension(:,:,:),pointer :: ptr3DR4 
 end type ESMF_ArrWrap3DR4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap3DR8 
 real (ESMF_KIND_R8),dimension(:,:,:),pointer :: ptr3DR8 
 end type ESMF_ArrWrap3DR8 
 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap4DI4 
 integer (ESMF_KIND_I4),dimension(:,:,:,:),pointer :: ptr4DI4 
 end type ESMF_ArrWrap4DI4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap4DI8 
 integer (ESMF_KIND_I8),dimension(:,:,:,:),pointer :: ptr4DI8 
 end type ESMF_ArrWrap4DI8 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap4DR4 
 real (ESMF_KIND_R4),dimension(:,:,:,:),pointer :: ptr4DR4 
 end type ESMF_ArrWrap4DR4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap4DR8 
 real (ESMF_KIND_R8),dimension(:,:,:,:),pointer :: ptr4DR8 
 end type ESMF_ArrWrap4DR8 
 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap5DI4 
 integer (ESMF_KIND_I4),dimension(:,:,:,:,:),pointer :: ptr5DI4 
 end type ESMF_ArrWrap5DI4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap5DI8 
 integer (ESMF_KIND_I8),dimension(:,:,:,:,:),pointer :: ptr5DI8 
 end type ESMF_ArrWrap5DI8 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap5DR4 
 real (ESMF_KIND_R4),dimension(:,:,:,:,:),pointer :: ptr5DR4 
 end type ESMF_ArrWrap5DR4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap5DR8 
 real (ESMF_KIND_R8),dimension(:,:,:,:,:),pointer :: ptr5DR8 
 end type ESMF_ArrWrap5DR8 
 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap6DI4 
 integer (ESMF_KIND_I4),dimension(:,:,:,:,:,:),pointer :: ptr6DI4 
 end type ESMF_ArrWrap6DI4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap6DI8 
 integer (ESMF_KIND_I8),dimension(:,:,:,:,:,:),pointer :: ptr6DI8 
 end type ESMF_ArrWrap6DI8 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap6DR4 
 real (ESMF_KIND_R4),dimension(:,:,:,:,:,:),pointer :: ptr6DR4 
 end type ESMF_ArrWrap6DR4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap6DR8 
 real (ESMF_KIND_R8),dimension(:,:,:,:,:,:),pointer :: ptr6DR8 
 end type ESMF_ArrWrap6DR8 
 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap7DI4 
 integer (ESMF_KIND_I4),dimension(:,:,:,:,:,:,:),pointer :: ptr7DI4 
 end type ESMF_ArrWrap7DI4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap7DI8 
 integer (ESMF_KIND_I8),dimension(:,:,:,:,:,:,:),pointer :: ptr7DI8 
 end type ESMF_ArrWrap7DI8 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap7DR4 
 real (ESMF_KIND_R4),dimension(:,:,:,:,:,:,:),pointer :: ptr7DR4 
 end type ESMF_ArrWrap7DR4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap7DR8 
 real (ESMF_KIND_R8),dimension(:,:,:,:,:,:,:),pointer :: ptr7DR8 
 end type ESMF_ArrWrap7DR8 
 
 
#endif 
 
! < end macro - do not edit directly > 
 

!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:
  public ESMF_LocalArrayGet
!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LocalArrayGet -- Get LocalArray internal information
! !INTERFACE:
  interface ESMF_LocalArrayGet
! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_LocalArrayGetDefault
    !------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
#ifndef ESMF_NO_INTEGER_1_BYTE 
 module procedure ESMF_LocalArrayGetData1DI1 
 module procedure ESMF_LocalArrayGetData2DI1 
 module procedure ESMF_LocalArrayGetData3DI1 
 module procedure ESMF_LocalArrayGetData4DI1 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_LocalArrayGetData5DI1 
 module procedure ESMF_LocalArrayGetData6DI1 
 module procedure ESMF_LocalArrayGetData7DI1 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
 module procedure ESMF_LocalArrayGetData1DI2 
 module procedure ESMF_LocalArrayGetData2DI2 
 module procedure ESMF_LocalArrayGetData3DI2 
 module procedure ESMF_LocalArrayGetData4DI2 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_LocalArrayGetData5DI2 
 module procedure ESMF_LocalArrayGetData6DI2 
 module procedure ESMF_LocalArrayGetData7DI2 
#endif 
#endif 
 module procedure ESMF_LocalArrayGetData1DI4 
 module procedure ESMF_LocalArrayGetData1DI8 
 module procedure ESMF_LocalArrayGetData1DR4 
 module procedure ESMF_LocalArrayGetData1DR8 
 module procedure ESMF_LocalArrayGetData2DI4 
 module procedure ESMF_LocalArrayGetData2DI8 
 module procedure ESMF_LocalArrayGetData2DR4 
 module procedure ESMF_LocalArrayGetData2DR8 
 module procedure ESMF_LocalArrayGetData3DI4 
 module procedure ESMF_LocalArrayGetData3DI8 
 module procedure ESMF_LocalArrayGetData3DR4 
 module procedure ESMF_LocalArrayGetData3DR8 
 module procedure ESMF_LocalArrayGetData4DI4 
 module procedure ESMF_LocalArrayGetData4DI8 
 module procedure ESMF_LocalArrayGetData4DR4 
 module procedure ESMF_LocalArrayGetData4DR8 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_LocalArrayGetData5DI4 
 module procedure ESMF_LocalArrayGetData5DI8 
 module procedure ESMF_LocalArrayGetData5DR4 
 module procedure ESMF_LocalArrayGetData5DR8 
 module procedure ESMF_LocalArrayGetData6DI4 
 module procedure ESMF_LocalArrayGetData6DI8 
 module procedure ESMF_LocalArrayGetData6DR4 
 module procedure ESMF_LocalArrayGetData6DR8 
 module procedure ESMF_LocalArrayGetData7DI4 
 module procedure ESMF_LocalArrayGetData7DI8 
 module procedure ESMF_LocalArrayGetData7DR4 
 module procedure ESMF_LocalArrayGetData7DR8 
#endif 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

! !DESCRIPTION:
! This interface provides a single entry point for the various
! types of {\tt ESMF\_LocalArrayGet} functions.
!
!EOPI
end interface
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!==============================================================================
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! Query for information from the array.
!
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayGetDefault"
!BOP
! !IROUTINE: ESMF_LocalArrayGet - Return LocalArray information.
!
! !INTERFACE:
  ! Private name; call using ESMF_LocalArrayGet()
  subroutine ESMF_LocalArrayGetDefault(larray, rank, typekind, counts, lbounds, &
    ubounds, base, name, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(in) :: larray
    integer, intent(out), optional :: rank
    type(ESMF_TypeKind), intent(out), optional :: typekind
    integer, intent(out), optional :: counts(:)
    integer, intent(out), optional :: lbounds(:)
    integer, intent(out), optional :: ubounds(:)
    type(ESMF_Pointer), intent(out), optional :: base
    character(len=ESMF_MAXSTR), intent(out), optional :: name
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Returns information about the {\tt ESMF\_LocalArray}.
!
! The arguments are:
! \begin{description}
! \item[larray]
! Queried {\tt ESMF\_LocalArray} object.
! \item[{[rank]}]
! Rank of the LocalArray object.
! \item[{[typekind]}]
! TypeKind of the LocalArray object.
! \item[{[counts]}]
! Count per dimension.
! \item[{[lbounds]}]
! Lower bound per dimension.
! \item[{[ubounds]}]
! Upper bound per dimension.
! \item[{[base]}]
! Base class object.
! \item[{[name]}]
! Name of the LocalArray object.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc ! local return code
    integer :: lrank ! Local use to get rank
    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, larray, rc)
    if (present(rank)) then
      call c_ESMC_LocalArrayGetRank(larray, rank, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    if (present(typekind)) then
      call c_ESMC_LocalArrayGetTypeKind(larray, typekind, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    if (present(counts)) then
      call c_ESMC_LocalArrayGetRank(larray, lrank, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      call c_ESMC_LocalArrayGetLengths(larray, lrank, counts, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    if (present(lbounds)) then
      call c_ESMC_LocalArrayGetRank(larray, lrank, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      call c_ESMC_LocalArrayGetLbounds(larray, lrank, lbounds, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    if (present(ubounds)) then
      call c_ESMC_LocalArrayGetRank(larray, lrank, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      call c_ESMC_LocalArrayGetUbounds(larray, lrank, ubounds, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    if (present(base)) then
      call c_ESMC_LocalArrayGetBaseAddr(larray, base, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    if (present(name)) then
      call c_ESMC_GetName(larray, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_LocalArrayGetDefault
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!! < start of macros which become actual function bodies after expansion >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!------------------------------------------------------------------------------ 
!BOP 
! !IROUTINE: ESMF_LocalArrayGet - Get access to data in LocalArray object 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_LocalArrayGet() 
! subroutine ESMF_LocalArrayGetData<rank><type><kind>(larray, fptr, docopy, rc) 
! 
! !ARGUMENTS: 
! type(ESMF_LocalArray) :: larray 
! <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: fptr 
! type(ESMF_CopyFlag), intent(in), optional :: docopy 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the data buffer, or return a Fortran pointer 
! to a new copy of the data. 
! 
! The arguments are: 
! \begin{description} 
! \item[larray] 
! The {\tt ESMF\_LocalArray} to get the value from. 
! \item[fptr] 
! An unassociated or associated Fortran pointer correctly allocated.
! \item[{[docopy]}] 
! An optional copy flag which can be specified. 
! Can either make a new copy of the data or reference existing data. 
! See section \ref{opt:copyflag} for a list of possible values. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData1DI1(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I1), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap1DI1) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(1), ub(1) ! size info for the array 
 integer (ESMF_KIND_I1), dimension(:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 1, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 1, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr1DI1 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr1DI1 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr1DI1 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData1DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData2DI1(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I1), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap2DI1) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(2), ub(2) ! size info for the array 
 integer (ESMF_KIND_I1), dimension(:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 2, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 2, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr2DI1 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr2DI1 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr2DI1 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData2DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData3DI1(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I1), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap3DI1) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(3), ub(3) ! size info for the array 
 integer (ESMF_KIND_I1), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 3, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 3, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr3DI1 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr3DI1 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr3DI1 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData3DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData4DI1(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I1), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap4DI1) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(4), ub(4) ! size info for the array 
 integer (ESMF_KIND_I1), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 4, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 4, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr4DI1 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr4DI1 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr4DI1 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData4DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData5DI1(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap5DI1) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(5), ub(5) ! size info for the array 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 5, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 5, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr5DI1 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr5DI1 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr5DI1 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData5DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData6DI1(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap6DI1) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(6), ub(6) ! size info for the array 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 6, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 6, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr6DI1 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr6DI1 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr6DI1 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData6DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData7DI1(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap7DI1) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(7), ub(7) ! size info for the array 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 7, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 7, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr7DI1 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr7DI1 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr7DI1 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData7DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData1DI2(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I2), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap1DI2) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(1), ub(1) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 1, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 1, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr1DI2 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr1DI2 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr1DI2 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData2DI2(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap2DI2) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(2), ub(2) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 2, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 2, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr2DI2 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr2DI2 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr2DI2 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData3DI2(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap3DI2) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(3), ub(3) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 3, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 3, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr3DI2 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr3DI2 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr3DI2 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData4DI2(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap4DI2) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(4), ub(4) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 4, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 4, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr4DI2 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr4DI2 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr4DI2 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData5DI2(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap5DI2) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(5), ub(5) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 5, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 5, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr5DI2 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr5DI2 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr5DI2 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData6DI2(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap6DI2) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(6), ub(6) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 6, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 6, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr6DI2 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr6DI2 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr6DI2 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData6DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData7DI2(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap7DI2) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(7), ub(7) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 7, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 7, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr7DI2 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr7DI2 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr7DI2 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData7DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData1DI4(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I4), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap1DI4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(1), ub(1) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 1, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 1, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr1DI4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr1DI4 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr1DI4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData1DI8(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I8), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap1DI8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(1), ub(1) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I8) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 1, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 1, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr1DI8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr1DI8 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr1DI8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData1DR4(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 real (ESMF_KIND_R4), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap1DR4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(1), ub(1) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_R4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 1, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 1, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr1DR4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr1DR4 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr1DR4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData1DR8(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 real (ESMF_KIND_R8), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap1DR8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(1), ub(1) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_R8) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 1, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 1, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr1DR8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr1DR8 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr1DR8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData2DI4(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap2DI4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(2), ub(2) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 2, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 2, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr2DI4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr2DI4 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr2DI4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData2DI8(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap2DI8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(2), ub(2) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I8) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 2, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 2, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr2DI8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr2DI8 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr2DI8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData2DR4(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap2DR4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(2), ub(2) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_R4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 2, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 2, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr2DR4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr2DR4 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr2DR4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData2DR8(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap2DR8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(2), ub(2) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_R8) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 2, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 2, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr2DR8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr2DR8 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr2DR8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData3DI4(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap3DI4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(3), ub(3) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 3, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 3, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr3DI4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr3DI4 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr3DI4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData3DI8(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap3DI8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(3), ub(3) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I8) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 3, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 3, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr3DI8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr3DI8 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr3DI8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData3DR4(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap3DR4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(3), ub(3) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_R4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 3, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 3, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr3DR4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr3DR4 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr3DR4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData3DR8(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap3DR8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(3), ub(3) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_R8) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 3, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 3, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr3DR8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr3DR8 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr3DR8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData4DI4(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap4DI4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(4), ub(4) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 4, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 4, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr4DI4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr4DI4 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr4DI4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData4DI8(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap4DI8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(4), ub(4) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I8) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 4, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 4, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr4DI8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr4DI8 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr4DI8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData4DR4(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap4DR4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(4), ub(4) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_R4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 4, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 4, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr4DR4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr4DR4 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr4DR4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData4DR8(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap4DR8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(4), ub(4) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_R8) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 4, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 4, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr4DR8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr4DR8 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr4DR8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData5DI4(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap5DI4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(5), ub(5) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 5, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 5, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr5DI4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr5DI4 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr5DI4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData5DI8(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap5DI8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(5), ub(5) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I8) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 5, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 5, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr5DI8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr5DI8 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr5DI8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData5DR4(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap5DR4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(5), ub(5) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_R4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 5, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 5, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr5DR4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr5DR4 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr5DR4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData5DR8(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap5DR8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(5), ub(5) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_R8) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 5, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 5, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr5DR8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr5DR8 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr5DR8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData6DI4(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap6DI4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(6), ub(6) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 6, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 6, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr6DI4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr6DI4 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr6DI4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData6DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData6DI8(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap6DI8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(6), ub(6) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I8) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 6, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 6, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr6DI8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr6DI8 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr6DI8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData6DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData6DR4(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap6DR4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(6), ub(6) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_R4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 6, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 6, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr6DR4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr6DR4 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr6DR4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData6DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData6DR8(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap6DR8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(6), ub(6) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_R8) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 6, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 6, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr6DR8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr6DR8 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr6DR8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData6DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData7DI4(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap7DI4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(7), ub(7) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 7, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 7, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr7DI4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr7DI4 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr7DI4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData7DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData7DI8(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap7DI8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(7), ub(7) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_I8) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 7, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 7, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr7DI8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr7DI8 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr7DI8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData7DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData7DR4(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap7DR4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(7), ub(7) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_R4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 7, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 7, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr7DR4 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr7DR4 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr7DR4 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData7DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayGetData" 
 subroutine ESMF_LocalArrayGetData7DR8(larray, fptr, docopy, rc) 
 
 type(ESMF_LocalArray) :: larray 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: copyreq ! did user specify copy? 
 integer :: rank 
 type(ESMF_TypeKind) :: typekind 
 
 type (ESMF_ArrWrap7DR8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(7), ub(7) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Check docopy argument and set copyreq 
 copyreq = .FALSE. ! default do not copy but return by reference 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 ! Check tkr matching between larray and fptr 
 call ESMF_LocalArrayGet(larray, typekind=typekind, rank=rank, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! check typekind match 
 if (typekind /= ESMF_TYPEKIND_R8) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr typekind does not match LocalArray typekind", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! check rank match 
 if (rank /= 7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr rank does not match LocalArray rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Gain access to the F90 array pointer stored in larray 
 call c_ESMC_LocalArrayGetF90Ptr(larray, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(larray, 7, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(larray, 7, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! test if incoming pointer was associated 
 if (associated(fptr)) then 
 if (size(fptr) .ne. size(lp)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- fptr was associated but of incorrect size", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 ! this must do a contents assignment to perform a copy operation 
 fptr = wrap%ptr7DR8 
 deallocate(lp) 
 else 
 ! this must do a contents assignment to perform a copy operation 
 lp = wrap%ptr7DR8 
 fptr => lp 
 endif 
 else 
 fptr => wrap%ptr7DR8 ! return a reference 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData7DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
end module ESMF_LocalArrayGetMod
