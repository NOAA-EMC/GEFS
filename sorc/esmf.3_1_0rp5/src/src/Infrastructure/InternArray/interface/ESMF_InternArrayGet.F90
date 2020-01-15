! $Id: ESMF_InternArrayGet.cppF90,v 1.1.2.3 2010/02/01 20:51:08 svasquez Exp $
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
#define ESMF_FILENAME "ESMF_InternArrayGet.F90"
!==============================================================================
!
! ESMF InternArrayGet module
module ESMF_InternArrayGetMod
!
!==============================================================================
!
! This file contains the Array class methods which are automatically
! generated from macros to handle the type/kind/rank overloading.
! See ESMF_ArrayBase.F90 for non-macroized entry points.
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
! < ignore blank lines below. they are created by the files which
! define various macros. >
#include "ESMF.h"
!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod ! ESMF utility types
  use ESMF_InitMacrosMod ! ESMF initializer macros
  use ESMF_BaseMod ! ESMF base class
  use ESMF_LogErrMod ! ESMF error handling
  use ESMF_LocalArrayMod
  use ESMF_InternArrayMod
  use ESMF_InternArrayCreateMod
  implicit none
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
!------------------------------------------------------------------------------
  ! Internal wrapper structures for passing f90 pointers to C++ and
  ! guaranteeing they are passed by reference on all compilers and all
  ! platforms. These are never seen outside this module.
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
  public ESMF_InternArraySetData
  public ESMF_InternArrayGetData
  public operator(.eq.), operator(.ne.)
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_InternArrayGet.cppF90,v 1.1.2.3 2010/02/01 20:51:08 svasquez Exp $'
!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_InternArrayGetData -- Get a Fortran pointer to the data contents
!
! !INTERFACE:
  interface ESMF_InternArrayGetData
!
! !PRIVATE MEMBER FUNCTIONS:
!
      ! < declarations of interfaces for each T/K/R >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
#ifndef ESMF_NO_INTEGER_1_BYTE 
 module procedure ESMF_ArrayGetData1DI1 
 module procedure ESMF_ArrayGetData2DI1 
 module procedure ESMF_ArrayGetData3DI1 
 module procedure ESMF_ArrayGetData4DI1 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_ArrayGetData5DI1 
 module procedure ESMF_ArrayGetData6DI1 
 module procedure ESMF_ArrayGetData7DI1 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
 module procedure ESMF_ArrayGetData1DI2 
 module procedure ESMF_ArrayGetData2DI2 
 module procedure ESMF_ArrayGetData3DI2 
 module procedure ESMF_ArrayGetData4DI2 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_ArrayGetData5DI2 
 module procedure ESMF_ArrayGetData6DI2 
 module procedure ESMF_ArrayGetData7DI2 
#endif 
#endif 
 module procedure ESMF_ArrayGetData1DI4 
 module procedure ESMF_ArrayGetData1DI8 
 module procedure ESMF_ArrayGetData1DR4 
 module procedure ESMF_ArrayGetData1DR8 
 module procedure ESMF_ArrayGetData2DI4 
 module procedure ESMF_ArrayGetData2DI8 
 module procedure ESMF_ArrayGetData2DR4 
 module procedure ESMF_ArrayGetData2DR8 
 module procedure ESMF_ArrayGetData3DI4 
 module procedure ESMF_ArrayGetData3DI8 
 module procedure ESMF_ArrayGetData3DR4 
 module procedure ESMF_ArrayGetData3DR8 
 module procedure ESMF_ArrayGetData4DI4 
 module procedure ESMF_ArrayGetData4DI8 
 module procedure ESMF_ArrayGetData4DR4 
 module procedure ESMF_ArrayGetData4DR8 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_ArrayGetData5DI4 
 module procedure ESMF_ArrayGetData5DI8 
 module procedure ESMF_ArrayGetData5DR4 
 module procedure ESMF_ArrayGetData5DR8 
 module procedure ESMF_ArrayGetData6DI4 
 module procedure ESMF_ArrayGetData6DI8 
 module procedure ESMF_ArrayGetData6DR4 
 module procedure ESMF_ArrayGetData6DR8 
 module procedure ESMF_ArrayGetData7DI4 
 module procedure ESMF_ArrayGetData7DI8 
 module procedure ESMF_ArrayGetData7DR4 
 module procedure ESMF_ArrayGetData7DR8 
#endif 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

! !DESCRIPTION:
! This interface provides a single entry point for the various
! types of {\tt ESMF\_InternArrayGetData} functions.
!
!EOPI
  end interface
!==============================================================================
  contains
!==============================================================================
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_InternArraySetData
!
! !INTERFACE:
  subroutine ESMF_InternArraySetData(array, databuf, docopy, rc)
!
! !ARGUMENTS:
    type(ESMF_InternArray), intent(inout) :: array
    real(ESMF_KIND_R8), dimension (:), pointer :: databuf
    type(ESMF_CopyFlag), intent(in) :: docopy
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used only with the version of ArrayCreate which creates an empty
! Array and allows the Data to be specified later. Otherwise it is an
! error to replace the data contents associated with a Array.
!
! TODO: this needs to be macroized for T/K/R, just like create
!
!EOPI
! !REQUIREMENTS:
!
! Changed BOP/EOP to BOPI/EOPI until code is added.
! TODO: code goes here
!
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
  end subroutine ESMF_InternArraySetData
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!! < start of macros which become actual function bodies after expansion >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_InternArrayGetData - Retrieve a Fortran pointer to Array data 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_InternArrayGetData() 
! subroutine ESMF_ArrayGetData<rank><type><kind>(array, fptr, docopy, rc) 
! 
! !ARGUMENTS: 
! type(ESMF_InternArray) :: array 
! <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: fptr 
! type(ESMF_CopyFlag), intent(in), optional :: docopy 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Given an {\tt ESMF\_Array} 
! return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! Valid type/kind/rank combinations supported by the 
! framework are: ranks 1 to 7, type real of kind *4 or *8, 
! and type integer of kind *1, *2, *4, or *8. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! An {\tt ESMF\_Array}. 
! \item[farr] 
! An allocatable (but currently unallocated) Fortran array pointer. 
! \item[docopy] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the space. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
 
 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData1DI1(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I1), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap1DI1) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(1), ub(1) ! size info for the array 
 integer (ESMF_KIND_I1), dimension(:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 1, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 1, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr1DI1 
 fptr => localp 
 else 
 fptr => wrap % ptr1DI1 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData1DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData2DI1(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I1), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap2DI1) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(2), ub(2) ! size info for the array 
 integer (ESMF_KIND_I1), dimension(:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 2, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 2, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr2DI1 
 fptr => localp 
 else 
 fptr => wrap % ptr2DI1 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData2DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData3DI1(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I1), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap3DI1) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(3), ub(3) ! size info for the array 
 integer (ESMF_KIND_I1), dimension(:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 3, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 3, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr3DI1 
 fptr => localp 
 else 
 fptr => wrap % ptr3DI1 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData3DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData4DI1(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I1), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap4DI1) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(4), ub(4) ! size info for the array 
 integer (ESMF_KIND_I1), dimension(:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 4, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 4, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr4DI1 
 fptr => localp 
 else 
 fptr => wrap % ptr4DI1 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData4DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData5DI1(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap5DI1) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(5), ub(5) ! size info for the array 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 5, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 5, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr5DI1 
 fptr => localp 
 else 
 fptr => wrap % ptr5DI1 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData5DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData6DI1(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap6DI1) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(6), ub(6) ! size info for the array 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 6, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 6, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr6DI1 
 fptr => localp 
 else 
 fptr => wrap % ptr6DI1 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData6DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData7DI1(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap7DI1) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(7), ub(7) ! size info for the array 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 7, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 7, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr7DI1 
 fptr => localp 
 else 
 fptr => wrap % ptr7DI1 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData7DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData1DI2(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I2), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap1DI2) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(1), ub(1) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 1, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 1, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr1DI2 
 fptr => localp 
 else 
 fptr => wrap % ptr1DI2 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData2DI2(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap2DI2) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(2), ub(2) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 2, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 2, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr2DI2 
 fptr => localp 
 else 
 fptr => wrap % ptr2DI2 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData3DI2(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap3DI2) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(3), ub(3) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 3, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 3, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr3DI2 
 fptr => localp 
 else 
 fptr => wrap % ptr3DI2 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData4DI2(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap4DI2) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(4), ub(4) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 4, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 4, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr4DI2 
 fptr => localp 
 else 
 fptr => wrap % ptr4DI2 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData5DI2(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap5DI2) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(5), ub(5) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 5, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 5, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr5DI2 
 fptr => localp 
 else 
 fptr => wrap % ptr5DI2 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData6DI2(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap6DI2) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(6), ub(6) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 6, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 6, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr6DI2 
 fptr => localp 
 else 
 fptr => wrap % ptr6DI2 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData6DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData7DI2(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap7DI2) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(7), ub(7) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 7, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 7, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr7DI2 
 fptr => localp 
 else 
 fptr => wrap % ptr7DI2 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData7DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData1DI4(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I4), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap1DI4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(1), ub(1) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 1, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 1, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr1DI4 
 fptr => localp 
 else 
 fptr => wrap % ptr1DI4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData1DI8(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I8), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap1DI8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(1), ub(1) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 1, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 1, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr1DI8 
 fptr => localp 
 else 
 fptr => wrap % ptr1DI8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData1DR4(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 real (ESMF_KIND_R4), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap1DR4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(1), ub(1) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 1, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 1, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr1DR4 
 fptr => localp 
 else 
 fptr => wrap % ptr1DR4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData1DR8(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 real (ESMF_KIND_R8), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap1DR8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(1), ub(1) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 1, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 1, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr1DR8 
 fptr => localp 
 else 
 fptr => wrap % ptr1DR8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData2DI4(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap2DI4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(2), ub(2) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 2, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 2, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr2DI4 
 fptr => localp 
 else 
 fptr => wrap % ptr2DI4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData2DI8(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap2DI8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(2), ub(2) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 2, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 2, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr2DI8 
 fptr => localp 
 else 
 fptr => wrap % ptr2DI8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData2DR4(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap2DR4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(2), ub(2) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 2, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 2, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr2DR4 
 fptr => localp 
 else 
 fptr => wrap % ptr2DR4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData2DR8(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap2DR8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(2), ub(2) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 2, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 2, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr2DR8 
 fptr => localp 
 else 
 fptr => wrap % ptr2DR8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData3DI4(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap3DI4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(3), ub(3) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 3, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 3, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr3DI4 
 fptr => localp 
 else 
 fptr => wrap % ptr3DI4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData3DI8(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap3DI8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(3), ub(3) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 3, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 3, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr3DI8 
 fptr => localp 
 else 
 fptr => wrap % ptr3DI8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData3DR4(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap3DR4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(3), ub(3) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 3, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 3, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr3DR4 
 fptr => localp 
 else 
 fptr => wrap % ptr3DR4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData3DR8(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap3DR8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(3), ub(3) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 3, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 3, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr3DR8 
 fptr => localp 
 else 
 fptr => wrap % ptr3DR8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData4DI4(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap4DI4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(4), ub(4) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 4, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 4, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr4DI4 
 fptr => localp 
 else 
 fptr => wrap % ptr4DI4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData4DI8(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap4DI8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(4), ub(4) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 4, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 4, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr4DI8 
 fptr => localp 
 else 
 fptr => wrap % ptr4DI8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData4DR4(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap4DR4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(4), ub(4) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 4, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 4, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr4DR4 
 fptr => localp 
 else 
 fptr => wrap % ptr4DR4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData4DR8(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap4DR8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(4), ub(4) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 4, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 4, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr4DR8 
 fptr => localp 
 else 
 fptr => wrap % ptr4DR8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData5DI4(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap5DI4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(5), ub(5) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 5, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 5, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr5DI4 
 fptr => localp 
 else 
 fptr => wrap % ptr5DI4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData5DI8(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap5DI8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(5), ub(5) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 5, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 5, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr5DI8 
 fptr => localp 
 else 
 fptr => wrap % ptr5DI8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData5DR4(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap5DR4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(5), ub(5) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 5, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 5, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr5DR4 
 fptr => localp 
 else 
 fptr => wrap % ptr5DR4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData5DR8(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap5DR8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(5), ub(5) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 5, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 5, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr5DR8 
 fptr => localp 
 else 
 fptr => wrap % ptr5DR8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData6DI4(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap6DI4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(6), ub(6) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 6, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 6, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr6DI4 
 fptr => localp 
 else 
 fptr => wrap % ptr6DI4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData6DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData6DI8(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap6DI8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(6), ub(6) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 6, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 6, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr6DI8 
 fptr => localp 
 else 
 fptr => wrap % ptr6DI8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData6DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData6DR4(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap6DR4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(6), ub(6) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 6, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 6, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr6DR4 
 fptr => localp 
 else 
 fptr => wrap % ptr6DR4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData6DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData6DR8(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap6DR8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(6), ub(6) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 6, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 6, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr6DR8 
 fptr => localp 
 else 
 fptr => wrap % ptr6DR8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData6DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData7DI4(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap7DI4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(7), ub(7) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 7, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 7, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr7DI4 
 fptr => localp 
 else 
 fptr => wrap % ptr7DI4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData7DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData7DI8(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap7DI8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(7), ub(7) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 7, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 7, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr7DI8 
 fptr => localp 
 else 
 fptr => wrap % ptr7DI8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData7DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData7DR4(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap7DR4) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(7), ub(7) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 7, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 7, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr7DR4 
 fptr => localp 
 else 
 fptr => wrap % ptr7DR4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData7DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_InternArrayGetData##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_InternArrayGetData" 
 subroutine ESMF_ArrayGetData7DR8(array, fptr, docopy, rc) 
 
 type(ESMF_InternArray) :: array 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrap7DR8) :: wrap ! for passing f90 ptr to C++ 
 integer :: lb(7), ub(7) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Check init status of arguments 
 ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc) 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_IArrayGetLbounds(array, 7, lb, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(array, 7, ub, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Array local copy", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 localp = wrap % ptr7DR8 
 fptr => localp 
 else 
 fptr => wrap % ptr7DR8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData7DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
end module ESMF_InternArrayGetMod
