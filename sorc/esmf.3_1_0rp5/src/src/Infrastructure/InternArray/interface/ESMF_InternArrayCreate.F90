! $Id: ESMF_InternArrayCreate.cppF90,v 1.1.2.3 2010/02/01 20:51:08 svasquez Exp $
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
#define ESMF_FILENAME "ESMF_InternArrayCreate.F90"
!==============================================================================
!
! ESMF InternArrayCreate module
module ESMF_InternArrayCreateMod
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
  use ESMF_ArraySpecMod
  use ESMF_LocalArrayMod
  use ESMF_InternArrayMod
  implicit none
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
!------------------------------------------------------------------------------
  ! ESMF_ArrayOrigin
  ! Private flag which indicates the create was initiated on the F90 side.
  ! This matches an enum on the C++ side and the values must match.
  ! Update ../include/ESMC_Array.h if you change these values.
  type ESMF_ArrayOrigin
  sequence
  private
    integer :: origin
  end type
  type(ESMF_ArrayOrigin), parameter :: &
    ESMF_FROM_FORTRAN = ESMF_ArrayOrigin(1), &
    ESMF_FROM_CPLUSPLUS = ESMF_ArrayOrigin(2)
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
  public ESMF_InternArrayCreate
  public ESMF_InternArrayDestroy
  public ESMF_InternArrayF90Allocate
  public ESMF_InternArrayF90Deallocate
  public ESMF_InternArrayConstructF90Ptr ! needed for C++ callback only
!EOP
  public operator(.eq.), operator(.ne.)
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_InternArrayCreate.cppF90,v 1.1.2.3 2010/02/01 20:51:08 svasquez Exp $'
!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !IROUTINE: ESMF_InternArrayCreate -- Generic interface to create an Array
!
! !INTERFACE:
  interface ESMF_InternArrayCreate
!
! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_ArrayCreateByList ! specify TKR
    module procedure ESMF_ArrayCreateBySpec ! specify ArraySpec
!
        ! Plus interfaces for each T/K/R
!EOP
       ! < interfaces for each T/K/R >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
#ifndef ESMF_NO_INTEGER_1_BYTE 
 module procedure ESMF_ArrayCreateByMTPtr1DI1 
 module procedure ESMF_ArrayCreateByMTPtr2DI1 
 module procedure ESMF_ArrayCreateByMTPtr3DI1 
 module procedure ESMF_ArrayCreateByMTPtr4DI1 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_ArrayCreateByMTPtr5DI1 
 module procedure ESMF_ArrayCreateByMTPtr6DI1 
 module procedure ESMF_ArrayCreateByMTPtr7DI1 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
 module procedure ESMF_ArrayCreateByMTPtr1DI2 
 module procedure ESMF_ArrayCreateByMTPtr2DI2 
 module procedure ESMF_ArrayCreateByMTPtr3DI2 
 module procedure ESMF_ArrayCreateByMTPtr4DI2 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_ArrayCreateByMTPtr5DI2 
 module procedure ESMF_ArrayCreateByMTPtr6DI2 
 module procedure ESMF_ArrayCreateByMTPtr7DI2 
#endif 
#endif 
 module procedure ESMF_ArrayCreateByMTPtr1DI4 
 module procedure ESMF_ArrayCreateByMTPtr1DI8 
 module procedure ESMF_ArrayCreateByMTPtr1DR4 
 module procedure ESMF_ArrayCreateByMTPtr1DR8 
 module procedure ESMF_ArrayCreateByMTPtr2DI4 
 module procedure ESMF_ArrayCreateByMTPtr2DI8 
 module procedure ESMF_ArrayCreateByMTPtr2DR4 
 module procedure ESMF_ArrayCreateByMTPtr2DR8 
 module procedure ESMF_ArrayCreateByMTPtr3DI4 
 module procedure ESMF_ArrayCreateByMTPtr3DI8 
 module procedure ESMF_ArrayCreateByMTPtr3DR4 
 module procedure ESMF_ArrayCreateByMTPtr3DR8 
 module procedure ESMF_ArrayCreateByMTPtr4DI4 
 module procedure ESMF_ArrayCreateByMTPtr4DI8 
 module procedure ESMF_ArrayCreateByMTPtr4DR4 
 module procedure ESMF_ArrayCreateByMTPtr4DR8 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_ArrayCreateByMTPtr5DI4 
 module procedure ESMF_ArrayCreateByMTPtr5DI8 
 module procedure ESMF_ArrayCreateByMTPtr5DR4 
 module procedure ESMF_ArrayCreateByMTPtr5DR8 
 module procedure ESMF_ArrayCreateByMTPtr6DI4 
 module procedure ESMF_ArrayCreateByMTPtr6DI8 
 module procedure ESMF_ArrayCreateByMTPtr6DR4 
 module procedure ESMF_ArrayCreateByMTPtr6DR8 
 module procedure ESMF_ArrayCreateByMTPtr7DI4 
 module procedure ESMF_ArrayCreateByMTPtr7DI8 
 module procedure ESMF_ArrayCreateByMTPtr7DR4 
 module procedure ESMF_ArrayCreateByMTPtr7DR8 
#endif 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

       ! < interfaces for each T/K/R >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
#ifndef ESMF_NO_INTEGER_1_BYTE 
 module procedure ESMF_ArrayCreateByFullPtr1DI1 
 module procedure ESMF_ArrayCreateByFullPtr2DI1 
 module procedure ESMF_ArrayCreateByFullPtr3DI1 
 module procedure ESMF_ArrayCreateByFullPtr4DI1 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_ArrayCreateByFullPtr5DI1 
 module procedure ESMF_ArrayCreateByFullPtr6DI1 
 module procedure ESMF_ArrayCreateByFullPtr7DI1 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
 module procedure ESMF_ArrayCreateByFullPtr1DI2 
 module procedure ESMF_ArrayCreateByFullPtr2DI2 
 module procedure ESMF_ArrayCreateByFullPtr3DI2 
 module procedure ESMF_ArrayCreateByFullPtr4DI2 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_ArrayCreateByFullPtr5DI2 
 module procedure ESMF_ArrayCreateByFullPtr6DI2 
 module procedure ESMF_ArrayCreateByFullPtr7DI2 
#endif 
#endif 
 module procedure ESMF_ArrayCreateByFullPtr1DI4 
 module procedure ESMF_ArrayCreateByFullPtr1DI8 
 module procedure ESMF_ArrayCreateByFullPtr1DR4 
 module procedure ESMF_ArrayCreateByFullPtr1DR8 
 module procedure ESMF_ArrayCreateByFullPtr2DI4 
 module procedure ESMF_ArrayCreateByFullPtr2DI8 
 module procedure ESMF_ArrayCreateByFullPtr2DR4 
 module procedure ESMF_ArrayCreateByFullPtr2DR8 
 module procedure ESMF_ArrayCreateByFullPtr3DI4 
 module procedure ESMF_ArrayCreateByFullPtr3DI8 
 module procedure ESMF_ArrayCreateByFullPtr3DR4 
 module procedure ESMF_ArrayCreateByFullPtr3DR8 
 module procedure ESMF_ArrayCreateByFullPtr4DI4 
 module procedure ESMF_ArrayCreateByFullPtr4DI8 
 module procedure ESMF_ArrayCreateByFullPtr4DR4 
 module procedure ESMF_ArrayCreateByFullPtr4DR8 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_ArrayCreateByFullPtr5DI4 
 module procedure ESMF_ArrayCreateByFullPtr5DI8 
 module procedure ESMF_ArrayCreateByFullPtr5DR4 
 module procedure ESMF_ArrayCreateByFullPtr5DR8 
 module procedure ESMF_ArrayCreateByFullPtr6DI4 
 module procedure ESMF_ArrayCreateByFullPtr6DI8 
 module procedure ESMF_ArrayCreateByFullPtr6DR4 
 module procedure ESMF_ArrayCreateByFullPtr6DR8 
 module procedure ESMF_ArrayCreateByFullPtr7DI4 
 module procedure ESMF_ArrayCreateByFullPtr7DI8 
 module procedure ESMF_ArrayCreateByFullPtr7DR4 
 module procedure ESMF_ArrayCreateByFullPtr7DR8 
#endif 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!BOP
! !DESCRIPTION:
! This interface provides a single (heavily overloaded) entry point for
! the various types of {\tt ESMF\_ArrayCreate} functions.
!
! There are 3 options for setting the contents of the {\tt ESMF\_Array}
! at creation time:
! \begin{description}
! \item[Allocate Space Only]
! Data space is allocated but not initialized. The caller can query
! for a pointer to the start of the space to address it directly.
! The caller must not deallocate the space; the
! {\tt ESMF\_Array} will release the space when it is destroyed.
! \item[Data Copy]
! An existing Fortran array is specified and the data contents are copied
! into new space allocated by the {\tt ESMF\_Array}.
! The caller must not deallocate the space; the
! {\tt ESMF\_Array} will release the space when it is destroyed.
! \item[Data Reference]
! An existing Fortran array is specified and the data contents reference
! it directly. The caller is responsible for deallocating the space;
! when the {\tt ESMF\_Array} is destroyed it will not release the space.
! \end{description}
!
! There are 3 options for
! specifying the type/kind/rank of the {\tt ESMF\_Array} data:
! \begin{description}
! \item[List]
! The characteristics of the {\tt ESMF\_Array} are given explicitly
! by individual arguments to the create function.
! \item[ArraySpec]
! A previously created {\tt ESMF\_ArraySpec} object is given which
! describes the characteristics.
! \item[Fortran 90 Pointer]
! An associated or unassociated Fortran 90 array pointer is used to
! describe the array.
! (Only available from the Fortran interface.)
! \end{description}
!
! The concept of an ``empty'' {\tt Array} does not exist. To make an
! ESMF object which stores the Type/Kind/Rank information create an
! {\tt ESMF\_ArraySpec} object which can then be used repeatedly in
! subsequent {\tt Array} Create calls.
!
  end interface
!EOP
!==============================================================================
  contains
!==============================================================================
!------------------------------------------------------------------------------
!! < start of macros which become actual function bodies after expansion >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreate - Make an ESMF array from an allocated Fortran array 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_ArrayCreate() 
! function ESMF_ArrayCreateByFullPtr<rank><type><kind>(farr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
! type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr<rank><type><kind> 
! 
! !ARGUMENTS: 
! <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: farr 
! type(ESMF_CopyFlag), intent(in), optional :: docopy 
! integer, intent(in), optional :: haloWidth 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Create an {\tt ESMF\_Array} based on an already allocated Fortran array 
! pointer. This routine can make a copy or reference the existing data 
! and saves all necessary information about bounds, data type, kind, etc. 
! Valid type/kind/rank combinations supported by the 
! framework are: ranks 1 to 7, type real of kind *4 or *8, 
! and type integer of kind *1, *2, *4, or *8. 
! 
! The function return is an {\tt ESMF\_Array} type. 
! 
! The arguments are: 
! \begin{description} 
! \item[farr] 
! An allocated Fortran array pointer. 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer 
! into the new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
 
 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr1DI1(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr1DI1 
 
 integer (ESMF_KIND_I1), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: counts ! per dim 
 integer, dimension(1) :: lbounds ! per dim 
 integer, dimension(1) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 1, ESMF_TYPEKIND_I1, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr1DI1(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr1DI1 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr1DI1) 
 
 end function ESMF_ArrayCreateByFullPtr1DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr2DI1(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr2DI1 
 
 integer (ESMF_KIND_I1), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: counts ! per dim 
 integer, dimension(2) :: lbounds ! per dim 
 integer, dimension(2) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 2, ESMF_TYPEKIND_I1, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr2DI1(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr2DI1 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr2DI1) 
 
 end function ESMF_ArrayCreateByFullPtr2DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr3DI1(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr3DI1 
 
 integer (ESMF_KIND_I1), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: counts ! per dim 
 integer, dimension(3) :: lbounds ! per dim 
 integer, dimension(3) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 3, ESMF_TYPEKIND_I1, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr3DI1(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr3DI1 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr3DI1) 
 
 end function ESMF_ArrayCreateByFullPtr3DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr4DI1(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr4DI1 
 
 integer (ESMF_KIND_I1), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: counts ! per dim 
 integer, dimension(4) :: lbounds ! per dim 
 integer, dimension(4) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 4, ESMF_TYPEKIND_I1, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr4DI1(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr4DI1 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr4DI1) 
 
 end function ESMF_ArrayCreateByFullPtr4DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr5DI1(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr5DI1 
 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: counts ! per dim 
 integer, dimension(5) :: lbounds ! per dim 
 integer, dimension(5) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 5, ESMF_TYPEKIND_I1, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr5DI1(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr5DI1 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr5DI1) 
 
 end function ESMF_ArrayCreateByFullPtr5DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr6DI1(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr6DI1 
 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(6) :: counts ! per dim 
 integer, dimension(6) :: lbounds ! per dim 
 integer, dimension(6) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 6, ESMF_TYPEKIND_I1, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr6DI1(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr6DI1 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr6DI1) 
 
 end function ESMF_ArrayCreateByFullPtr6DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr7DI1(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr7DI1 
 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(7) :: counts ! per dim 
 integer, dimension(7) :: lbounds ! per dim 
 integer, dimension(7) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 7, ESMF_TYPEKIND_I1, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr7DI1(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr7DI1 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr7DI1) 
 
 end function ESMF_ArrayCreateByFullPtr7DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr1DI2(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr1DI2 
 
 integer (ESMF_KIND_I2), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: counts ! per dim 
 integer, dimension(1) :: lbounds ! per dim 
 integer, dimension(1) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 1, ESMF_TYPEKIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr1DI2(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr1DI2 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr1DI2) 
 
 end function ESMF_ArrayCreateByFullPtr1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr2DI2(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr2DI2 
 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: counts ! per dim 
 integer, dimension(2) :: lbounds ! per dim 
 integer, dimension(2) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 2, ESMF_TYPEKIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr2DI2(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr2DI2 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr2DI2) 
 
 end function ESMF_ArrayCreateByFullPtr2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr3DI2(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr3DI2 
 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: counts ! per dim 
 integer, dimension(3) :: lbounds ! per dim 
 integer, dimension(3) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 3, ESMF_TYPEKIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr3DI2(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr3DI2 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr3DI2) 
 
 end function ESMF_ArrayCreateByFullPtr3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr4DI2(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr4DI2 
 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: counts ! per dim 
 integer, dimension(4) :: lbounds ! per dim 
 integer, dimension(4) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 4, ESMF_TYPEKIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr4DI2(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr4DI2 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr4DI2) 
 
 end function ESMF_ArrayCreateByFullPtr4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr5DI2(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr5DI2 
 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: counts ! per dim 
 integer, dimension(5) :: lbounds ! per dim 
 integer, dimension(5) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 5, ESMF_TYPEKIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr5DI2(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr5DI2 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr5DI2) 
 
 end function ESMF_ArrayCreateByFullPtr5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr6DI2(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr6DI2 
 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(6) :: counts ! per dim 
 integer, dimension(6) :: lbounds ! per dim 
 integer, dimension(6) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 6, ESMF_TYPEKIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr6DI2(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr6DI2 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr6DI2) 
 
 end function ESMF_ArrayCreateByFullPtr6DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr7DI2(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr7DI2 
 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(7) :: counts ! per dim 
 integer, dimension(7) :: lbounds ! per dim 
 integer, dimension(7) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 7, ESMF_TYPEKIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr7DI2(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr7DI2 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr7DI2) 
 
 end function ESMF_ArrayCreateByFullPtr7DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr1DI4(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr1DI4 
 
 integer (ESMF_KIND_I4), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: counts ! per dim 
 integer, dimension(1) :: lbounds ! per dim 
 integer, dimension(1) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 1, ESMF_TYPEKIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr1DI4(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr1DI4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr1DI4) 
 
 end function ESMF_ArrayCreateByFullPtr1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr1DI8(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr1DI8 
 
 integer (ESMF_KIND_I8), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: counts ! per dim 
 integer, dimension(1) :: lbounds ! per dim 
 integer, dimension(1) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 1, ESMF_TYPEKIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr1DI8(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr1DI8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr1DI8) 
 
 end function ESMF_ArrayCreateByFullPtr1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr1DR4(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr1DR4 
 
 real (ESMF_KIND_R4), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: counts ! per dim 
 integer, dimension(1) :: lbounds ! per dim 
 integer, dimension(1) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 1, ESMF_TYPEKIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr1DR4(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr1DR4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr1DR4) 
 
 end function ESMF_ArrayCreateByFullPtr1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr1DR8(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr1DR8 
 
 real (ESMF_KIND_R8), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: counts ! per dim 
 integer, dimension(1) :: lbounds ! per dim 
 integer, dimension(1) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 1, ESMF_TYPEKIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr1DR8(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr1DR8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr1DR8) 
 
 end function ESMF_ArrayCreateByFullPtr1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr2DI4(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr2DI4 
 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: counts ! per dim 
 integer, dimension(2) :: lbounds ! per dim 
 integer, dimension(2) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 2, ESMF_TYPEKIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr2DI4(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr2DI4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr2DI4) 
 
 end function ESMF_ArrayCreateByFullPtr2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr2DI8(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr2DI8 
 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: counts ! per dim 
 integer, dimension(2) :: lbounds ! per dim 
 integer, dimension(2) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 2, ESMF_TYPEKIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr2DI8(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr2DI8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr2DI8) 
 
 end function ESMF_ArrayCreateByFullPtr2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr2DR4(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr2DR4 
 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: counts ! per dim 
 integer, dimension(2) :: lbounds ! per dim 
 integer, dimension(2) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 2, ESMF_TYPEKIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr2DR4(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr2DR4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr2DR4) 
 
 end function ESMF_ArrayCreateByFullPtr2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr2DR8(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr2DR8 
 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: counts ! per dim 
 integer, dimension(2) :: lbounds ! per dim 
 integer, dimension(2) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 2, ESMF_TYPEKIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr2DR8(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr2DR8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr2DR8) 
 
 end function ESMF_ArrayCreateByFullPtr2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr3DI4(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr3DI4 
 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: counts ! per dim 
 integer, dimension(3) :: lbounds ! per dim 
 integer, dimension(3) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 3, ESMF_TYPEKIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr3DI4(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr3DI4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr3DI4) 
 
 end function ESMF_ArrayCreateByFullPtr3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr3DI8(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr3DI8 
 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: counts ! per dim 
 integer, dimension(3) :: lbounds ! per dim 
 integer, dimension(3) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 3, ESMF_TYPEKIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr3DI8(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr3DI8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr3DI8) 
 
 end function ESMF_ArrayCreateByFullPtr3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr3DR4(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr3DR4 
 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: counts ! per dim 
 integer, dimension(3) :: lbounds ! per dim 
 integer, dimension(3) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 3, ESMF_TYPEKIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr3DR4(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr3DR4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr3DR4) 
 
 end function ESMF_ArrayCreateByFullPtr3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr3DR8(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr3DR8 
 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: counts ! per dim 
 integer, dimension(3) :: lbounds ! per dim 
 integer, dimension(3) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 3, ESMF_TYPEKIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr3DR8(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr3DR8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr3DR8) 
 
 end function ESMF_ArrayCreateByFullPtr3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr4DI4(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr4DI4 
 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: counts ! per dim 
 integer, dimension(4) :: lbounds ! per dim 
 integer, dimension(4) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 4, ESMF_TYPEKIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr4DI4(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr4DI4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr4DI4) 
 
 end function ESMF_ArrayCreateByFullPtr4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr4DI8(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr4DI8 
 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: counts ! per dim 
 integer, dimension(4) :: lbounds ! per dim 
 integer, dimension(4) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 4, ESMF_TYPEKIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr4DI8(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr4DI8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr4DI8) 
 
 end function ESMF_ArrayCreateByFullPtr4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr4DR4(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr4DR4 
 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: counts ! per dim 
 integer, dimension(4) :: lbounds ! per dim 
 integer, dimension(4) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 4, ESMF_TYPEKIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr4DR4(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr4DR4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr4DR4) 
 
 end function ESMF_ArrayCreateByFullPtr4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr4DR8(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr4DR8 
 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: counts ! per dim 
 integer, dimension(4) :: lbounds ! per dim 
 integer, dimension(4) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 4, ESMF_TYPEKIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr4DR8(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr4DR8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr4DR8) 
 
 end function ESMF_ArrayCreateByFullPtr4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr5DI4(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr5DI4 
 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: counts ! per dim 
 integer, dimension(5) :: lbounds ! per dim 
 integer, dimension(5) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 5, ESMF_TYPEKIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr5DI4(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr5DI4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr5DI4) 
 
 end function ESMF_ArrayCreateByFullPtr5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr5DI8(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr5DI8 
 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: counts ! per dim 
 integer, dimension(5) :: lbounds ! per dim 
 integer, dimension(5) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 5, ESMF_TYPEKIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr5DI8(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr5DI8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr5DI8) 
 
 end function ESMF_ArrayCreateByFullPtr5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr5DR4(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr5DR4 
 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: counts ! per dim 
 integer, dimension(5) :: lbounds ! per dim 
 integer, dimension(5) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 5, ESMF_TYPEKIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr5DR4(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr5DR4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr5DR4) 
 
 end function ESMF_ArrayCreateByFullPtr5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr5DR8(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr5DR8 
 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: counts ! per dim 
 integer, dimension(5) :: lbounds ! per dim 
 integer, dimension(5) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 5, ESMF_TYPEKIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr5DR8(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr5DR8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr5DR8) 
 
 end function ESMF_ArrayCreateByFullPtr5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr6DI4(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr6DI4 
 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(6) :: counts ! per dim 
 integer, dimension(6) :: lbounds ! per dim 
 integer, dimension(6) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 6, ESMF_TYPEKIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr6DI4(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr6DI4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr6DI4) 
 
 end function ESMF_ArrayCreateByFullPtr6DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr6DI8(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr6DI8 
 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(6) :: counts ! per dim 
 integer, dimension(6) :: lbounds ! per dim 
 integer, dimension(6) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 6, ESMF_TYPEKIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr6DI8(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr6DI8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr6DI8) 
 
 end function ESMF_ArrayCreateByFullPtr6DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr6DR4(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr6DR4 
 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(6) :: counts ! per dim 
 integer, dimension(6) :: lbounds ! per dim 
 integer, dimension(6) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 6, ESMF_TYPEKIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr6DR4(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr6DR4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr6DR4) 
 
 end function ESMF_ArrayCreateByFullPtr6DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr6DR8(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr6DR8 
 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(6) :: counts ! per dim 
 integer, dimension(6) :: lbounds ! per dim 
 integer, dimension(6) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 6, ESMF_TYPEKIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr6DR8(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr6DR8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr6DR8) 
 
 end function ESMF_ArrayCreateByFullPtr6DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr7DI4(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr7DI4 
 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(7) :: counts ! per dim 
 integer, dimension(7) :: lbounds ! per dim 
 integer, dimension(7) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 7, ESMF_TYPEKIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr7DI4(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr7DI4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr7DI4) 
 
 end function ESMF_ArrayCreateByFullPtr7DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr7DI8(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr7DI8 
 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(7) :: counts ! per dim 
 integer, dimension(7) :: lbounds ! per dim 
 integer, dimension(7) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 7, ESMF_TYPEKIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr7DI8(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr7DI8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr7DI8) 
 
 end function ESMF_ArrayCreateByFullPtr7DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr7DR4(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr7DR4 
 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(7) :: counts ! per dim 
 integer, dimension(7) :: lbounds ! per dim 
 integer, dimension(7) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 7, ESMF_TYPEKIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr7DR4(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr7DR4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr7DR4) 
 
 end function ESMF_ArrayCreateByFullPtr7DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" 
 
 function ESMF_ArrayCreateByFullPtr7DR8(fptr, docopy, & 
 haloWidth, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr7DR8 
 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(7) :: counts ! per dim 
 integer, dimension(7) :: lbounds ! per dim 
 integer, dimension(7) :: ubounds ! per dim 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer must already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(fptr) 
 lbounds = lbound(fptr) 
 ubounds = ubound(fptr) 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 7, ESMF_TYPEKIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr7DR8(array, counts, hwidth,& 
 fptr, copy, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr7DR8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr7DR8) 
 
 end function ESMF_ArrayCreateByFullPtr7DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!! < end of automatically generated functions >
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayCreateByList"
!BOP
! !IROUTINE: ESMF_ArrayCreate -- Create an Array specifying all options.
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayCreate()
      function ESMF_ArrayCreateByList(rank, kind, counts, &
                                      haloWidth, lbounds, ubounds, rc)
!
! !RETURN VALUE:
      type(ESMF_InternArray) :: ESMF_ArrayCreateByList
!
! !ARGUMENTS:
      integer, intent(in) :: rank
      type(ESMF_TypeKind), intent(in) :: kind
      integer, dimension(:), intent(in) :: counts
      integer, intent(in), optional :: haloWidth
      integer, dimension(:), intent(in), optional :: lbounds
      integer, dimension(:), intent(in), optional :: ubounds
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Create a new {\tt ESMF\_Array} and allocate data space, which
! remains uninitialized. The return value is the new {\tt ESMF\_Array}.
!
! The arguments are:
! \begin{description}
! \item[rank]
! Array rank (dimensionality -- 1D, 2D, etc). Maximum allowed is 7D.
! \item[kind]
! Array kind. Valid kinds include {\tt ESMF\_I4},
! {\tt ESMF\_I8}, {\tt ESMF\_R4}, {\tt ESMF\_R8}.
! %%% {\tt ESMF\_C8}, {\tt ESMF\_C16}. % add back when supported
! \item[counts]
! The number of items in each dimension of the array. This is a 1D
! integer array the same length as the {\tt rank}.
! \item[{[haloWidth]}]
! Set the maximum width of the halo region on all edges. Defaults to 0.
! \item[{[lbounds]}]
! An integer array of length {\tt rank} with the lower index
! for each dimension.
! \item[{[ubounds]}]
! An integer array of length {\tt rank} with the upper index
! for each dimension.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
        ! Local vars
        type(ESMF_InternArray) :: array ! new C++ Array
        integer :: hwidth ! local copy of halo width
        integer, dimension(ESMF_MAXDIM) :: lb, ub ! local bounds
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.
        array%this = ESMF_NULL_POINTER
        ! Initialize return code; assume routine not implemented
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif
        ! Always supply a halo value, setting it to 0 if not specified.
        if (present(haloWidth)) then
          hwidth = haloWidth
        else
          hwidth = 0
        endif
        ! Assume defaults first, then alter if lb or ub specified.
        lb = 1
        ub(1:size(counts)) = counts
        if (present(lbounds)) then
            lb(1:size(lbounds)) = lbounds
        endif
        if (present(ubounds)) then
            ub(1:size(ubounds)) = ubounds
        endif
        ! TODO: should this take the counts, or not? for now i am going to
        ! set the counts after i have created the f90 array and not here.
        call c_ESMC_IArrayCreateNoData(array, rank, kind, &
                                            ESMF_FROM_FORTRAN, status)
        if (ESMF_LogMsgFoundError(status, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        call ESMF_InternArrayConstructF90Ptr(array, counts, hwidth, rank, &
                                       kind, lb, ub, status)
        ! Set return values
        ESMF_ArrayCreateByList = array
        if (rcpresent) rc = status
        ! Set init code
        ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByList)
        end function ESMF_ArrayCreateByList
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!! < start of macros which become actual function bodies after expansion >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreate - Make an ESMF array from an unallocated Fortran array pointer 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_ArrayCreate() 
! function ESMF_ArrayCreateByMTPtr<rank><type><kind>(farr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
! type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr<rank><type><kind> 
! 
! !ARGUMENTS: 
! <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: farr 
! integer, dimension(:), intent(in) :: counts 
! integer, intent(in), optional :: haloWidth 
! integer, dimension(:), intent(in), optional :: lbounds 
! integer, dimension(:), intent(in), optional :: ubounds 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt ESMF\_Array} based on an unallocated (but allocatable) 
! Fortran array pointer. This routine allocates memory to the array and 
! saves all necessary information about bounds, data type, kind, etc. 
! Valid type/kind/rank combinations supported by the 
! framework are: ranks 1 to 7, type real of kind *4 or *8, 
! and type integer of kind *1, *2, *4, or *8. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[farr] 
! An allocatable (but currently unallocated) Fortran array pointer. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length 
! as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length 
! as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOPI 
 
 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr1DI1(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr1DI1 
 
 integer (ESMF_KIND_I1), dimension(:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 1, ESMF_TYPEKIND_I1, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr1DI1(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr1DI1 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr1DI1) 
 
 end function ESMF_ArrayCreateByMTPtr1DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr2DI1(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr2DI1 
 
 integer (ESMF_KIND_I1), dimension(:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 2, ESMF_TYPEKIND_I1, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr2DI1(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr2DI1 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr2DI1) 
 
 end function ESMF_ArrayCreateByMTPtr2DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr3DI1(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr3DI1 
 
 integer (ESMF_KIND_I1), dimension(:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 3, ESMF_TYPEKIND_I1, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr3DI1(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr3DI1 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr3DI1) 
 
 end function ESMF_ArrayCreateByMTPtr3DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr4DI1(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr4DI1 
 
 integer (ESMF_KIND_I1), dimension(:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 4, ESMF_TYPEKIND_I1, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr4DI1(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr4DI1 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr4DI1) 
 
 end function ESMF_ArrayCreateByMTPtr4DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr5DI1(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr5DI1 
 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 5, ESMF_TYPEKIND_I1, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr5DI1(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr5DI1 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr5DI1) 
 
 end function ESMF_ArrayCreateByMTPtr5DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr6DI1(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr6DI1 
 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 6, ESMF_TYPEKIND_I1, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr6DI1(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr6DI1 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr6DI1) 
 
 end function ESMF_ArrayCreateByMTPtr6DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr7DI1(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr7DI1 
 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 7, ESMF_TYPEKIND_I1, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr7DI1(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr7DI1 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr7DI1) 
 
 end function ESMF_ArrayCreateByMTPtr7DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr1DI2(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr1DI2 
 
 integer (ESMF_KIND_I2), dimension(:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 1, ESMF_TYPEKIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr1DI2(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr1DI2 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr1DI2) 
 
 end function ESMF_ArrayCreateByMTPtr1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr2DI2(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr2DI2 
 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 2, ESMF_TYPEKIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr2DI2(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr2DI2 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr2DI2) 
 
 end function ESMF_ArrayCreateByMTPtr2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr3DI2(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr3DI2 
 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 3, ESMF_TYPEKIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr3DI2(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr3DI2 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr3DI2) 
 
 end function ESMF_ArrayCreateByMTPtr3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr4DI2(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr4DI2 
 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 4, ESMF_TYPEKIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr4DI2(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr4DI2 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr4DI2) 
 
 end function ESMF_ArrayCreateByMTPtr4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr5DI2(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr5DI2 
 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 5, ESMF_TYPEKIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr5DI2(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr5DI2 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr5DI2) 
 
 end function ESMF_ArrayCreateByMTPtr5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr6DI2(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr6DI2 
 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 6, ESMF_TYPEKIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr6DI2(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr6DI2 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr6DI2) 
 
 end function ESMF_ArrayCreateByMTPtr6DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr7DI2(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr7DI2 
 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 7, ESMF_TYPEKIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr7DI2(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr7DI2 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr7DI2) 
 
 end function ESMF_ArrayCreateByMTPtr7DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr1DI4(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr1DI4 
 
 integer (ESMF_KIND_I4), dimension(:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 1, ESMF_TYPEKIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr1DI4(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr1DI4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr1DI4) 
 
 end function ESMF_ArrayCreateByMTPtr1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr1DI8(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr1DI8 
 
 integer (ESMF_KIND_I8), dimension(:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 1, ESMF_TYPEKIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr1DI8(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr1DI8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr1DI8) 
 
 end function ESMF_ArrayCreateByMTPtr1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr1DR4(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr1DR4 
 
 real (ESMF_KIND_R4), dimension(:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 1, ESMF_TYPEKIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr1DR4(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr1DR4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr1DR4) 
 
 end function ESMF_ArrayCreateByMTPtr1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr1DR8(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr1DR8 
 
 real (ESMF_KIND_R8), dimension(:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 1, ESMF_TYPEKIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr1DR8(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr1DR8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr1DR8) 
 
 end function ESMF_ArrayCreateByMTPtr1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr2DI4(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr2DI4 
 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 2, ESMF_TYPEKIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr2DI4(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr2DI4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr2DI4) 
 
 end function ESMF_ArrayCreateByMTPtr2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr2DI8(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr2DI8 
 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 2, ESMF_TYPEKIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr2DI8(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr2DI8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr2DI8) 
 
 end function ESMF_ArrayCreateByMTPtr2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr2DR4(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr2DR4 
 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 2, ESMF_TYPEKIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr2DR4(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr2DR4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr2DR4) 
 
 end function ESMF_ArrayCreateByMTPtr2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr2DR8(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr2DR8 
 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 2, ESMF_TYPEKIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr2DR8(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr2DR8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr2DR8) 
 
 end function ESMF_ArrayCreateByMTPtr2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr3DI4(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr3DI4 
 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 3, ESMF_TYPEKIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr3DI4(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr3DI4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr3DI4) 
 
 end function ESMF_ArrayCreateByMTPtr3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr3DI8(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr3DI8 
 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 3, ESMF_TYPEKIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr3DI8(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr3DI8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr3DI8) 
 
 end function ESMF_ArrayCreateByMTPtr3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr3DR4(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr3DR4 
 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 3, ESMF_TYPEKIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr3DR4(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr3DR4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr3DR4) 
 
 end function ESMF_ArrayCreateByMTPtr3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr3DR8(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr3DR8 
 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 3, ESMF_TYPEKIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr3DR8(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr3DR8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr3DR8) 
 
 end function ESMF_ArrayCreateByMTPtr3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr4DI4(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr4DI4 
 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 4, ESMF_TYPEKIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr4DI4(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr4DI4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr4DI4) 
 
 end function ESMF_ArrayCreateByMTPtr4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr4DI8(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr4DI8 
 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 4, ESMF_TYPEKIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr4DI8(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr4DI8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr4DI8) 
 
 end function ESMF_ArrayCreateByMTPtr4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr4DR4(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr4DR4 
 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 4, ESMF_TYPEKIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr4DR4(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr4DR4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr4DR4) 
 
 end function ESMF_ArrayCreateByMTPtr4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr4DR8(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr4DR8 
 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 4, ESMF_TYPEKIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr4DR8(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr4DR8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr4DR8) 
 
 end function ESMF_ArrayCreateByMTPtr4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr5DI4(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr5DI4 
 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 5, ESMF_TYPEKIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr5DI4(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr5DI4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr5DI4) 
 
 end function ESMF_ArrayCreateByMTPtr5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr5DI8(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr5DI8 
 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 5, ESMF_TYPEKIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr5DI8(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr5DI8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr5DI8) 
 
 end function ESMF_ArrayCreateByMTPtr5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr5DR4(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr5DR4 
 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 5, ESMF_TYPEKIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr5DR4(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr5DR4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr5DR4) 
 
 end function ESMF_ArrayCreateByMTPtr5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr5DR8(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr5DR8 
 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 5, ESMF_TYPEKIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr5DR8(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr5DR8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr5DR8) 
 
 end function ESMF_ArrayCreateByMTPtr5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr6DI4(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr6DI4 
 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 6, ESMF_TYPEKIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr6DI4(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr6DI4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr6DI4) 
 
 end function ESMF_ArrayCreateByMTPtr6DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr6DI8(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr6DI8 
 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 6, ESMF_TYPEKIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr6DI8(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr6DI8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr6DI8) 
 
 end function ESMF_ArrayCreateByMTPtr6DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr6DR4(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr6DR4 
 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 6, ESMF_TYPEKIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr6DR4(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr6DR4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr6DR4) 
 
 end function ESMF_ArrayCreateByMTPtr6DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr6DR8(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr6DR8 
 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 6, ESMF_TYPEKIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr6DR8(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr6DR8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr6DR8) 
 
 end function ESMF_ArrayCreateByMTPtr6DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr7DI4(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr7DI4 
 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 7, ESMF_TYPEKIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr7DI4(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr7DI4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr7DI4) 
 
 end function ESMF_ArrayCreateByMTPtr7DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr7DI8(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr7DI8 
 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 7, ESMF_TYPEKIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr7DI8(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr7DI8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr7DI8) 
 
 end function ESMF_ArrayCreateByMTPtr7DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr7DR4(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr7DR4 
 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 7, ESMF_TYPEKIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr7DR4(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr7DR4 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr7DR4) 
 
 end function ESMF_ArrayCreateByMTPtr7DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" 
 function ESMF_ArrayCreateByMTPtr7DR8(fptr, counts, & 
 haloWidth, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr7DR8 
 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type(ESMF_InternArray) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(fptr)) then 
 call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & 
 "Pointer cannot already be allocated", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_IArrayCreateNoData(array, 7, ESMF_TYPEKIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_ArrayConstructF90Ptr7DR8(array, counts, hwidth,& 
 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr7DR8 = array 
 if (present(rc)) rc = status 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr7DR8) 
 
 end function ESMF_ArrayCreateByMTPtr7DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!! < end of automatically generated functions >
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayCreateBySpec"
!BOP
! !IROUTINE: ESMF_ArrayCreate -- Create a new Array from an ArraySpec
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayCreate()
      function ESMF_ArrayCreateBySpec(arrayspec, counts, haloWidth, &
                                      lbounds, ubounds, rc)
!
! !RETURN VALUE:
      type(ESMF_InternArray) :: ESMF_ArrayCreateBySpec
!
! !ARGUMENTS:
      type(ESMF_ArraySpec), intent(inout) :: arrayspec
      integer, intent(in), dimension(:) :: counts
      integer, intent(in), optional :: haloWidth
      integer, dimension(:), intent(in), optional :: lbounds
      integer, dimension(:), intent(in), optional :: ubounds
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Create a new {\tt ESMF\_Array} and allocate data space, which
! remains uninitialized. The return value is the new {\tt ESMF\_Array}.
!
! The arguments are:
! \begin{description}
! \item[arrayspec]
! An {\tt ESMF\_ArraySpec} object which contains the type/kind/rank
! information for the data.
! \item[counts]
! The number of items in each dimension of the array. This is a 1D
! integer array the same length as the {\tt rank}.
! \item[{[haloWidth]}]
! Set the maximum width of the halo region on all edges. Defaults to 0.
! \item[{[lbounds]}]
! An integer array of length {\tt rank} with the lower index
! for each dimension.
! \item[{[ubounds]}]
! An integer array of length {\tt rank}, with the upper index
! for each dimension.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
        ! Local vars
        type(ESMF_InternArray) :: array ! new C++ Array
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?
        integer :: rank
        type(ESMF_TypeKind) :: kind
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.
        array%this = ESMF_NULL_POINTER
        ! Initialize return code; assume routine not implemented
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif
    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArraySpecGetInit, ESMF_ArraySpecInit,arrayspec)
        call ESMF_ArraySpecGet(arrayspec, rank, kind, status)
        if (status .ne. ESMF_SUCCESS) return
        ! Call the list function to make the array
        ESMF_ArrayCreateBySpec = ESMF_ArrayCreateByList(rank, kind, &
                                                       counts, haloWidth, &
                                                       lbounds, ubounds, status)
        if (rcpresent) rc = status
        ! Set init code
        ESMF_INIT_SET_CREATED(ESMF_ArrayCreateBySpec)
        end function ESMF_ArrayCreateBySpec
!------------------------------------------------------------------------------
!! < start of macros which become actual function bodies after expansion >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
! subroutine ESMF_ArrayDeallocate<rank><type><kind>(array, wrap, rc) 
! 
! !ARGUMENTS: 
! type(ESMF_InternArray) :: array 
! type (ESMF_ArrWrap<rank><type><kind>) :: wrap 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if {\tt ESMF\_Array} is responsible 
! for deleting data space. This routine is for internal use only. 
! 
! \begin{description} 
! \item[array] 
! An {\tt ESMF\_Array} object. 
! \item[wrap] 
! A Fortran pointer of the proper type/kind/rank, wrapped in a 
! derived type to allow the pointer itself to be passed by reference. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOPI 
 
 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate1DI1(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap1DI1) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr1DI1) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate1DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate2DI1(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap2DI1) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr2DI1) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate2DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate3DI1(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap3DI1) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr3DI1) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate3DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate4DI1(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap4DI1) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr4DI1) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate4DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate5DI1(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap5DI1) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr5DI1) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate5DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate6DI1(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap6DI1) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr6DI1) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate6DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate7DI1(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap7DI1) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr7DI1) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate7DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate1DI2(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap1DI2) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr1DI2) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate2DI2(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap2DI2) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr2DI2) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate3DI2(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap3DI2) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr3DI2) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate4DI2(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap4DI2) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr4DI2) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate5DI2(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap5DI2) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr5DI2) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate6DI2(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap6DI2) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr6DI2) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate6DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate7DI2(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap7DI2) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr7DI2) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate7DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate1DI4(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap1DI4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr1DI4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate1DI8(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap1DI8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr1DI8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate1DR4(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap1DR4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr1DR4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate1DR8(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap1DR8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr1DR8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate2DI4(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap2DI4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr2DI4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate2DI8(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap2DI8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr2DI8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate2DR4(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap2DR4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr2DR4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate2DR8(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap2DR8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr2DR8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate3DI4(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap3DI4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr3DI4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate3DI8(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap3DI8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr3DI8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate3DR4(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap3DR4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr3DR4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate3DR8(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap3DR8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr3DR8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate4DI4(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap4DI4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr4DI4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate4DI8(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap4DI8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr4DI8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate4DR4(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap4DR4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr4DR4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate4DR8(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap4DR8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr4DR8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate5DI4(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap5DI4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr5DI4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate5DI8(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap5DI8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr5DI8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate5DR4(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap5DR4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr5DR4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate5DR8(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap5DR8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr5DR8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate6DI4(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap6DI4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr6DI4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate6DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate6DI8(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap6DI8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr6DI8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate6DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate6DR4(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap6DR4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr6DR4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate6DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate6DR8(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap6DR8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr6DR8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate6DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate7DI4(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap7DI4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr7DI4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate7DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate7DI8(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap7DI8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr7DI8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate7DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate7DR4(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap7DR4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr7DR4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate7DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayDeallocate" 
 subroutine ESMF_ArrayDeallocate7DR8(array, wrap, rc) 
 
 type(ESMF_InternArray) :: array 
 type (ESMF_ArrWrap7DR8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: status ! local error status 
 
 status = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_IArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr7DR8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate7DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!! < end of automatically generated functions >
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_InternArrayDestroy"
!BOP
! !INTERFACE:
      subroutine ESMF_InternArrayDestroy(array, rc)
!
! !ARGUMENTS:
      type(ESMF_InternArray) :: array
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Releases all resources associated with this {\tt ESMF\_Array}.
!
! The arguments are:
! \begin{description}
! \item[array]
! Destroy contents of this {\tt ESMF\_Array}.
! \item[[rc]]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
! To reduce the depth of crossings of the F90/C++ boundary we first
! query to see if we are responsible for deleting the data space. If so,
! first deallocate the space and then call the C++ code to release
! the object space. When it returns we are done and can return to the user.
! Otherwise we would need to make a nested call back into F90 from C++ to do
! the deallocation during the object delete.
!
!EOP
        ! Local vars
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?
        logical :: needsdealloc ! do we need to free space?
        integer :: rank
        type(ESMF_TypeKind) :: kind
        ! Initialize return code; assume routine not implemented
        status = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit, array, rc)
        needsdealloc = .FALSE.
        ! TODO: document the current rule - if we do the allocate in
        ! the case of ESMF_DATA_COPY at create time then we delete the
        ! space. otherwise, the user needs to destroy the array
        ! (we will ignore the data) and call deallocate themselves.
        ! Call Destruct first, then free this memory
        call c_ESMC_IArrayNeedsDealloc(array, needsdealloc, status)
        if (needsdealloc) then
          call c_ESMC_IArrayGetRank(array, rank, status)
          call c_ESMC_IArrayGetTypeKind(array, kind, status)
          call ESMF_InternArrayF90Deallocate(array, rank, kind, status)
          if (ESMF_LogMsgFoundAllocError(status, "Array deallocate", &
                                         ESMF_CONTEXT, rc)) return
          call c_ESMC_IArraySetNoDealloc(array, status)
        endif
        ! Calling deallocate first means this will not return back to F90
        ! before returning for good.
        call c_ESMC_IArrayDestroy(array, status)
        if (ESMF_LogMsgFoundError(status, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        ! mark this as destroyed
        array%this = ESMF_NULL_POINTER
        ! Set init code
        ESMF_INIT_SET_DELETED(array)
        ! set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS
        end subroutine ESMF_InternArrayDestroy
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_InternArrayConstructF90Ptr"
!BOPI
! !IROUTINE: ESMF_InternArrayConstructF90Ptr - Create and add a Fortran ptr to array
!
! !INTERFACE:
     subroutine ESMF_InternArrayConstructF90Ptr(array, counts, hwidth, &
                                         rank, kind, lbounds, ubounds, rc)
!
! !ARGUMENTS:
      type(ESMF_InternArray), intent(inout) :: array
      integer, dimension(:), intent(in) :: counts
      integer, intent(in) :: hwidth
      integer, intent(in) :: rank
      type(ESMF_TypeKind), intent(in) :: kind
      integer, dimension(:), intent(in) :: lbounds
      integer, dimension(:), intent(in) :: ubounds
      integer, intent(out) :: rc
!
! !DESCRIPTION:
! Take a partially created {\tt ESMF\_Array} and type/kind/rank
! information and call the proper subroutine to create an F90 pointer,
! allocate space, and set the corresponding values in the
! {\tt ESMF\_Array} object header.
!
! The arguments are:
! \begin{description}
! \item[array]
! Partially created {\tt ESMF\_Array} object. This entry point is used
! during both the C++ and F90 create calls if we need to create an F90
! pointer to be used later.
! \item[counts]
! The number of items in each dimension of the array. This is a 1D
! integer array the same length as the {\tt rank}.
! \item[hwidth]
! The halo width on all edges. Used to set the computational area
! in the array.
! \item[rank]
! Array rank.
! This must match what is already in the array - it is here only as
! a convenience.
! \item[kind]
! Array kind.
! This must match what is already in the array - it is here only as
! a convenience.
! \item[lbounds]
! The lower index values per rank.
! \item[ubounds]
! The upper index values per rank.
! \item[rc]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
        ! Local vars
        integer :: localkind
        ! Initialize return code; assume routine not implemented
        ! Note from this point down in the calling stack rc is not optional.
        ! This is all internal code, heavily macroized - no reason to add
        ! unnecessary code to check for non-present error return variables.
        rc = ESMF_RC_NOT_IMPL
        localkind = kind%dkind
        ! Call a T/K/R specific interface in order to create the proper
        ! type of F90 pointer, allocate the space, set the values in the
        ! Array object, and return. (The routine this code is calling is
        ! generated by macro.)
        ! Call proper create F90 ptr routine
        select case (localkind)
#ifndef ESMF_NO_INTEGER_1_BYTE
          case (ESMF_TYPEKIND_I1%dkind)
            select case (rank)
       case (1)
                call ESMF_ArrayConstructF90Ptr1DI1(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (2)
                call ESMF_ArrayConstructF90Ptr2DI1(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (3)
                call ESMF_ArrayConstructF90Ptr3DI1(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (4)
                call ESMF_ArrayConstructF90Ptr4DI1(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_ArrayConstructF90Ptr5DI1(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (6)
                call ESMF_ArrayConstructF90Ptr6DI1(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
              case (7)
                call ESMF_ArrayConstructF90Ptr7DI1(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
#endif
#ifndef ESMF_NO_INTEGER_2_BYTE
          case (ESMF_TYPEKIND_I2%dkind)
     select case(rank)
       case (1)
                call ESMF_ArrayConstructF90Ptr1DI2(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (2)
                call ESMF_ArrayConstructF90Ptr2DI2(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (3)
                call ESMF_ArrayConstructF90Ptr3DI2(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (4)
                call ESMF_ArrayConstructF90Ptr4DI2(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_ArrayConstructF90Ptr5DI2(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (6)
                call ESMF_ArrayConstructF90Ptr6DI2(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
              case (7)
                call ESMF_ArrayConstructF90Ptr7DI2(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
#endif
          case (ESMF_TYPEKIND_I4%dkind)
     select case(rank)
       case (1)
                call ESMF_ArrayConstructF90Ptr1DI4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (2)
                call ESMF_ArrayConstructF90Ptr2DI4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (3)
                call ESMF_ArrayConstructF90Ptr3DI4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (4)
                call ESMF_ArrayConstructF90Ptr4DI4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_ArrayConstructF90Ptr5DI4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (6)
                call ESMF_ArrayConstructF90Ptr6DI4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
              case (7)
                call ESMF_ArrayConstructF90Ptr7DI4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
          case (ESMF_TYPEKIND_I8%dkind)
     select case(rank)
       case (1)
                call ESMF_ArrayConstructF90Ptr1DI8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (2)
                call ESMF_ArrayConstructF90Ptr2DI8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (3)
                call ESMF_ArrayConstructF90Ptr3DI8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (4)
                call ESMF_ArrayConstructF90Ptr4DI8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_ArrayConstructF90Ptr5DI8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (6)
                call ESMF_ArrayConstructF90Ptr6DI8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
              case (7)
                call ESMF_ArrayConstructF90Ptr7DI8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
          case (ESMF_TYPEKIND_R4%dkind)
     select case(rank)
       case (1)
                call ESMF_ArrayConstructF90Ptr1DR4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (2)
                call ESMF_ArrayConstructF90Ptr2DR4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (3)
                call ESMF_ArrayConstructF90Ptr3DR4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (4)
                call ESMF_ArrayConstructF90Ptr4DR4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_ArrayConstructF90Ptr5DR4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (6)
                call ESMF_ArrayConstructF90Ptr6DR4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
              case (7)
                call ESMF_ArrayConstructF90Ptr7DR4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
          case (ESMF_TYPEKIND_R8%dkind)
     select case(rank)
       case (1)
                call ESMF_ArrayConstructF90Ptr1DR8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (2)
                call ESMF_ArrayConstructF90Ptr2DR8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (3)
                call ESMF_ArrayConstructF90Ptr3DR8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (4)
                call ESMF_ArrayConstructF90Ptr4DR8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_ArrayConstructF90Ptr5DR8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
       case (6)
                call ESMF_ArrayConstructF90Ptr6DR8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
              case (7)
                call ESMF_ArrayConstructF90Ptr7DR8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
          case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", &
                                 ESMF_CONTEXT, rc)) return
        end select
        ! Note: rc is already set, nothing to do here.
        end subroutine ESMF_InternArrayConstructF90Ptr
!------------------------------------------------------------------------------
!! < start of macros which become actual function bodies after expansion >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr - Create a Fortran Pointer of the proper T/K/R 
! 
! !INTERFACE: 
! subroutine ESMF_ArrayConstructF90Ptr<rank><type><kind>(array, counts, hwidth, fptr, & 
! docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
! type(ESMF_InternArray), intent(inout) :: array 
! integer, dimension(:), intent(in) :: counts 
! integer, intent(in) :: hwidth 
! <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer, optional :: fptr 
! type(ESMF_CopyFlag), intent(in), optional :: docopy 
! integer, dimension(:), intent(in), optional :: lbounds 
! integer, dimension(:), intent(in), optional :: ubounds 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Create a Fortran pointer of the requested type/kind/rank. 
! After creating the pointer and doing the allocation 
! based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! Valid type/kind/rank combinations supported by the 
! framework are: ranks 1 to 7, type real of kind *4 or *8, 
! and type integer of kind *1, *2, *4, or *8. 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] The {\tt ESMF\_Array} to set the values into. 
! \item[counts] An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] An integer halo width. Currently same width on each edge. 
! \item[{[fptr]}] An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the {\tt docopy} is specified. 
! \item[{[docopy]}] An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOPI 
 
 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr1DI1(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I1), dimension(:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap1DI1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I1), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 1 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr1DI1 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr1DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr2DI1(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I1), dimension(:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap2DI1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I1), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 2 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr2DI1 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr2DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr3DI1(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I1), dimension(:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap3DI1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I1), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 3 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr3DI1 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr3DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr4DI1(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I1), dimension(:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap4DI1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I1), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 4 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr4DI1 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr4DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr5DI1(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap5DI1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 5 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4),counts(5) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr5DI1 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr5DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr6DI1(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap6DI1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 6 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4),counts(5),counts(6) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr6DI1 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr6DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr7DI1(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap7DI1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 7 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4),counts(5),counts(6),counts(7) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr7DI1 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr7DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr1DI2(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I2), dimension(:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap1DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 1 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr1DI2 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr2DI2(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I2), dimension(:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap2DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 2 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr2DI2 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr3DI2(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap3DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 3 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr3DI2 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr4DI2(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap4DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 4 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr4DI2 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr5DI2(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap5DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 5 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4),counts(5) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr5DI2 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr6DI2(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap6DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 6 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4),counts(5),counts(6) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr6DI2 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr6DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr7DI2(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap7DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 7 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4),counts(5),counts(6),counts(7) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr7DI2 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr7DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr1DI4(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I4), dimension(:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap1DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 1 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr1DI4 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr1DI8(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I8), dimension(:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap1DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 1 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr1DI8 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr1DR4(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R4), dimension(:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap1DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 1 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr1DR4 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr1DR8(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R8), dimension(:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap1DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 1 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr1DR8 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr2DI4(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I4), dimension(:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap2DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 2 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr2DI4 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr2DI8(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I8), dimension(:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap2DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 2 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr2DI8 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr2DR4(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R4), dimension(:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap2DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 2 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr2DR4 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr2DR8(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R8), dimension(:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap2DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 2 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr2DR8 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr3DI4(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap3DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 3 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr3DI4 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr3DI8(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap3DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 3 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr3DI8 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr3DR4(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap3DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 3 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr3DR4 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr3DR8(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap3DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 3 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr3DR8 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr4DI4(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap4DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 4 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr4DI4 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr4DI8(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap4DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 4 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr4DI8 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr4DR4(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap4DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 4 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr4DR4 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr4DR8(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap4DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 4 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr4DR8 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr5DI4(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap5DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 5 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4),counts(5) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr5DI4 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr5DI8(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap5DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 5 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4),counts(5) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr5DI8 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr5DR4(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap5DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 5 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4),counts(5) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr5DR4 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr5DR8(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap5DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 5 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4),counts(5) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr5DR8 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr6DI4(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap6DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 6 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4),counts(5),counts(6) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr6DI4 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr6DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr6DI8(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap6DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 6 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4),counts(5),counts(6) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr6DI8 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr6DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr6DR4(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap6DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 6 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4),counts(5),counts(6) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr6DR4 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr6DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr6DR8(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap6DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 6 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4),counts(5),counts(6) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr6DR8 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr6DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr7DI4(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap7DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 7 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4),counts(5),counts(6),counts(7) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr7DI4 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr7DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr7DI8(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap7DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 7 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4),counts(5),counts(6),counts(7) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr7DI8 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr7DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr7DR4(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap7DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 7 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4),counts(5),counts(6),counts(7) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr7DR4 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr7DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" 
 subroutine ESMF_ArrayConstructF90Ptr7DR8(array, counts, hwidth, fptr, & 
 docopy, lbounds, ubounds, rc) 
 
 type(ESMF_InternArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: status ! local error status 
 integer :: i ! loop counter 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 logical :: zerosize ! one or more counts = 0 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrap7DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 status = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 zerosize = .FALSE. 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (.not. associated(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => fptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(fptr) 
 ub(1:size(counts)) = ubound(fptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 ! more error checking; for allocation lb must be 
 ! less than or equal ub. if any count is 0, all counts 
 ! will be 0 for now. 
 zerosize = .FALSE. 
 do i=1, 7 
 if (counts(i) .le. 0) then 
 zerosize = .TRUE. 
 lb(i) = 0 
 ub(i) = 0 
 else if (lb(i) .gt. ub(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & 
 "Lower bounds must be .le. upper bounds", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 if (zerosize) then 
 allocate(newp ( counts(1),counts(2),counts(3),counts(4),counts(5),counts(6),counts(7) ), stat=status) 
 else 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) ), stat=status) 
 endif 
 if (ESMF_LogMsgFoundAllocError(status, "Array data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr7DR8 => newp 
 if (zerosize) then 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 else 
 call c_ESMC_IArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 endif 
 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr7DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!! < end of automatically generated functions >
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is Allocate/Deallocate for Arrays
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_InternArrayF90Allocate"
!BOPI
! !IROUTINE: ESMF_InternArrayF90Allocate - Allocate an F90 pointer and set Array info
!
! !INTERFACE:
     subroutine ESMF_InternArrayF90Allocate(array, rank, kind, &
                                      counts, lbounds, ubounds, hwidth, rc)
!
! !ARGUMENTS:
      type(ESMF_InternArray), intent(inout) :: array
      integer, intent(in) :: rank
      type(ESMF_TypeKind), intent(in) :: kind
      integer, dimension(:), intent(in) :: counts
      integer, dimension(:), intent(in) :: lbounds
      integer, dimension(:), intent(in) :: ubounds
      integer, intent(in) :: hwidth
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Allocate data contents for an {\tt ESMF\_Array} created
! from the C++ interface.
! The arguments are:
! \begin{description}
! \item[array]
! A partially created {\tt ESMF\_Array} object.
! \item[rank]
! The {\tt ESMF\_Array} rank.
! \item[kind]
! The {\tt ESMF\_Array} kind (short/2, long/8, etc).
! \item[counts]
! An integer array, size {\tt rank}, of each dimension length.
! \item[lbounds]
! An integer array, size {\tt rank}, of each dimension lower index.
! \item[ubounds]
! An integer array, size {\tt rank}, of each dimension upper index.
! \item[hwidth]
! An integer halo width, a single value, applied to each dimension.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
! !REQUIREMENTS:
    integer :: status ! local error status
    integer, dimension(ESMF_MAXDIM) :: lb, ub
    integer, dimension(ESMF_MAXDIM) :: offsets
    integer :: localkind
    !! local variables, expanded by macro

#ifndef ESMF_NO_INTEGER_1_BYTE 
 type(ESMF_ArrWrap1DI1) :: l1DI1 
 type(ESMF_ArrWrap2DI1) :: l2DI1 
 type(ESMF_ArrWrap3DI1) :: l3DI1 
 type(ESMF_ArrWrap4DI1) :: l4DI1 
#ifndef ESMF_NO_GREATER_THAN_4D 
 type(ESMF_ArrWrap5DI1) :: l5DI1 
 type(ESMF_ArrWrap6DI1) :: l6DI1 
 type(ESMF_ArrWrap7DI1) :: l7DI1 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
 type(ESMF_ArrWrap1DI2) :: l1DI2 
 type(ESMF_ArrWrap2DI2) :: l2DI2 
 type(ESMF_ArrWrap3DI2) :: l3DI2 
 type(ESMF_ArrWrap4DI2) :: l4DI2 
#ifndef ESMF_NO_GREATER_THAN_4D 
 type(ESMF_ArrWrap5DI2) :: l5DI2 
 type(ESMF_ArrWrap6DI2) :: l6DI2 
 type(ESMF_ArrWrap7DI2) :: l7DI2 
#endif 
#endif 
 type(ESMF_ArrWrap1DI4) :: l1DI4 
 type(ESMF_ArrWrap1DI8) :: l1DI8 
 type(ESMF_ArrWrap1DR4) :: l1DR4 
 type(ESMF_ArrWrap1DR8) :: l1DR8 
 
 type(ESMF_ArrWrap2DI4) :: l2DI4 
 type(ESMF_ArrWrap2DI8) :: l2DI8 
 type(ESMF_ArrWrap2DR4) :: l2DR4 
 type(ESMF_ArrWrap2DR8) :: l2DR8 
 
 type(ESMF_ArrWrap3DI4) :: l3DI4 
 type(ESMF_ArrWrap3DI8) :: l3DI8 
 type(ESMF_ArrWrap3DR4) :: l3DR4 
 type(ESMF_ArrWrap3DR8) :: l3DR8 
 
 type(ESMF_ArrWrap4DI4) :: l4DI4 
 type(ESMF_ArrWrap4DI8) :: l4DI8 
 type(ESMF_ArrWrap4DR4) :: l4DR4 
 type(ESMF_ArrWrap4DR8) :: l4DR8 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
 
 type(ESMF_ArrWrap5DI4) :: l5DI4 
 type(ESMF_ArrWrap5DI8) :: l5DI8 
 type(ESMF_ArrWrap5DR4) :: l5DR4 
 type(ESMF_ArrWrap5DR8) :: l5DR8 
 
 type(ESMF_ArrWrap6DI4) :: l6DI4 
 type(ESMF_ArrWrap6DI8) :: l6DI8 
 type(ESMF_ArrWrap6DR4) :: l6DR4 
 type(ESMF_ArrWrap6DR8) :: l6DR8 
 
 type(ESMF_ArrWrap7DI4) :: l7DI4 
 type(ESMF_ArrWrap7DI8) :: l7DI8 
 type(ESMF_ArrWrap7DR4) :: l7DR4 
 type(ESMF_ArrWrap7DR8) :: l7DR8 
 
#endif 
 
! < end macro - do not edit directly > 
 

    status = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    lb(1:size(lbounds)) = lbounds
    ub(1:size(ubounds)) = ubounds
    localkind = kind%dkind
    !! macros which are expanded by the preprocessor
        select case (localkind)
#ifndef ESMF_NO_INTEGER_1_BYTE
          case (ESMF_TYPEKIND_I1%dkind)
            select case (rank)
       case (1)
! <Created by macro - do not edit directly > 
 allocate(l1DI1 % ptr1DI1( lb(1):ub(1) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l1DI1, & 
 ESMF_DATA_ADDRESS(l1DI1 % ptr1DI1 (lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 allocate(l2DI1 % ptr2DI1( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l2DI1, & 
 ESMF_DATA_ADDRESS(l2DI1 % ptr2DI1 (lb(1),lb(2)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 allocate(l3DI1 % ptr3DI1( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l3DI1, & 
 ESMF_DATA_ADDRESS(l3DI1 % ptr3DI1 (lb(1),lb(2),lb(3)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 allocate(l4DI1 % ptr4DI1( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l4DI1, & 
 ESMF_DATA_ADDRESS(l4DI1 % ptr4DI1 (lb(1),lb(2),lb(3),lb(4)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 allocate(l5DI1 % ptr5DI1( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l5DI1, & 
 ESMF_DATA_ADDRESS(l5DI1 % ptr5DI1 (lb(1),lb(2),lb(3),lb(4),lb(5)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 allocate(l6DI1 % ptr6DI1( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l6DI1, & 
 ESMF_DATA_ADDRESS(l6DI1 % ptr6DI1 (lb(1),lb(2),lb(3),lb(4),lb(5),lb(6)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 allocate(l7DI1 % ptr7DI1( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l7DI1, & 
 ESMF_DATA_ADDRESS(l7DI1 % ptr7DI1 (lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
#endif
#ifndef ESMF_NO_INTEGER_2_BYTE
          case (ESMF_TYPEKIND_I2%dkind)
     select case(rank)
       case (1)
! <Created by macro - do not edit directly > 
 allocate(l1DI2 % ptr1DI2( lb(1):ub(1) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l1DI2, & 
 ESMF_DATA_ADDRESS(l1DI2 % ptr1DI2 (lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 allocate(l2DI2 % ptr2DI2( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l2DI2, & 
 ESMF_DATA_ADDRESS(l2DI2 % ptr2DI2 (lb(1),lb(2)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 allocate(l3DI2 % ptr3DI2( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l3DI2, & 
 ESMF_DATA_ADDRESS(l3DI2 % ptr3DI2 (lb(1),lb(2),lb(3)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 allocate(l4DI2 % ptr4DI2( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l4DI2, & 
 ESMF_DATA_ADDRESS(l4DI2 % ptr4DI2 (lb(1),lb(2),lb(3),lb(4)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 allocate(l5DI2 % ptr5DI2( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l5DI2, & 
 ESMF_DATA_ADDRESS(l5DI2 % ptr5DI2 (lb(1),lb(2),lb(3),lb(4),lb(5)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 allocate(l6DI2 % ptr6DI2( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l6DI2, & 
 ESMF_DATA_ADDRESS(l6DI2 % ptr6DI2 (lb(1),lb(2),lb(3),lb(4),lb(5),lb(6)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 allocate(l7DI2 % ptr7DI2( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l7DI2, & 
 ESMF_DATA_ADDRESS(l7DI2 % ptr7DI2 (lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
#endif
          case (ESMF_TYPEKIND_I4%dkind)
     select case(rank)
       case (1)
! <Created by macro - do not edit directly > 
 allocate(l1DI4 % ptr1DI4( lb(1):ub(1) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l1DI4, & 
 ESMF_DATA_ADDRESS(l1DI4 % ptr1DI4 (lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 allocate(l2DI4 % ptr2DI4( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l2DI4, & 
 ESMF_DATA_ADDRESS(l2DI4 % ptr2DI4 (lb(1),lb(2)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 allocate(l3DI4 % ptr3DI4( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l3DI4, & 
 ESMF_DATA_ADDRESS(l3DI4 % ptr3DI4 (lb(1),lb(2),lb(3)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 allocate(l4DI4 % ptr4DI4( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l4DI4, & 
 ESMF_DATA_ADDRESS(l4DI4 % ptr4DI4 (lb(1),lb(2),lb(3),lb(4)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 allocate(l5DI4 % ptr5DI4( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l5DI4, & 
 ESMF_DATA_ADDRESS(l5DI4 % ptr5DI4 (lb(1),lb(2),lb(3),lb(4),lb(5)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 allocate(l6DI4 % ptr6DI4( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l6DI4, & 
 ESMF_DATA_ADDRESS(l6DI4 % ptr6DI4 (lb(1),lb(2),lb(3),lb(4),lb(5),lb(6)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 allocate(l7DI4 % ptr7DI4( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l7DI4, & 
 ESMF_DATA_ADDRESS(l7DI4 % ptr7DI4 (lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
          case (ESMF_TYPEKIND_I8%dkind)
     select case(rank)
       case (1)
! <Created by macro - do not edit directly > 
 allocate(l1DI8 % ptr1DI8( lb(1):ub(1) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l1DI8, & 
 ESMF_DATA_ADDRESS(l1DI8 % ptr1DI8 (lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 allocate(l2DI8 % ptr2DI8( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l2DI8, & 
 ESMF_DATA_ADDRESS(l2DI8 % ptr2DI8 (lb(1),lb(2)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 allocate(l3DI8 % ptr3DI8( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l3DI8, & 
 ESMF_DATA_ADDRESS(l3DI8 % ptr3DI8 (lb(1),lb(2),lb(3)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 allocate(l4DI8 % ptr4DI8( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l4DI8, & 
 ESMF_DATA_ADDRESS(l4DI8 % ptr4DI8 (lb(1),lb(2),lb(3),lb(4)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 allocate(l5DI8 % ptr5DI8( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l5DI8, & 
 ESMF_DATA_ADDRESS(l5DI8 % ptr5DI8 (lb(1),lb(2),lb(3),lb(4),lb(5)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 allocate(l6DI8 % ptr6DI8( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l6DI8, & 
 ESMF_DATA_ADDRESS(l6DI8 % ptr6DI8 (lb(1),lb(2),lb(3),lb(4),lb(5),lb(6)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 allocate(l7DI8 % ptr7DI8( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l7DI8, & 
 ESMF_DATA_ADDRESS(l7DI8 % ptr7DI8 (lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
          case (ESMF_TYPEKIND_R4%dkind)
     select case(rank)
       case (1)
! <Created by macro - do not edit directly > 
 allocate(l1DR4 % ptr1DR4( lb(1):ub(1) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l1DR4, & 
 ESMF_DATA_ADDRESS(l1DR4 % ptr1DR4 (lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 allocate(l2DR4 % ptr2DR4( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l2DR4, & 
 ESMF_DATA_ADDRESS(l2DR4 % ptr2DR4 (lb(1),lb(2)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 allocate(l3DR4 % ptr3DR4( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l3DR4, & 
 ESMF_DATA_ADDRESS(l3DR4 % ptr3DR4 (lb(1),lb(2),lb(3)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 allocate(l4DR4 % ptr4DR4( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l4DR4, & 
 ESMF_DATA_ADDRESS(l4DR4 % ptr4DR4 (lb(1),lb(2),lb(3),lb(4)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 allocate(l5DR4 % ptr5DR4( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l5DR4, & 
 ESMF_DATA_ADDRESS(l5DR4 % ptr5DR4 (lb(1),lb(2),lb(3),lb(4),lb(5)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 allocate(l6DR4 % ptr6DR4( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l6DR4, & 
 ESMF_DATA_ADDRESS(l6DR4 % ptr6DR4 (lb(1),lb(2),lb(3),lb(4),lb(5),lb(6)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 allocate(l7DR4 % ptr7DR4( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l7DR4, & 
 ESMF_DATA_ADDRESS(l7DR4 % ptr7DR4 (lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
          case (ESMF_TYPEKIND_R8%dkind)
     select case(rank)
       case (1)
! <Created by macro - do not edit directly > 
 allocate(l1DR8 % ptr1DR8( lb(1):ub(1) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l1DR8, & 
 ESMF_DATA_ADDRESS(l1DR8 % ptr1DR8 (lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 allocate(l2DR8 % ptr2DR8( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l2DR8, & 
 ESMF_DATA_ADDRESS(l2DR8 % ptr2DR8 (lb(1),lb(2)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 allocate(l3DR8 % ptr3DR8( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l3DR8, & 
 ESMF_DATA_ADDRESS(l3DR8 % ptr3DR8 (lb(1),lb(2),lb(3)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 allocate(l4DR8 % ptr4DR8( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l4DR8, & 
 ESMF_DATA_ADDRESS(l4DR8 % ptr4DR8 (lb(1),lb(2),lb(3),lb(4)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 allocate(l5DR8 % ptr5DR8( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l5DR8, & 
 ESMF_DATA_ADDRESS(l5DR8 % ptr5DR8 (lb(1),lb(2),lb(3),lb(4),lb(5)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 allocate(l6DR8 % ptr6DR8( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l6DR8, & 
 ESMF_DATA_ADDRESS(l6DR8 % ptr6DR8 (lb(1),lb(2),lb(3),lb(4),lb(5),lb(6)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 allocate(l7DR8 % ptr7DR8( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) ), stat=status) 
 if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 ! (Since I am not sure if these are used, set offsets to 0 for now.) 
 offsets = 0 
 
 call c_ESMC_IArraySetInfo(array, l7DR8, & 
 ESMF_DATA_ADDRESS(l7DR8 % ptr7DR8 (lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 if (ESMF_LogMsgFoundError(status, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
          case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", &
                                 ESMF_CONTEXT, rc)) return
        end select
     if (present(rc)) rc = status
     end subroutine ESMF_InternArrayF90Allocate
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_InternArrayF90Deallocate"
!BOPI
! !IROUTINE: ESMF_InternArrayF90Deallocate - Deallocate an F90 pointer
!
! !INTERFACE:
     subroutine ESMF_InternArrayF90Deallocate(array, rank, kind, rc)
!
! !ARGUMENTS:
      type(ESMF_InternArray) :: array
      integer :: rank
      type(ESMF_TypeKind) :: kind
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Deallocate data contents for an {\tt ESMF\_Array} created
! from the C++ interface.
! The arguments are:
! \begin{description}
! \item[array]
! A partially created {\tt ESMF\_Array} object.
! \item[rank]
! The {\tt ESMF\_Array} rank.
! \item[kind]
! The {\tt ESMF\_Array} kind (short/2, long/8, etc).
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
    integer :: status ! local error status
    integer :: localkind
    !! local variables, expanded by macro

#ifndef ESMF_NO_INTEGER_1_BYTE 
 type(ESMF_ArrWrap1DI1) :: l1DI1 
 type(ESMF_ArrWrap2DI1) :: l2DI1 
 type(ESMF_ArrWrap3DI1) :: l3DI1 
 type(ESMF_ArrWrap4DI1) :: l4DI1 
#ifndef ESMF_NO_GREATER_THAN_4D 
 type(ESMF_ArrWrap5DI1) :: l5DI1 
 type(ESMF_ArrWrap6DI1) :: l6DI1 
 type(ESMF_ArrWrap7DI1) :: l7DI1 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
 type(ESMF_ArrWrap1DI2) :: l1DI2 
 type(ESMF_ArrWrap2DI2) :: l2DI2 
 type(ESMF_ArrWrap3DI2) :: l3DI2 
 type(ESMF_ArrWrap4DI2) :: l4DI2 
#ifndef ESMF_NO_GREATER_THAN_4D 
 type(ESMF_ArrWrap5DI2) :: l5DI2 
 type(ESMF_ArrWrap6DI2) :: l6DI2 
 type(ESMF_ArrWrap7DI2) :: l7DI2 
#endif 
#endif 
 type(ESMF_ArrWrap1DI4) :: l1DI4 
 type(ESMF_ArrWrap1DI8) :: l1DI8 
 type(ESMF_ArrWrap1DR4) :: l1DR4 
 type(ESMF_ArrWrap1DR8) :: l1DR8 
 
 type(ESMF_ArrWrap2DI4) :: l2DI4 
 type(ESMF_ArrWrap2DI8) :: l2DI8 
 type(ESMF_ArrWrap2DR4) :: l2DR4 
 type(ESMF_ArrWrap2DR8) :: l2DR8 
 
 type(ESMF_ArrWrap3DI4) :: l3DI4 
 type(ESMF_ArrWrap3DI8) :: l3DI8 
 type(ESMF_ArrWrap3DR4) :: l3DR4 
 type(ESMF_ArrWrap3DR8) :: l3DR8 
 
 type(ESMF_ArrWrap4DI4) :: l4DI4 
 type(ESMF_ArrWrap4DI8) :: l4DI8 
 type(ESMF_ArrWrap4DR4) :: l4DR4 
 type(ESMF_ArrWrap4DR8) :: l4DR8 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
 
 type(ESMF_ArrWrap5DI4) :: l5DI4 
 type(ESMF_ArrWrap5DI8) :: l5DI8 
 type(ESMF_ArrWrap5DR4) :: l5DR4 
 type(ESMF_ArrWrap5DR8) :: l5DR8 
 
 type(ESMF_ArrWrap6DI4) :: l6DI4 
 type(ESMF_ArrWrap6DI8) :: l6DI8 
 type(ESMF_ArrWrap6DR4) :: l6DR4 
 type(ESMF_ArrWrap6DR8) :: l6DR8 
 
 type(ESMF_ArrWrap7DI4) :: l7DI4 
 type(ESMF_ArrWrap7DI8) :: l7DI8 
 type(ESMF_ArrWrap7DR4) :: l7DR4 
 type(ESMF_ArrWrap7DR8) :: l7DR8 
 
#endif 
 
! < end macro - do not edit directly > 
 

    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localkind = kind
    !! macros which are expanded by the preprocessor
        select case (localkind)
#ifndef ESMF_NO_INTEGER_1_BYTE
          case (ESMF_TYPEKIND_I1%dkind)
            select case (rank)
       case (1)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l1DI1, status) 
 deallocate(l1DI1 % ptr1DI1, stat=status) 
 nullify(l1DI1 % ptr1DI1) 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l2DI1, status) 
 deallocate(l2DI1 % ptr2DI1, stat=status) 
 nullify(l2DI1 % ptr2DI1) 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l3DI1, status) 
 deallocate(l3DI1 % ptr3DI1, stat=status) 
 nullify(l3DI1 % ptr3DI1) 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l4DI1, status) 
 deallocate(l4DI1 % ptr4DI1, stat=status) 
 nullify(l4DI1 % ptr4DI1) 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l5DI1, status) 
 deallocate(l5DI1 % ptr5DI1, stat=status) 
 nullify(l5DI1 % ptr5DI1) 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l6DI1, status) 
 deallocate(l6DI1 % ptr6DI1, stat=status) 
 nullify(l6DI1 % ptr6DI1) 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l7DI1, status) 
 deallocate(l7DI1 % ptr7DI1, stat=status) 
 nullify(l7DI1 % ptr7DI1) 
! < End macro - do not edit directly > 

#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
#endif
#ifndef ESMF_NO_INTEGER_2_BYTE
          case (ESMF_TYPEKIND_I2%dkind)
     select case(rank)
       case (1)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l1DI2, status) 
 deallocate(l1DI2 % ptr1DI2, stat=status) 
 nullify(l1DI2 % ptr1DI2) 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l2DI2, status) 
 deallocate(l2DI2 % ptr2DI2, stat=status) 
 nullify(l2DI2 % ptr2DI2) 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l3DI2, status) 
 deallocate(l3DI2 % ptr3DI2, stat=status) 
 nullify(l3DI2 % ptr3DI2) 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l4DI2, status) 
 deallocate(l4DI2 % ptr4DI2, stat=status) 
 nullify(l4DI2 % ptr4DI2) 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l5DI2, status) 
 deallocate(l5DI2 % ptr5DI2, stat=status) 
 nullify(l5DI2 % ptr5DI2) 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l6DI2, status) 
 deallocate(l6DI2 % ptr6DI2, stat=status) 
 nullify(l6DI2 % ptr6DI2) 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l7DI2, status) 
 deallocate(l7DI2 % ptr7DI2, stat=status) 
 nullify(l7DI2 % ptr7DI2) 
! < End macro - do not edit directly > 

#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
#endif
          case (ESMF_TYPEKIND_I4%dkind)
     select case(rank)
       case (1)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l1DI4, status) 
 deallocate(l1DI4 % ptr1DI4, stat=status) 
 nullify(l1DI4 % ptr1DI4) 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l2DI4, status) 
 deallocate(l2DI4 % ptr2DI4, stat=status) 
 nullify(l2DI4 % ptr2DI4) 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l3DI4, status) 
 deallocate(l3DI4 % ptr3DI4, stat=status) 
 nullify(l3DI4 % ptr3DI4) 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l4DI4, status) 
 deallocate(l4DI4 % ptr4DI4, stat=status) 
 nullify(l4DI4 % ptr4DI4) 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l5DI4, status) 
 deallocate(l5DI4 % ptr5DI4, stat=status) 
 nullify(l5DI4 % ptr5DI4) 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l6DI4, status) 
 deallocate(l6DI4 % ptr6DI4, stat=status) 
 nullify(l6DI4 % ptr6DI4) 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l7DI4, status) 
 deallocate(l7DI4 % ptr7DI4, stat=status) 
 nullify(l7DI4 % ptr7DI4) 
! < End macro - do not edit directly > 

#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
          case (ESMF_TYPEKIND_I8%dkind)
     select case(rank)
       case (1)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l1DI8, status) 
 deallocate(l1DI8 % ptr1DI8, stat=status) 
 nullify(l1DI8 % ptr1DI8) 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l2DI8, status) 
 deallocate(l2DI8 % ptr2DI8, stat=status) 
 nullify(l2DI8 % ptr2DI8) 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l3DI8, status) 
 deallocate(l3DI8 % ptr3DI8, stat=status) 
 nullify(l3DI8 % ptr3DI8) 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l4DI8, status) 
 deallocate(l4DI8 % ptr4DI8, stat=status) 
 nullify(l4DI8 % ptr4DI8) 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l5DI8, status) 
 deallocate(l5DI8 % ptr5DI8, stat=status) 
 nullify(l5DI8 % ptr5DI8) 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l6DI8, status) 
 deallocate(l6DI8 % ptr6DI8, stat=status) 
 nullify(l6DI8 % ptr6DI8) 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l7DI8, status) 
 deallocate(l7DI8 % ptr7DI8, stat=status) 
 nullify(l7DI8 % ptr7DI8) 
! < End macro - do not edit directly > 

#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
          case (ESMF_TYPEKIND_R4%dkind)
     select case(rank)
       case (1)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l1DR4, status) 
 deallocate(l1DR4 % ptr1DR4, stat=status) 
 nullify(l1DR4 % ptr1DR4) 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l2DR4, status) 
 deallocate(l2DR4 % ptr2DR4, stat=status) 
 nullify(l2DR4 % ptr2DR4) 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l3DR4, status) 
 deallocate(l3DR4 % ptr3DR4, stat=status) 
 nullify(l3DR4 % ptr3DR4) 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l4DR4, status) 
 deallocate(l4DR4 % ptr4DR4, stat=status) 
 nullify(l4DR4 % ptr4DR4) 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l5DR4, status) 
 deallocate(l5DR4 % ptr5DR4, stat=status) 
 nullify(l5DR4 % ptr5DR4) 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l6DR4, status) 
 deallocate(l6DR4 % ptr6DR4, stat=status) 
 nullify(l6DR4 % ptr6DR4) 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l7DR4, status) 
 deallocate(l7DR4 % ptr7DR4, stat=status) 
 nullify(l7DR4 % ptr7DR4) 
! < End macro - do not edit directly > 

#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
          case (ESMF_TYPEKIND_R8%dkind)
     select case(rank)
       case (1)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l1DR8, status) 
 deallocate(l1DR8 % ptr1DR8, stat=status) 
 nullify(l1DR8 % ptr1DR8) 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l2DR8, status) 
 deallocate(l2DR8 % ptr2DR8, stat=status) 
 nullify(l2DR8 % ptr2DR8) 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l3DR8, status) 
 deallocate(l3DR8 % ptr3DR8, stat=status) 
 nullify(l3DR8 % ptr3DR8) 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l4DR8, status) 
 deallocate(l4DR8 % ptr4DR8, stat=status) 
 nullify(l4DR8 % ptr4DR8) 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l5DR8, status) 
 deallocate(l5DR8 % ptr5DR8, stat=status) 
 nullify(l5DR8 % ptr5DR8) 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l6DR8, status) 
 deallocate(l6DR8 % ptr6DR8, stat=status) 
 nullify(l6DR8 % ptr6DR8) 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 call c_ESMC_IArrayGetF90Ptr(array, l7DR8, status) 
 deallocate(l7DR8 % ptr7DR8, stat=status) 
 nullify(l7DR8 % ptr7DR8) 
! < End macro - do not edit directly > 

#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
          case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", &
                                 ESMF_CONTEXT, rc)) return
        end select
        if (ESMF_LogMsgFoundAllocError(status, "Array Deallocation", &
                                       ESMF_CONTEXT, rc)) return
        if (present(rc)) rc = ESMF_SUCCESS
     end subroutine ESMF_InternArrayF90Deallocate
!------------------------------------------------------------------------------
        end module ESMF_InternArrayCreateMod
