! $Id: ESMF_LocalArrayCreate.cppF90,v 1.5.2.7 2010/02/01 20:51:27 svasquez Exp $
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
#define ESMF_FILENAME "ESMF_LocalArrayCreate.F90"
!==============================================================================
!
! ESMF LocalArrayCreate module
module ESMF_LocalArrayCreateMod
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
! !MODULE: ESMF_LocalArrayCreateMod - Manage data uniformly between F90 and C++
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
  implicit none
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
!------------------------------------------------------------------------------
  ! ESMF_CopyFlag
  ! Indicates whether a data array should be copied or referenced.
  ! This matches an enum on the C++ side and the values must match.
  ! Update ../include/ESMC_LocalArray.h if you change these values.
  type ESMF_CopyFlag
  sequence
  private
    integer :: docopy
  end type
  type(ESMF_CopyFlag), parameter :: &
    ESMF_DATA_COPY = ESMF_CopyFlag(1), &
    ESMF_DATA_REF = ESMF_CopyFlag(2), &
    ESMF_DATA_DEFER = ESMF_CopyFlag(3), &
    ESMF_DATA_SPACE = ESMF_CopyFlag(4), &
    ESMF_DATA_NONE = ESMF_CopyFlag(5) ! this value is _not_ public
!------------------------------------------------------------------------------
  ! ESMF_LocalArrayOrigin
  ! Private flag which indicates the create was initiated on the F90 side.
  ! This matches an enum on the C++ side and the values must match.
  ! Update ../include/ESMC_LocalArray.h if you change these values.
  type ESMF_LocalArrayOrigin
  sequence
  private
    integer :: origin
  end type
  type(ESMF_LocalArrayOrigin), parameter :: &
    ESMF_FROM_FORTRAN = ESMF_LocalArrayOrigin(1), &
    ESMF_FROM_CPLUSPLUS = ESMF_LocalArrayOrigin(2)
!------------------------------------------------------------------------------
  ! ESMF_LocalArray
  ! LocalArray data type. All information is kept on the C++ side inside
  ! the class structure.
  type ESMF_LocalArray
  sequence
  !private
    type(ESMF_Pointer) :: this
    ESMF_INIT_DECLARE
  end type
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
! !PUBLIC TYPES:
  public ESMF_CopyFlag, ESMF_DATA_COPY, ESMF_DATA_REF, ESMF_DATA_SPACE
  public ESMF_LocalArray
!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:
  public ESMF_LocalArrayCreate
  public ESMF_LocalArrayDestroy
  public ESMF_LocalArraySetData
  public ESMF_LocalArraySetInfo
  public ESMF_LocalArrayF90Allocate
  public ESMF_LocalArrayF90Deallocate
  public ESMF_LocalArrConstrF90Ptr ! needed for C++ callback only
  public ESMF_LocalArraySlice
  !public ESMF_LocalArrayReshape
  public ESMF_LocalArrayWriteRestart
  public ESMF_LocalArrayReadRestart
  public ESMF_LocalArrayWrite
  public ESMF_LocalArrayRead
  !public ESMF_LocalArraySerialize, ESMF_LocalArraySerializeNoData
  !public ESMF_LocalArrayDeserialize, ESMF_LocalArrayDeserializeNoData
  public ESMF_LocalArrayValidate
  public ESMF_LocalArrayPrint
  public ESMF_LocalArrayAdjust
  public ESMF_LocalArrayCopyF90Ptr
  public ESMF_LocalArrayGetInit
  public ESMF_LocalArraySetInitCreated
  public ESMF_LocalArrayGetThis
  public ESMF_LocalArraySetThis
  public operator(.eq.), operator(.ne.)
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_LocalArrayCreate.cppF90,v 1.5.2.7 2010/02/01 20:51:27 svasquez Exp $'
!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !IROUTINE: ESMF_LocalArrayCreate -- Generic interface to create an LocalArray
! !INTERFACE:
  interface ESMF_LocalArrayCreate
! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_LocalArrayCreateByTKR ! specify explicit TKR
    module procedure ESMF_LocalArrayCreateBySpec ! specify ArraySpec
    module procedure ESMF_LocalArrayCreateCopy ! create a copy
    ! Plus interfaces for each T/K/R expanded by macro.
!EOPI
    ! < interfaces for each T/K/R >
    !------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
#ifndef ESMF_NO_INTEGER_1_BYTE 
 module procedure ESMF_LocalArrCreateByPtr1DI1 
 module procedure ESMF_LocalArrCreateByPtr2DI1 
 module procedure ESMF_LocalArrCreateByPtr3DI1 
 module procedure ESMF_LocalArrCreateByPtr4DI1 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_LocalArrCreateByPtr5DI1 
 module procedure ESMF_LocalArrCreateByPtr6DI1 
 module procedure ESMF_LocalArrCreateByPtr7DI1 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
 module procedure ESMF_LocalArrCreateByPtr1DI2 
 module procedure ESMF_LocalArrCreateByPtr2DI2 
 module procedure ESMF_LocalArrCreateByPtr3DI2 
 module procedure ESMF_LocalArrCreateByPtr4DI2 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_LocalArrCreateByPtr5DI2 
 module procedure ESMF_LocalArrCreateByPtr6DI2 
 module procedure ESMF_LocalArrCreateByPtr7DI2 
#endif 
#endif 
 module procedure ESMF_LocalArrCreateByPtr1DI4 
 module procedure ESMF_LocalArrCreateByPtr1DI8 
 module procedure ESMF_LocalArrCreateByPtr1DR4 
 module procedure ESMF_LocalArrCreateByPtr1DR8 
 module procedure ESMF_LocalArrCreateByPtr2DI4 
 module procedure ESMF_LocalArrCreateByPtr2DI8 
 module procedure ESMF_LocalArrCreateByPtr2DR4 
 module procedure ESMF_LocalArrCreateByPtr2DR8 
 module procedure ESMF_LocalArrCreateByPtr3DI4 
 module procedure ESMF_LocalArrCreateByPtr3DI8 
 module procedure ESMF_LocalArrCreateByPtr3DR4 
 module procedure ESMF_LocalArrCreateByPtr3DR8 
 module procedure ESMF_LocalArrCreateByPtr4DI4 
 module procedure ESMF_LocalArrCreateByPtr4DI8 
 module procedure ESMF_LocalArrCreateByPtr4DR4 
 module procedure ESMF_LocalArrCreateByPtr4DR8 
#ifndef ESMF_NO_GREATER_THAN_4D 
 module procedure ESMF_LocalArrCreateByPtr5DI4 
 module procedure ESMF_LocalArrCreateByPtr5DI8 
 module procedure ESMF_LocalArrCreateByPtr5DR4 
 module procedure ESMF_LocalArrCreateByPtr5DR8 
 module procedure ESMF_LocalArrCreateByPtr6DI4 
 module procedure ESMF_LocalArrCreateByPtr6DI8 
 module procedure ESMF_LocalArrCreateByPtr6DR4 
 module procedure ESMF_LocalArrCreateByPtr6DR8 
 module procedure ESMF_LocalArrCreateByPtr7DI4 
 module procedure ESMF_LocalArrCreateByPtr7DI8 
 module procedure ESMF_LocalArrCreateByPtr7DR4 
 module procedure ESMF_LocalArrCreateByPtr7DR8 
#endif 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!BOPI
! !DESCRIPTION:
! This interface provides a single (heavily overloaded) entry point for
! the various types of {\tt ESMF\_LocalArrayCreate} functions.
!
! There are 3 options for setting the contents of the {\tt ESMF\_LocalArray}
! at creation time:
! \begin{description}
! \item[Allocate Space Only]
! Data space is allocated but not initialized. The caller can query
! for a pointer to the start of the space to address it directly.
! The caller must not deallocate the space; the
! {\tt ESMF\_LocalArray} will release the space when it is destroyed.
! \item[Data Copy]
! An existing Fortran array is specified and the data contents are copied
! into new space allocated by the {\tt ESMF\_LocalArray}.
! The caller must not deallocate the space; the
! {\tt ESMF\_LocalArray} will release the space when it is destroyed.
! \item[Data Reference]
! An existing Fortran array is specified and the data contents reference
! it directly. The caller is responsible for deallocating the space;
! when the {\tt ESMF\_LocalArray} is destroyed it will not release the space.
! \end{description}
!
! There are 3 options for
! specifying the type/kind/rank of the {\tt ESMF\_LocalArray} data:
! \begin{description}
! \item[List]
! The characteristics of the {\tt ESMF\_LocalArray} are given explicitly
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
! The concept of an ``empty'' {\tt ESMF\_LocalArray} does not exist. To make an
! ESMF object which stores the Type/Kind/Rank information create an
! {\tt ESMF\_ArraySpec} object which can then be used repeatedly in
! subsequent {\tt ESMF\_LocalArray} Create calls.
!
!EOPI
end interface
!------------------------------------------------------------------------------
interface operator (.eq.)
 module procedure ESMF_cfeq
end interface
interface operator (.ne.)
 module procedure ESMF_cfne
end interface
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! functions to compare two ESMF_CopyFlags to see if they are the same or not
function ESMF_cfeq(cf1, cf2)
 logical ESMF_cfeq
 type(ESMF_CopyFlag), intent(in) :: cf1, cf2
 ESMF_cfeq = (cf1%docopy .eq. cf2%docopy)
end function
function ESMF_cfne(cf1, cf2)
 logical ESMF_cfne
 type(ESMF_CopyFlag), intent(in) :: cf1, cf2
 ESMF_cfne = (cf1%docopy .ne. cf2%docopy)
end function
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the LocalArray Create and Destroy methods.
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayCreateByTKR"
!BOP
! !IROUTINE: ESMF_LocalArrayCreate -- Create a LocalArray explicitly specifying TKR arguments.
! !INTERFACE:
  ! Private name; call using ESMF_LocalArrayCreate()
  function ESMF_LocalArrayCreateByTKR(rank, typekind, counts, lbounds, &
    ubounds, rc)
!
! !RETURN VALUE:
    type(ESMF_LocalArray) :: ESMF_LocalArrayCreateByTKR
!
! !ARGUMENTS:
    integer, intent(in) :: rank
    type(ESMF_TypeKind), intent(in) :: typekind
    integer, intent(in), optional :: counts(:)
    integer, intent(in), optional :: lbounds(:)
    integer, intent(in), optional :: ubounds(:)
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Create a new {\tt ESMF\_LocalArray} and allocate data space, which remains
! uninitialized. The return value is a new LocalArray.
!
! The arguments are:
! \begin{description}
! \item[rank]
! Array rank (dimensionality, 1D, 2D, etc). Maximum allowed is 7D.
! \item[typekind]
! Array typekind. See section \ref{opt:typekind} for valid values.
! \item[{[counts]}]
! The number of items in each dimension of the array. This is a 1D
! integer array the same length as the rank. The {\tt count} argument may
! be omitted if both {\tt lbounds} and {\tt ubounds} arguments are present.
! \item[{[lbounds]}]
! An integer array of length rank, with the lower index for each dimension.
! \item[{[ubounds]}]
! An integer array of length rank, with the upper index for each dimension.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    ! Local vars
    integer :: localrc ! local return code
    type (ESMF_LocalArray) :: array ! new C++ LocalArray
    integer, dimension(ESMF_MAXDIM) :: cnts ! local counts
    integer, dimension(ESMF_MAXDIM) :: lb, ub ! local bounds
    integer:: i
    array%this = ESMF_NULL_POINTER
    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    ! Check that enough info from counts, lbounds and ubounds is present
    if (.not.present(counts)) then
      if (.not.present(lbounds).or..not.present(ubounds)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, &
          "- lbounds and ubounds must be present when counts argument is not present", &
          ESMF_CONTEXT, rc)
        return
      endif
    endif
    ! Check size of optional counts and bounds and fill the local variables
    if (present(lbounds)) then
      if (size(lbounds)<rank) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &
          "- lbounds argument must be of size rank", &
          ESMF_CONTEXT, rc)
        return
      endif
      lb(1:rank) = lbounds(1:rank)
    endif
    if (present(ubounds)) then
      if (size(ubounds)<rank) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &
          "- ubounds argument must be of size rank", &
          ESMF_CONTEXT, rc)
        return
      endif
      ub(1:rank) = ubounds(1:rank)
    endif
    if (present(counts)) then
      if (size(counts)<rank) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &
          "- counts argument must be of size rank", &
          ESMF_CONTEXT, rc)
        return
      endif
      cnts(1:rank) = counts(1:rank)
    else
      cnts(1:rank) = ub(1:rank) - lb(1:rank) + 1
    endif
    if (.not.present(lbounds).and..not.present(ubounds)) then
      lb(1:rank) = 1
      ub(1:rank) = cnts(1:rank)
    else if (.not.present(lbounds)) then
      lb(1:rank) = ub(1:rank) - cnts(1:rank) + 1
    else if (.not.present(ubounds)) then
      ub(1:rank) = lb(1:rank) + cnts(1:rank) - 1
    endif
    ! Check that the local bounds and counts variables match
    do i=1, rank
      if (cnts(i).ne.(ub(i)-lb(i)+1)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
          "- counts and bounds mismatch detected", &
          ESMF_CONTEXT, rc)
        return
      endif
    enddo
    ! Create an initial LocalArray object that must be completed below
    call c_ESMC_LocalArrayCreateNoData(array, rank, typekind, &
      ESMF_FROM_FORTRAN, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return
    ! Complete the initial LocalArray object
    call ESMF_LocalArrConstrF90Ptr(array, cnts, rank, typekind, lb, ub, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return
    ! Set return value
    ESMF_LocalArrayCreateByTKR = array
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_LocalArrayCreateByTKR)
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end function ESMF_LocalArrayCreateByTKR
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayCreateBySpec"
!BOP
! !IROUTINE: ESMF_LocalArrayCreate -- Create a LocalArray specifying an ArraySpec
! !INTERFACE:
  ! Private name; call using ESMF_LocalArrayCreate()
  function ESMF_LocalArrayCreateBySpec(arrayspec, counts, lbounds, ubounds, rc)
!
! !RETURN VALUE:
    type(ESMF_LocalArray) :: ESMF_LocalArrayCreateBySpec
!
! !ARGUMENTS:
    type(ESMF_ArraySpec), intent(inout) :: arrayspec
    integer, intent(in), optional :: counts(:)
    integer, intent(in), optional :: lbounds(:)
    integer, intent(in), optional :: ubounds(:)
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Create a new {\tt ESMF\_LocalArray} and allocate data space, which remains
! uninitialized. The return value is a new LocalArray.
!
! The arguments are:
! \begin{description}
! \item[arrayspec]
! ArraySpec object specifying typekind and rank.
! \item[{[counts]}]
! The number of items in each dimension of the array. This is a 1D
! integer array the same length as the rank. The {\tt count} argument may
! be omitted if both {\tt lbounds} and {\tt ubounds} arguments are present.
! \item[{[lbounds]}]
! An integer array of length rank, with the lower index for each dimension.
! \item[{[ubounds]}]
! An integer array of length rank, with the upper index for each dimension.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    ! Local vars
    type (ESMF_LocalArray) :: array ! new C++ LocalArray
    integer :: localrc ! local return code
    integer :: rank
    type(ESMF_TypeKind) :: typekind
    array%this = ESMF_NULL_POINTER
    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArraySpecGetInit, ESMF_ArraySpecInit,arrayspec)
    call ESMF_ArraySpecGet(arrayspec, rank, typekind, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return
    ! Call the CreateByTKR function to make the array
    ESMF_LocalArrayCreateBySpec = ESMF_LocalArrayCreateByTKR(rank, &
      typekind, counts, lbounds, ubounds, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_LocalArrayCreateBySpec)
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end function ESMF_LocalArrayCreateBySpec
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayCreateCopy"
!BOP
! !IROUTINE: ESMF_LocalArrayCreate -- Create a LocalArray from existing one
! !INTERFACE:
  ! Private name; call using ESMF_LocalArrayCreate()
  function ESMF_LocalArrayCreateCopy(larray, rc)
!
! !RETURN VALUE:
    type(ESMF_LocalArray) :: ESMF_LocalArrayCreateCopy
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(in) :: larray
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Perform a deep copy of an existing {\tt ESMF\_LocalArray} object. The return
! value is a new LocalArray.
!
! The arguments are:
! \begin{description}
! \item[larray]
! Existing LocalArray to be copied.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc ! local return code
    type(ESMF_LocalArray) :: larrayOut ! opaque pointer to new C++ LocalArray
    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    ! Mark this LocalArray object as invalid
    larrayOut%this = ESMF_NULL_POINTER
    ! Call into the C++ interface
    call c_ESMC_LocalArrayCreateCopy(larray, larrayOut, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! Set return value
    ESMF_LocalArrayCreateCopy = larrayOut
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_LocalArrayCreateCopy)
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end function ESMF_LocalArrayCreateCopy
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayCopyF90Ptr"
!BOPI
! !IROUTINE: ESMF_LocalArrayCopyF90Ptr - Copy F90 pointer
! !INTERFACE:
  subroutine ESMF_LocalArrayCopyF90Ptr(larrayIn, larrayOut, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(inout) :: larrayIn
    type(ESMF_LocalArray), intent(inout) :: larrayOut
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Copy F90 pointer contents from {\tt arrayIn} to (\tt arrayOut}.
!
! The arguments are:
! \begin{description}
! \item[arrayIn]
! Existing {\tt ESMF\_LocalArray} object.
! \item[arrayOut]
! Existing {\tt ESMF\_LocalArray} object without alloc for data
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    ! Local vars
    integer :: localrc ! local return code
    integer :: localkind
    integer :: rank
    type(ESMF_TypeKind) :: typekind
    ! Initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, larrayIn, rc)
    ! Identify larrayIn TKR
    call c_ESMC_LocalArrayGetRank(larrayIn, rank, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call c_ESMC_LocalArrayGetTypeKind(larrayIn, typekind, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    !TODO: check TKR consistency against larrayOut
    ! Call a T/K/R specific interface in order to create the proper
    ! type of F90 pointer, allocate the space, set the values in the
    ! LocalArray object, and return. (The routine this code is calling is
    ! generated by macro.)
    localkind = typekind%dkind
    !! calling routines generated from macros by the preprocessor
    select case (localkind)
#ifndef ESMF_NO_INTEGER_1_BYTE
          case (ESMF_TYPEKIND_I1%dkind)
            select case (rank)
       case (1)
                call ESMF_LocalArrayCopy1DI1(larrayIn, larrayOut, rc=localrc)
       case (2)
                call ESMF_LocalArrayCopy2DI1(larrayIn, larrayOut, rc=localrc)
       case (3)
                call ESMF_LocalArrayCopy3DI1(larrayIn, larrayOut, rc=localrc)
       case (4)
                call ESMF_LocalArrayCopy4DI1(larrayIn, larrayOut, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrayCopy5DI1(larrayIn, larrayOut, rc=localrc)
       case (6)
                call ESMF_LocalArrayCopy6DI1(larrayIn, larrayOut, rc=localrc)
              case (7)
                call ESMF_LocalArrayCopy7DI1(larrayIn, larrayOut, rc=localrc)
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
                call ESMF_LocalArrayCopy1DI2(larrayIn, larrayOut, rc=localrc)
       case (2)
                call ESMF_LocalArrayCopy2DI2(larrayIn, larrayOut, rc=localrc)
       case (3)
                call ESMF_LocalArrayCopy3DI2(larrayIn, larrayOut, rc=localrc)
       case (4)
                call ESMF_LocalArrayCopy4DI2(larrayIn, larrayOut, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrayCopy5DI2(larrayIn, larrayOut, rc=localrc)
       case (6)
                call ESMF_LocalArrayCopy6DI2(larrayIn, larrayOut, rc=localrc)
              case (7)
                call ESMF_LocalArrayCopy7DI2(larrayIn, larrayOut, rc=localrc)
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
                call ESMF_LocalArrayCopy1DI4(larrayIn, larrayOut, rc=localrc)
       case (2)
                call ESMF_LocalArrayCopy2DI4(larrayIn, larrayOut, rc=localrc)
       case (3)
                call ESMF_LocalArrayCopy3DI4(larrayIn, larrayOut, rc=localrc)
       case (4)
                call ESMF_LocalArrayCopy4DI4(larrayIn, larrayOut, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrayCopy5DI4(larrayIn, larrayOut, rc=localrc)
       case (6)
                call ESMF_LocalArrayCopy6DI4(larrayIn, larrayOut, rc=localrc)
              case (7)
                call ESMF_LocalArrayCopy7DI4(larrayIn, larrayOut, rc=localrc)
#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
          case (ESMF_TYPEKIND_I8%dkind)
     select case(rank)
       case (1)
                call ESMF_LocalArrayCopy1DI8(larrayIn, larrayOut, rc=localrc)
       case (2)
                call ESMF_LocalArrayCopy2DI8(larrayIn, larrayOut, rc=localrc)
       case (3)
                call ESMF_LocalArrayCopy3DI8(larrayIn, larrayOut, rc=localrc)
       case (4)
                call ESMF_LocalArrayCopy4DI8(larrayIn, larrayOut, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrayCopy5DI8(larrayIn, larrayOut, rc=localrc)
       case (6)
                call ESMF_LocalArrayCopy6DI8(larrayIn, larrayOut, rc=localrc)
              case (7)
                call ESMF_LocalArrayCopy7DI8(larrayIn, larrayOut, rc=localrc)
#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
          case (ESMF_TYPEKIND_R4%dkind)
     select case(rank)
       case (1)
                call ESMF_LocalArrayCopy1DR4(larrayIn, larrayOut, rc=localrc)
       case (2)
                call ESMF_LocalArrayCopy2DR4(larrayIn, larrayOut, rc=localrc)
       case (3)
                call ESMF_LocalArrayCopy3DR4(larrayIn, larrayOut, rc=localrc)
       case (4)
                call ESMF_LocalArrayCopy4DR4(larrayIn, larrayOut, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrayCopy5DR4(larrayIn, larrayOut, rc=localrc)
       case (6)
                call ESMF_LocalArrayCopy6DR4(larrayIn, larrayOut, rc=localrc)
              case (7)
                call ESMF_LocalArrayCopy7DR4(larrayIn, larrayOut, rc=localrc)
#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
          case (ESMF_TYPEKIND_R8%dkind)
     select case(rank)
       case (1)
                call ESMF_LocalArrayCopy1DR8(larrayIn, larrayOut, rc=localrc)
       case (2)
                call ESMF_LocalArrayCopy2DR8(larrayIn, larrayOut, rc=localrc)
       case (3)
                call ESMF_LocalArrayCopy3DR8(larrayIn, larrayOut, rc=localrc)
       case (4)
                call ESMF_LocalArrayCopy4DR8(larrayIn, larrayOut, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrayCopy5DR8(larrayIn, larrayOut, rc=localrc)
       case (6)
                call ESMF_LocalArrayCopy6DR8(larrayIn, larrayOut, rc=localrc)
              case (7)
                call ESMF_LocalArrayCopy7DR8(larrayIn, larrayOut, rc=localrc)
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
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! Set init code
    ESMF_INIT_SET_CREATED(larrayOut)
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_LocalArrayCopyF90Ptr
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr"
!BOPI
! !IROUTINE: ESMF_LocalArrConstrF90Ptr - Create and add F90 ptr to array
! !INTERFACE:
     subroutine ESMF_LocalArrConstrF90Ptr(array, counts, rank, kind, &
                                          lbounds, ubounds, rc)
!
! !ARGUMENTS:
      type(ESMF_LocalArray), intent(inout) :: array
      integer, dimension(:), intent(in) :: counts
      integer, intent(in) :: rank
      type(ESMF_TypeKind), intent(in) :: kind
      integer, dimension(:), intent(in) :: lbounds
      integer, dimension(:), intent(in) :: ubounds
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Take a partially created {\tt ESMF\_LocalArray} and T/K/R information and call
! the proper subroutine to create an F90 pointer, allocate space, and set the
! corresponding values in the {\tt ESMF\_LocalArray} object.
!
! The arguments are:
! \begin{description}
! \item[array]
! Partially created {\tt ESMF\_LocalArray} object. This entry point is used
! during both the C++ and F90 create calls if we need to create an F90
! pointer to be used later.
! \item[counts]
! The number of items in each dimension of the array. This is a 1D
! integer array the same length as the rank.
! \item[rank]
! Array rank.
! This must match what is already in the array - it is here only as
! a convienience.
! \item[kind]
! Array kind.
! This must match what is already in the array - it is here only as
! a convienience.
! \item[lbounds]
! The lower index values per rank.
! \item[ubounds]
! The upper index values per rank.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
        ! Local vars
        integer :: localrc ! local return code
        logical :: rcpresent ! did user specify rc?
        integer :: localkind
        localrc = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.
        ! Initialize return code; assume routine not implemented
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif
        ! Cannot check init status of array argument here because
        ! the array object is only partially created at this point
        localkind = kind%dkind
        ! Call a T/K/R specific interface in order to create the proper
        ! type of F90 pointer, allocate the space, set the values in the
        ! Local Array object, and return. (The routine this code is calling is
        ! generated by macro.)
        !! calling routines generated from macros by the preprocessor
        select case (localkind)
#ifndef ESMF_NO_INTEGER_1_BYTE
          case (ESMF_TYPEKIND_I1%dkind)
            select case (rank)
       case (1)
                call ESMF_LocalArrConstrF90Ptr1DI1(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (2)
                call ESMF_LocalArrConstrF90Ptr2DI1(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (3)
                call ESMF_LocalArrConstrF90Ptr3DI1(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (4)
                call ESMF_LocalArrConstrF90Ptr4DI1(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrConstrF90Ptr5DI1(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (6)
                call ESMF_LocalArrConstrF90Ptr6DI1(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
              case (7)
                call ESMF_LocalArrConstrF90Ptr7DI1(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
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
                call ESMF_LocalArrConstrF90Ptr1DI2(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (2)
                call ESMF_LocalArrConstrF90Ptr2DI2(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (3)
                call ESMF_LocalArrConstrF90Ptr3DI2(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (4)
                call ESMF_LocalArrConstrF90Ptr4DI2(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrConstrF90Ptr5DI2(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (6)
                call ESMF_LocalArrConstrF90Ptr6DI2(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
              case (7)
                call ESMF_LocalArrConstrF90Ptr7DI2(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
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
                call ESMF_LocalArrConstrF90Ptr1DI4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (2)
                call ESMF_LocalArrConstrF90Ptr2DI4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (3)
                call ESMF_LocalArrConstrF90Ptr3DI4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (4)
                call ESMF_LocalArrConstrF90Ptr4DI4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrConstrF90Ptr5DI4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (6)
                call ESMF_LocalArrConstrF90Ptr6DI4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
              case (7)
                call ESMF_LocalArrConstrF90Ptr7DI4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
          case (ESMF_TYPEKIND_I8%dkind)
     select case(rank)
       case (1)
                call ESMF_LocalArrConstrF90Ptr1DI8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (2)
                call ESMF_LocalArrConstrF90Ptr2DI8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (3)
                call ESMF_LocalArrConstrF90Ptr3DI8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (4)
                call ESMF_LocalArrConstrF90Ptr4DI8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrConstrF90Ptr5DI8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (6)
                call ESMF_LocalArrConstrF90Ptr6DI8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
              case (7)
                call ESMF_LocalArrConstrF90Ptr7DI8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
          case (ESMF_TYPEKIND_R4%dkind)
     select case(rank)
       case (1)
                call ESMF_LocalArrConstrF90Ptr1DR4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (2)
                call ESMF_LocalArrConstrF90Ptr2DR4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (3)
                call ESMF_LocalArrConstrF90Ptr3DR4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (4)
                call ESMF_LocalArrConstrF90Ptr4DR4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrConstrF90Ptr5DR4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (6)
                call ESMF_LocalArrConstrF90Ptr6DR4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
              case (7)
                call ESMF_LocalArrConstrF90Ptr7DR4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
          case (ESMF_TYPEKIND_R8%dkind)
     select case(rank)
       case (1)
                call ESMF_LocalArrConstrF90Ptr1DR8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (2)
                call ESMF_LocalArrConstrF90Ptr2DR8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (3)
                call ESMF_LocalArrConstrF90Ptr3DR8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (4)
                call ESMF_LocalArrConstrF90Ptr4DR8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrConstrF90Ptr5DR8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
       case (6)
                call ESMF_LocalArrConstrF90Ptr6DR8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
              case (7)
                call ESMF_LocalArrConstrF90Ptr7DR8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=localrc)
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
        ! Set return code if caller specified it
        if (rcpresent) rc = localrc
        end subroutine ESMF_LocalArrConstrF90Ptr
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!! < start of macros which become actual function bodies after expansion >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!------------------------------------------------------------------------------ 
!BOP 
! !IROUTINE: ESMF_LocalArrayCreate - Create a LocalArray from a Fortran pointer (associated or unassociated) 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_LocalArrayCreate() 
! function ESMF_LocalArrCreateByPtr<rank><type><kind>(fptr, docopy, counts, & 
! lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
! type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr<rank><type><kind> 
! 
! !ARGUMENTS: 
! <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: fptr 
! type(ESMF_CopyFlag), intent(in), optional :: docopy 
! integer, intent(in), optional :: counts(:) 
! integer, intent(in), optional :: lbounds(:) 
! integer, intent(in), optional :: ubounds(:) 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt ESMF\_LocalArray} based on a Fortran array pointer. 
! Two cases must be distinguished. 
! 
! First, if {\tt fptr} is associated 
! the optional {\tt docopy} argument may be used to indicate whether the 
! associated data is to be copied or referenced. For associated {\tt fptr} 
! the optional {\tt counts}, {\tt lbounds} and {\tt ubounds} arguments need 
! not be specified. However, all present arguments will be checked against 
! {\tt fptr} for consistency. 
! 
! Second, if {\tt fptr} is unassociated the optional argument {\tt docopy} 
! must not be specified. However, in this case a complete set of counts and 
! bounds information must be provided. Any combination of present {\tt counts} 
! {\tt lbounds} and {\tt ubounds} arguments that provides a complete 
! specification is valid. All input information will be checked for 
! consistency. 
! 
! The arguments are: 
! \begin{description} 
! \item[fptr] 
! A Fortran array pointer (associated or unassociated). 
! \item[{[docopy]}] 
! Indicate copy vs. reference behavior in case of associated {\tt fptr}. 
! This argument must {\em not} be present for unassociated {\tt fptr}. 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} 
! reference the associated data array. If set to {\tt ESMF\_DATA\_COPY} this 
! routine allocates new memory and copies the data from the pointer into 
! the new LocalArray allocation. 
! \item[{[counts]}] 
! The number of items in each dimension of the array. This is a 1D 
! integer array the same length as the rank. The {\tt count} argument may 
! be omitted if both {\tt lbounds} and {\tt ubounds} arguments are present. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
!---------------------------------------------------------------------------- 
 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr1DI1(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr1DI1 
 
 integer (ESMF_KIND_I1), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: cnts ! local counts 
 integer, dimension(1) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 1 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:1) = lbounds(1:1) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 1 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:1) = ubounds(1:1) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 1 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:1) = counts(1:1) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:1) = ub(1:1) - lb(1:1) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:1) = 1 
 ub(1:1) = cnts(1:1) 
 else if (.not.present(lbounds)) then 
 lb(1:1) = ub(1:1) - cnts(1:1) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:1) = lb(1:1) + cnts(1:1) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 1 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_TYPEKIND_I1, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr1DI1(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr1DI1 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr1DI1) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr1DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr2DI1(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr2DI1 
 
 integer (ESMF_KIND_I1), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: cnts ! local counts 
 integer, dimension(2) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 2 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:2) = lbounds(1:2) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 2 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:2) = ubounds(1:2) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 2 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:2) = counts(1:2) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:2) = ub(1:2) - lb(1:2) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:2) = 1 
 ub(1:2) = cnts(1:2) 
 else if (.not.present(lbounds)) then 
 lb(1:2) = ub(1:2) - cnts(1:2) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:2) = lb(1:2) + cnts(1:2) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 2 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_TYPEKIND_I1, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr2DI1(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr2DI1 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr2DI1) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr2DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr3DI1(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr3DI1 
 
 integer (ESMF_KIND_I1), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: cnts ! local counts 
 integer, dimension(3) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 3 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:3) = lbounds(1:3) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 3 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:3) = ubounds(1:3) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 3 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:3) = counts(1:3) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:3) = ub(1:3) - lb(1:3) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:3) = 1 
 ub(1:3) = cnts(1:3) 
 else if (.not.present(lbounds)) then 
 lb(1:3) = ub(1:3) - cnts(1:3) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:3) = lb(1:3) + cnts(1:3) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 3 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_TYPEKIND_I1, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr3DI1(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr3DI1 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr3DI1) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr3DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr4DI1(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr4DI1 
 
 integer (ESMF_KIND_I1), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: cnts ! local counts 
 integer, dimension(4) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 4 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:4) = lbounds(1:4) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 4 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:4) = ubounds(1:4) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 4 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:4) = counts(1:4) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:4) = ub(1:4) - lb(1:4) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:4) = 1 
 ub(1:4) = cnts(1:4) 
 else if (.not.present(lbounds)) then 
 lb(1:4) = ub(1:4) - cnts(1:4) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:4) = lb(1:4) + cnts(1:4) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 4 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_TYPEKIND_I1, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr4DI1(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr4DI1 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr4DI1) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr4DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr5DI1(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr5DI1 
 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: cnts ! local counts 
 integer, dimension(5) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 5 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:5) = lbounds(1:5) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 5 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:5) = ubounds(1:5) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 5 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:5) = counts(1:5) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:5) = ub(1:5) - lb(1:5) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:5) = 1 
 ub(1:5) = cnts(1:5) 
 else if (.not.present(lbounds)) then 
 lb(1:5) = ub(1:5) - cnts(1:5) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:5) = lb(1:5) + cnts(1:5) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 5 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_TYPEKIND_I1, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr5DI1(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr5DI1 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr5DI1) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr5DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr6DI1(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr6DI1 
 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(6) :: cnts ! local counts 
 integer, dimension(6) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 6 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:6) = lbounds(1:6) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 6 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:6) = ubounds(1:6) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 6 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:6) = counts(1:6) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:6) = ub(1:6) - lb(1:6) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:6) = 1 
 ub(1:6) = cnts(1:6) 
 else if (.not.present(lbounds)) then 
 lb(1:6) = ub(1:6) - cnts(1:6) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:6) = lb(1:6) + cnts(1:6) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 6 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 6, ESMF_TYPEKIND_I1, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr6DI1(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr6DI1 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr6DI1) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr6DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr7DI1(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr7DI1 
 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(7) :: cnts ! local counts 
 integer, dimension(7) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 7 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:7) = lbounds(1:7) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 7 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:7) = ubounds(1:7) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 7 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:7) = counts(1:7) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:7) = ub(1:7) - lb(1:7) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:7) = 1 
 ub(1:7) = cnts(1:7) 
 else if (.not.present(lbounds)) then 
 lb(1:7) = ub(1:7) - cnts(1:7) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:7) = lb(1:7) + cnts(1:7) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 7 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 7, ESMF_TYPEKIND_I1, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr7DI1(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr7DI1 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr7DI1) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr7DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr1DI2(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr1DI2 
 
 integer (ESMF_KIND_I2), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: cnts ! local counts 
 integer, dimension(1) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 1 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:1) = lbounds(1:1) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 1 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:1) = ubounds(1:1) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 1 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:1) = counts(1:1) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:1) = ub(1:1) - lb(1:1) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:1) = 1 
 ub(1:1) = cnts(1:1) 
 else if (.not.present(lbounds)) then 
 lb(1:1) = ub(1:1) - cnts(1:1) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:1) = lb(1:1) + cnts(1:1) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 1 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_TYPEKIND_I2, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr1DI2(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr1DI2 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr1DI2) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr2DI2(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr2DI2 
 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: cnts ! local counts 
 integer, dimension(2) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 2 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:2) = lbounds(1:2) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 2 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:2) = ubounds(1:2) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 2 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:2) = counts(1:2) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:2) = ub(1:2) - lb(1:2) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:2) = 1 
 ub(1:2) = cnts(1:2) 
 else if (.not.present(lbounds)) then 
 lb(1:2) = ub(1:2) - cnts(1:2) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:2) = lb(1:2) + cnts(1:2) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 2 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_TYPEKIND_I2, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr2DI2(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr2DI2 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr2DI2) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr3DI2(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr3DI2 
 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: cnts ! local counts 
 integer, dimension(3) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 3 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:3) = lbounds(1:3) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 3 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:3) = ubounds(1:3) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 3 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:3) = counts(1:3) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:3) = ub(1:3) - lb(1:3) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:3) = 1 
 ub(1:3) = cnts(1:3) 
 else if (.not.present(lbounds)) then 
 lb(1:3) = ub(1:3) - cnts(1:3) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:3) = lb(1:3) + cnts(1:3) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 3 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_TYPEKIND_I2, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr3DI2(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr3DI2 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr3DI2) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr4DI2(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr4DI2 
 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: cnts ! local counts 
 integer, dimension(4) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 4 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:4) = lbounds(1:4) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 4 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:4) = ubounds(1:4) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 4 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:4) = counts(1:4) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:4) = ub(1:4) - lb(1:4) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:4) = 1 
 ub(1:4) = cnts(1:4) 
 else if (.not.present(lbounds)) then 
 lb(1:4) = ub(1:4) - cnts(1:4) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:4) = lb(1:4) + cnts(1:4) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 4 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_TYPEKIND_I2, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr4DI2(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr4DI2 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr4DI2) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr5DI2(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr5DI2 
 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: cnts ! local counts 
 integer, dimension(5) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 5 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:5) = lbounds(1:5) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 5 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:5) = ubounds(1:5) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 5 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:5) = counts(1:5) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:5) = ub(1:5) - lb(1:5) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:5) = 1 
 ub(1:5) = cnts(1:5) 
 else if (.not.present(lbounds)) then 
 lb(1:5) = ub(1:5) - cnts(1:5) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:5) = lb(1:5) + cnts(1:5) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 5 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_TYPEKIND_I2, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr5DI2(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr5DI2 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr5DI2) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr6DI2(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr6DI2 
 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(6) :: cnts ! local counts 
 integer, dimension(6) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 6 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:6) = lbounds(1:6) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 6 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:6) = ubounds(1:6) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 6 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:6) = counts(1:6) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:6) = ub(1:6) - lb(1:6) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:6) = 1 
 ub(1:6) = cnts(1:6) 
 else if (.not.present(lbounds)) then 
 lb(1:6) = ub(1:6) - cnts(1:6) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:6) = lb(1:6) + cnts(1:6) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 6 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 6, ESMF_TYPEKIND_I2, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr6DI2(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr6DI2 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr6DI2) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr6DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr7DI2(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr7DI2 
 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(7) :: cnts ! local counts 
 integer, dimension(7) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 7 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:7) = lbounds(1:7) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 7 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:7) = ubounds(1:7) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 7 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:7) = counts(1:7) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:7) = ub(1:7) - lb(1:7) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:7) = 1 
 ub(1:7) = cnts(1:7) 
 else if (.not.present(lbounds)) then 
 lb(1:7) = ub(1:7) - cnts(1:7) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:7) = lb(1:7) + cnts(1:7) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 7 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 7, ESMF_TYPEKIND_I2, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr7DI2(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr7DI2 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr7DI2) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr7DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr1DI4(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr1DI4 
 
 integer (ESMF_KIND_I4), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: cnts ! local counts 
 integer, dimension(1) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 1 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:1) = lbounds(1:1) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 1 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:1) = ubounds(1:1) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 1 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:1) = counts(1:1) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:1) = ub(1:1) - lb(1:1) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:1) = 1 
 ub(1:1) = cnts(1:1) 
 else if (.not.present(lbounds)) then 
 lb(1:1) = ub(1:1) - cnts(1:1) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:1) = lb(1:1) + cnts(1:1) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 1 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_TYPEKIND_I4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr1DI4(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr1DI4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr1DI4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr1DI8(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr1DI8 
 
 integer (ESMF_KIND_I8), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: cnts ! local counts 
 integer, dimension(1) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 1 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:1) = lbounds(1:1) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 1 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:1) = ubounds(1:1) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 1 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:1) = counts(1:1) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:1) = ub(1:1) - lb(1:1) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:1) = 1 
 ub(1:1) = cnts(1:1) 
 else if (.not.present(lbounds)) then 
 lb(1:1) = ub(1:1) - cnts(1:1) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:1) = lb(1:1) + cnts(1:1) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 1 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_TYPEKIND_I8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr1DI8(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr1DI8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr1DI8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr1DR4(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr1DR4 
 
 real (ESMF_KIND_R4), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: cnts ! local counts 
 integer, dimension(1) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 1 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:1) = lbounds(1:1) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 1 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:1) = ubounds(1:1) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 1 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:1) = counts(1:1) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:1) = ub(1:1) - lb(1:1) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:1) = 1 
 ub(1:1) = cnts(1:1) 
 else if (.not.present(lbounds)) then 
 lb(1:1) = ub(1:1) - cnts(1:1) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:1) = lb(1:1) + cnts(1:1) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 1 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_TYPEKIND_R4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr1DR4(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr1DR4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr1DR4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr1DR8(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr1DR8 
 
 real (ESMF_KIND_R8), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: cnts ! local counts 
 integer, dimension(1) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 1 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:1) = lbounds(1:1) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 1 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:1) = ubounds(1:1) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<1) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 1 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:1) = counts(1:1) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:1) = ub(1:1) - lb(1:1) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:1) = 1 
 ub(1:1) = cnts(1:1) 
 else if (.not.present(lbounds)) then 
 lb(1:1) = ub(1:1) - cnts(1:1) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:1) = lb(1:1) + cnts(1:1) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 1 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_TYPEKIND_R8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr1DR8(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr1DR8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr1DR8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr2DI4(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr2DI4 
 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: cnts ! local counts 
 integer, dimension(2) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 2 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:2) = lbounds(1:2) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 2 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:2) = ubounds(1:2) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 2 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:2) = counts(1:2) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:2) = ub(1:2) - lb(1:2) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:2) = 1 
 ub(1:2) = cnts(1:2) 
 else if (.not.present(lbounds)) then 
 lb(1:2) = ub(1:2) - cnts(1:2) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:2) = lb(1:2) + cnts(1:2) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 2 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_TYPEKIND_I4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr2DI4(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr2DI4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr2DI4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr2DI8(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr2DI8 
 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: cnts ! local counts 
 integer, dimension(2) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 2 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:2) = lbounds(1:2) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 2 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:2) = ubounds(1:2) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 2 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:2) = counts(1:2) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:2) = ub(1:2) - lb(1:2) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:2) = 1 
 ub(1:2) = cnts(1:2) 
 else if (.not.present(lbounds)) then 
 lb(1:2) = ub(1:2) - cnts(1:2) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:2) = lb(1:2) + cnts(1:2) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 2 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_TYPEKIND_I8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr2DI8(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr2DI8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr2DI8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr2DR4(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr2DR4 
 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: cnts ! local counts 
 integer, dimension(2) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 2 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:2) = lbounds(1:2) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 2 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:2) = ubounds(1:2) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 2 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:2) = counts(1:2) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:2) = ub(1:2) - lb(1:2) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:2) = 1 
 ub(1:2) = cnts(1:2) 
 else if (.not.present(lbounds)) then 
 lb(1:2) = ub(1:2) - cnts(1:2) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:2) = lb(1:2) + cnts(1:2) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 2 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_TYPEKIND_R4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr2DR4(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr2DR4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr2DR4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr2DR8(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr2DR8 
 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: cnts ! local counts 
 integer, dimension(2) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 2 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:2) = lbounds(1:2) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 2 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:2) = ubounds(1:2) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<2) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 2 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:2) = counts(1:2) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:2) = ub(1:2) - lb(1:2) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:2) = 1 
 ub(1:2) = cnts(1:2) 
 else if (.not.present(lbounds)) then 
 lb(1:2) = ub(1:2) - cnts(1:2) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:2) = lb(1:2) + cnts(1:2) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 2 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_TYPEKIND_R8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr2DR8(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr2DR8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr2DR8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr3DI4(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr3DI4 
 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: cnts ! local counts 
 integer, dimension(3) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 3 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:3) = lbounds(1:3) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 3 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:3) = ubounds(1:3) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 3 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:3) = counts(1:3) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:3) = ub(1:3) - lb(1:3) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:3) = 1 
 ub(1:3) = cnts(1:3) 
 else if (.not.present(lbounds)) then 
 lb(1:3) = ub(1:3) - cnts(1:3) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:3) = lb(1:3) + cnts(1:3) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 3 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_TYPEKIND_I4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr3DI4(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr3DI4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr3DI4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr3DI8(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr3DI8 
 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: cnts ! local counts 
 integer, dimension(3) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 3 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:3) = lbounds(1:3) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 3 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:3) = ubounds(1:3) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 3 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:3) = counts(1:3) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:3) = ub(1:3) - lb(1:3) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:3) = 1 
 ub(1:3) = cnts(1:3) 
 else if (.not.present(lbounds)) then 
 lb(1:3) = ub(1:3) - cnts(1:3) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:3) = lb(1:3) + cnts(1:3) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 3 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_TYPEKIND_I8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr3DI8(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr3DI8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr3DI8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr3DR4(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr3DR4 
 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: cnts ! local counts 
 integer, dimension(3) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 3 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:3) = lbounds(1:3) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 3 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:3) = ubounds(1:3) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 3 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:3) = counts(1:3) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:3) = ub(1:3) - lb(1:3) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:3) = 1 
 ub(1:3) = cnts(1:3) 
 else if (.not.present(lbounds)) then 
 lb(1:3) = ub(1:3) - cnts(1:3) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:3) = lb(1:3) + cnts(1:3) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 3 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_TYPEKIND_R4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr3DR4(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr3DR4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr3DR4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr3DR8(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr3DR8 
 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: cnts ! local counts 
 integer, dimension(3) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 3 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:3) = lbounds(1:3) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 3 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:3) = ubounds(1:3) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<3) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 3 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:3) = counts(1:3) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:3) = ub(1:3) - lb(1:3) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:3) = 1 
 ub(1:3) = cnts(1:3) 
 else if (.not.present(lbounds)) then 
 lb(1:3) = ub(1:3) - cnts(1:3) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:3) = lb(1:3) + cnts(1:3) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 3 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_TYPEKIND_R8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr3DR8(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr3DR8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr3DR8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr4DI4(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr4DI4 
 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: cnts ! local counts 
 integer, dimension(4) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 4 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:4) = lbounds(1:4) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 4 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:4) = ubounds(1:4) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 4 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:4) = counts(1:4) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:4) = ub(1:4) - lb(1:4) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:4) = 1 
 ub(1:4) = cnts(1:4) 
 else if (.not.present(lbounds)) then 
 lb(1:4) = ub(1:4) - cnts(1:4) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:4) = lb(1:4) + cnts(1:4) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 4 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_TYPEKIND_I4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr4DI4(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr4DI4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr4DI4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr4DI8(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr4DI8 
 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: cnts ! local counts 
 integer, dimension(4) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 4 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:4) = lbounds(1:4) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 4 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:4) = ubounds(1:4) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 4 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:4) = counts(1:4) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:4) = ub(1:4) - lb(1:4) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:4) = 1 
 ub(1:4) = cnts(1:4) 
 else if (.not.present(lbounds)) then 
 lb(1:4) = ub(1:4) - cnts(1:4) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:4) = lb(1:4) + cnts(1:4) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 4 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_TYPEKIND_I8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr4DI8(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr4DI8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr4DI8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr4DR4(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr4DR4 
 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: cnts ! local counts 
 integer, dimension(4) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 4 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:4) = lbounds(1:4) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 4 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:4) = ubounds(1:4) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 4 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:4) = counts(1:4) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:4) = ub(1:4) - lb(1:4) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:4) = 1 
 ub(1:4) = cnts(1:4) 
 else if (.not.present(lbounds)) then 
 lb(1:4) = ub(1:4) - cnts(1:4) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:4) = lb(1:4) + cnts(1:4) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 4 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_TYPEKIND_R4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr4DR4(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr4DR4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr4DR4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr4DR8(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr4DR8 
 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: cnts ! local counts 
 integer, dimension(4) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 4 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:4) = lbounds(1:4) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 4 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:4) = ubounds(1:4) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<4) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 4 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:4) = counts(1:4) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:4) = ub(1:4) - lb(1:4) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:4) = 1 
 ub(1:4) = cnts(1:4) 
 else if (.not.present(lbounds)) then 
 lb(1:4) = ub(1:4) - cnts(1:4) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:4) = lb(1:4) + cnts(1:4) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 4 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_TYPEKIND_R8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr4DR8(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr4DR8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr4DR8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr5DI4(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr5DI4 
 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: cnts ! local counts 
 integer, dimension(5) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 5 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:5) = lbounds(1:5) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 5 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:5) = ubounds(1:5) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 5 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:5) = counts(1:5) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:5) = ub(1:5) - lb(1:5) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:5) = 1 
 ub(1:5) = cnts(1:5) 
 else if (.not.present(lbounds)) then 
 lb(1:5) = ub(1:5) - cnts(1:5) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:5) = lb(1:5) + cnts(1:5) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 5 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_TYPEKIND_I4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr5DI4(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr5DI4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr5DI4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr5DI8(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr5DI8 
 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: cnts ! local counts 
 integer, dimension(5) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 5 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:5) = lbounds(1:5) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 5 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:5) = ubounds(1:5) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 5 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:5) = counts(1:5) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:5) = ub(1:5) - lb(1:5) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:5) = 1 
 ub(1:5) = cnts(1:5) 
 else if (.not.present(lbounds)) then 
 lb(1:5) = ub(1:5) - cnts(1:5) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:5) = lb(1:5) + cnts(1:5) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 5 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_TYPEKIND_I8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr5DI8(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr5DI8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr5DI8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr5DR4(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr5DR4 
 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: cnts ! local counts 
 integer, dimension(5) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 5 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:5) = lbounds(1:5) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 5 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:5) = ubounds(1:5) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 5 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:5) = counts(1:5) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:5) = ub(1:5) - lb(1:5) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:5) = 1 
 ub(1:5) = cnts(1:5) 
 else if (.not.present(lbounds)) then 
 lb(1:5) = ub(1:5) - cnts(1:5) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:5) = lb(1:5) + cnts(1:5) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 5 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_TYPEKIND_R4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr5DR4(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr5DR4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr5DR4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr5DR8(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr5DR8 
 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: cnts ! local counts 
 integer, dimension(5) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 5 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:5) = lbounds(1:5) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 5 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:5) = ubounds(1:5) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<5) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 5 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:5) = counts(1:5) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:5) = ub(1:5) - lb(1:5) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:5) = 1 
 ub(1:5) = cnts(1:5) 
 else if (.not.present(lbounds)) then 
 lb(1:5) = ub(1:5) - cnts(1:5) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:5) = lb(1:5) + cnts(1:5) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 5 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_TYPEKIND_R8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr5DR8(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr5DR8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr5DR8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr6DI4(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr6DI4 
 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(6) :: cnts ! local counts 
 integer, dimension(6) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 6 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:6) = lbounds(1:6) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 6 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:6) = ubounds(1:6) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 6 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:6) = counts(1:6) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:6) = ub(1:6) - lb(1:6) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:6) = 1 
 ub(1:6) = cnts(1:6) 
 else if (.not.present(lbounds)) then 
 lb(1:6) = ub(1:6) - cnts(1:6) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:6) = lb(1:6) + cnts(1:6) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 6 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 6, ESMF_TYPEKIND_I4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr6DI4(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr6DI4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr6DI4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr6DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr6DI8(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr6DI8 
 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(6) :: cnts ! local counts 
 integer, dimension(6) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 6 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:6) = lbounds(1:6) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 6 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:6) = ubounds(1:6) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 6 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:6) = counts(1:6) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:6) = ub(1:6) - lb(1:6) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:6) = 1 
 ub(1:6) = cnts(1:6) 
 else if (.not.present(lbounds)) then 
 lb(1:6) = ub(1:6) - cnts(1:6) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:6) = lb(1:6) + cnts(1:6) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 6 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 6, ESMF_TYPEKIND_I8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr6DI8(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr6DI8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr6DI8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr6DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr6DR4(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr6DR4 
 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(6) :: cnts ! local counts 
 integer, dimension(6) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 6 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:6) = lbounds(1:6) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 6 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:6) = ubounds(1:6) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 6 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:6) = counts(1:6) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:6) = ub(1:6) - lb(1:6) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:6) = 1 
 ub(1:6) = cnts(1:6) 
 else if (.not.present(lbounds)) then 
 lb(1:6) = ub(1:6) - cnts(1:6) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:6) = lb(1:6) + cnts(1:6) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 6 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 6, ESMF_TYPEKIND_R4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr6DR4(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr6DR4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr6DR4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr6DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr6DR8(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr6DR8 
 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(6) :: cnts ! local counts 
 integer, dimension(6) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 6 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:6) = lbounds(1:6) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 6 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:6) = ubounds(1:6) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<6) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 6 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:6) = counts(1:6) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:6) = ub(1:6) - lb(1:6) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:6) = 1 
 ub(1:6) = cnts(1:6) 
 else if (.not.present(lbounds)) then 
 lb(1:6) = ub(1:6) - cnts(1:6) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:6) = lb(1:6) + cnts(1:6) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 6 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 6, ESMF_TYPEKIND_R8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr6DR8(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr6DR8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr6DR8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr6DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr7DI4(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr7DI4 
 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(7) :: cnts ! local counts 
 integer, dimension(7) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 7 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:7) = lbounds(1:7) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 7 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:7) = ubounds(1:7) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 7 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:7) = counts(1:7) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:7) = ub(1:7) - lb(1:7) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:7) = 1 
 ub(1:7) = cnts(1:7) 
 else if (.not.present(lbounds)) then 
 lb(1:7) = ub(1:7) - cnts(1:7) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:7) = lb(1:7) + cnts(1:7) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 7 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 7, ESMF_TYPEKIND_I4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr7DI4(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr7DI4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr7DI4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr7DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr7DI8(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr7DI8 
 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(7) :: cnts ! local counts 
 integer, dimension(7) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 7 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:7) = lbounds(1:7) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 7 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:7) = ubounds(1:7) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 7 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:7) = counts(1:7) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:7) = ub(1:7) - lb(1:7) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:7) = 1 
 ub(1:7) = cnts(1:7) 
 else if (.not.present(lbounds)) then 
 lb(1:7) = ub(1:7) - cnts(1:7) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:7) = lb(1:7) + cnts(1:7) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 7 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 7, ESMF_TYPEKIND_I8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr7DI8(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr7DI8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr7DI8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr7DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr7DR4(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr7DR4 
 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(7) :: cnts ! local counts 
 integer, dimension(7) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 7 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:7) = lbounds(1:7) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 7 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:7) = ubounds(1:7) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 7 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:7) = counts(1:7) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:7) = ub(1:7) - lb(1:7) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:7) = 1 
 ub(1:7) = cnts(1:7) 
 else if (.not.present(lbounds)) then 
 lb(1:7) = ub(1:7) - cnts(1:7) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:7) = lb(1:7) + cnts(1:7) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 7 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 7, ESMF_TYPEKIND_R4, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr7DR4(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr7DR4 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr7DR4) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr7DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrCreateByPtr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrCreateByPtr" 
 function ESMF_LocalArrCreateByPtr7DR8(fptr, docopy, counts, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByPtr7DR8 
 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: localrc ! local return code 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(7) :: cnts ! local counts 
 integer, dimension(7) :: lb, ub ! local bounds 
 integer:: i 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 array%this = ESMF_NULL_POINTER 
 
 ! Test to see if fptr is associated and check consistency of arguments 
 if (associated(fptr)) then 
 ! Get sizes from current F90 array, to check args 
 cnts = shape(fptr) 
 lb = lbound(fptr) 
 ub = ubound(fptr) 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 else 
 if (present(docopy)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- docopy argument is only allowed with associated fptr argument", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 copy = ESMF_DATA_SPACE 
 endif 
 
 ! Check that enough info from counts, lbounds and ubounds is present 
 if (.not.associated(fptr) .and. .not.present(counts)) then 
 if (.not.present(lbounds) .or. .not.present(ubounds)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_OPT, & 
 "- lbounds and ubounds must be present when counts argument is not present", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 endif 
 
 ! Check size of optional counts and bounds and fill the local variables 
 if (present(lbounds)) then 
 if (size(lbounds)<7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- lbounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 7 
 if (lb(i) .ne. lbounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided lbounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 lb(1:7) = lbounds(1:7) 
 endif 
 endif 
 if (present(ubounds)) then 
 if (size(ubounds)<7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- ubounds argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 7 
 if (ub(i) .ne. ubounds(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided ubounds are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 ub(1:7) = ubounds(1:7) 
 endif 
 endif 
 if (present(counts)) then 
 if (size(counts)<7) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, & 
 "- counts argument must be of size rank", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 if (associated(fptr)) then 
 do i=1, 7 
 if (cnts(i) .ne. counts(i)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, & 
 "- provided counts are incompatible with associated fptr", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 else 
 cnts(1:7) = counts(1:7) 
 endif 
 else if (.not.associated(fptr)) then 
 cnts(1:7) = ub(1:7) - lb(1:7) + 1 
 endif 
 if (.not.associated(fptr)) then 
 if (.not.present(lbounds) .and. .not.present(ubounds)) then 
 lb(1:7) = 1 
 ub(1:7) = cnts(1:7) 
 else if (.not.present(lbounds)) then 
 lb(1:7) = ub(1:7) - cnts(1:7) + 1 
 else if (.not.present(ubounds)) then 
 ub(1:7) = lb(1:7) + cnts(1:7) - 1 
 endif 
 endif 
 
 ! Check that the local bounds and counts variables match 
 do i=1, 7 
 if (cnts(i) .ne. (ub(i)-lb(i)+1)) then 
 call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, & 
 "- counts and bounds mismatch detected", & 
 ESMF_CONTEXT, rc) 
 return 
 endif 
 enddo 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 7, ESMF_TYPEKIND_R8, & 
 ESMF_FROM_FORTRAN, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 call ESMF_LocalArrConstrF90Ptr7DR8(array, cnts, fptr,& 
 copy, lb, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByPtr7DR8 = array 
 
 ! Set init code 
 ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByPtr7DR8) 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end function ESMF_LocalArrCreateByPtr7DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!! < start of macros which become actual function bodies after expansion >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr<rank><type><kind> - Create a Fortran Ptr of the proper T/K/R 
! 
! !INTERFACE: 
! subroutine ESMF_LocalArrConstrF90Ptr<rank><type><kind>(array, counts, fptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
! type(ESMF_LocalArray), intent(inout) :: array 
! integer, dimension(:), intent(in) :: counts 
! <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer, optional :: fptr 
! type(ESMF_CopyFlag), intent(in), optional :: docopy 
! integer, dimension(:), intent(in), optional :: lbounds 
! integer, dimension(:), intent(in), optional :: ubounds 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
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
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[fptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if a Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOPI 
 
 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr1DI1(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I1), dimension(:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap1DI1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I1), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr1DI1 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr1DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr2DI1(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I1), dimension(:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap2DI1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I1), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr2DI1 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr2DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr3DI1(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I1), dimension(:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap3DI1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I1), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr3DI1 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr3DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr4DI1(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I1), dimension(:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap4DI1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I1), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr4DI1 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr4DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr5DI1(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap5DI1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr5DI1 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr5DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr6DI1(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap6DI1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr6DI1 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr6DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr7DI1(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap7DI1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr7DI1 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr7DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr1DI2(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I2), dimension(:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap1DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr1DI2 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr2DI2(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I2), dimension(:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap2DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr2DI2 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr3DI2(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap3DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr3DI2 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr4DI2(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap4DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr4DI2 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr5DI2(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap5DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr5DI2 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr6DI2(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap6DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr6DI2 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr6DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr7DI2(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap7DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr7DI2 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr7DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr1DI4(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I4), dimension(:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap1DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr1DI4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr1DI8(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I8), dimension(:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap1DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr1DI8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr1DR4(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R4), dimension(:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap1DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr1DR4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr1DR8(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R8), dimension(:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap1DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr1DR8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr2DI4(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I4), dimension(:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap2DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr2DI4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr2DI8(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I8), dimension(:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap2DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr2DI8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr2DR4(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R4), dimension(:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap2DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr2DR4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr2DR8(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R8), dimension(:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap2DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr2DR8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr3DI4(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap3DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr3DI4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr3DI8(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap3DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr3DI8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr3DR4(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap3DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr3DR4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr3DR8(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap3DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr3DR8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr4DI4(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap4DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr4DI4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr4DI8(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap4DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr4DI8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr4DR4(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap4DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr4DR4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr4DR8(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap4DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr4DR8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr5DI4(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap5DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr5DI4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr5DI8(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap5DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr5DI8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr5DR4(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap5DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr5DR4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr5DR8(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap5DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr5DR8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr6DI4(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap6DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr6DI4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr6DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr6DI8(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap6DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr6DI8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr6DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr6DR4(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap6DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr6DR4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr6DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr6DR8(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap6DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr6DR8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr6DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr7DI4(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap7DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr7DI4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr7DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr7DI8(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap7DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr7DI8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr7DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr7DR4(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap7DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr7DR4 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr7DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" 
 
 subroutine ESMF_LocalArrConstrF90Ptr7DR8(array, counts, fptr, docopy, lbounds, ubounds, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer, optional :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap7DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(fptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
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
 allocate(newp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "LocalArray data space", & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 if (willcopy) then 
 newp = fptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! Fortran pointer, the base addr, the counts, etc. 
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr7DR8 => newp 
 if (size(newp) .ne. 0) then 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 else 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_NULL_POINTER, counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, localrc) 
 endif 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 if (rcpresent) rc = localrc 
 
 end subroutine ESMF_LocalArrConstrF90Ptr7DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!! < start of macros which become actual function bodies after expansion >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrayAdjust<rank><type><kind> - Adjust the bounds of the Fortran pointer member according to the proper T/K/R 
! 
! !INTERFACE: 
! recursive subroutine ESMF_LocalArrayAdjust<rank><type><kind>(array,&
! counts, lb, ub, fshape, rc) 
! 
! !ARGUMENTS: 
! type(ESMF_LocalArray), intent(inout) :: array 
! integer, dimension(:), intent(in) :: counts 
! integer, dimension(:), intent(in), optional :: lb 
! integer, dimension(:), intent(in), optional :: ub 
! mname (ESMF_KIND_mtypekind), dimension(mdim), target, optional ::&
! fshape(mrng) 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Each LocalArray object internally keeps a reference to an F90 array pointer. 
! This call modifies the meta-data associated with this F90 array pointer 
! by passing the F90 array pointer into a F90 subroutine with an explicit shape 
! dummy argument. On this interface the bounds meta data for the dummy argument 
! is not those of the actual argument but is reset to the bounds specified 
! on the subroutine interface. Using macros the bounds on the callee side are 
! set to match those of the LocalArray object meta data. Finally the internal 
! F90 array pointer is reset to reflect the desired bounds in the F90 dope 
! vector. The risk of data copy on this interface should be minimal because 
! the shape is not changed and the dummy argument has the target attribute. 
!EOPI 
 
 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust1DI1(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I1), dimension(:), target, optional :: &
 fshape(lb(1):ub(1)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap1DI1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I1), dimension(:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr1DI1 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr1DI1 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust1DI1(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust1DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust2DI1(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I1), dimension(:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap2DI1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I1), dimension(:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr2DI1 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr2DI1 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust2DI1(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust2DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust3DI1(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I1), dimension(:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap3DI1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I1), dimension(:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr3DI1 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr3DI1 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust3DI1(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust3DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust4DI1(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I1), dimension(:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap4DI1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I1), dimension(:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr4DI1 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr4DI1 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust4DI1(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust4DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust5DI1(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap5DI1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr5DI1 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr5DI1 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust5DI1(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust5DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust6DI1(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap6DI1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr6DI1 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr6DI1 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust6DI1(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust6DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust7DI1(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap7DI1) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr7DI1 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr7DI1 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust7DI1(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust7DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust1DI2(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I2), dimension(:), target, optional :: &
 fshape(lb(1):ub(1)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap1DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr1DI2 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr1DI2 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust1DI2(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust2DI2(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I2), dimension(:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap2DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr2DI2 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr2DI2 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust2DI2(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust3DI2(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I2), dimension(:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap3DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr3DI2 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr3DI2 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust3DI2(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust4DI2(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap4DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr4DI2 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr4DI2 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust4DI2(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust5DI2(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap5DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr5DI2 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr5DI2 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust5DI2(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust6DI2(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap6DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr6DI2 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr6DI2 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust6DI2(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust6DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust7DI2(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap7DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr7DI2 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr7DI2 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust7DI2(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust7DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust1DI4(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I4), dimension(:), target, optional :: &
 fshape(lb(1):ub(1)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap1DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr1DI4 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr1DI4 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust1DI4(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust1DI8(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I8), dimension(:), target, optional :: &
 fshape(lb(1):ub(1)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap1DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr1DI8 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr1DI8 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust1DI8(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust1DR4(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_R4), dimension(:), target, optional :: &
 fshape(lb(1):ub(1)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap1DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr1DR4 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr1DR4 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust1DR4(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust1DR8(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_R8), dimension(:), target, optional :: &
 fshape(lb(1):ub(1)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap1DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr1DR8 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr1DR8 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust1DR8(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust2DI4(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I4), dimension(:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap2DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr2DI4 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr2DI4 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust2DI4(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust2DI8(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I8), dimension(:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap2DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr2DI8 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr2DI8 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust2DI8(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust2DR4(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_R4), dimension(:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap2DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr2DR4 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr2DR4 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust2DR4(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust2DR8(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_R8), dimension(:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap2DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr2DR8 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr2DR8 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust2DR8(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust3DI4(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I4), dimension(:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap3DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr3DI4 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr3DI4 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust3DI4(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust3DI8(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I8), dimension(:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap3DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr3DI8 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr3DI8 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust3DI8(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust3DR4(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_R4), dimension(:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap3DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr3DR4 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr3DR4 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust3DR4(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust3DR8(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_R8), dimension(:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap3DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr3DR8 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr3DR8 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust3DR8(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust4DI4(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap4DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr4DI4 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr4DI4 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust4DI4(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust4DI8(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap4DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr4DI8 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr4DI8 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust4DI8(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust4DR4(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_R4), dimension(:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap4DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr4DR4 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr4DR4 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust4DR4(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust4DR8(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_R8), dimension(:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap4DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr4DR8 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr4DR8 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust4DR8(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust5DI4(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap5DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr5DI4 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr5DI4 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust5DI4(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust5DI8(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap5DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr5DI8 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr5DI8 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust5DI8(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust5DR4(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap5DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr5DR4 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr5DR4 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust5DR4(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust5DR8(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap5DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr5DR8 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr5DR8 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust5DR8(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust6DI4(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap6DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr6DI4 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr6DI4 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust6DI4(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust6DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust6DI8(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap6DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr6DI8 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr6DI8 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust6DI8(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust6DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust6DR4(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap6DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr6DR4 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr6DR4 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust6DR4(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust6DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust6DR8(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap6DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr6DR8 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr6DR8 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust6DR8(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust6DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust7DI4(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap7DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr7DI4 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr7DI4 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust7DI4(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust7DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust7DI8(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap7DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr7DI8 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr7DI8 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust7DI8(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust7DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust7DR4(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap7DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr7DR4 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr7DR4 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust7DR4(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust7DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayAdjust" 
 
 recursive subroutine ESMF_LocalArrayAdjust7DR8(array, &
 counts, lb, ub, fshape, rc) 
 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in) :: lb 
 integer, dimension(:), intent(in) :: ub 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), target, optional :: &
 fshape(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)) 
 integer, intent(out), optional :: rc 
 
 ! Local variables 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap7DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 
 ! Initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 
 ! Recursive branch 
 if (present(fshape)) then 
 ! second recursion -> set the member in LocalArray 
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) 
!call c_esmc_vmpointerprint(fshape) 
 wrap%ptr7DR8 => fshape 
 call c_ESMC_LocalArraySetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! some compilers will have made a copy on the way to down here 
 ! the following call forces the base address encoded in the F90 
 ! dope vector to point to the actual memory allocation *if* a mismatch 
 ! on the first data element location is detected, i.e. we are dealing 
 ! with a temporary copy of the actual array. 
 call c_ESMC_LocalArrayForceF90Ptr(array, & 
 ESMF_DATA_ADDRESS(fshape(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 else 
 ! first recursion -> get F90ptr member and call subr. recursively 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 fptr => wrap%ptr7DR8 
!print *, "First recursion: ", lbound(fptr), ubound(fptr) 
!call c_esmc_vmpointerprint(fptr) 
 call ESMF_LocalArrayAdjust7DR8(array, counts, lb, ub, fptr, rc=localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 endif 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayAdjust7DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!! < start of macros which become actual function bodies after expansion >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
! subroutine ESMF_LocalArrayCopy<rank><type><kind>(arrayIn, arrayOut, rc) 
! 
! !ARGUMENTS: 
! type(ESMF_LocalArray), intent(in) :: arrayIn 
! type(ESMF_LocalArray), intent(inout) :: arrayOut 
! type(ESMF_CopyFlag), intent(in), optional :: docopy 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the data buffer, or return a Fortran pointer 
! to a new copy of the data. 
! Valid type/kind/rank combinations supported by the 
! framework are: ranks 1 to 7, type real of kind *4 or *8, 
! and type integer of kind *1, *2, *4, or *8. 
! 
! The arguments are: 
! \begin{description} 
! \item[arrayIn] 
! The {\tt ESMF\_LocalArray} to copy. 
! \item[arrayOut] 
! The copied array. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified. 
! Can either make a new copy of the data or reference existing data. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOPI 
 
 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy1DI1(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap1DI1) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap1DI1) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(1), ub(1) ! size info for the array 
 integer (ESMF_KIND_I1), dimension(:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 1, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 1, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr1DI1 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr1DI1 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy1DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy2DI1(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap2DI1) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap2DI1) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(2), ub(2) ! size info for the array 
 integer (ESMF_KIND_I1), dimension(:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 2, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 2, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr2DI1 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr2DI1 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy2DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy3DI1(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap3DI1) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap3DI1) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(3), ub(3) ! size info for the array 
 integer (ESMF_KIND_I1), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 3, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 3, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr3DI1 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr3DI1 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy3DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy4DI1(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap4DI1) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap4DI1) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(4), ub(4) ! size info for the array 
 integer (ESMF_KIND_I1), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 4, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 4, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr4DI1 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr4DI1 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy4DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy5DI1(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap5DI1) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap5DI1) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(5), ub(5) ! size info for the array 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 5, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 5, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr5DI1 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr5DI1 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy5DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy6DI1(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap6DI1) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap6DI1) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(6), ub(6) ! size info for the array 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 6, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 6, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr6DI1 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr6DI1 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy6DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy7DI1(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap7DI1) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap7DI1) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(7), ub(7) ! size info for the array 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 7, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 7, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr7DI1 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr7DI1 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy7DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy1DI2(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap1DI2) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap1DI2) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(1), ub(1) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 1, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 1, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr1DI2 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr1DI2 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy2DI2(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap2DI2) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap2DI2) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(2), ub(2) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 2, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 2, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr2DI2 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr2DI2 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy3DI2(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap3DI2) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap3DI2) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(3), ub(3) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 3, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 3, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr3DI2 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr3DI2 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy4DI2(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap4DI2) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap4DI2) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(4), ub(4) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 4, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 4, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr4DI2 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr4DI2 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy5DI2(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap5DI2) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap5DI2) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(5), ub(5) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 5, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 5, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr5DI2 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr5DI2 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy6DI2(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap6DI2) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap6DI2) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(6), ub(6) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 6, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 6, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr6DI2 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr6DI2 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy6DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy7DI2(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap7DI2) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap7DI2) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(7), ub(7) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 7, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 7, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr7DI2 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr7DI2 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy7DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy1DI4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap1DI4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap1DI4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(1), ub(1) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 1, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 1, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr1DI4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr1DI4 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy1DI8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap1DI8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap1DI8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(1), ub(1) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 1, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 1, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr1DI8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr1DI8 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy1DR4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap1DR4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap1DR4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(1), ub(1) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 1, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 1, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr1DR4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr1DR4 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy1DR8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap1DR8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap1DR8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(1), ub(1) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 1, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 1, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr1DR8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr1DR8 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy2DI4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap2DI4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap2DI4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(2), ub(2) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 2, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 2, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr2DI4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr2DI4 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy2DI8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap2DI8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap2DI8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(2), ub(2) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 2, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 2, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr2DI8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr2DI8 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy2DR4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap2DR4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap2DR4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(2), ub(2) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 2, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 2, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr2DR4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr2DR4 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy2DR8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap2DR8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap2DR8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(2), ub(2) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 2, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 2, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr2DR8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr2DR8 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy3DI4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap3DI4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap3DI4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(3), ub(3) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 3, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 3, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr3DI4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr3DI4 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy3DI8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap3DI8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap3DI8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(3), ub(3) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 3, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 3, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr3DI8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr3DI8 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy3DR4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap3DR4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap3DR4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(3), ub(3) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 3, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 3, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr3DR4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr3DR4 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy3DR8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap3DR8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap3DR8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(3), ub(3) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 3, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 3, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr3DR8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr3DR8 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy4DI4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap4DI4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap4DI4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(4), ub(4) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 4, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 4, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr4DI4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr4DI4 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy4DI8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap4DI8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap4DI8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(4), ub(4) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 4, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 4, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr4DI8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr4DI8 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy4DR4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap4DR4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap4DR4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(4), ub(4) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 4, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 4, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr4DR4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr4DR4 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy4DR8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap4DR8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap4DR8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(4), ub(4) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 4, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 4, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr4DR8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr4DR8 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy5DI4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap5DI4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap5DI4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(5), ub(5) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 5, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 5, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr5DI4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr5DI4 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy5DI8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap5DI8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap5DI8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(5), ub(5) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 5, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 5, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr5DI8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr5DI8 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy5DR4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap5DR4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap5DR4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(5), ub(5) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 5, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 5, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr5DR4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr5DR4 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy5DR8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap5DR8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap5DR8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(5), ub(5) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 5, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 5, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr5DR8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr5DR8 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy6DI4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap6DI4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap6DI4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(6), ub(6) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 6, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 6, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr6DI4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr6DI4 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy6DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy6DI8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap6DI8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap6DI8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(6), ub(6) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 6, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 6, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr6DI8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr6DI8 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy6DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy6DR4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap6DR4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap6DR4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(6), ub(6) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 6, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 6, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr6DR4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr6DR4 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy6DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy6DR8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap6DR8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap6DR8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(6), ub(6) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 6, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 6, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr6DR8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr6DR8 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy6DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy7DI4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap7DI4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap7DI4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(7), ub(7) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 7, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 7, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr7DI4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr7DI4 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy7DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy7DI8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap7DI8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap7DI8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(7), ub(7) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 7, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 7, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr7DI8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr7DI8 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy7DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy7DR4(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap7DR4) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap7DR4) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(7), ub(7) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 7, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 7, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr7DR4 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr7DR4 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy7DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayCopy##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayCopy" 
 subroutine ESMF_LocalArrayCopy7DR8(arrayIn, arrayOut, rc) 
 
 type(ESMF_LocalArray) :: arrayIn 
 type(ESMF_LocalArray) :: arrayOut 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 type (ESMF_ArrWrap7DR8) :: wrapIn ! for passing f90 ptr to C++ 
 type (ESMF_ArrWrap7DR8) :: wrapOut ! for passing f90 ptr to C++ 
 
 integer :: lb(7), ub(7) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: lp ! local copy 
 
 ! initialize return code; assume routine not implemented 
 localrc = ESMF_RC_NOT_IMPL 
 if (present(rc)) then 
 rc = ESMF_RC_NOT_IMPL 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(arrayIn, wrapIn, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! Allocate a new buffer and return a copy 
 call c_ESMC_IArrayGetLbounds(arrayIn, 7, lb, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_IArrayGetUbounds(arrayIn, 7, ub, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 ! lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7) is a macro that has correct construction of lb and ub elements 
 allocate(lp(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, & 
 "local data space", & 
 ESMF_CONTEXT, rc)) return 
 ! this must do a contents assignment 
 lp = wrapIn%ptr7DR8 
 ! point to this memory allocation in the arrayOut 
 wrapOut%ptr7DR8 => lp 
 call c_ESMC_LocalArraySetF90Ptr(arrayOut, wrapOut, localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 call c_ESMC_LocalArraySetBaseAddr(arrayOut, & 
 ESMF_DATA_ADDRESS(lp(lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7))), localrc) 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
 
 ! return successfully 
 if (present(rc)) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayCopy7DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!! < start of macros which become actual function bodies after expansion >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
! subroutine ESMF_LocalArrayDeallocate<rank><type><kind>(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
! type(ESMF_LocalArray) :: array 
! type (ESMF_ArrWrapmrankDmtypekind) :: wrap 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to get the value from. 
! \item[wrap] 
! An internal derived type containing the Fortran pointer. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOPI 
 
 
#ifndef ESMF_NO_INTEGER_1_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate1DI1(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap1DI1) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr1DI1) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate1DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate2DI1(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap2DI1) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr2DI1) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate2DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate3DI1(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap3DI1) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr3DI1) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate3DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate4DI1(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap4DI1) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr4DI1) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate4DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate5DI1(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap5DI1) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr5DI1) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate5DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate6DI1(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap6DI1) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr6DI1) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate6DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate7DI1(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap7DI1) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr7DI1) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate7DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
#ifndef ESMF_NO_INTEGER_2_BYTE 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate1DI2(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap1DI2) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr1DI2) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate2DI2(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap2DI2) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr2DI2) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate3DI2(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap3DI2) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr3DI2) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate4DI2(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap4DI2) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr4DI2) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate5DI2(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap5DI2) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr5DI2) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate6DI2(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap6DI2) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr6DI2) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate6DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate7DI2(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap7DI2) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr7DI2) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate7DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
#endif 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate1DI4(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap1DI4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr1DI4) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate1DI8(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap1DI8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr1DI8) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate1DR4(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap1DR4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr1DR4) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate1DR8(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap1DR8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr1DR8) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate2DI4(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap2DI4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr2DI4) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate2DI8(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap2DI8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr2DI8) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate2DR4(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap2DR4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr2DR4) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate2DR8(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap2DR8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr2DR8) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate3DI4(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap3DI4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr3DI4) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate3DI8(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap3DI8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr3DI8) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate3DR4(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap3DR4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr3DR4) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate3DR8(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap3DR8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr3DR8) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate4DI4(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap4DI4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr4DI4) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate4DI8(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap4DI8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr4DI8) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate4DR4(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap4DR4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr4DR4) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate4DR8(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap4DR8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr4DR8) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#ifndef ESMF_NO_GREATER_THAN_4D 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate5DI4(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap5DI4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr5DI4) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate5DI8(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap5DI8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr5DI8) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate5DR4(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap5DR4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr5DR4) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate5DR8(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap5DR8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr5DR8) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate6DI4(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap6DI4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr6DI4) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate6DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate6DI8(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap6DI8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr6DI8) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate6DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate6DR4(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap6DR4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr6DR4) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate6DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate6DR8(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap6DR8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr6DR8) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate6DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate7DI4(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap7DI4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr7DI4) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate7DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate7DI8(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap7DI8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr7DI8) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate7DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate7DR4(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap7DR4) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr7DR4) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate7DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
#undef ESMF_METHOD 
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" 
#define ESMF_METHOD "ESMF_LocalArrayDeallocate" 
 subroutine ESMF_LocalArrayDeallocate7DR8(array, wrap, rc) 
 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap7DR8) :: wrap 
 integer, intent(out), optional :: rc 
 
 integer :: localrc ! local return code 
 
 ! Initialize return code; assume routine not implemented 
 if (present(rc)) rc = ESMF_RC_NOT_IMPL 
 localrc = ESMF_RC_NOT_IMPL 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc) 
 deallocate(wrap%ptr7DR8) 
 
 if (present(rc)) rc = localrc 
 
 end subroutine ESMF_LocalArrayDeallocate7DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
#endif 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!! < end of automatically generated function >
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayDestroy"
!BOP
! !IROUTINE: ESMF_LocalArrayDestroy - Destroy a LocalArray object
!
! !INTERFACE:
  subroutine ESMF_LocalArrayDestroy(larray, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(inout) :: larray
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Releases all resources associated with this {\tt ESMF\_LocalArray} object.
!
! The arguments are:
! \begin{description}
! \item[larray]
! Destroy contents of this {\tt ESMF\_LocalArray}.
! \item[[rc]]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    ! Local vars
    integer :: localrc ! local return code
    logical :: needsdealloc ! do we need to free space?
    integer :: rank
    type(ESMF_TypeKind) :: kind
! To reduce the depth of crossings of the F90/C++ boundary we first
! query to see if we are responsible for deleting the data space. If so,
! first deallocate the space and then call the C++ code to release
! the object space. When it returns we are done and can return to the user.
! Otherwise we would need to make a nested call back into F90 from C++ to do
! the deallocate() during the object delete.
    ! Initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, larray, rc)
    needsdealloc = .FALSE.
    ! TODO: document the current rule - if we do the allocate in
    ! the case of ESMF_DATA_COPY at create time then we delete the
    ! space. otherwise, the user needs to destroy the array
    ! (we will ignore the data) and call deallocate themselves.
    ! Call Destruct first, then free this memory
    call c_ESMC_LocalArrayNeedsDealloc(larray, needsdealloc, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return
    if (needsdealloc) then
      call c_ESMC_LocalArrayGetRank(larray, rank, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
      call c_ESMC_LocalArrayGetTypeKind(larray, kind, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
      call ESMF_LocalArrayF90Deallocate(larray, rank, kind, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
      call c_ESMC_LocalArraySetNoDealloc(larray, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
    endif
    ! Calling deallocate first means this will not return back to F90
    ! before returning for good.
    call c_ESMC_LocalArrayDestroy(larray, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return
    ! Set init code
    ESMF_INIT_SET_DELETED(larray)
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_LocalArrayDestroy
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArraySetInfo"
!BOPI
! !IROUTINE: ESMF_LocalArraySetInfo
!
! !INTERFACE:
  subroutine ESMF_LocalArraySetInfo(array, counts, lbounds, ubounds, &
                                        offsets, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(inout) :: array
    integer, dimension(:), intent(in), optional :: counts
    integer, dimension(:), intent(in), optional :: lbounds
    integer, dimension(:), intent(in), optional :: ubounds
    integer, dimension(:), intent(in), optional :: offsets
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Must be used with care - if you set the values on an already created
! array object to be inconsistent with the F90 pointer, then bad things
! will happen.
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc
    ! Initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, array, rc)
    call c_ESMC_LocalArraySetInfo(array, counts, lbounds, ubounds, offsets, &
      localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_LocalArraySetInfo
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArraySetData"
!BOPI
! !IROUTINE: ESMF_LocalArraySetData
!
! !INTERFACE:
  subroutine ESMF_LocalArraySetData(array, databuf, docopy, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(inout) :: array
    real(ESMF_KIND_R8), dimension (:), pointer :: databuf
    type(ESMF_CopyFlag), intent(in) :: docopy
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used only with the version of {\tt ESMF\_LocalArrayCreate} which
! creates an empty {\tt ESMF\_LocalArray} and allows the Data to be
! specified later. Otherwise it is an error to replace the data contents
! associated with a {\tt ESMF\_LocalArray}.
!
!EOPI
!------------------------------------------------------------------------------
!
! added BOPI/EOPI until code is written
! TODO: code goes here
!
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, array, rc)
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
  end subroutine ESMF_LocalArraySetData
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is Allocate/Deallocate for LocalArrays
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!!! TODO: the interface now calls ESMF_LocalArrConstrF90Ptr instead of
!!! this routine. It maybe can go away? and can we do something with
!!! ESMF_LocalArrayF90Deallocate to get rid of it as well, so the interfaces
!!! are more symmetric?
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayF90Allocate"
!BOPI
! !IROUTINE: ESMF_LocalArrayF90Allocate - Allocate an F90 pointer and set LocalArray info
!
! !INTERFACE:
     subroutine ESMF_LocalArrayF90Allocate(array, rank, kind, counts, &
                                           lbounds, ubounds, rc)
!
! !ARGUMENTS:
      type(ESMF_LocalArray), intent(inout) :: array
      integer, intent(in) :: rank
      type(ESMF_TypeKind), intent(in) :: kind
      integer, dimension(:), intent(in) :: counts
      integer, dimension(:), intent(in) :: lbounds
      integer, dimension(:), intent(in) :: ubounds
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Allocate data contents for an {\tt ESMF\_LocalArray} created from the
! C++ interface. The arguments are:
! \begin{description}
! \item[array]
! A partially created {\tt ESMF\_LocalArray} object.
! \item[rank]
! The {\tt ESMF\_LocalArray} rank.
! \item[kind]
! The {\tt ESMF\_LocalArray} kind (short/2, long/8, etc).
! \item[counts]
! An integer array, size {\tt rank}, of each dimension length.
! \item[lbounds]
! An integer array, size {\tt rank}, of each dimensions lower index.
! \item[ubounds]
! An integer array, size {\tt rank}, of each dimensions upper index.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
    integer :: localrc ! local return code
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
 

    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    lb(1:size(lbounds)) = lbounds
    ub(1:size(ubounds)) = ubounds
    localkind = kind%dkind
    !! calling routines generated from macros by the preprocessor
        select case (localkind)
#ifndef ESMF_NO_INTEGER_1_BYTE
          case (ESMF_TYPEKIND_I1%dkind)
            select case (rank)
       case (1)
! <Created by macro - do not edit directly > 
 allocate(l1DI1%ptr1DI1(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l1DI1, & 
 ESMF_DATA_ADDRESS(l1DI1%ptr1DI1 ( lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 allocate(l2DI1%ptr2DI1(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l2DI1, & 
 ESMF_DATA_ADDRESS(l2DI1%ptr2DI1 ( lb(1),lb(2) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 allocate(l3DI1%ptr3DI1(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l3DI1, & 
 ESMF_DATA_ADDRESS(l3DI1%ptr3DI1 ( lb(1),lb(2),lb(3) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 allocate(l4DI1%ptr4DI1(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l4DI1, & 
 ESMF_DATA_ADDRESS(l4DI1%ptr4DI1 ( lb(1),lb(2),lb(3),lb(4) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 allocate(l5DI1%ptr5DI1(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l5DI1, & 
 ESMF_DATA_ADDRESS(l5DI1%ptr5DI1 ( lb(1),lb(2),lb(3),lb(4),lb(5) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 allocate(l6DI1%ptr6DI1(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l6DI1, & 
 ESMF_DATA_ADDRESS(l6DI1%ptr6DI1 ( lb(1),lb(2),lb(3),lb(4),lb(5),lb(6) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 allocate(l7DI1%ptr7DI1(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l7DI1, & 
 ESMF_DATA_ADDRESS(l7DI1%ptr7DI1 ( lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
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
 allocate(l1DI2%ptr1DI2(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l1DI2, & 
 ESMF_DATA_ADDRESS(l1DI2%ptr1DI2 ( lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 allocate(l2DI2%ptr2DI2(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l2DI2, & 
 ESMF_DATA_ADDRESS(l2DI2%ptr2DI2 ( lb(1),lb(2) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 allocate(l3DI2%ptr3DI2(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l3DI2, & 
 ESMF_DATA_ADDRESS(l3DI2%ptr3DI2 ( lb(1),lb(2),lb(3) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 allocate(l4DI2%ptr4DI2(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l4DI2, & 
 ESMF_DATA_ADDRESS(l4DI2%ptr4DI2 ( lb(1),lb(2),lb(3),lb(4) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 allocate(l5DI2%ptr5DI2(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l5DI2, & 
 ESMF_DATA_ADDRESS(l5DI2%ptr5DI2 ( lb(1),lb(2),lb(3),lb(4),lb(5) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 allocate(l6DI2%ptr6DI2(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l6DI2, & 
 ESMF_DATA_ADDRESS(l6DI2%ptr6DI2 ( lb(1),lb(2),lb(3),lb(4),lb(5),lb(6) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 allocate(l7DI2%ptr7DI2(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l7DI2, & 
 ESMF_DATA_ADDRESS(l7DI2%ptr7DI2 ( lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
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
 allocate(l1DI4%ptr1DI4(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l1DI4, & 
 ESMF_DATA_ADDRESS(l1DI4%ptr1DI4 ( lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 allocate(l2DI4%ptr2DI4(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l2DI4, & 
 ESMF_DATA_ADDRESS(l2DI4%ptr2DI4 ( lb(1),lb(2) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 allocate(l3DI4%ptr3DI4(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l3DI4, & 
 ESMF_DATA_ADDRESS(l3DI4%ptr3DI4 ( lb(1),lb(2),lb(3) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 allocate(l4DI4%ptr4DI4(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l4DI4, & 
 ESMF_DATA_ADDRESS(l4DI4%ptr4DI4 ( lb(1),lb(2),lb(3),lb(4) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 allocate(l5DI4%ptr5DI4(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l5DI4, & 
 ESMF_DATA_ADDRESS(l5DI4%ptr5DI4 ( lb(1),lb(2),lb(3),lb(4),lb(5) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 allocate(l6DI4%ptr6DI4(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l6DI4, & 
 ESMF_DATA_ADDRESS(l6DI4%ptr6DI4 ( lb(1),lb(2),lb(3),lb(4),lb(5),lb(6) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 allocate(l7DI4%ptr7DI4(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l7DI4, & 
 ESMF_DATA_ADDRESS(l7DI4%ptr7DI4 ( lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
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
 allocate(l1DI8%ptr1DI8(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l1DI8, & 
 ESMF_DATA_ADDRESS(l1DI8%ptr1DI8 ( lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 allocate(l2DI8%ptr2DI8(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l2DI8, & 
 ESMF_DATA_ADDRESS(l2DI8%ptr2DI8 ( lb(1),lb(2) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 allocate(l3DI8%ptr3DI8(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l3DI8, & 
 ESMF_DATA_ADDRESS(l3DI8%ptr3DI8 ( lb(1),lb(2),lb(3) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 allocate(l4DI8%ptr4DI8(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l4DI8, & 
 ESMF_DATA_ADDRESS(l4DI8%ptr4DI8 ( lb(1),lb(2),lb(3),lb(4) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 allocate(l5DI8%ptr5DI8(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l5DI8, & 
 ESMF_DATA_ADDRESS(l5DI8%ptr5DI8 ( lb(1),lb(2),lb(3),lb(4),lb(5) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 allocate(l6DI8%ptr6DI8(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l6DI8, & 
 ESMF_DATA_ADDRESS(l6DI8%ptr6DI8 ( lb(1),lb(2),lb(3),lb(4),lb(5),lb(6) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 allocate(l7DI8%ptr7DI8(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l7DI8, & 
 ESMF_DATA_ADDRESS(l7DI8%ptr7DI8 ( lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
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
 allocate(l1DR4%ptr1DR4(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l1DR4, & 
 ESMF_DATA_ADDRESS(l1DR4%ptr1DR4 ( lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 allocate(l2DR4%ptr2DR4(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l2DR4, & 
 ESMF_DATA_ADDRESS(l2DR4%ptr2DR4 ( lb(1),lb(2) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 allocate(l3DR4%ptr3DR4(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l3DR4, & 
 ESMF_DATA_ADDRESS(l3DR4%ptr3DR4 ( lb(1),lb(2),lb(3) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 allocate(l4DR4%ptr4DR4(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l4DR4, & 
 ESMF_DATA_ADDRESS(l4DR4%ptr4DR4 ( lb(1),lb(2),lb(3),lb(4) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 allocate(l5DR4%ptr5DR4(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l5DR4, & 
 ESMF_DATA_ADDRESS(l5DR4%ptr5DR4 ( lb(1),lb(2),lb(3),lb(4),lb(5) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 allocate(l6DR4%ptr6DR4(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l6DR4, & 
 ESMF_DATA_ADDRESS(l6DR4%ptr6DR4 ( lb(1),lb(2),lb(3),lb(4),lb(5),lb(6) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 allocate(l7DR4%ptr7DR4(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l7DR4, & 
 ESMF_DATA_ADDRESS(l7DR4%ptr7DR4 ( lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
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
 allocate(l1DR8%ptr1DR8(lb(1):ub(1)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l1DR8, & 
 ESMF_DATA_ADDRESS(l1DR8%ptr1DR8 ( lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 allocate(l2DR8%ptr2DR8(lb(1):ub(1),lb(2):ub(2)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l2DR8, & 
 ESMF_DATA_ADDRESS(l2DR8%ptr2DR8 ( lb(1),lb(2) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 allocate(l3DR8%ptr3DR8(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l3DR8, & 
 ESMF_DATA_ADDRESS(l3DR8%ptr3DR8 ( lb(1),lb(2),lb(3) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 allocate(l4DR8%ptr4DR8(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l4DR8, & 
 ESMF_DATA_ADDRESS(l4DR8%ptr4DR8 ( lb(1),lb(2),lb(3),lb(4) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 allocate(l5DR8%ptr5DR8(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l5DR8, & 
 ESMF_DATA_ADDRESS(l5DR8%ptr5DR8 ( lb(1),lb(2),lb(3),lb(4),lb(5) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 allocate(l6DR8%ptr6DR8(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l6DR8, & 
 ESMF_DATA_ADDRESS(l6DR8%ptr6DR8 ( lb(1),lb(2),lb(3),lb(4),lb(5),lb(6) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
 ESMF_ERR_PASSTHRU, & 
 ESMF_CONTEXT, rc)) return 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 allocate(l7DR8%ptr7DR8(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)), stat=localrc) 
 if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray allocate", & 
 ESMF_CONTEXT, rc)) return 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, l7DR8, & 
 ESMF_DATA_ADDRESS(l7DR8%ptr7DR8 ( lb(1),lb(2),lb(3),lb(4),lb(5),lb(6),lb(7) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, localrc) 
 
 if (ESMF_LogMsgFoundError(localrc, & 
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
        if (present(rc)) rc = localrc
        end subroutine ESMF_LocalArrayF90Allocate
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayF90Deallocate"
!BOPI
! !IROUTINE: ESMF_LocalArrayF90Deallocate - Deallocate an F90 pointer
!
! !INTERFACE:
     subroutine ESMF_LocalArrayF90Deallocate(array, rank, kind, rc)
!
! !ARGUMENTS:
      type(ESMF_LocalArray), intent(inout) :: array
      integer :: rank
      type(ESMF_TypeKind) :: kind
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Deallocate data contents for an {\tt ESMF\_LocalArray} created from
! the C++ interface. The arguments are:
! \begin{description}
! \item[array]
! A partially created {\tt ESMF\_LocalArray} object.
! \item[rank]
! The {\tt ESMF\_LocalArray} rank.
! \item[kind]
! The {\tt ESMF\_LocalArray} kind (short/2, long/8, etc).
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
    integer :: localrc ! local return code
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
    localrc = ESMF_RC_NOT_IMPL
    localkind = kind
    !! calling routines generated from macros by the preprocessor
        select case (localkind)
#ifndef ESMF_NO_INTEGER_1_BYTE
          case (ESMF_TYPEKIND_I1%dkind)
            select case (rank)
       case (1)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l1DI1, localrc) 
 deallocate(l1DI1%ptr1DI1, stat=localrc) 
 nullify(l1DI1%ptr1DI1) 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l2DI1, localrc) 
 deallocate(l2DI1%ptr2DI1, stat=localrc) 
 nullify(l2DI1%ptr2DI1) 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l3DI1, localrc) 
 deallocate(l3DI1%ptr3DI1, stat=localrc) 
 nullify(l3DI1%ptr3DI1) 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l4DI1, localrc) 
 deallocate(l4DI1%ptr4DI1, stat=localrc) 
 nullify(l4DI1%ptr4DI1) 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l5DI1, localrc) 
 deallocate(l5DI1%ptr5DI1, stat=localrc) 
 nullify(l5DI1%ptr5DI1) 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l6DI1, localrc) 
 deallocate(l6DI1%ptr6DI1, stat=localrc) 
 nullify(l6DI1%ptr6DI1) 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l7DI1, localrc) 
 deallocate(l7DI1%ptr7DI1, stat=localrc) 
 nullify(l7DI1%ptr7DI1) 
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
 call c_ESMC_LocalArrayGetF90Ptr(array, l1DI2, localrc) 
 deallocate(l1DI2%ptr1DI2, stat=localrc) 
 nullify(l1DI2%ptr1DI2) 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l2DI2, localrc) 
 deallocate(l2DI2%ptr2DI2, stat=localrc) 
 nullify(l2DI2%ptr2DI2) 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l3DI2, localrc) 
 deallocate(l3DI2%ptr3DI2, stat=localrc) 
 nullify(l3DI2%ptr3DI2) 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l4DI2, localrc) 
 deallocate(l4DI2%ptr4DI2, stat=localrc) 
 nullify(l4DI2%ptr4DI2) 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l5DI2, localrc) 
 deallocate(l5DI2%ptr5DI2, stat=localrc) 
 nullify(l5DI2%ptr5DI2) 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l6DI2, localrc) 
 deallocate(l6DI2%ptr6DI2, stat=localrc) 
 nullify(l6DI2%ptr6DI2) 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l7DI2, localrc) 
 deallocate(l7DI2%ptr7DI2, stat=localrc) 
 nullify(l7DI2%ptr7DI2) 
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
 call c_ESMC_LocalArrayGetF90Ptr(array, l1DI4, localrc) 
 deallocate(l1DI4%ptr1DI4, stat=localrc) 
 nullify(l1DI4%ptr1DI4) 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l2DI4, localrc) 
 deallocate(l2DI4%ptr2DI4, stat=localrc) 
 nullify(l2DI4%ptr2DI4) 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l3DI4, localrc) 
 deallocate(l3DI4%ptr3DI4, stat=localrc) 
 nullify(l3DI4%ptr3DI4) 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l4DI4, localrc) 
 deallocate(l4DI4%ptr4DI4, stat=localrc) 
 nullify(l4DI4%ptr4DI4) 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l5DI4, localrc) 
 deallocate(l5DI4%ptr5DI4, stat=localrc) 
 nullify(l5DI4%ptr5DI4) 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l6DI4, localrc) 
 deallocate(l6DI4%ptr6DI4, stat=localrc) 
 nullify(l6DI4%ptr6DI4) 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l7DI4, localrc) 
 deallocate(l7DI4%ptr7DI4, stat=localrc) 
 nullify(l7DI4%ptr7DI4) 
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
 call c_ESMC_LocalArrayGetF90Ptr(array, l1DI8, localrc) 
 deallocate(l1DI8%ptr1DI8, stat=localrc) 
 nullify(l1DI8%ptr1DI8) 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l2DI8, localrc) 
 deallocate(l2DI8%ptr2DI8, stat=localrc) 
 nullify(l2DI8%ptr2DI8) 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l3DI8, localrc) 
 deallocate(l3DI8%ptr3DI8, stat=localrc) 
 nullify(l3DI8%ptr3DI8) 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l4DI8, localrc) 
 deallocate(l4DI8%ptr4DI8, stat=localrc) 
 nullify(l4DI8%ptr4DI8) 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l5DI8, localrc) 
 deallocate(l5DI8%ptr5DI8, stat=localrc) 
 nullify(l5DI8%ptr5DI8) 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l6DI8, localrc) 
 deallocate(l6DI8%ptr6DI8, stat=localrc) 
 nullify(l6DI8%ptr6DI8) 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l7DI8, localrc) 
 deallocate(l7DI8%ptr7DI8, stat=localrc) 
 nullify(l7DI8%ptr7DI8) 
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
 call c_ESMC_LocalArrayGetF90Ptr(array, l1DR4, localrc) 
 deallocate(l1DR4%ptr1DR4, stat=localrc) 
 nullify(l1DR4%ptr1DR4) 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l2DR4, localrc) 
 deallocate(l2DR4%ptr2DR4, stat=localrc) 
 nullify(l2DR4%ptr2DR4) 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l3DR4, localrc) 
 deallocate(l3DR4%ptr3DR4, stat=localrc) 
 nullify(l3DR4%ptr3DR4) 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l4DR4, localrc) 
 deallocate(l4DR4%ptr4DR4, stat=localrc) 
 nullify(l4DR4%ptr4DR4) 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l5DR4, localrc) 
 deallocate(l5DR4%ptr5DR4, stat=localrc) 
 nullify(l5DR4%ptr5DR4) 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l6DR4, localrc) 
 deallocate(l6DR4%ptr6DR4, stat=localrc) 
 nullify(l6DR4%ptr6DR4) 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l7DR4, localrc) 
 deallocate(l7DR4%ptr7DR4, stat=localrc) 
 nullify(l7DR4%ptr7DR4) 
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
 call c_ESMC_LocalArrayGetF90Ptr(array, l1DR8, localrc) 
 deallocate(l1DR8%ptr1DR8, stat=localrc) 
 nullify(l1DR8%ptr1DR8) 
! < End macro - do not edit directly > 

       case (2)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l2DR8, localrc) 
 deallocate(l2DR8%ptr2DR8, stat=localrc) 
 nullify(l2DR8%ptr2DR8) 
! < End macro - do not edit directly > 

       case (3)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l3DR8, localrc) 
 deallocate(l3DR8%ptr3DR8, stat=localrc) 
 nullify(l3DR8%ptr3DR8) 
! < End macro - do not edit directly > 

       case (4)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l4DR8, localrc) 
 deallocate(l4DR8%ptr4DR8, stat=localrc) 
 nullify(l4DR8%ptr4DR8) 
! < End macro - do not edit directly > 

#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l5DR8, localrc) 
 deallocate(l5DR8%ptr5DR8, stat=localrc) 
 nullify(l5DR8%ptr5DR8) 
! < End macro - do not edit directly > 

       case (6)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l6DR8, localrc) 
 deallocate(l6DR8%ptr6DR8, stat=localrc) 
 nullify(l6DR8%ptr6DR8) 
! < End macro - do not edit directly > 

              case (7)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, l7DR8, localrc) 
 deallocate(l7DR8%ptr7DR8, stat=localrc) 
 nullify(l7DR8%ptr7DR8) 
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
     if (ESMF_LogMsgFoundAllocError(localrc, "LocalArray Deallocation", &
                                    ESMF_CONTEXT, rc)) return
     if (present(rc)) rc = ESMF_SUCCESS
     end subroutine ESMF_LocalArrayF90Deallocate
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is for higher level LocalArray funcs
!
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArraySlice"
!BOPI
! !IROUTINE: ESMF_LocalArraySlice - extract a subset from a LocalArray
!
! !INTERFACE:
      function ESMF_LocalArraySlice(array, slicedim, sliceloc, rc)
!
! !RETURN VALUE:
      type(ESMF_LocalArray) :: ESMF_LocalArraySlice
!
! !ARGUMENTS:
      type(ESMF_LocalArray), intent(in) :: array
      integer, intent(in) :: slicedim
      integer, intent(in) :: sliceloc
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Extract an (N-1)D array from an N-D array. The dimension to be
! dropped is the {\tt slicedim} argument, and the location along
! the dropped dimension is the {\tt sliceloc} argument. This routine
! allocates new space and copies the data, leaving the original array
! unchanged.
!
!EOPI
        ! BOPI/EOPI because the actual function is not working yet
        ! Local vars
        type (ESMF_LocalArray) :: newarray ! new C++ LocalArray
        integer :: localrc ! local return code
        logical :: rcpresent ! did user specify rc?
        integer :: rank
        type (ESMF_TypeKind) :: kind
        integer :: i, counts(ESMF_MAXDIM), lb(ESMF_MAXDIM), ub(ESMF_MAXDIM)
        localrc = ESMF_RC_NOT_IMPL
        rcpresent = .FALSE.
        newarray%this = ESMF_NULL_POINTER
        ! Initialize return code; assume routine not implemented
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_RC_NOT_IMPL
        endif
        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, array, rc)
        ! Get info from the existing array
        call c_ESMC_LocalArrayGetRank(array, rank, localrc)
        call c_ESMC_LocalArrayGetTypeKind(array, kind, localrc)
        call c_ESMC_LocalArrayGetLengths(array, rank, counts, localrc)
        ! Basic sanity checks - slice dim is ok, sliced location exists, etc.
        if ((slicedim .lt. 1) .or. (slicedim .gt. rank)) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Bad value for slicedim", &
                                 ESMF_CONTEXT, rc)) return
            ! "ESMF_LocalArraySlice: slicedim value ", slicedim, &
            ! " must be between 1 and ", rank
        endif
        if ((sliceloc .lt. 1) .or. (sliceloc .gt. counts(rank))) then
           if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Bad value for sliceloc", &
                                 ESMF_CONTEXT, rc)) return
            !"ESMF_LocalArraySlice: sliceloc value ", sliceloc, &
            ! " must be between 1 and ", counts(rank)
        endif
        ! This slice will be rank < 1. Remove the counts corresponding
        ! to the sliced dim (save for later error checking).
        ! TODO: add error checks
        do i=sliceloc, rank-1
           counts(i) = counts(i+1)
        enddo
        rank = rank - 1
        call c_ESMC_LocalArrayCreateNoData(newarray, rank, kind, &
                                           ESMF_FROM_FORTRAN, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        call ESMF_LocalArrConstrF90Ptr(newarray, counts, rank, kind, &
                                          lb, ub, localrc)
        ! At this point the new array exists, and has space allocated, but it
        ! does not contain data from the old array. Now we have a T/K/R prob.
        ! put old F90 ptr into wrap
        ! call c_ESMC_LocalArrayGetF90Ptr(array, wrap, localrc)
        ! put new F90 ptr into wrap
        ! call c_ESMC_LocalArrayGetF90Ptr(newarray, wrap, localrc)
        ! there must be something like this we can do here...
        ! newarray = RESHAPE(array(sliceloc, :, :), counts)
        ! Set return values
        ESMF_LocalArraySlice = newarray
        if (rcpresent) rc = localrc
        end function ESMF_LocalArraySlice
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for LocalArrays
!
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayWriteRestart"
!BOPI
! !IROUTINE: ESMF_LocalArrayWriteRestart - checkpoint a LocalArray
!
! !INTERFACE:
      subroutine ESMF_LocalArrayWriteRestart(array, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_LocalArray), intent(in) :: array
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to save all data to disk as quickly as possible.
! (see Read/Write for other options). Internally this routine uses the
! same I/O interface as Read/Write, but the default options are to
! select the fastest way to save data to disk.
!
!EOPI
!
! TODO: code goes here
!
        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_LocalArrayGetInit, array, rc)
        !todo: init check iospec
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        end subroutine ESMF_LocalArrayWriteRestart
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayReadRestart"
!BOPI
! !IROUTINE: ESMF_LocalArrayReadRestart - restore a restart file
!
! !INTERFACE:
      function ESMF_LocalArrayReadRestart(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_LocalArray) :: ESMF_LocalArrayReadRestart
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name ! array name to restore
      type(ESMF_IOSpec), intent(in), optional :: iospec ! file specs
      integer, intent(out), optional :: rc ! return code
!
! !DESCRIPTION:
! Used to reinitialize all data associated with a {\tt ESMF\_LocalArray}
! from the last call to WriteRestart.
!
!EOPI
        type (ESMF_LocalArray) :: a
! this is just to shut the compiler up
        a%this = ESMF_NULL_POINTER
!
! TODO: add code here
!
        ESMF_LocalArrayReadRestart = a
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        end function ESMF_LocalArrayReadRestart
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayWrite"
!BOPI
! !IROUTINE: ESMF_LocalArrayWrite - save contents to file
!
! !INTERFACE:
      subroutine ESMF_LocalArrayWrite(array, iospec, filename, rc)
!
! !ARGUMENTS:
      type(ESMF_LocalArray), intent(inout) :: array
      type(ESMF_IOSpec), intent(in), optional :: iospec
      character(len=*), intent(in), optional :: filename
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to write data to persistent storage in a variety of formats.
! (see WriteRestart/ReadRestart for quick data dumps.) Details of I/O
! options specified in the IOSpec derived type.
!
!
!EOPI
       character (len=16) :: defaultopts ! default write options
       character (len=16) :: defaultfile ! default filename
       integer :: localrc ! local return code
       logical :: rcpresent
       ! Initialize return code; assume routine not implemented
       localrc = ESMF_RC_NOT_IMPL
       rcpresent = .FALSE.
       if (present(rc)) then
         rcpresent = .TRUE.
         rc = ESMF_RC_NOT_IMPL
       endif
        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, array, rc)
        !todo: init check iospec
       defaultopts = "singlefile"
       defaultfile = "datafile"
       if(present(filename)) then
           call c_ESMC_LocalArrayWrite(array, defaultopts, trim(filename), localrc)
       else
           call c_ESMC_LocalArrayWrite(array, defaultopts, trim(defaultfile), localrc)
       endif
      if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
       ! set return values
       if (rcpresent) rc = ESMF_SUCCESS
        end subroutine ESMF_LocalArrayWrite
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayRead"
!BOPI
! !IROUTINE: ESMF_LocalArrayRead - read data contents
!
! !INTERFACE:
      function ESMF_LocalArrayRead(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_LocalArray) :: ESMF_LocalArrayRead
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name ! array name to read
      type(ESMF_IOSpec), intent(in), optional :: iospec ! file specs
      integer, intent(out), optional :: rc ! return code
!
! !DESCRIPTION:
! Used to read data from persistent storage in a variety of formats.
!
!
!EOPI
        type (ESMF_LocalArray) :: a
! this is just to shut the compiler up
        a%this = ESMF_NULL_POINTER
!
! TODO: add code here
!
        ESMF_LocalArrayRead = a
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        end function ESMF_LocalArrayRead
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayValidate"
!BOPI
! !IROUTINE: ESMF_LocalArrayValidate - Check validity of LocalArray object
!
! !INTERFACE:
  subroutine ESMF_LocalArrayValidate(array, options, rc)
!
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(in) :: array
    character(len = *), intent(in), optional :: options
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Validate a {\tt ESMF\_LocalArray} object.
!
!EOPI
    character (len=6) :: defaultopts ! default print options
    integer :: localrc ! local return code
    logical :: rcpresent
    ! Initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.
      rc = ESMF_RC_NOT_IMPL
    endif
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, array, rc)
    defaultopts = "brief"
    if(present(options)) then
      !call c_ESMC_LocalArrayValidate(array, options, localrc)
    else
      !call c_ESMC_LocalArrayValidate(array, defaultopts, localrc)
    endif
    ! Return successfully
    if (rcpresent) rc = ESMF_SUCCESS
  end subroutine ESMF_LocalArrayValidate
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayPrint"
!BOPI
! !IROUTINE: ESMF_LocalArrayPrint - Print contents of an LocalArray object
!
! !INTERFACE:
  subroutine ESMF_LocalArrayPrint(array, options, rc)
!
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(in) :: array
    character(len = *), intent(in), optional :: options
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Print information about a {\tt ESMF\_LocalArray}.
!
!EOPI
    character (len=6) :: defaultopts ! default print options
    integer :: localrc ! local return code
    logical :: rcpresent
    !character(len=ESMF_MAXSTR) :: msgbuf
    ! Initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.
      rc = ESMF_RC_NOT_IMPL
    endif
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, array, rc)
    if (array%this .eq. ESMF_NULL_POINTER) then
      !write(msgbuf,*) "LocalArray Print:"
      !call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
      write(*,*) "LocalArray Print:"
      !write(msgbuf,*) " Empty or Uninitialized LocalArray"
      !call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
      write(*,*) " Empty or Uninitialized LocalArray"
      if (present(rc)) rc = ESMF_SUCCESS
      return
    endif
    defaultopts = "brief"
    if(present(options)) then
        call c_ESMC_LocalArrayPrint(array, options, localrc)
    else
        call c_ESMC_LocalArrayPrint(array, defaultopts, localrc)
    endif
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return
    ! Return successfully
    if (rcpresent) rc = ESMF_SUCCESS
  end subroutine ESMF_LocalArrayPrint
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayAdjust"
!BOPI
! !IROUTINE: ESMF_LocalArrayAdjust - Adjust bounds of F90 array member
!
! !INTERFACE:
      subroutine ESMF_LocalArrayAdjust(array, counts, rank, kind, &
        lbounds, ubounds, rc)
!
! !ARGUMENTS:
      type(ESMF_LocalArray), intent(inout) :: array
      integer, dimension(:), intent(in) :: counts
      integer, intent(in) :: rank
      type(ESMF_TypeKind), intent(in) :: kind
      integer, dimension(:), intent(in) :: lbounds
      integer, dimension(:), intent(in) :: ubounds
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Adjust bounds of F90 array member in {\tt ESMF\_LocalArray} object.
!
!EOPI
        ! Local vars
        integer :: localrc ! local return code
        integer :: localkind
        localrc = ESMF_RC_NOT_IMPL
        ! Cannot check init status of array argument here because
        ! the array object is only partially created at this point
        localkind = kind%dkind
        ! Call a T/K/R specific interface
        select case (localkind)
#ifndef ESMF_NO_INTEGER_1_BYTE
          case (ESMF_TYPEKIND_I1%dkind)
            select case (rank)
       case (1)
                call ESMF_LocalArrayAdjust1DI1(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (2)
                call ESMF_LocalArrayAdjust2DI1(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (3)
                call ESMF_LocalArrayAdjust3DI1(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (4)
                call ESMF_LocalArrayAdjust4DI1(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrayAdjust5DI1(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (6)
                call ESMF_LocalArrayAdjust6DI1(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
              case (7)
                call ESMF_LocalArrayAdjust7DI1(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
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
                call ESMF_LocalArrayAdjust1DI2(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (2)
                call ESMF_LocalArrayAdjust2DI2(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (3)
                call ESMF_LocalArrayAdjust3DI2(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (4)
                call ESMF_LocalArrayAdjust4DI2(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrayAdjust5DI2(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (6)
                call ESMF_LocalArrayAdjust6DI2(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
              case (7)
                call ESMF_LocalArrayAdjust7DI2(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
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
                call ESMF_LocalArrayAdjust1DI4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (2)
                call ESMF_LocalArrayAdjust2DI4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (3)
                call ESMF_LocalArrayAdjust3DI4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (4)
                call ESMF_LocalArrayAdjust4DI4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrayAdjust5DI4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (6)
                call ESMF_LocalArrayAdjust6DI4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
              case (7)
                call ESMF_LocalArrayAdjust7DI4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
          case (ESMF_TYPEKIND_I8%dkind)
     select case(rank)
       case (1)
                call ESMF_LocalArrayAdjust1DI8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (2)
                call ESMF_LocalArrayAdjust2DI8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (3)
                call ESMF_LocalArrayAdjust3DI8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (4)
                call ESMF_LocalArrayAdjust4DI8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrayAdjust5DI8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (6)
                call ESMF_LocalArrayAdjust6DI8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
              case (7)
                call ESMF_LocalArrayAdjust7DI8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
          case (ESMF_TYPEKIND_R4%dkind)
     select case(rank)
       case (1)
                call ESMF_LocalArrayAdjust1DR4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (2)
                call ESMF_LocalArrayAdjust2DR4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (3)
                call ESMF_LocalArrayAdjust3DR4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (4)
                call ESMF_LocalArrayAdjust4DR4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrayAdjust5DR4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (6)
                call ESMF_LocalArrayAdjust6DR4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
              case (7)
                call ESMF_LocalArrayAdjust7DR4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
#endif
              case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", &
                                 ESMF_CONTEXT, rc)) return
            end select
          case (ESMF_TYPEKIND_R8%dkind)
     select case(rank)
       case (1)
                call ESMF_LocalArrayAdjust1DR8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (2)
                call ESMF_LocalArrayAdjust2DR8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (3)
                call ESMF_LocalArrayAdjust3DR8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (4)
                call ESMF_LocalArrayAdjust4DR8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
#ifndef ESMF_NO_GREATER_THAN_4D
       case (5)
                call ESMF_LocalArrayAdjust5DR8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
       case (6)
                call ESMF_LocalArrayAdjust6DR8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
              case (7)
                call ESMF_LocalArrayAdjust7DR8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=localrc)
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
       ! check localrc for errors
       if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
       ! return successfully
       if (present(rc)) rc = ESMF_SUCCESS
       end subroutine ESMF_LocalArrayAdjust
! -------------------------- ESMF-internal method -----------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayGetInit"
!BOPI
! !IROUTINE: ESMF_LocalArrayGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_LocalArrayGetInit(array)
!
! !RETURN VALUE:
    ESMF_INIT_TYPE :: ESMF_LocalArrayGetInit
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(in), optional :: array
!
! !DESCRIPTION:
! Access deep object init code.
!
! The arguments are:
! \begin{description}
! \item [array]
! LocalArray object.
! \end{description}
!
!EOPI
    if (present(array)) then
      ESMF_LocalArrayGetInit = ESMF_INIT_GET(array)
    else
      ESMF_LocalArrayGetInit = ESMF_INIT_CREATED
    endif
  end function ESMF_LocalArrayGetInit
!------------------------------------------------------------------------------
! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_LocalArraySetInitCreated - Set LocalArray init code to "CREATED"
! !INTERFACE:
  subroutine ESMF_LocalArraySetInitCreated(array, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(inout) :: array
    integer, intent(out), optional :: rc
!
!
! !DESCRIPTION:
! Set init code in LocalArray object to "CREATED".
!
! The arguments are:
! \begin{description}
! \item[array]
! Specified {\tt ESMF\_LocalArray} object.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc ! local return code
    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    ! Set init code
    ESMF_INIT_SET_CREATED(array)
    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_LocalArraySetInitCreated
!------------------------------------------------------------------------------
! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_LocalArrayGetThis - Internal access routine for C++ pointer
! !INTERFACE:
  subroutine ESMF_LocalArrayGetThis(array, this, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(in), optional :: array
    type(ESMF_Pointer), intent(out) :: this
    integer, intent(out),optional :: rc
!
!
! !DESCRIPTION:
! Internal access routine for C++ pointer.
!
! The arguments are:
! \begin{description}
! \item[array]
! Specified {\tt ESMF\_LocalArray} object.
! \item[this]
! C++ pointer.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc ! local return code
    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    ! Copy C++ pointer
    this = array%this
    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_LocalArrayGetThis
!------------------------------------------------------------------------------
! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_LocalArraySetThis - Set C++ pointer in LocalArray
! !INTERFACE:
  subroutine ESMF_LocalArraySetThis(localarray, this, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(inout) :: localarray
    type(ESMF_Pointer), intent(in) :: this
    integer, intent(out), optional :: rc
!
!
! !DESCRIPTION:
! Set C++ pointer in LocalArray.
!
! The arguments are:
! \begin{description}
! \item[localarray]
! Specified {\tt ESMF\_LocalArray} object.
! \item[this]
! C++ pointer.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc ! local return code
    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    ! Copy C++ pointer
    localarray%this = this
    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_LocalArraySetThis
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
end module ESMF_LocalArrayCreateMod
