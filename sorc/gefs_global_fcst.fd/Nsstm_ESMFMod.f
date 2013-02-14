!
! !MODULE: Nsstm_ESMFMod  ---                Definition of the Nsst model
!                                            fields in the ESMF internal state.
!
! !DESCRIPTION: Nsstm_ESMFMod ---            Define the Nsst model  variables
!                                            in the ESMF internal state.
!---------------------------------------------------------------------------
! !REVISION HISTORY:
!
!  May 2008      Shrinivas Moorthi Initial code.
!
! !INTERFACE:
!
 MODULE Nsstm_ESMFMod

 use machine , only : kind_phys

 IMPLICIT none

 TYPE Nsst_Var_Data
!   real(kind=kind_phys),pointer:: ocnr(:,:,:)=>null()
!   real(kind=kind_phys),pointer:: ocnf(:,:,:)=>null()

    real(kind=kind_phys),pointer:: slmsk(:,:)   =>null()
    real(kind=kind_phys),pointer:: ifd(:,:)     =>null()
    real(kind=kind_phys),pointer:: time_old(:,:)=>null()
    real(kind=kind_phys),pointer:: time_ins(:,:)=>null()
    real(kind=kind_phys),pointer:: I_Sw(:,:)    =>null()
    real(kind=kind_phys),pointer:: I_Q(:,:)     =>null()
    real(kind=kind_phys),pointer:: I_Qrain(:,:) =>null()
    real(kind=kind_phys),pointer:: I_M(:,:)     =>null()
    real(kind=kind_phys),pointer:: I_Tau(:,:)   =>null()
    real(kind=kind_phys),pointer:: I_Sw_Zw(:,:) =>null()
    real(kind=kind_phys),pointer:: I_Q_Ts(:,:)  =>null()
    real(kind=kind_phys),pointer:: I_M_Ts(:,:)  =>null()
    real(kind=kind_phys),pointer:: Tref(:,:)    =>null()
    real(kind=kind_phys),pointer:: dt_cool(:,:) =>null()
    real(kind=kind_phys),pointer:: z_c(:,:)     =>null()
    real(kind=kind_phys),pointer:: dt_warm(:,:) =>null()
    real(kind=kind_phys),pointer:: z_w(:,:)     =>null()
    real(kind=kind_phys),pointer:: c_0(:,:)     =>null()
    real(kind=kind_phys),pointer:: c_d(:,:)     =>null()
    real(kind=kind_phys),pointer:: w_0(:,:)     =>null()
    real(kind=kind_phys),pointer:: w_d(:,:)     =>null()
 end type Nsst_Var_Data
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    contains
    subroutine nsstmvar_aldata(dim1,dim2,data,iret)
       implicit none
       integer, intent(in)               :: dim1, dim2
       type(nsst_var_data),intent(inout) :: data
       integer, intent(out)              :: iret
!
allocate(                        &
      data%slmsk    (dim1,dim2), &
      data%ifd      (dim1,dim2), &
      data%time_old (dim1,dim2), &
      data%time_ins (dim1,dim2), &
      data%I_Sw     (dim1,dim2), &
      data%I_Q      (dim1,dim2), &
      data%I_Qrain  (dim1,dim2), &
      data%I_M      (dim1,dim2), &
      data%I_Tau    (dim1,dim2), &
      data%I_Sw_Zw  (dim1,dim2), &
      data%I_Q_Ts   (dim1,dim2), &
      data%I_M_Ts   (dim1,dim2), &
      data%Tref     (dim1,dim2), &
      data%dt_cool  (dim1,dim2), &
      data%z_c      (dim1,dim2), &
      data%dt_warm  (dim1,dim2), &
      data%z_w      (dim1,dim2), &
      data%c_0      (dim1,dim2), &
      data%c_d      (dim1,dim2), &
      data%w_0      (dim1,dim2), &
      data%w_d      (dim1,dim2), &
      stat=iret)
    if(iret.ne.0) iret=-3
    return
  end subroutine
 END MODULE Nsstm_ESMFMod
