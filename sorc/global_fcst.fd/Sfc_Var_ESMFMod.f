!
! !MODULE: Sfc_Flx_ESMFMod  ---              Definition of the surface
!                                            fields in the ESMF internal state.
!
! !DESCRIPTION: Sfc_Flx_ESMFMod ---            Define the surfacee  variables
!                                              in the ESMF internal state.
!---------------------------------------------------------------------------
! !REVISION HISTORY:
!
!  March 2007      Shrinivas Moorthi Initial code.
!  March 2008      Y.-T. Hou         add Sunshine_Duration (suntim) to Flx_Var_Data
!  Jan 2009        Moorthi           add Ho Chun's changes
!
! !INTERFACE:
!
 MODULE Sfc_Flx_ESMFMod

 use machine , only : kind_phys

 IMPLICIT none

 TYPE Sfc_Var_Data
    real(kind=kind_phys),pointer:: tsea(:,:)=>null()
    real(kind=kind_phys),pointer:: smc(:,:,:)=>null()
    real(kind=kind_phys),pointer:: sheleg(:,:)=>null()
    real(kind=kind_phys),pointer:: sncovr(:,:)=>null()
    real(kind=kind_phys),pointer:: stc(:,:,:)=>null()
    real(kind=kind_phys),pointer:: tg3(:,:)=>null()
    real(kind=kind_phys),pointer:: zorl(:,:)=>null()
    real(kind=kind_phys),pointer:: cv(:,:)=>null()
    real(kind=kind_phys),pointer:: cvb(:,:)=>null()
    real(kind=kind_phys),pointer:: cvt(:,:)=>null()
    real(kind=kind_phys),pointer:: alvsf(:,:)=>null()
    real(kind=kind_phys),pointer:: alvwf(:,:)=>null()
    real(kind=kind_phys),pointer:: alnsf(:,:)=>null()
    real(kind=kind_phys),pointer:: alnwf(:,:)=>null()
    real(kind=kind_phys),pointer:: slmsk(:,:)=>null()
    real(kind=kind_phys),pointer:: vfrac(:,:)=>null()
    real(kind=kind_phys),pointer:: canopy(:,:)=>null()
    real(kind=kind_phys),pointer:: f10m(:,:)=>null()
    real(kind=kind_phys),pointer:: t2m(:,:)=>null()
    real(kind=kind_phys),pointer:: q2m(:,:)=>null()
    real(kind=kind_phys),pointer:: vtype(:,:)=>null()
    real(kind=kind_phys),pointer:: stype(:,:)=>null()
    real(kind=kind_phys),pointer:: facsf(:,:)=>null()
    real(kind=kind_phys),pointer:: facwf(:,:)=>null()
    real(kind=kind_phys),pointer:: uustar(:,:)=>null()
    real(kind=kind_phys),pointer:: ffmm(:,:)=>null()
    real(kind=kind_phys),pointer:: ffhh(:,:)=>null()
    real(kind=kind_phys),pointer:: hice(:,:)=>null()
    real(kind=kind_phys),pointer:: fice(:,:)=>null()
    real(kind=kind_phys),pointer:: tisfc(:,:)=>null()
    real(kind=kind_phys),pointer:: tprcp(:,:)=>null()
    real(kind=kind_phys),pointer:: srflag(:,:)=>null()
    real(kind=kind_phys),pointer:: snwdph(:,:)=>null()
    real(kind=kind_phys),pointer:: slc(:,:,:)=>null()
    real(kind=kind_phys),pointer:: shdmin(:,:)=>null()
    real(kind=kind_phys),pointer:: shdmax(:,:)=>null()
    real(kind=kind_phys),pointer:: slope(:,:)=>null()
    real(kind=kind_phys),pointer:: snoalb(:,:)=>null()
    real(kind=kind_phys),pointer:: oro(:,:)=>null()
 end type Sfc_Var_Data
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 TYPE Flx_Var_Data
    real(kind=kind_phys),pointer:: SFCDSW(:,:)=>null()
    real(kind=kind_phys),pointer:: COSZEN(:,:)=>null()
    real(kind=kind_phys),pointer:: PWAT(:,:)=>null()
    real(kind=kind_phys),pointer:: TMPMIN(:,:)=>null()
    real(kind=kind_phys),pointer:: TMPMAX(:,:)=>null()
!jwang add spfhmax/spfhmin
    real(kind=kind_phys),pointer:: SPFHMIN(:,:)=>null()
    real(kind=kind_phys),pointer:: SPFHMAX(:,:)=>null()
    real(kind=kind_phys),pointer:: DUSFC(:,:)=>null()
    real(kind=kind_phys),pointer:: DVSFC(:,:)=>null()
    real(kind=kind_phys),pointer:: DTSFC(:,:)=>null()
    real(kind=kind_phys),pointer:: DQSFC(:,:)=>null()
    real(kind=kind_phys),pointer:: DLWSFC(:,:)=>null()
    real(kind=kind_phys),pointer:: ULWSFC(:,:)=>null()
    real(kind=kind_phys),pointer:: GFLUX(:,:)=>null()
    real(kind=kind_phys),pointer:: RUNOFF(:,:)=>null()
    real(kind=kind_phys),pointer:: EP(:,:)=>null()
    real(kind=kind_phys),pointer:: CLDWRK(:,:)=>null()
    real(kind=kind_phys),pointer:: DUGWD(:,:)=>null()
    real(kind=kind_phys),pointer:: DVGWD(:,:)=>null()
    real(kind=kind_phys),pointer:: PSMEAN(:,:)=>null()
    real(kind=kind_phys),pointer:: GESHEM(:,:)=>null()
    real(kind=kind_phys),pointer:: BENGSH(:,:)=>null()
    real(kind=kind_phys),pointer:: SFCNSW(:,:)=>null()
    real(kind=kind_phys),pointer:: SFCDLW(:,:)=>null()
    real(kind=kind_phys),pointer:: TSFLW(:,:)=>null()
    real(kind=kind_phys),pointer:: PSURF(:,:)=>null()
    real(kind=kind_phys),pointer:: U10M(:,:)=>null()
    real(kind=kind_phys),pointer:: V10M(:,:)=>null()
    real(kind=kind_phys),pointer:: HPBL(:,:)=>null()
    real(kind=kind_phys),pointer:: CHH(:,:)=>null()
    real(kind=kind_phys),pointer:: CMM(:,:)=>null()
    real(kind=kind_phys),pointer:: EPI(:,:)=>null()
    real(kind=kind_phys),pointer:: DLWSFCI(:,:)=>null()
    real(kind=kind_phys),pointer:: ULWSFCI(:,:)=>null()
    real(kind=kind_phys),pointer:: USWSFCI(:,:)=>null()
    real(kind=kind_phys),pointer:: DSWSFCI(:,:)=>null()
    real(kind=kind_phys),pointer:: DTSFCI(:,:)=>null()
    real(kind=kind_phys),pointer:: DQSFCI(:,:)=>null()
    real(kind=kind_phys),pointer:: GFLUXI(:,:)=>null()
    real(kind=kind_phys),pointer:: SRUNOFF(:,:)=>null()
    real(kind=kind_phys),pointer:: T1(:,:)=>null()
    real(kind=kind_phys),pointer:: Q1(:,:)=>null()
    real(kind=kind_phys),pointer:: U1(:,:)=>null()
    real(kind=kind_phys),pointer:: V1(:,:)=>null()
    real(kind=kind_phys),pointer:: ZLVL(:,:)=>null()
    real(kind=kind_phys),pointer:: EVBSA(:,:)=>null()
    real(kind=kind_phys),pointer:: EVCWA(:,:)=>null()
    real(kind=kind_phys),pointer:: TRANSA(:,:)=>null()
    real(kind=kind_phys),pointer:: SBSNOA(:,:)=>null()
    real(kind=kind_phys),pointer:: SNOWCA(:,:)=>null()
    real(kind=kind_phys),pointer:: SOILM(:,:)=>null()
    real(kind=kind_phys),pointer:: SNOHFA(:,:)=>null()
    real(kind=kind_phys),pointer:: SMCWLT2(:,:)=>null()
    real(kind=kind_phys),pointer:: SMCREF2(:,:)=>null()
    real(kind=kind_phys),pointer:: suntim(:,:)=>null()       ! sunshine duration time
!hchuang code change [+8L] xx/xx/2007 : add 2D
    real(kind=kind_phys),pointer:: gsoil(:,:)=>null()
    real(kind=kind_phys),pointer:: gtmp2m(:,:)=>null()
    real(kind=kind_phys),pointer:: gustar(:,:)=>null()
    real(kind=kind_phys),pointer:: gpblh(:,:)=>null()
    real(kind=kind_phys),pointer:: gu10m(:,:)=>null()
    real(kind=kind_phys),pointer:: gv10m(:,:)=>null()
    real(kind=kind_phys),pointer:: gzorl(:,:)=>null()
    real(kind=kind_phys),pointer:: goro(:,:)=>null()
 end type Flx_Var_Data
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    contains
    subroutine sfcvar_aldata(dim1,dim2,dim3,data,iret)
       implicit none
       integer, intent(in)              :: dim1, dim2, dim3
       type(sfc_var_data),intent(inout) :: data
       integer, intent(out)             :: iret
!
allocate(                          &
      data%tsea   (dim1,dim2),     &
      data%smc    (dim3,dim1,dim2),&
      data%sheleg (dim1,dim2),     &
      data%sncovr (dim1,dim2),     &
      data%stc    (dim3,dim1,dim2),&
      data%tg3    (dim1,dim2),     &
      data%zorl   (dim1,dim2),     &
      data%cv     (dim1,dim2),     &
      data%cvb    (dim1,dim2),     &
      data%cvt    (dim1,dim2),     &
      data%alvsf  (dim1,dim2),     &
      data%alvwf  (dim1,dim2),     &
      data%alnsf  (dim1,dim2),     &
      data%alnwf  (dim1,dim2),     &
      data%slmsk  (dim1,dim2),     &
      data%vfrac  (dim1,dim2),     &
      data%canopy (dim1,dim2),     &
      data%f10m   (dim1,dim2),     &
      data%t2m    (dim1,dim2),     &
      data%q2m    (dim1,dim2),     &
      data%vtype  (dim1,dim2),     &
      data%stype  (dim1,dim2),     &
      data%facsf  (dim1,dim2),     &
      data%facwf  (dim1,dim2),     &
      data%uustar (dim1,dim2),     &
      data%ffmm   (dim1,dim2),     &
      data%ffhh   (dim1,dim2),     &
      data%hice   (dim1,dim2),     &
      data%fice   (dim1,dim2),     &
      data%tisfc  (dim1,dim2),     &
      data%tprcp  (dim1,dim2),     &
      data%srflag (dim1,dim2),     &
      data%snwdph (dim1,dim2),     &
      data%slc    (dim3,dim1,dim2),&
      data%shdmin (dim1,dim2),     &
      data%shdmax (dim1,dim2),     &
      data%slope  (dim1,dim2),     &
      data%snoalb (dim1,dim2),     &
      data%oro    (dim1,dim2),     &
      stat=iret)
    if(iret.ne.0) iret=-3
    return
  end subroutine
    subroutine flxvar_aldata(dim1,dim2,data,iret)
       implicit none
       integer, intent(in)              :: dim1, dim2
       type(flx_var_data),intent(inout) :: data
       integer, intent(out)             :: iret
!
!   allocate(data%SFCDSW  (dim1,dim2))
    allocate(                  &
     data%SFCDSW  (dim1,dim2), &
     data%COSZEN  (dim1,dim2), &
     data%PWAT    (dim1,dim2), &
     data%TMPMIN  (dim1,dim2), &
     data%TMPMAX  (dim1,dim2), &
!jwang add spfhmax/spfhmin
     data%SPFHMIN (dim1,dim2), &
     data%SPFHMAX (dim1,dim2), &
     data%DUSFC   (dim1,dim2), &
     data%DVSFC   (dim1,dim2), &
     data%DTSFC   (dim1,dim2), &
     data%DQSFC   (dim1,dim2), &
     data%DLWSFC  (dim1,dim2), &
     data%ULWSFC  (dim1,dim2), &
     data%GFLUX   (dim1,dim2), &
     data%RUNOFF  (dim1,dim2), &
     data%EP      (dim1,dim2), &
     data%CLDWRK  (dim1,dim2), &
     data%DUGWD   (dim1,dim2), &
     data%DVGWD   (dim1,dim2), &
     data%PSMEAN  (dim1,dim2), &
     data%GESHEM  (dim1,dim2), &
     data%BENGSH  (dim1,dim2), &
     data%SFCNSW  (dim1,dim2), &
     data%SFCDLW  (dim1,dim2), &
     data%TSFLW   (dim1,dim2), &
     data%PSURF   (dim1,dim2), &
     data%U10M    (dim1,dim2), &
     data%V10M    (dim1,dim2), &
     data%HPBL    (dim1,dim2), &
     data%CHH     (dim1,dim2), &
     data%CMM     (dim1,dim2), &
     data%EPI     (dim1,dim2), &
     data%DLWSFCI (dim1,dim2), &
     data%ULWSFCI (dim1,dim2), &
     data%USWSFCI (dim1,dim2), &
     data%DSWSFCI (dim1,dim2), &
     data%DTSFCI  (dim1,dim2), &
     data%DQSFCI  (dim1,dim2), &
     data%GFLUXI  (dim1,dim2), &
     data%SRUNOFF (dim1,dim2), &
     data%T1      (dim1,dim2), &
     data%Q1      (dim1,dim2), &
     data%U1      (dim1,dim2), &
     data%V1      (dim1,dim2), &
     data%ZLVL    (dim1,dim2), &
     data%EVBSA   (dim1,dim2), &
     data%EVCWA   (dim1,dim2), &
     data%TRANSA  (dim1,dim2), &
     data%SBSNOA  (dim1,dim2), &
     data%SNOWCA  (dim1,dim2), &
     data%SOILM   (dim1,dim2), &
     data%SNOHFA  (dim1,dim2), &
     data%SMCWLT2 (dim1,dim2), &
     data%SMCREF2 (dim1,dim2), &
     data%suntim  (dim1,dim2), &                !yth mar/08
!hchuang
     data%gsoil   (dim1,dim2), &
     data%gtmp2m  (dim1,dim2), &
     data%gustar  (dim1,dim2), &
     data%gpblh   (dim1,dim2), &
     data%gu10m   (dim1,dim2), &
     data%gv10m   (dim1,dim2), &
     data%gzorl   (dim1,dim2), &
     data%goro    (dim1,dim2), &
     stat=iret)

    if(iret.ne.0) iret=-4
    return
  end subroutine
    subroutine flx_init(data,iret)
       implicit none
       type(flx_var_data),intent(inout) :: data
       integer, intent(out)             :: iret
!
       data%TMPMIN  = 1.e10
       data%TMPMAX  = 0.
!jwang add spfhmax/spfhmin
       data%SPFHMIN = 1.e10
       data%SPFHMAX = 0.
       data%GESHEM  = 0.
       data%BENGSH  = 0.
       data%DUSFC   = 0.
       data%DVSFC   = 0.
       data%DTSFC   = 0.
       data%DQSFC   = 0.
       data%DLWSFC  = 0.
       data%ULWSFC  = 0.
!yth  add suntim   mar/08
       data%suntim  = 0.
       data%GFLUX   = 0.
!
       data%RUNOFF  = 0.
       data%EP      = 0.
       data%CLDWRK  = 0.
       data%DUGWD   = 0.
       data%DVGWD   = 0.
       data%PSMEAN  = 0.
!
       data%EVBSA   = 0.
       data%EVCWA   = 0.
       data%TRANSA  = 0.
       data%SBSNOA  = 0.
       data%SNOWCA  = 0.
       data%SRUNOFF = 0.
       data%SNOHFA  = 0.
!hchuang
       data%gsoil   = 0.
       data%gtmp2m  = 0.
       data%gustar  = 0.
       data%gpblh   = 0.
       data%gu10m   = 0.
       data%gv10m   = 0.
       data%gzorl   = 0.
       data%goro    = 0.
     return
  end subroutine
 END MODULE Sfc_Flx_ESMFMod
