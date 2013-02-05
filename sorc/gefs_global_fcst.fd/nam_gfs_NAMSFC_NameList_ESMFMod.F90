!
! !MODULE: nam_gfs_NAMSFC_NameList_ESMFMod  --- Definition of the name list
!                                               in the ESMF internal state.
!
! !DESCRIPTION: nam_gfs_NAMSFC_NameList_ESMFMod --- Define the name list variables
!                                                   in the ESMF internal state.
!---------------------------------------------------------------------------
! !REVISION HISTORY:
!
!  November 2004      Weiyu Yang Initial code.
!
! !INTERFACE:
!
 MODULE nam_gfs_NAMSFC_NameList_ESMFMod

 USE MACHINE,      ONLY : kind_evod, kind_io8, kind_io4

 IMPLICIT none

 TYPE nam_gfs_NameList
      integer              :: nszer, nsres, nslwr, nsout, nsswr
     &,                       nsdfi, nscyc, igen,  jo3,   ngptc
     &,                       lsm,   ntrac, nxpt,  nypt,  jintmx
     &,                       jcap,  levs,  lonf,  lonr,  latg
     &,                       latr,  ntoz,  ntcw,  ncld,  lsoil, nmtvr
!
      real(kind=kind_evod) :: fhswr, fhlwr, fhrot, fhseg, fhmax, fhout
     &,                       fhres, fhzer, fhini, fhdfi, fhcyc, deltim
!
      logical              :: lggfs3d, ldiag3d, ras,   zhao_mic,  hybrid
     &,                       adiab,   pre_rad, random_xkt2, liope
      logical              :: lsfwd,   lssav, lscca,  lsswr, lslwr
      logical              :: shuff_lats_a,   shuff_lats_r
     &,                       reshuff_lats_a, reshuff_lats_r
!
 END TYPE nam_gfs_NameList

 TYPE SOIL_VEG_NameList
      logical              :: LPARAM
 END TYPE SOIL_VEG_NameList

 TYPE NAMSFC_NameList
      CHARACTER(80)          :: FNGLAC
      CHARACTER(80)          :: FNMXIC
      CHARACTER(80)          :: FNTSFC
      CHARACTER(80)          :: FNSNOC
      CHARACTER(80)          :: FNZORC
      CHARACTER(80)          :: FNALBC
      CHARACTER(80)          :: FNAISC
      CHARACTER(80)          :: FNTG3C
      CHARACTER(80)          :: FNVEGC
      CHARACTER(80)          :: FNVETC
      CHARACTER(80)          :: FNSOTC
      CHARACTER(80)          :: FNSMCC
      CHARACTER(80)          :: FNMSKH
      CHARACTER(80)          :: FNTSFA
      CHARACTER(80)          :: FNACNA
      CHARACTER(80)          :: FNSNOA
      CHARACTER(80)          :: FNVMNC
      CHARACTER(80)          :: FNVMXC
      CHARACTER(80)          :: FNSLPC
      CHARACTER(80)          :: FNABSC

      INTEGER                :: LDEBUG
      REAL(KIND=KIND_IO8)    :: FSMCL(25)
      REAL(KIND=KIND_IO8)    :: FTSFS
      REAL(KIND=KIND_IO8)    :: FAISS
      REAL(KIND=KIND_IO8)    :: FSNOL
      REAL(KIND=KIND_IO8)    :: FTSFL
      REAL(KIND=KIND_IO8)    :: FAISL
      REAL(KIND=KIND_IO8)    :: FSNOS
      REAL(KIND=KIND_IO8)    :: FSICL
      REAL(KIND=KIND_IO8)    :: FSICS
 END TYPE NAMSFC_NameList

 TYPE ESMF_State_Namelist
! For the sigma file.
!--------------------
      INTEGER                :: idate1_import
      INTEGER                :: z_import
      INTEGER                :: ps_import
      INTEGER                :: vor_import
      INTEGER                :: div_import
      INTEGER                :: temp_import
      INTEGER                :: q_import
      INTEGER                :: oz_import
      INTEGER                :: scld_import

      INTEGER                :: idate1_export
      INTEGER                :: z_export
      INTEGER                :: ps_export
      INTEGER                :: vor_export
      INTEGER                :: div_export
      INTEGER                :: temp_export
      INTEGER                :: q_export
      INTEGER                :: oz_export
      INTEGER                :: scld_export
!For the surface file.
!---------------------
      INTEGER                :: orography_import
      INTEGER                :: t_skin_import
      INTEGER                :: soil_mois_import
      INTEGER                :: snow_depth_import
      INTEGER                :: soil_t_import
      INTEGER                :: deep_soil_t_import
      INTEGER                :: roughness_import
      INTEGER                :: conv_cloud_cover_import
      INTEGER                :: conv_cloud_base_import
      INTEGER                :: conv_cloud_top_import
      INTEGER                :: albedo_visible_scattered_import
      INTEGER                :: albedo_visible_beam_import
      INTEGER                :: albedo_nearIR_scattered_import
      INTEGER                :: albedo_nearIR_beam_import
      INTEGER                :: sea_level_ice_mask_import
      INTEGER                :: vegetation_cover_import
      INTEGER                :: canopy_water_import
      INTEGER                :: m10_wind_fraction_import
      INTEGER                :: vegetation_type_import
      INTEGER                :: soil_type_import
      INTEGER                :: zeneith_angle_facsf_import
      INTEGER                :: zeneith_angle_facwf_import
      INTEGER                :: uustar_import
      INTEGER                :: ffmm_import
      INTEGER                :: ffhh_import
      INTEGER                :: sea_ice_thickness_import
      INTEGER                :: sea_ice_concentration_import
      INTEGER                :: tprcp_import
      INTEGER                :: srflag_import
      INTEGER                :: actual_snow_depth_import
      INTEGER                :: liquid_soil_moisture_import
      INTEGER                :: vegetation_cover_min_import
      INTEGER                :: vegetation_cover_max_import
      INTEGER                :: slope_type_import
      INTEGER                :: snow_albedo_max_import

      INTEGER                :: orography_export
      INTEGER                :: t_skin_export
      INTEGER                :: soil_mois_export
      INTEGER                :: snow_depth_export
      INTEGER                :: soil_t_export
      INTEGER                :: deep_soil_t_export
      INTEGER                :: roughness_export
      INTEGER                :: conv_cloud_cover_export
      INTEGER                :: conv_cloud_base_export
      INTEGER                :: conv_cloud_top_export
      INTEGER                :: albedo_visible_scattered_export
      INTEGER                :: albedo_visible_beam_export
      INTEGER                :: albedo_nearIR_scattered_export
      INTEGER                :: albedo_nearIR_beam_export
      INTEGER                :: sea_level_ice_mask_export
      INTEGER                :: vegetation_cover_export
      INTEGER                :: canopy_water_export
      INTEGER                :: m10_wind_fraction_export
      INTEGER                :: vegetation_type_export
      INTEGER                :: soil_type_export
      INTEGER                :: zeneith_angle_facsf_export
      INTEGER                :: zeneith_angle_facwf_export
      INTEGER                :: uustar_export
      INTEGER                :: ffmm_export
      INTEGER                :: ffhh_export
      INTEGER                :: sea_ice_thickness_export
      INTEGER                :: sea_ice_concentration_export
      INTEGER                :: tprcp_export
      INTEGER                :: srflag_export
      INTEGER                :: actual_snow_depth_export
      INTEGER                :: liquid_soil_moisture_export
      INTEGER                :: vegetation_cover_min_export
      INTEGER                :: vegetation_cover_max_export
      INTEGER                :: slope_type_export
      INTEGER                :: snow_albedo_max_export
 END TYPE ESMF_State_Namelist
 END MODULE nam_gfs_NAMSFC_NameList_ESMFMod
