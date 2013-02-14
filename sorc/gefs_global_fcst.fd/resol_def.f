      module resol_def
      use machine
      implicit none
      save
      integer   jcap,jcap1,jcap2,latg,latg2,latr,latr2
      integer   levh,levm1,levp1,levs,lnt,lnt2,lnt22,levr
      integer   lnte,lnted,lnto,lntod,lnuv
      integer   lonf,lonfx,lonr,lonrx
      integer   ntrac
      integer   nxpt,nypt,jintmx,latgd
      integer   ntoz,ntcw
      integer   lsoil,nmtvr,ncld,num_p3d,num_p2d,nrcm
      integer   ngrids_sfcc, ngrids_flx, nfxr, ngrids_gg
      integer   ngrids_nsst,nr_nsst,nf_nsst
      integer   ivsupa, ivssfc, ivssfc_restart, ivsinp
      integer   ivsnsst
      integer   nlunit
      integer   thermodyn_id, sfcpress_id			! hmhj
      integer   idvt
!
      INTEGER   P_GZ,P_ZEM,P_DIM,P_TEM,P_RM,P_QM
      INTEGER   P_ZE,P_DI,P_TE,P_RQ,P_Q,P_DLAM,P_DPHI,P_ULN,P_VLN
      INTEGER   P_W,P_X,P_Y,P_RT,P_ZQ
      INTEGER   LOTS,LOTD,LOTA

      integer kwq,kwte,kwdz,kwrq

!     For Ensemble concurrency run. Weiyu
!     INTEGER :: Ensemble_Id, Total_member

      end module resol_def
!
      module ozne_def
      use machine , only : kind_phys
      implicit none
      save
      integer, parameter :: kozpl=28, kozc=48
      integer latsozp, levozp, timeoz, latsozc, levozc, timeozc
     &,       PL_Coeff
      real (kind=kind_phys) blatc, dphiozc
      real (kind=kind_phys), allocatable :: PL_LAT(:), PL_Pres(:)
     &,                                     PL_TIME(:)
      end module ozne_def
