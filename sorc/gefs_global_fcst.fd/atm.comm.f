      MODULE constant_cc

      USE MACHINE, ONLY: kind_phys

      USE physcons

      END MODULE constant_cc
!
!***********************************************************************
!
      MODULE ATM_cc

      USE CMP_COMM, ONLY:
     >   MPI_COMM_Atmos => COMM_local,
     >   Coupler_id,
     >   component_master_rank_local,
     >   process_rank_local,
!          Note: the latter two are only to compare with each
!          other and thus determine if the process is the local
!          master (root) process. (Comparison of
!          component_master_rank_global with process_rank_global
!          would not work because the former is known only to
!          Coupler process and the local master process itself.)
     >   component_nprocs,
     >   kind_REAL,MPI_kind_REAL,
     >   MPI_INTEGER,MPI_STATUS_SIZE,
     >   ibuffer
      USE mpi_def, ONLY: COMM_TILES => MC_COMP
      USE layout1, ONLY: TILES_nprocs => nodes_comp

      implicit none

      integer latg,latr,lonf,lonr
      integer latd
      integer lats_node_r,ipt_lats_node_r

      integer N2D

      integer, allocatable:: global_lats_r(:),lonsperlar(:)

      logical COMP /.false./

!controls:
!     integer nunit_announce_cc /6/, VerbLev /5/
!     integer nunit_announce_cc /6/, VerbLev /2/
      integer nunit_announce_cc /6/, VerbLev /1/

      save

      END MODULE ATM_cc
!
!***********************************************************************
!
      MODULE SURFACE_cc

      USE constant_cc, ONLY:
     >           hvap_cc=>con_hvap,         ! - this is L, to use in LE
                                            ! Check: if L in LE must
                                            ! rather be either evap.
                                            ! heat or evap.+melt. heat
     >           JCAL_cc=>con_JCAL,         ! - J in Cal
     >           kind_phys_cc=>kind_phys

      implicit none

      integer, parameter:: 
     >           kind_sfcflux=8,
     >           kind_SST=8,
     >           kind_SLMSK=8,
     >           kind_OROGR=8,
     >           kind_dt_cc=8,              !-->cpl insertion: add model vars precision here <--
     >           kind_modelvar=8

      integer,allocatable:: ISLM_RG(:,:),ISLM_FG(:,:)
      real (kind=kind_sfcflux),allocatable:: 
     >DUSFC_cc(:,:),DVSFC_cc(:,:),
     >DTSFC_cc(:,:),DQSFC_cc(:,:),PRECR_cc(:,:),
     >DLWSFC_cc(:,:),ULWSFC_cc(:,:),SWSFC_cc(:,:),
!-->cpl insertion
     >XMU_cc(:,:),DSW_cc(:,:),DLW_cc(:,:),ffmm_cc(:,:),ffhh_cc(:,:),
     >SNW_cc(:,:),LPREC_cc(:,:),SST_ave(:,:)
!<--cpl insertion

      real (kind=kind_SST),allocatable:: SST_cc(:,:)

      real (kind=kind_dt_cc) dt_cc,dto_cc         !-->cpl insertion: add dto_cc

!--> cpl insertion: add model vars here:
      real (kind=kind_modelvar),allocatable::
     >   T_BOT_cc(:,:),U_BOT_cc(:,:),V_BOT_cc(:,:), Q_BOT_cc(:,:), 
     >   P_BOT_cc(:,:),P_SURF_cc(:,:),Z_BOT_cc(:,:),T_SFC_cc(:,:)
     &,  FICE_SFC_cc(:,:), HICE_SFC_cc(:,:)
!<-- cpl insertion

      logical lssav_cc,lsout_cc,lgetSSTICE_cc,l_df_cc
!--> cpl insertion
      logical lsout_cc_momice,lsout_cc_momocn
      integer i_dto2dta_cc
!<-- cpl insertion
      integer i_dtc2dta_cc
!     parameter (i_dtc2dta_cc=3) ! <- ratio of time steps in OM and AM
      real (kind=kind_dt_cc) dta2dtc_cc,dta2dto_cc

      real(kind=kind_phys_cc) CONVRAD_cc
      PARAMETER (CONVRAD_cc=JCAL_cc*1.E4/60.) ! - see progtmr.f,
                                              ! subr. progtm

      integer n_do_tstep_cc /0/,kdtmax_cc/0/

      character*180 s_cc

      integer ISLM_OS_value,ISLM_SI_value,ISLM_L_value
      parameter (ISLM_OS_value=0,
                  !<- must be integer open sea value in AM sea/land mask
     >           ISLM_L_value=1,
                  !<- must be integer land value in AM sea/land mask
     >           ISLM_SI_value=2)
                  !<- must be integer sea ice value in AM sea/land mask

      real SLM_OS_value,unrealistically_low_SST,
     >unrealistically_low_SV,unrealistically_low_SVp
     >,unrealistically_low_SF
      parameter (unrealistically_low_SST=0.01,
                           ! <- must be unreal low but >=0., see
                           ! subr. O2A --- check!
     >     unrealistically_low_SV=-1.E30)
                           ! <- must be negative unreal low surface flux
                           ! or other surface value to be sent
                           ! to Coupler, see Coupler code
      parameter (SLM_OS_value=REAL(ISLM_OS_value),
                   ! <- must be real open sea value in AM
                   ! sea/land mask array 
     >           unrealistically_low_SVp=0.99*unrealistically_low_SV,
     >               unrealistically_low_SF=unrealistically_low_SV)
                        !<- this used to be the name of the value; it
                        ! is not used any more but may be referred to
                        ! in comments

      save

      END MODULE SURFACE_cc
!
!***********************************************************************
!
      SUBROUTINE ATM_CMP_START

      USE ATM_cc, ONLY: component_nprocs,VerbLev,ibuffer,Coupler_id

      implicit none

      integer Atmos_id /1/, Atmos_master_rank_local /0/
      character*20 s
!

!        print*,'AM: to call CMP_INIT'
                      !<-id of AM as a component of the coupled system
      call CMP_INIT(Atmos_id,1)
                             !<-"flexibility level"
!        print*,'AM: back from CMP_INIT'
!      if (Coupler_id.ge.0) VerbLev=min(VerbLev,ibuffer(4))
      if (Coupler_id.ge.0) VerbLev=min(VerbLev,2)

        Atmos_master_rank_local=component_nprocs-1
                               !<- this redefinition is to meet the
                               ! requirement of subr. split2d_r used
                               ! in DISASSEMBLE_cc for disassembling
                               ! 2D fields. The requirement seems
                               ! to be that the input argument
                               ! representing a whole grid array be
                               ! defined in process of the largest rank
                               ! which seems to be considered i/o
                               ! process. To use a different value,
                               ! e.g. the conventional 0, split2d_r
                               ! (or DISASSEMBLE_cc) must be rewritten.
                     ! (Strangely, unsplit2d_r does not pose this
                     ! requirement and uses a dummy arg. ioproc to
                     ! identify the process where the whole grid array
                     ! is to be defined. Seemingly.)

      Atmos_master_rank_local=0  ! see above for modifications needed
                                 ! to support this change

      call CMP_INTRO(Atmos_master_rank_local)

      write(s,'(i2)') VerbLev
      call ATM_ANNOUNCE('back from CMP_INTRO, VerbLev='//s,2)

      return
      END
!
!***********************************************************************
!
      SUBROUTINE ATM_CMP_START1

      USE ATM_cc, ONLY: process_rank_local,VerbLev,ibuffer,Coupler_id

      implicit none

      integer Atmos_id /1/
!

                      !<-id of AM as a component of the coupling system
      call CMP_INIT(Atmos_id,1)
                             !<-"flexibility level"

!      if (Coupler_id.ge.0) VerbLev=min(VerbLev,ibuffer(4))
      if (Coupler_id.ge.0) VerbLev=min(VerbLev,2)

!           print*,'AM: back from CMP_INIT, process_rank_local=',
!    >      process_rank_local

      return
      END
!
!***********************************************************************
!
      SUBROUTINE ATM_CMP_START2(me)      

      USE ATM_cc, ONLY: VerbLev

      implicit none

      integer me

      character*20 s
!

      if (me .eq. 0) then
        CALL CMP_INTRO_m
      else
        CALL CMP_INTRO_s
      end if

      write(s,'(i2)') VerbLev
      call ATM_ANNOUNCE('back from CMP_INTRO_m, VerbLev='//s,1)

      return
      END
!
!***********************************************************************
!
      SUBROUTINE ATM_TILES_INIT(lonr_dummy,latr_dummy,lonf_dummy,
     >latg_dummy,latd_dummy,ipt_lats_node_r_dummy,
     >global_lats_r_dummy,lonsperlar_dummy)

      USE ATM_cc

      implicit none

      integer lonr_dummy,latr_dummy,lonf_dummy,latg_dummy,latd_dummy
      integer ipt_lats_node_r_dummy
      integer global_lats_r_dummy(latr_dummy),
     >        lonsperlar_dummy(latr_dummy)

      character*10 s
!

      lonr=lonr_dummy
      latr=latr_dummy
      lonf=lonf_dummy
      latg=latg_dummy
      latd=latd_dummy
      lats_node_r=latd
      ipt_lats_node_r=ipt_lats_node_r_dummy

      N2D=lonf*latg

      write(s,'(i5)') lonr
      CALL ATM_ANNOUNCE('ATM_TILES_INIT: lonr='//s,2)
      write(s,'(i5)') latr
      CALL ATM_ANNOUNCE('ATM_TILES_INIT: latr='//s,2)
      write(s,'(i5)') lonf
      CALL ATM_ANNOUNCE('ATM_TILES_INIT: lonf='//s,2)
      write(s,'(i5)') latg
      CALL ATM_ANNOUNCE('ATM_TILES_INIT: latg='//s,2)
      write(s,'(i5)') latd
      CALL ATM_ANNOUNCE('ATM_TILES_INIT: latd='//s,2)

      call GLOB_ABORT(abs(lonr-lonf)+abs(latr-latg),
     >'Unexpected: lonr, lonf or latr, latg differ. Aborting',1)

      if (.not. allocated(global_lats_r)) allocate(global_lats_r(latr))
      if (.not. allocated(lonsperlar))    allocate(lonsperlar(latr))
      global_lats_r=global_lats_r_dummy
      lonsperlar=lonsperlar_dummy

      CALL ATM_ANNOUNCE(
     >'ATM_TILES_INIT: global_lats_r, lonsperlar assigned',2)
      if (VerbLev.ge.2) then
        print*,'AM: ATM_TILES_INIT',component_master_rank_local,
     >  ' ipt_lats_node_r=',ipt_lats_node_r,' latd=',latd
        print*,'AM: ATM_TILES_INIT',component_master_rank_local,
     >  ' global_lats_r: ',global_lats_r
        print*,'AM: ATM_TILES_INIT',component_master_rank_local,
     >  ' lonsperlar: ',lonsperlar
      end if

      call INITIALIZE_TILING

      return
      END
!
!***********************************************************************
!
      SUBROUTINE ATM_SURF_INIT

      USE ATM_cc, ONLY: lonr,latd,lonf,latg

      USE SURFACE_cc

      implicit none

      integer rc
C

      write(s_cc,'(4i5)') lonr,latd,lonf,latg
      CALL ATM_ANNOUNCE(
     >'ATM_SURF_INIT: lonr,latd,lonf,latg: '//s_cc,2)
!--> cpl insertion
      if (.not. allocated(T_BOT_cc))    allocate(T_BOT_cc(lonr,latd))
      if (.not. allocated(U_BOT_cc))    allocate(U_BOT_cc(lonr,latd))
      if (.not. allocated(V_BOT_cc))    allocate(V_BOT_cc(lonr,latd))
      if (.not. allocated(Q_BOT_cc))    allocate(Q_BOT_cc(lonr,latd))
      if (.not. allocated(P_BOT_cc))    allocate(P_BOT_cc(lonr,latd))
      if (.not. allocated(Z_BOT_cc))    allocate(Z_BOT_cc(lonr,latd))
      if (.not. allocated(P_SURF_cc))   allocate(P_SURF_cc(lonr,latd))
      if (.not. allocated(T_SFC_cc))    allocate(T_SFC_cc(lonr,latd))
      if (.not. allocated(FICE_SFC_cc)) allocate(FICE_SFC_cc(lonr,latd))
      if (.not. allocated(HICE_SFC_cc)) allocate(HICE_SFC_cc(lonr,latd))
      if (.not. allocated(XMU_cc))      allocate(XMU_cc(lonr,latd))
      if (.not. allocated(DSW_cc))      allocate(DSW_cc(lonr,latd))
      if (.not. allocated(DLW_cc))      allocate(DLW_cc(lonr,latd))
      if (.not. allocated(ffmm_cc))     allocate(ffmm_cc(lonr,latd))
      if (.not. allocated(ffhh_cc))     allocate(ffhh_cc(lonr,latd))
!
!     allocate(T_BOT_cc(lonr,latd),U_BOT_cc(lonr,latd),
!    >     V_BOT_cc (lonr,latd),Q_BOT_cc(lonr,latd),
!    >     P_BOT_cc (lonr,latd),P_SURF_cc(lonr,latd),
!    >     Z_BOT_cc (lonr,latd),
!    >     T_SFC_cc (lonr,latd),
!    >     FICE_SFC_cc (lonr,latd),HICE_SFC_cc (lonr,latd),
!    >     XMU_cc(lonr,latd),
!    >     DSW_cc(lonr,latd), DLW_cc(lonr,latd),
!    >     ffmm_cc(lonr,latd), ffhh_cc(lonr,latd) )
!
      T_BOT_cc=0.
      U_BOT_cc=0.
      V_BOT_cc=0.
      Q_BOT_cc=0.
      P_BOT_cc=0.
      P_SURF_cc=0.
      Z_BOT_cc=0.
      T_SFC_cc=0.
      FICE_SFC_cc=0.
      HICE_SFC_cc=0.
      XMU_cc=0.
      DSW_cc=0.
      DLW_cc=0.
      ffmm_cc=0.
      ffhh_cc=0.
!<-- cpl insertion

      if (.not. allocated(DUSFC_cc))  allocate(DUSFC_cc(lonr,latd))
      if (.not. allocated(DVSFC_cc))  allocate(DVSFC_cc(lonr,latd))
      if (.not. allocated(DTSFC_cc))  allocate(DTSFC_cc(lonr,latd))
      if (.not. allocated(DQSFC_cc))  allocate(DQSFC_cc(lonr,latd))
      if (.not. allocated(PRECR_cc))  allocate(PRECR_cc(lonr,latd))
      if (.not. allocated(SST_cc))    allocate(SST_cc(lonr,latd))
      if (.not. allocated(DLWSFC_cc)) allocate(DLWSFC_cc(lonr,latd))
      if (.not. allocated(ULWSFC_cc)) allocate(ULWSFC_cc(lonr,latd))
      if (.not. allocated(SWSFC_cc))  allocate(SWSFC_cc(lonr,latd))
      if (.not. allocated(SST_ave))   allocate(SST_ave(lonr,latd))
      if (.not. allocated(SNW_cc))    allocate(SNW_cc(lonr,latd))
      if (.not. allocated(LPREC_cc))  allocate(LPREC_cc(lonr,latd))
!
!     allocate(DUSFC_cc(lonr,latd),DVSFC_cc(lonr,latd),
!    >     DTSFC_cc (lonr,latd),DQSFC_cc(lonr,latd),
!    >     PRECR_cc(lonr,latd),SST_cc(lonr,latd),
!    >     DLWSFC_cc(lonr,latd),ULWSFC_cc(lonr,latd),
!    >     SWSFC_cc(lonr,latd) ,SST_ave(lonr,latd),
!    >     SNW_cc(lonr,latd), LPREC_cc(lonr,latd) )     

      DUSFC_cc=0.
      DVSFC_cc=0.
      DTSFC_cc=0.
      DQSFC_cc=0.
      PRECR_cc=0.
      SNW_cc=0.
      LPREC_cc=0.
      DLWSFC_cc=0.
      ULWSFC_cc=0.
      SWSFC_cc=0.
      SST_ave=0.

      if (.not. allocated(ISLM_RG)) allocate(ISLM_RG(lonr,latd))
      if (.not. allocated(ISLM_FG)) allocate(ISLM_FG(lonr,latd))
!
!     allocate(ISLM_RG(lonr,latd),ISLM_FG(lonr,latd))

      call ATM_ANNOUNCE('ATM_SURF_INIT: ISLM_RG, ISLM_FG allocated',1)

      if (kind_sfcflux.ne.kind_phys_cc) then
        print*,'ATM_SURF_INIT: kind_sfcflux, kind_phys: ',
     >  kind_sfcflux, kind_phys_cc
        call GLOB_ABORT(1,'kind_sfcflux.ne.kind_phys_cc, GBPHYS args'//
     >  ' must be redeclared and code adjustments made',rc)
      end if

      return
      END
!
!***********************************************************************
!
      SUBROUTINE ATM_RECVdtc(dta)

      USE ATM_cc, ONLY:
     >   MPI_COMM_Atmos,
     >   Coupler_id,
     >   component_master_rank_local,
     >   kind_REAL,MPI_kind_REAL

      USE SURFACE_cc, ONLY:
     >dt_cc,dta2dtc_cc,i_dtc2dta_cc,i_dto2dta_cc,
     >s_cc , dto_cc,dta2dto_cc                  !--> cpl insertion: add dto_cc, dta2dto_cc

      implicit none

      real dta
      real (kind=kind_REAL) buf(2)
      integer rc,sizebuf
      character*40 s

      call ATM_ANNOUNCE('ATM_RECVdtc: to receive C time step',2)
      buf=0.
      sizebuf=size(buf)
      call CMP_RECV(buf,sizebuf)
      if (Coupler_id.lt.0) then
        dt_cc=0.
        dto_cc=0.
        call ATM_ANNOUNCE(
     >  'ATM_RECVdtc: C time step assigned 0, as it is standalone mode'
     >  ,2)
      else
        write(s,'(e20.12,e20.12)') buf(1),buf(2)
        call ATM_ANNOUNCE(
     >  'ATM_RECVdtc: C time step ='//trim(s)//' received',2)
        call MPI_BCAST(buf,2,MPI_kind_REAL,
     >  component_master_rank_local,MPI_COMM_Atmos,rc)
        call ATM_ANNOUNCE('ATM_RECVdtc: C time step broadcast',2)
        dt_cc=buf(1)
        dto_cc=buf(2)
      end if

      i_dtc2dta_cc = dt_cc/dta  + 0.001
      i_dto2dta_cc = dto_cc/dta + 0.001

      print *,' dto_cc=',dto_cc,' dta=',dta,' i_dto2dta_cc=',
     & i_dto2dta_cc,' dt_cc=',dt_cc,' i_dtc2dta_cc=',i_dtc2dta_cc

      if (i_dtc2dta_cc.eq.0) then
        i_dtc2dta_cc=4
        call ATM_ANNOUNCE('ratio of OM/AM time steps =0, assigned 4 .'//
     >  ' This should only occur if it is standalone mode',2)
      else
        write(s_cc,'(i2,i2)') i_dtc2dta_cc,i_dto2dta_cc
!       print *,' s_cc=',s_cc
        call ATM_ANNOUNCE('ratio of OM/AM time steps: '//trim(s_cc),2)
      end if
      dta2dtc_cc=1./i_dtc2dta_cc
      dta2dto_cc=1./i_dto2dta_cc

      RETURN
      END
!
!***********************************************************************
!
      SUBROUTINE ATM_SENDGRID(XLON,XLAT)

      USE ATM_cc

      implicit none

      real (kind=kind_REAL) XLON(lonr,latd),XLAT(lonr,latd)
      real (kind=kind_REAL) ALON(lonf),ALAT(latg),
     >x(lonf,latg),y(lonf,latg)

      integer buf(2),i,j

      logical fg

      character*50 s
      
      if (Coupler_id.lt.0) return    !   <- standalone mode

      buf(1)=lonf
      buf(2)=latg
      call ATM_ANNOUNCE('to send grid dimensions',1)
      call CMP_INTEGER_SEND(buf,2)
      call ATM_ANNOUNCE('grid dimensions sent',1)

      call ASSEMBLE_cc(x,XLON)

!-->cpl deletion, mom4, do not need laon, alat
!      if (component_master_rank_local.eq.process_rank_local) then
!
!c       ALON=x(:,1)
!        ALON=x(:,latg/2) ! assigns closest to equator lat. circle,
!                         ! where in reduced grid numb. of longitudes
!                         ! is maximal and = that in full grid
!
!        fg=.true.
!        do j=1,latg
!        do i=1,lonf
!          if (ALON(i).ne.x(i,j)) then
!            fg=.false.
!            write(s,'(2i5,1p2e16.7)') j,i,ALON(i),x(i,j)
!c           call GLOB_ABORT(1,
!            call ATM_ANNOUNCE(
!     >      'ATM_SENDGRID: inhomogeneous longitudes'//s,2)
!            exit
!          end if
!        end do
!        end do
!        if (fg) then
!          call ATM_ANNOUNCE('ATM_SENDGRID: full grid',1)
!        else
!          call ATM_ANNOUNCE('ATM_SENDGRID: reduced grid',1)
!        end if
!
!        call ATM_ANNOUNCE('to send array of longitudes',1)
!        call CMP_SEND(ALON,lonf)
!        call ATM_ANNOUNCE('array of longitudes sent',1)
!
!      end if
!<-- cpl deletion
 
      call ASSEMBLE_cc(x,XLAT)

!-->cpl deletion, mom4, do not need laon, alat
!      if (component_master_rank_local.eq.process_rank_local) then
!
!        ALAT=x(1,:)
!
!        do j=1,latg
!          if (ALAT(j).ne.x(2,j)) then
!            write(s,'(i5,1p2e16.7)') j,ALAT(j),x(2,j)
!            call GLOB_ABORT(1,
!     >      'ATM_SENDGRID: inhomogenous latitudes, aborting'//s,1)
!          end if
!        end do
!
!        call ATM_ANNOUNCE('to send array of latitudes',1)
!        call CMP_SEND(ALAT,latg)
!        call ATM_ANNOUNCE('array of latitudes sent',1)
!
!      end if
!<-- cpl deletion

      return
      END
!
!***********************************************************************
!
      SUBROUTINE ATM_SENDSLM(SLMSK)
!
!        This is to send sea/land mask with 0. on sea (either open sea
!        or sea ice) and 1. on land. For the assumptions about SLMSK
!        argument, see code/comments below

      USE ATM_cc

      USE SURFACE_cc, ONLY: ISLM_RG,ISLM_FG,kind_SLMSK

      implicit none

      real (kind=kind_SLMSK) SLMSK(lonr,latd)

      real(kind=kind_REAL), dimension(lonr,latd):: SLM1,SLM2,SLM0
      real SLM(lonf,latg)
      integer i,j,lat,lons
      character*80 s
      logical bad_SLM /.false./

      if (Coupler_id.lt.0) return    !   <- standalone mode

      if (VerbLev.ge.2) then
         print*,'ATMSENDSLM entered, lonr,latd,lonf,latg: ',
     >   lonr,latd,lonf,latg
      end if

      do j=1,latd
      do i=1,lonr
        if (abs(SLMSK(i,j)-2.).lt.1.E-5              ! sea ice
     >      .or. abs(SLMSK(i,j)).lt.1.E-5) then      ! open sea
          SLM1(i,j)=0.
        else if (abs(SLMSK(i,j)-1.).lt.1.E-5) then   ! land
          SLM1(i,j)=1.
        else
          SLM1(i,j)=666.
        end if
      end do
      end do

      ISLM_RG=nint(SLM1)
            !<- store reduced grid integer mask array for future
            ! communications; it will only be needed for uninterpred_cc

!        print*,'ATMSENDSLM to call uninterpred_cc'

      call uninterpred_cc(1,ISLM_RG,SLM1,SLM2)
                  ! <- interpolation FROM reduced grid (i.e. with # of
                  ! longitudes varying from lat. circle to lat. circle)
                  ! to full grid. 

!        print*,'ATMSENDSLM back from uninterpred_cc'

        ! Because 1st arg. iord=1, ISLM_RG values do not matter here, it
        ! is just a dummy input argument with proper type/dimensions.
        ! Reduced grid mask SLM1 is interpolated to full grid mask
        ! SLM2 (both arrays are local (per process)) by taking the
        ! nearest value on the lat. circle. This procedure should be
        ! reversible.
! Reversibility test:->

!        print*,'ATMSENDSLM to call interpred_cc'

      call interpred_cc(1,ISLM_FG,SLM2,SLM0)
                           !<- same thing: ISLM_FG values don't matter.
                           ! And they are undefined here.

!        print*,'ATMSENDSLM back from interpred_cc'

      do j=1,latd
        lat=global_lats_r(ipt_lats_node_r-1+j)
        lons=lonsperlar(lat)
        do i=1,lons
          if (SLM0(i,j).ne.SLM1(i,j)) then
            write(s,'("SLM: R2F irreversible",2i6,2pe17.9)')
     >      i,j,SLM1(i,j),SLM0(i,j)
            bad_SLM=.true.
            exit
          end if
        end do
      end do
! <-: reversibility test

!        print*,'ATMSENDSLM finished reversibility test'

! Value test:->
      do j=1,latd
      do i=1,lonr
        if (SLM2(i,j).ne.0. .and. SLM2(i,j).ne.1.) then
          write(s,'("Bad SLM value",2i6,1pe20.12)') i,j,SLM2(i,j)
          bad_SLM=.true.
          exit
        end if
      end do
      end do
! <-: value test

!        print*,'ATMSENDSLM finished value test'

      if (bad_SLM) then
        call GLOB_ABORT(1,'ATM_SENDSLM: '//s,1)
      end if

!        print*,'ATMSENDSLM to assign ISLM_FG=nint(SLM2)'


      ISLM_FG=nint(SLM2)
            !<- store full grid integer mask array for future
            ! communications; it will only be needed for interpred_cc

!        print*,'ATMSENDSLM to call ASSEMBLE_cc'


      call ASSEMBLE_cc(SLM,SLM2)

!        print*,'ATMSENDSLM back from ASSEMBLE_cc'

!--> cpl deletion
!d      call CMP_SEND(SLM,N2D)
!<-- cpl deletion

!        print*,'ATMSENDSLM to return'


      return
      END
!
!***********************************************************************
!
      SUBROUTINE ATM_GETSSTICE
     >(TSEA,TISFC,FICE,HICE,SHELEG,SLMSK,OROGR,kdt)

      USE ATM_cc, ONLY: kind_REAL,lonr,latd,Coupler_id,N2D,latg,lonf

      USE SURFACE_cc, ONLY:
     > lgetSSTICE_cc,kind_SST,kind_SLMSK,kind_OROGR,ISLM_FG,
     >SST_cc, SLM_OS_value,unrealistically_low_SST,
     >SST_ave,lsout_cc_momocn,dta2dto_cc,i_dto2dta_cc

      implicit none

      integer kdt
      real (kind=kind_SST),dimension(lonr,latd),intent(inout) :: TSEA,
     >   TISFC, FICE, HICE, SHELEG
      real,dimension(:,:),allocatable :: FICE_cc,HICE_cc,
     >    HSNO_cc
      real (kind=kind_SLMSK) SLMSK(lonr,latd)
      real (kind=kind_OROGR) OROGR(lonr,latd)

      logical RECV

      real, PARAMETER:: RLAPSE=0.65E-2
      real, PARAMETER:: CIMIN=0.15, HIMIN=0.10, HIMAX=8.0, TFW=271.2
      real, PARAMETER:: DS=330.0

      integer i,j
!

      RECV=lgetSSTICE_cc

      allocate(FICE_cc(lonr,latd),HICE_cc(lonr,latd),
     >  HSNO_cc(lonr,latd) )

      if (RECV) then
        call ATM_ANNOUNCE('ATM_GETSSTICE: to receive SST',2)
        call ATM_TILES_RECV(SST_cc,fval=unrealistically_low_SST,iord=2)
        call ATM_ANNOUNCE('ATM_GETSSTICE: SST received',2)

!
        call ATM_ANNOUNCE('ATM_GETSSTICE: to receive FICE',2)
        call ATM_TILES_RECV(FICE_cc,iord=2)
        call ATM_ANNOUNCE('ATM_GETSSTICE: FICE received',2)
!       print *,'after recv FICE'

        call ATM_ANNOUNCE('ATM_GETSSTICE: to receive HICE',2)
        call ATM_TILES_RECV(HICE_cc,iord=2)
        call ATM_ANNOUNCE('ATM_GETSSTICE: HICE received',2)
!       print *,'after recv HICE'

        call ATM_ANNOUNCE('ATM_GETSSTICE: to receive HSNO',2)
        call ATM_TILES_RECV(HSNO_cc,iord=2)
        call ATM_ANNOUNCE('ATM_GETSSTICE: HSNO received',2)
!       print *,'after recv HSNO'

      end if
      
      if (Coupler_id.lt.0) return    !   <- standalone mode

      if (RECV .and. kdt > 1) then

        SST_ave=SST_ave+SST_cc
        do j=1,latd
          do i=1,lonr
            if (abs(SLMSK(i,j)-SLM_OS_value).lt.0.01) then 
            if (FICE_cc(i,j).GE.CIMIN) then
              SLMSK(i,j)=2.0
              FICE(i,j)=FICE_cc(i,j)
              HICE(i,j)=MAX(MIN(HICE_cc(i,j)/FICE_cc(i,j),HIMAX),HIMIN)
              SHELEG(i,j)=HSNO_cc(i,j)*DS
              TISFC(i,j)=(TSEA(i,j)-(1.-FICE_cc(i,j))*TFW)/FICE_cc(i,j)
            end if
            else if (SLMSK(i,j).GT.1.5) then
            if (FICE_cc(i,j).GE.CIMIN) then
              FICE(i,j)=FICE_cc(i,j)
              HICE(i,j)=MAX(MIN(HICE_cc(i,j)/FICE_cc(i,j),HIMAX),HIMIN)
              SHELEG(i,j)=HSNO_cc(i,j)*DS
              TSEA(i,j)=TISFC(i,j)*FICE_cc(i,j)+TFW*(1.-FICE_cc(i,j))
            else
              FICE(i,j)=0.0
              HICE(i,j)=0.0
              SHELEG(i,j)=0.0
              TSEA(i,j)=TFW
              TISFC(i,j)=TFW
              SLMSK(i,j)=0.0
            end if
            else
              FICE(i,j)=0.0
              HICE(i,j)=0.0
            end if
          end do
        end do

      endif

      if (lsout_cc_momocn) then
        if(kdt > i_dto2dta_cc) then
!     print *,' sst_ave=',sst_ave(1,1),' dta2dto_cc=',dta2dto_cc
          SST_ave=SST_ave*dta2dto_cc
          do j=1,latd
            do i=1,lonr
              if (abs(SLMSK(i,j)-SLM_OS_value).lt.0.01) then
                if (SST_ave(i,j).gt.unrealistically_low_SST)
     >          TSEA(i,j)=SST_ave(i,j)-OROGR(i,j)*RLAPSE
              end if
            end do
          end do
          SST_ave=0.
        else
          SST_ave=0.
        endif
      endif

      deallocate(FICE_cc)
      deallocate(HICE_cc)
      deallocate(HSNO_cc)

      contains

      SUBROUTINE ATM_TILES_RECV(f,fval,iord)

      implicit none
      real (kind=kind_REAL) f(lonr,latd)
      real,optional,intent(in) ::  fval
      integer,optional,intent(in) :: iord

      real (kind=kind_REAL) f1(lonr,latd)
      real (kind=kind_REAL) x(lonf,latg)
      integer kmsk(lonr,latd),i,j,iiord,ik
!

      if (Coupler_id.lt.0) return    !   <- standalone mode


      call CMP_RECV(x,N2D)

      call DISASSEMBLE_cc(x,f1)

      kmsk=ISLM_FG
      ik=0
      if ( present(fval) )then
       do j=1,latd
        do i=1,lonr
          if (f1(i,j).le.fval) kmsk(i,j)=1
          if (f1(i,j).le.fval) ik=ik+1
        end do
       end do
!      print *,'iiord=',iiord,'ik=',ik,'fval=',fval
      endif
      if ( present(iord) ) then
          iiord=iord
      else
          iiord=2
      endif
!      print *,'iiord=',iiord,'ik=',ik
      call interpred_cc(iiord,kmsk,f1,f)
                ! <- interpolation TO reduced grid (i.e. with # of
                ! longitudes varying from lat. circle to lat. circle)
                ! from full grid

      END subroutine ATM_TILES_RECV

      END subroutine
!
!***********************************************************************
!
      SUBROUTINE ATM_ANNOUNCE(s,DbgLev)

      USE ATM_cc, ONLY: nunit_announce_cc,VerbLev

      implicit none

      character*(*) s
      integer DbgLev
!
      if (DbgLev.le.VerbLev)
     >  CALL CMP_ANNOUNCE(nunit_announce_cc,'AM: '//s)

      return
      END
!
!***********************************************************************
!
      SUBROUTINE ATM_DBG1(KDT,s,DbgLev)

      USE ATM_cc, ONLY: nunit_announce_cc,VerbLev
      USE SURFACE_cc

      implicit none

      integer KDT
      character*(*) s
      integer DbgLev
!
      if (DbgLev.gt.VerbLev) RETURN

!--> cpl change: write lsout_cc_momice and lsout_cc_momocn  <--
      write(s_cc,'("'//trim(s)//
     >': KDT=",i8," lsout_cc_momice=",L1, 
     >" lsout_cc_momocn=",L1," lgetSSTICE_cc=",L1)'
     >) KDT,lsout_cc_momice,lsout_cc_momocn,lgetSSTICE_cc

      CALL CMP_ANNOUNCE(nunit_announce_cc,'AM: DBG1: '//s_cc)

      return
      END
!
!***********************************************************************
!
      SUBROUTINE ATM_DBG2(KDT,PHOUR,ZHOUR,SHOUR,DbgLev)

      USE ATM_cc, ONLY: nunit_announce_cc,VerbLev
      USE SURFACE_cc

      implicit none

      integer KDT
      real PHOUR,ZHOUR,SHOUR
      integer DbgLev
!
!           print*,'AM: ATM_DBG2 entered'

      if (DbgLev.gt.VerbLev) RETURN

!           print*,'AM: ATM_DBG2 to do write(s_cc, ...'

!--> cpl change: write lsout_cc_momice and lsout_cc_momocn  <--
!
      write(s_cc,'("do_tstep entry",i6," KDT=",i8,'//
     >'" PHOUR,ZHOUR,SHOUR: ",1p,3e15.7,0p," lsout_cc_momice=",L1,'//
     >'" lsout_cc_momocn=",L1,'//
     >'" lgetSSTICE_cc=",L1)') n_do_tstep_cc,KDT,PHOUR,ZHOUR,SHOUR,
     > lsout_cc_momice,lsout_cc_momocn,lgetSSTICE_cc

      CALL CMP_ANNOUNCE(nunit_announce_cc,'AM: DBG2: '//s_cc)

      return
      END
!
!***********************************************************************
!
      subroutine ATM_TSTEP_INIT(KDT)

      USE namelist_def, ONLY: lssav
      USE SURFACE_cc

      implicit none

      integer KDT
!

      call ATM_ANNOUNCE('DOTSTEP entered, in ATM_TSTEP_INIT',3)
      n_do_tstep_cc=n_do_tstep_cc+1
      lssav_cc=lssav
      l_df_cc=.not.lssav   ! - double-check
!--> cpl deletion 
!d      lsout_cc=(MOD(KDT,i_dtc2dta_cc).eq.0)  ! <- still double-check
!d     > .and. .not. l_df_cc
!<-- cpl deletion 
!--> cpl insertion
      lsout_cc_momice=(MOD(KDT,max(1,i_dtc2dta_cc)).eq.0)  ! <- still double-check
     > .and. .not. l_df_cc                         ! <- instantaneous vars
      lsout_cc_momocn=(MOD(KDT,max(1,i_dto2dta_cc)).eq.0)  ! <- still double-check
     > .and. .not. l_df_cc
!<-- cpl insertion
      lgetSSTICE_cc=MOD(KDT,max(1,i_dtc2dta_cc)).eq.0                !-check!
     > .and. .not. l_df_cc

      if (kdt == 1) then
        print *,'in ATM initial,kdt=',kdt,'dtc/dta=', i_dtc2dta_cc,
     >    'dto/dta=',i_dto2dta_cc,'lsout_cc_momice=',
     >    lsout_cc_momice, 
     >   'lsout_cc_momocn=',lsout_cc_momocn,'lgetSSTICE_cc=',
     &    lgetSSTICE_cc,'lssav=',lssav,MOD(KDT,max(1,i_dtc2dta_cc))
     >                  ,MOD(KDT,max(1,i_dto2dta_cc))
      endif
      return
      end
!
!***********************************************************************
!
      subroutine ATM_SENDFLUXES(SLMSK)

      USE ATM_cc, ONLY: lonr,latd

      USE SURFACE_cc

      implicit none

      real (kind=kind_SLMSK) SLMSK(lonr,latd)
      integer i,j
!

!--> cpl insertion: send model vars first to coupler
       if (lsout_cc_momice) then
        call ATM_ANNOUNCE('to send T_SFC',2)
        call ATM_SENDFLUX(T_SFC_cc)
        call ATM_ANNOUNCE('to send T_BOT',2)
!       print *,'SEND FLUXES, T_BOt(1:10)=',T_BOT_cc(1:10,1)
        call ATM_SENDFLUX(T_BOT_cc)
        call ATM_ANNOUNCE('to send U_BOT',2)
        call ATM_SENDFLUX(U_BOT_cc)
        call ATM_ANNOUNCE('to send V_BOT',2)
        call ATM_SENDFLUX(V_BOT_cc)
        call ATM_ANNOUNCE('to send Q_BOT',2)
        call ATM_SENDFLUX(Q_BOT_cc)
        call ATM_ANNOUNCE('to send P_BOT',2)
        call ATM_SENDFLUX(P_BOT_cc)
        call ATM_ANNOUNCE('to send P_SURF',2)
        call ATM_SENDFLUX(P_SURF_cc)
        call ATM_ANNOUNCE('to send Z_BOT',2)
        call ATM_SENDFLUX(Z_BOT_cc)
        call ATM_ANNOUNCE('to send XMU',2)
        call ATM_SENDFLUX(XMU_cc)
        call ATM_ANNOUNCE('to send DLW',2)
        call ATM_SENDFLUX(DLW_cc)
        call ATM_ANNOUNCE('to send DSW',2)
        call ATM_SENDFLUX(DSW_cc)
        call ATM_ANNOUNCE('to send ffmm',2)
        call ATM_SENDFLUX(ffmm_cc)
        call ATM_ANNOUNCE('to send ffhh',2)
        call ATM_SENDFLUX(ffhh_cc)
        call ATM_ANNOUNCE('end of send variables',2)

        call atm_maxmin(lonr,latd,SNW_cc,'in ATM, snw_cc')

        SNW_cc(:,:)=SNW_cc(:,:)/dt_cc*1.E3
        call atm_maxmin(lonr,latd,SNW_cc,'in ATM,2 snw_cc')

        call ATM_SENDFLUX(SNW_cc)
        call ATM_ANNOUNCE('precip SNW sent',2)

        LPREC_cc(:,:)=LPREC_cc(:,:)/dt_cc*1.E3
        call atm_maxmin(lonr,latd,LPREC_cc,'in ATM,2 lprec_cc')
        call ATM_SENDFLUX(LPREC_cc)
        call ATM_ANNOUNCE('liquid precip sent',2)

!       Sending original hice and fice
!
        call ATM_SENDFLUX(FICE_SFC_cc)
        call ATM_ANNOUNCE('to send fice',2)
        call ATM_SENDFLUX(HICE_SFC_cc)
        call ATM_ANNOUNCE('to send hice',2)
!

        T_BOT_cc=0.
        U_BOT_cc=0.
        V_BOT_cc=0.
        Q_BOT_cc=0.
        P_BOT_cc=0.
        P_SURF_cc=0.
        Z_BOT_cc=0.
        T_SFC_cc=0.
        XMU_cc=0.
        DSW_cc=0.
        DLW_cc=0.
        ffmm_cc=0.
        ffhh_cc=0.
        snw_cc=0.
        lprec_cc=0.
       endif
!<-- cpl insertion

      if (lsout_cc_momocn) then
        DUSFC_cc=-DUSFC_cc*dta2dto_cc !chk units, *const*ps may be needed
        DVSFC_cc=-DVSFC_cc*dta2dto_cc !chk units, *const*ps may be needed
        DTSFC_cc=DTSFC_cc*dta2dto_cc !chk units, *const*ps may be needed
        DQSFC_cc=DQSFC_cc*dta2dto_cc !chk units, *const*ps may be needed
        DLWSFC_cc=DLWSFC_cc*dta2dto_cc !-------, *const*ps may be needed
        ULWSFC_cc=ULWSFC_cc*dta2dto_cc !-------, *const*ps may be needed
        SWSFC_cc=-SWSFC_cc*dta2dto_cc !chk units, *const*ps may be needed
        PRECR_cc=PRECR_cc/dto_cc      ! assign dt_cc -- OM time step
                                ! <- (above, it was "AM" instead of
                                ! OM in the commentary - apparently
                                ! by mistake or misprint, but it
                                ! resulted in actual assignment of
                                ! AM time step to dt_cc)
     >            *1.E3   ! <- don't know why. See treatment of
                         ! GESHEM in wrtsfc.f, wrtsfc_comm.f (7/16/04)
        call ATM_ANNOUNCE('to send fluxes',2)
        call ATM_SENDFLUX(DUSFC_cc,SLMSK=SLMSK)
        call ATM_ANNOUNCE('x-stress sent',2)
        call ATM_SENDFLUX(DVSFC_cc,SLMSK=SLMSK)
        call ATM_ANNOUNCE('y-stress sent',2)
!        DTSFC_cc=DTSFC_cc+DQSFC_cc-DLWSFC_cc+ULWSFC_cc+SWSFC_cc
        DTSFC_cc=DTSFC_cc
        call ATM_SENDFLUX(DTSFC_cc,SLMSK=SLMSK)
        call ATM_ANNOUNCE('Q (net heat flux) sent',2)
!        DQSFC_cc=DQSFC_cc/hvap_cc-PRECR_cc
        DQSFC_cc=DQSFC_cc/hvap_cc
        call ATM_SENDFLUX(DQSFC_cc,SLMSK=SLMSK)
        call ATM_ANNOUNCE('E-P sent',2)
!
        DLWSFC_cc=DLWSFC_cc-ULWSFC_cc
        call ATM_SENDFLUX(DLWSFC_cc,SLMSK=SLMSK)
        call ATM_ANNOUNCE('net LWR sent',2)
        call ATM_SENDFLUX(SWSFC_cc,SLMSK=SLMSK)
        call ATM_ANNOUNCE('net SWR sent',2)
!XW     call ATM_SENDFLUX(PRECR_cc,SLMSK=SLMSK)
!XW     call ATM_ANNOUNCE('PRECIP sent',2)
!
        call ATM_ANNOUNCE('fluxes sent',2)
        DUSFC_cc=0.
        DVSFC_cc=0.
        DTSFC_cc=0.
        DQSFC_cc=0.
        PRECR_cc=0.
        DLWSFC_cc=0.
        ULWSFC_cc=0.
        SWSFC_cc=0.
      end if

      contains
!===
      SUBROUTINE ATM_SENDFLUX(f,SLMSK)

      USE ATM_cc

      USE SURFACE_cc, ONLY: ISLM_RG,
     >kind_sfcflux,kind_SLMSK,SLM_OS_value,
     >unrealistically_low_SV,unrealistically_low_SVp

      implicit none

      real (kind=kind_sfcflux),intent(in) :: f(lonr,latd)
!--> cpl deletion
!      real (kind=kind_SLMSK) SLMSK(lonr,latd)
!<-- cpl deletion
      real (kind=kind_SLMSK),optional,intent(in)  :: SLMSK(lonr,latd)

      real(kind=kind_REAL), dimension(lonr,latd):: f1,f2
      real (kind=kind_REAL) x(lonf,latg)
      integer kmsk(lonr,latd)
      integer iord /2/
      integer i,j
      character*40 s
!

      if (Coupler_id.lt.0) return    !   <- standalone mode

      f1(:,:)=f(:,:)
      kmsk=ISLM_RG

!      ISLM_RG is local (per process) mask array that is
!      CONSTANT in time. It contains 0 for either open sea (OS) or
!      sea ice (SI) and 1 for land (L). KEEP IN MIND: it's on REDUCED G.

!--> cpl insertion
      if ( present(SLMSK) ) then
!<-- cpl insertion
      do j=1,latd
      do i=1,lonr
!        if (abs(SLMSK(i,j)-SLM_OS_value).lt.0.01) then
!                                  ! i.e. if it is OS (open sea) AMGP
        if (abs(SLMSK(i,j)-2.).lt.1.E-5              ! sea ice
     >      .or. abs(SLMSK(i,j)).lt.1.E-5) then      ! open sea  AM
          kmsk(i,j)=0
        else
          kmsk(i,j)=1
        end if
      end do
      end do

      endif

!       SLMSK is (per-process-) local mask array regularly updated
!       with sea ice data

      call uninterpred_cc(iord,kmsk,f1,f2)
                ! <- interpolation FROM reduced grid (i.e. with # of
                ! longitudes varying from lat. circle to lat. circle)
                ! to full grid
!
!      print *,'in SEND_FLUX, before assemble_cc'
      call  ASSEMBLE_cc(x,f2)

!      print *,'in SEND_FLUX, testing, x=',x(1:5,1),'f=',f(1:5,1),
!     >   'f1=',f1(1:5,1),'f2=',f2(1:5,1)
      call CMP_SEND(x,N2D)

      END subroutine ATM_SENDFLUX

      end subroutine
! ******************************************************************
       subroutine atm_maxmin(xdim,ydim,x,s)
  
       USE ATM_cc

       implicit none

       integer xdim,ydim,i,j
       real(kind=kind_REAL) x(xdim,ydim),xmax,xmin
       character(*) s

      xmax=x(1,1)
      xmin=x(1,1)
      do j=1,ydim
      do i=1,xdim
       if ( xmax .lt. x(i,j) ) xmax=x(i,j)
       if ( xmin .gt. x(i,j) ) xmin=x(i,j)
      enddo
      enddo
!     print *,s//' in atm_maxmin,xdim=',xdim,'ydim=',ydim,
!    >   'xmax=',xmax,'xmin=',xmin

      return
      end
! ******************************************************************
       subroutine atm_maxmin_int(xdim,ydim,x,s)

       USE ATM_cc

       implicit none

       integer xdim,ydim,i,j
       integer x(xdim,ydim),xmax,xmin
       character(*) s

      xmax=x(1,1)
      xmin=x(1,1)
      do j=1,ydim
      do i=1,xdim
       if ( xmax .lt. x(i,j) ) xmax=x(i,j)
       if ( xmin .gt. x(i,j) ) xmin=x(i,j)
      enddo
      enddo
      print *,s//' in atm_maxmin,xdim=',xdim,'ydim=',ydim,
     >   'xmax=',xmax,'xmin=',xmin

      return
      end

