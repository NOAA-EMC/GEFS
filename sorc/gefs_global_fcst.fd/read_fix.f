      SUBROUTINE read_mtn_hprim_oz(SLMSK,HPRIME,NEEDORO,ORO,
     &           iozondp,ozplin,global_lats_r,lonsperlar)
!
!***********************************************************************
!
      use resol_def
      use layout1
      use mpi_def
      use ozne_def
      implicit none

!
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)
      real (kind=kind_io8) SLMSK(lonr,lats_node_r),
     &  HPRIME(NMTVR,lonr,lats_node_r),ORO(lonr,lats_node_r)
 
      integer iozondp
      real (kind=kind_io8) ozplin(latsozp,levozp,pl_coeff,timeoz)
 
      real(kind=kind_io4) buff1(lonr,latr),buffm(lonr,latr,nmtvr)
      real(kind=kind_io8) buffo(lonr,lats_node_r)
      real(kind=kind_io8) buff2(lonr,lats_node_r)
      integer kmsk0(lonr,latr)
      integer i,j,k,nmtn
      integer needoro
!
      kmsk0=0
!
!     Read HPRIME from file MTNVAR
!     ****************************
      nmtn=24
!jfe  IF (me.eq.0) THEN
      IF (icolor.eq.2.and.me.eq.nodes-1) THEN
        READ(nmtn) buffm
!!      do k=1,nmtvr
!!        write(200) buffm(:,:,k)
!!      enddo
      ENDIF
      DO k=1,nmtvr
       call split2d(buffm(1,1,k),buffo,global_lats_r)
       CALL interpred(1,kmsk0,buffo,buff2,global_lats_r,
     &                lonsperlar)
       HPRIME(k,:,:)=buff2(:,:)
      ENDDO
 
!my jordan's mb
!sela  print *, ' (*j*)  nmtvr= ',nmtvr, 'reading hprime'
!my      DO j=1,lats_node_r
!my      DO i=1,lonr
!my      DO k=1,NMTVR
!my        IF(SLMSK(i,j).NE.1.) HPRIME(k,i,j) = 0.
!my      ENDDO
!my      ENDDO
!my      ENDDO
 

 
      IF (iozondp.eq.1) CALL readoz_disprd(ozplin)
!
!     reading the grib orography and scattering the data
!
      if(needoro.eq.1) then

      IF (icolor.eq.2.and.me.eq.nodes-1) print *,'read grb orography'
      IF (icolor.eq.2.and.me.eq.nodes-1) THEN
        CALL ORORD(101,lonr,latr,buff1)
      endif
      call split2d(buff1,buffo,global_lats_r)
      CALL interpred(1,kmsk0,buffo,oro,global_lats_r,lonsperlar)
 
      endif
      RETURN
      END
      SUBROUTINE read_sfc(sfc_fld,NEEDORO,nread,
     &                    cfile,global_lats_r,lonsperlar)
!
!***********************************************************************
!
      use sfcio_module
      use resol_def
      use layout1
      use mpi_def
      use Sfc_Flx_ESMFMod
      use namelist_soilveg , only : salp_data, snupx
      use physcons, only : tgice => con_tice
      implicit none
!
      TYPE(Sfc_Var_Data)        :: sfc_fld
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)

      integer jump
      integer needoro

      real(kind=kind_io4) buff1(lonr,latr)
!    &,    buff4(lonr,latr,4),xhour
!$$$     &     buff4(lonr,latr,4),slmskful(lonr,latr),xhour
      real(kind=kind_io8) buffo(lonr,lats_node_r)
      real(kind=kind_io8) buff3(lonr,lats_node_r)
      integer nread,i,j,k,ij,idate(4),lonsfc,latsfc,lplsfc(latr2)
      character*(*) cfile
      integer kmsk(lonr,latr)
      CHARACTER*8 labfix(4)
!$$$      common /comfixio/slmskful
      real t1,t2,timef,rsnow
      type(sfcio_head) head
      type(sfcio_data) data
      integer iret, vegtyp
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
      t1=timef()

      if (me == 0) print *,' nread=',nread,' cfile=',cfile
      IF (icolor.eq.2.and.me.eq.nodes-1) THEN
        call sfcio_srohdc(nread,cfile,head,data,iret)

        PRINT 99,nread,head%fhour,head%idate,
     &         head%lonb,head%latb,head%lsoil,head%ivs,iret,lats_node_r
99      FORMAT(1H ,'in fixio nread=',i3,2x,'HOUR=',f8.2,3x,'IDATE=',
     &  4(1X,I4),4x,'lonsfc,latsfc,lsoil,ivssfc,iret=',6i8)

        if(iret.ne.0) goto 5000
!       if(head%ivs.ne.200412.and.head%ivs.ne.200501) goto 5000
        if(head%lonb.ne.lonr) goto 5000
        if(head%latb.ne.latr) goto 5000
        if(head%lsoil.ne.lsoil) goto 5000

      ENDIF

      kmsk=0

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%tsea
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%TSEA,global_lats_r,lonsperlar)

      DO K=1, LSOIL
        if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%smc(:,:,k)
        call split2d(buff1, buffo,global_lats_r)
        CALL interpred(1,kmsk,buffo,buff3,global_lats_r,lonsperlar)
        sfc_fld%SMC(k,:,:)=buff3(:,:)
      ENDDO

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%sheleg
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%SHELEG,
     &               global_lats_r,lonsperlar)

      DO K = 1, LSOIL
        if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%stc(:,:,k)
        call split2d(buff1, buffo,global_lats_r)
        CALL interpred(1,kmsk,buffo,buff3,global_lats_r,lonsperlar)
        sfc_fld%STC(k,:,:)=buff3(:,:)
      ENDDO

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%tg3
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%TG3,global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%zorl
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%ZORL,global_lats_r,lonsperlar)

      sfc_fld%cv  = 0
      sfc_fld%cvb = 0
      sfc_fld%cvt = 0

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%alvsf
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%ALVSF,
     &               global_lats_r,lonsperlar)
      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%alvwf
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%ALVWF,
     &               global_lats_r,lonsperlar)
      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%alnsf
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%ALNSF,
     &               global_lats_r,lonsperlar)
      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%alnwf
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%ALNWF,
     &               global_lats_r,lonsperlar)

!     The mask cannot be interpolated
      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%slmsk
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%SLMSK,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%vfrac
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%VFRAC,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%canopy
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%CANOPY,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%f10m
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%F10M,global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%vtype
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%VTYPE,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%stype
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%STYPE,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%facsf
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%FACSF,
     &               global_lats_r,lonsperlar)
      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%facwf
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%FACWF,
     &               global_lats_r,lonsperlar)

!szunyogh 06/16/99
        if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%uustar
         call split2d(buff1, buffo,global_lats_r)
         CALL interpred(1,kmsk,buffo,sfc_fld%UUSTAR,
     &               global_lats_r,lonsperlar)

        if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%ffmm
         call split2d(buff1, buffo,global_lats_r)
         CALL interpred(1,kmsk,buffo,sfc_fld%FFMM,
     &                  global_lats_r,lonsperlar)

        if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%ffhh
         call split2d(buff1, buffo,global_lats_r)
         CALL interpred(1,kmsk,buffo,sfc_fld%FFHH,
     &                  global_lats_r,lonsperlar)

!c-- XW: FOR SEA-ICE Nov04
!    Sea-ice (hice/fice) was added to the surface files.

         if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%hice
         call split2d(buff1, buffo,global_lats_r)
         CALL interpred(1,kmsk,buffo,sfc_fld%HICE,
     &                  global_lats_r,lonsperlar)

         if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%fice
         call split2d(buff1, buffo,global_lats_r)
         CALL interpred(1,kmsk,buffo,sfc_fld%FICE,
     &                  global_lats_r,lonsperlar)

         if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%tisfc
         call split2d(buff1, buffo,global_lats_r)
         CALL interpred(1,kmsk,buffo,sfc_fld%TISFC,
     &                  global_lats_r,lonsperlar)
         if (lats_node_r > 0 )  then
           if (sfc_fld%tisfc(1,1) < 0.0)  then
             DO j=1,lats_node_r
               DO i=1,LONR
                 sfc_fld%TISFC(i,j) = sfc_fld%TSEA(i,j)
                 IF(sfc_fld%SLMSK(i,j) >=  2. .AND.
     &             sfc_fld%FICE(i,j)  >= 0.5) THEN
                   sfc_fld%TISFC(i,j) = (sfc_fld%TSEA(i,j)
     &            -tgice*(1.-sfc_fld%FICE(i,j))) / sfc_fld%FICE(i,j)
                   sfc_fld%TISFC(i,j)=MIN(sfc_fld%TISFC(i,j),tgice)
                 ENDIF
               ENDDO
             ENDDO
           endif
         endif

!c-- XW: END SEA-ICE

!lu   11/10/2004
!*     surface files for GFS/Noah contain 8 additional records:
!*     tprcp, srflag, snwdph, slc, shdmin, shdmax, slope, snoalb

         if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%tprcp
         call split2d(buff1, buffo,global_lats_r)
         CALL interpred(1,kmsk,buffo,sfc_fld%TPRCP,
     &                  global_lats_r,lonsperlar)

!* srflag
         if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%srflag
         call split2d(buff1, buffo,global_lats_r)
         CALL interpred(1,kmsk,buffo,sfc_fld%SRFLAG,
     &                  global_lats_r,lonsperlar)

!* snwdph
         if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%snwdph
         call split2d(buff1, buffo,global_lats_r)
         CALL interpred(1,kmsk,buffo,sfc_fld%SNWDPH,
     &                  global_lats_r,lonsperlar)

!* slc
         DO K=1, LSOIL
         if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%slc(:,:,k)
         call split2d(buff1, buffo,global_lats_r)
         CALL interpred(1,kmsk,buffo,buff3,global_lats_r,lonsperlar)
         sfc_fld%SLC(k,:,:) = buff3(:,:)
         ENDDO

!* shdmin
         if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%shdmin
         call split2d(buff1, buffo,global_lats_r)
         CALL interpred(1,kmsk,buffo,sfc_fld%SHDMIN,
     &                  global_lats_r,lonsperlar)

!* shdmax
         if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%shdmax
         call split2d(buff1, buffo,global_lats_r)
         CALL interpred(1,kmsk,buffo,sfc_fld%SHDMAX,
     &                  global_lats_r,lonsperlar)

!* slope
         if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%slope
         call split2d(buff1, buffo,global_lats_r)
         CALL interpred(1,kmsk,buffo,sfc_fld%SLOPE,
     &                  global_lats_r,lonsperlar)

!* snoalb
         if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%snoalb
         call split2d(buff1, buffo,global_lats_r)
         CALL interpred(1,kmsk,buffo,sfc_fld%SNOALB,
     &                  global_lats_r,lonsperlar)
!     print *,' snoalb=',sfc_fld%snoalb(1,:)
!lu [+67L]: the addition of 8 Noah records ends here .........................

       if(needoro.eq.1) then
         if(icolor.eq.2.and.me.eq.nodes-1) then
           buff1=data%orog
           needoro=1
           if(all(data%orog.ne.sfcio_realfill)) needoro=0
           print *,'read sfc orography'
         endif
         call split2d(buff1, buffo,global_lats_r)
         CALL interpred(1,kmsk,buffo,sfc_fld%ORO,
     &                  global_lats_r,lonsperlar)
         call skip(needoro)
       endif
!
!Wei initialize snow fraction(sheleg is in mm)
      DO j=1,lats_node_r
        DO i=1,LONR
          sfc_fld%SNCOVR(i,j) = 0.0
          if (sfc_fld%slmsk(i,j) > 0.001) then
            vegtyp = sfc_fld%VTYPE(i,j)
            RSNOW  = 0.001*sfc_fld%SHELEG(i,j)/SNUPX(vegtyp)
            IF (0.001*sfc_fld%SHELEG(i,j) < SNUPX(vegtyp)) THEN
              sfc_fld%SNCOVR(i,j) = 1.0 - ( EXP(-SALP_DATA*RSNOW)
     &                                    - RSNOW*EXP(-SALP_DATA))
            ELSE
              sfc_fld%SNCOVR(i,j) = 1.0
            ENDIF
!           if (i == 1)
!    &       print*,SNUPX(vegtyp),SALP_DATA,sfc_fld%SNCOVR(i,j),
!    &       '************debug',sfc_fld%SHELEG(i,j),vegtyp,' j=',j
!    &,      ' snoalb1=',sfc_fld%snoalb(i,j)
!
          endif
        ENDDO
       ENDDO
!

       IF (icolor.eq.2.and.me.eq.nodes-1) then
         call sfcio_axdata(data,iret)
         t2=timef()
         print *,'FIXIO TIME ',t2-t1,t1,t2
       endif
!
      RETURN
 5000 PRINT *, ' ERROR IN INPUT IN FIXIO'
      STOP
      END
!
      SUBROUTINE read_nsst(nsst_fld, nread, cfile,
     &                    global_lats_r, lonsperlar)
!
!***********************************************************************
!
      use namelist_def
      use nsstio_module
      use resol_def
      use layout1
      use mpi_def
      use Nsstm_ESMFMod
      implicit none
!
      TYPE(Nsst_Var_Data)       :: nsst_fld
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)

!     real (kind=kind_io8) slmsk(lonr,lats_node_r),

      real(kind=kind_io4) buff1(lonr,latr)
      real(kind=kind_io8) buffo(lonr,lats_node_r)
      integer nread,i,j,k,ij,idate(4),lonnsst,latnsst,lplnsst(latr2)
      character*(*) cfile
      integer kmsk(lonr,latr)
      CHARACTER*8 labfix(4)
      real t1,t2,timef
      type(nsstio_head) head
      type(nsstio_data) data
      integer iret

c
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      t1=timef()

      IF (icolor.eq.2.and.me.eq.nodes-1) THEN
        call nsstio_srohdc(nread,cfile,head,data,iret)

        PRINT 99,nread,head%fhour,head%idate,
     &     head%lonb,head%latb,head%lsea,head%ivo,iret,lats_node_r
99      FORMAT(1H ,'in fixio nread=',i3,2x,'HOUR=',f8.2,3x,'IDATE=',
     &  4(1X,I4),4x,'lonnsst,latnsst,lsea,ivsnsst,iret=',6i8)

        if(iret.ne.0) goto 5000
        if(head%lonb.ne.lonr) goto 5000
        if(head%latb.ne.latr) goto 5000
        if(head%lsea.ne.lsea) goto 5000

      ENDIF

      kmsk=0
!
!     Assign ocnf(lonr,lats_node_r,nf_ocn)
!
      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%tref
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%TREF,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%dt_cool
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%DT_COOL,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%z_c
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%Z_C,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%dt_warm
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%DT_WARM,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%z_w
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%Z_W,
     &               global_lats_r,lonsperlar)
      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%c_0
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%C_0,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%c_d
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%C_D,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%w_0
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%W_0,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%w_d
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%W_D,
     &               global_lats_r,lonsperlar)

!
!     Assign ocnr(lonr,lats_node_r,nr_ocn)
!
!?    if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%slmsk
!?    call split2d(buff1, buffo,global_lats_r)
!?    CALL interpred(1,kmsk,buffo,SLMSK,
!?   &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%ifd
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%IFD,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%time_old
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%TIME_OLD,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%time_ins
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%TIME_INS,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%I_Sw
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%I_Sw,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%I_Q
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%I_Q,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%I_Qrain
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%I_QRAIN,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%I_M
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%I_M,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%I_Tau
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%I_TAU,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%I_Sw_Zw
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%I_Sw_Zw,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%I_Q_Ts
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%I_Q_Ts,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%I_M_Ts
      call split2d(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%I_M_Ts,
     &               global_lats_r,lonsperlar)

       IF (icolor.eq.2.and.me.eq.nodes-1) then
         call nsstio_axdata(data,iret)
         t2=timef()
         print *,'FIXIO for NSST TIME ',t2-t1,t1,t2
       endif
!
      RETURN
 5000 PRINT *, ' ERROR IN INPUT IN FIXIO NSST'
      STOP
      END
!
      SUBROUTINE set_nsst(tsea, nsst_fld)
c
c***********************************************************************
c
      use namelist_def
      use resol_def
      use layout1
      use Nsstm_ESMFMod
      use mpi_def
      implicit none
c
      TYPE(Nsst_Var_Data)       :: nsst_fld
      real (kind=kind_io8) tsea(lonr,lats_node_r)

      integer i,j,k
      real t1,t2,timef

c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      t1=timef()
!
!     Assign ocnf(lonr,lats_node_r,nf_ocn)
!
      nsst_fld%Tref    = tsea
      nsst_fld%dt_cool = 0.0
      nsst_fld%z_c = 0.0
      nsst_fld%dt_warm = 0.1
      nsst_fld%z_w     = 20.0
      nsst_fld%c_0     = 0.0
      nsst_fld%c_d     = 0.0
      nsst_fld%w_0     = 0.0
      nsst_fld%w_d     = 0.0
!
!     Assign ocnr(lonr,lats_node_r,nr_ocn)
!
      nsst_fld%ifd      = 0.0
      nsst_fld%time_old = 0.0
      nsst_fld%time_ins = 0.0
      nsst_fld%I_Sw     = 0.0
      nsst_fld%I_Q      = 0.0
      nsst_fld%I_Qrain  = 0.0
      nsst_fld%I_M      = 0.0
      nsst_fld%I_Tau    = 0.0
      nsst_fld%I_Sw_Zw  = 0.0
      nsst_fld%I_Q_Ts   = 0.0
      nsst_fld%I_M_Ts   = 0.0

      t2=timef()
      print *,'FIXIO for set_nsst TIME ',t2-t1,t1,t2
!
      RETURN
      END
!
!***********************************************************************
!
      subroutine interpred(iord,kmsk,f,fi,global_lats_r,lonsperlar)
!!
      use resol_def
      use layout1
      implicit none
!!
      integer              global_lats_r(latr)
      integer,intent(in):: iord
      integer,intent(in):: kmsk(lonr,lats_node_r)
      integer,intent(in):: lonsperlar(latr)
      real(kind=kind_io8),intent(in):: f(lonr,lats_node_r)
      real(kind=kind_io8),intent(out):: fi(lonr,lats_node_r)
      integer j,lons,lat
!!
      do j=1,lats_node_r
          lat=global_lats_r(ipt_lats_node_r-1+j)
          lons=lonsperlar(lat)
          if(lons.ne.lonr) then
            call intlon(iord,1,1,lonr,lons,
     &                  kmsk(1,j),f(1,j),fi(1,j))
cjfe        fi(lons+1:lonr,j)=-9999.e9
            fi(lons+1:lonr,j)=0.
          else
            fi(:,j)=f(:,j)
          endif
        enddo
      end subroutine
c
c***********************************************************************
c
      subroutine intlon(iord,imon,imsk,m1,m2,k1,f1,f2)
      use machine
      implicit none
      integer,intent(in):: iord,imon,imsk,m1,m2
      integer,intent(in):: k1(m1)
      real (kind=kind_io8),intent(in):: f1(m1)
      real (kind=kind_io8),intent(out):: f2(m2)
      integer i2,in,il,ir
      real (kind=kind_io8) r,x1
      r=real(m1)/real(m2)
      do i2=1,m2
         x1=(i2-1)*r
         il=int(x1)+1
         ir=mod(il,m1)+1
          if(iord.eq.2.and.(imsk.eq.0.or.k1(il).eq.k1(ir))) then
            f2(i2)=f1(il)*(il-x1)+f1(ir)*(x1-il+1)
          else
            in=mod(nint(x1),m1)+1
            f2(i2)=f1(in)
          endif
      enddo
      end subroutine
c
c**********************************************************************
c
      SUBROUTINE readoz_disprd(ozplin)
 
      use resol_def
      use layout1
      use ozne_def
      implicit none
!!
      integer n,k,kk,i
      real (kind=kind_phys) ozplin(latsozp,levozp,pl_coeff,timeoz)
      real(kind=kind_io4) tempin(latsozp)
!
      DO I=1,timeoz
        do n=1,pl_coeff
          DO k=1,levozp
            READ(kozpl) tempin
            ozplin(:,k,n,i) = tempin(:)
          ENDDO
        enddo
      ENDDO
 
      RETURN
      END
c
c***********************************************************************
c
      SUBROUTINE ORORD(LUGB,IORO,JORO,ORO)
!
      use resol_def
      use layout1
      implicit none
!!
      integer lugb, ioro, joro, kpdoro, ior, jor, i,k
      CHARACTER*80 FNOROG
!
      real (kind=kind_io4) oro(ioro,joro)
      real (kind=kind_io8) orog(ioro,joro), blnm, bltm
      logical gausm
!
      FNOROG = 'orography'
      kpdoro = 8
      IOR    = IORO
      JOR    = JORO
      CALL FIXRDG(LUGB,IOR,JOR,FNOROG,
     &            KPDORO,OROG,GAUSM,BLNM,BLTM,me)
!
      if (ior .ne. ioro .or. jor .ne. joro) then
         print *,' orography file not o.k. run aborted'
         call abort
      endif
      ORO = OROG
!
      RETURN
      END
c
c***********************************************************************
c
      subroutine split2d(x,xl,global_lats_r)
c
c***********************************************************************
c
      use resol_def
      use layout1
      use mpi_def
      implicit none
!!
      real(kind=kind_io4) x(lonr,latr)
      real (kind=kind_io8) xl(lonr,lats_node_r)
      real(kind=kind_io4) tmp(lonr,latr)
      integer global_lats_r(latr)
      integer nprocf,nodesr
!     integer maxfld,nprocf,nodesr
!     integer proc,j,lat,msgtag,nproc,i,msgtag1,buff,startlat,ierr
      integer proc,j,lat,nproc,i,buff,startlat,ierr
      integer ifld/0/
      save ifld
      real t1,t2,t3,t4,timef,ta,tb
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
!!
      XL=0.
!     maxfld=50
      ifld=ifld+1
!!
      IF (icolor.eq.2.and.me.eq.nodes-1) THEN
        ta=timef()
        t3=ta
c        DO proc=1,nodes-1
         do proc=1,1
c
c         Sending the data
c         ----------------
         tmp=0.
         do j=1,latr
            do i=1,lonr
              tmp(i,j)=X(i,j)
            enddo
         enddo
!Moor    msgtag=1000+proc*nodes*maxfld+ifld
         t1=timef()
!sela    print *,' GWVX BROADCASTING FROM ',nodes-1
         call mpi_bcast
     1 (tmp,lonr*latr,MPI_R_IO,nodes-1,MPI_COMM_ALL,info)
         call mpi_comm_rank(MPI_COMM_ALL,i,info)
c         CALL mpi_send(tmp,lonr*latr,MPI_R_IO,proc-1,msgtag,
c     &                  MPI_COMM_ALL,info)
         t2=timef()
!sela    print 102,t2-t1
 
 102    format(' SEND TIME ',f10.5)
        enddo
        t4=timef()
      ELSE
        if (.NOT.LIOPE) then
          nodesr=nodes
        else
          nodesr=nodes+1
        endif
!Moor   msgtag=1000+(me+1)*nodesr*maxfld+ifld
!sela    print *,' GWVX BROADCASTREC  FROM ',nodesr-1
         call mpi_bcast
     1 (tmp,lonr*latr,MPI_R_IO,nodesr-1,MPI_COMM_ALL,info)
         call mpi_comm_rank(MPI_COMM_ALL,i,info)
!sela    print *,'GWVX IPT ',ipt
c        CALL mpi_recv(tmp,lonr*latr,MPI_R_IO,nodesr-1,
c     &                msgtag,MPI_COMM_ALL,stat,info)
        do j=1,lats_node_r
           lat=global_lats_r(ipt_lats_node_r-1+j)
           do i=1,lonr
              xl(i,j)=tmp(i,lat)
           enddo
        enddo
!!
      ENDIF
!!
!!     for pes nodes-1
      if (.NOT.LIOPE) then
        if (me.eq.nodes-1) then
          do j=1,lats_node_r
             lat=global_lats_r(ipt_lats_node_r-1+j)
             do i=1,lonr
                xl(i,j)=X(i,lat)
             enddo
          enddo
        endif
      endif
!!
      tb=timef()
         call mpi_comm_rank(MPI_COMM_ALL,i,info)
 
!sela  if(icolor.eq.2.and.me.eq.nodes-1)print 103,tb-ta,t4-t3
 103  format(' GLOBAL AND SEND TIMES  SPLIT2D',2f10.5)
      return
      end
c
c***********************************************************************
c
      SUBROUTINE skip(jump)
 
c*************************************************************************
 
      use resol_def
      use layout1
      use mpi_def
      implicit none
 
      integer jump,ipe
 
      IF (icolor.eq.2) then
         ipe=nodes-1
      else
         ipe=nodes
      endif
 
      CALL MPI_BCAST(jump,1,MPI_INTEGER,ipe,MPI_COMM_ALL,info)
 
      RETURN
      END
!
c
c***********************************************************************
c
      SUBROUTINE EXCHA(lats_nodes_r,global_lats_r,X1,X2,Y1,Y2)
c
c***********************************************************************
c
      use resol_def
      use layout1
      use mpi_def
      implicit none
 
      integer n,i,j,ierr,ilat,lat,node,nsend
      integer              global_lats_r(latr)
      integer              lats_nodes_r(nodes)
      real(kind=kind_io8) X1(lats_node_r),X2(lats_node_r)
      real(kind=kind_io8) Y1(latr),Y2(latr)
cjfe  real(kind=kind_mpi) tmps(2,lats_node_r_max,nodes)
cjfe  real(kind=kind_mpi) tmpr(2,lats_node_r_max,nodes)
      real(kind=kind_io8) tmps(2,lats_node_r_max,nodes)
      real(kind=kind_io8) tmpr(2,lats_node_r_max,nodes)
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      if (nodes.ne.1) then
        do node=1,nodes
          do i=1,lats_node_r
           lat=global_lats_r(ipt_lats_node_r-1+i)
           tmps(1,i,node)=X1(I)
           tmps(2,i,node)=X2(I)
          enddo
        enddo
!!
        nsend=2*lats_node_r_max
cjfe    call mpi_alltoall(tmps,nsend,MPI_R_MPI,
cjfe x                     tmpr,nsend,MPI_R_MPI,
cjfe x                     MC_COMP,ierr)
        call mpi_alltoall(tmps,nsend,MPI_R_DEF,
     x                     tmpr,nsend,MPI_R_DEF,
     x                     MC_COMP,ierr)
!!
        ilat=1
        do node=1,nodes
          do i=1,lats_nodes_r(node)
             lat=global_lats_r(ilat)
             Y1(lat)=tmpr(1,i,node)
             Y2(lat)=tmpr(2,i,node)
             ilat=ilat+1
          enddo
        enddo
!!
      ELSE
        Y1=X1
        Y2=X2
      ENDIF
!!
      RETURN
      END
c
c***********************************************************************
c
      SUBROUTINE SUMLAT(n,X,nodes)
c
c***********************************************************************
c
      use mpi_def
      implicit none
 
      integer n,i,j,np,mr,nodes
      real(kind=kind_io8) X(n),Y(N)
      real(kind=kind_io4) Z(n)
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      if (nodes.ne.1) then
        DO i=1,n
          Y(i)=X(i)
        ENDDO
        CALL mpi_allreduce(Y,X,n,MPI_R_DEF,MPI_SUM,
     &                    MC_COMP   ,info)
      endif
        DO i=1,n
          Z(i)=X(i)
        ENDDO
        DO i=1,n
          X(i)=Z(i)
        ENDDO
!!
      RETURN
      END
c
c***********************************************************************
c
      subroutine unsplit2d(ioproc,x,xl,global_lats_r)
c
c***********************************************************************
c
      use resol_def
      use layout1
      use mpi_def
      implicit none
!!
      real(kind=kind_io4) x(lonr,latr)
      real (kind=kind_io8) xl(lonr,lats_node_r)
      real(kind=kind_io4) tmp(lonr,latr+2)
      integer global_lats_r(latr),ipt_lats_node_rl,nodesr
      integer lats_nodes_rl
      integer maxfld,ioproc,nproct
      integer proc,j,lat,msgtag,nproc,i,msgtag1,buff,startlat,ierr
      integer ifldu/0/
      save ifldu
      integer illen,ncc
      data ncc/0/
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
!!
      X=0.
      maxfld=50
      ifldu=ifldu+1
!!
      IF (me.ne.ioproc) THEN
c
c         Sending the data
c         ----------------
         tmp=0.
         tmp(lonr,latr+1)=ipt_lats_node_r
         tmp(lonr,latr+2)=lats_node_r
         do j=1,lats_node_r
            do i=1,lonr
              tmp(i,j)=XL(i,j)
            enddo
         enddo
         if (.NOT.LIOPE) then
           nodesr=nodes
         else
           nodesr=nodes+1
         endif
         msgtag=1000+(me+1)*nodesr*maxfld+ifldu
          call MPI_SEND(tmp(lonr,latr+1),1,MPI_R_IO,ioproc,
     &                  msgtag,MPI_COMM_ALL,info)
          call MPI_SEND(tmp(lonr,latr+2),1,MPI_R_IO,ioproc,
     &                  msgtag,MPI_COMM_ALL,info)
         illen=tmp(lonr,latr+2)
c send the local grid domain
         CALL mpi_send(tmp(1,1),illen*lonr,MPI_R_IO,ioproc,
     &                  msgtag,MPI_COMM_ALL,info)
      ELSE
!!
!!     for pes ioproc
        if (.NOT.LIOPE) then
          nproct=nodes
          do j=1,lats_node_r
             lat=global_lats_r(ipt_lats_node_r-1+j)
             do i=1,lonr
                x(i,lat)=XL(i,j)
             enddo
          enddo
        else
          nproct=nodes-1
        endif
        DO proc=1,nproct
         if (proc.ne.ioproc+1) then
         msgtag=1000+proc*nodes*maxfld+ifldu
          CALL mpi_recv(tmp(lonr,latr+1),1,MPI_R_IO,proc-1,
     &                msgtag,MPI_COMM_ALL,stat,info)
          CALL mpi_recv(tmp(lonr,latr+2),1,MPI_R_IO,proc-1,
     &                msgtag,MPI_COMM_ALL,stat,info)
         illen=tmp(lonr,latr+2)
          CALL mpi_recv(tmp(1,1),illen*lonr ,MPI_R_IO,proc-1,
     &                msgtag,MPI_COMM_ALL,stat,info)
         if (.NOT.LIOPE) then
           ipt_lats_node_rl=tmp(lonr,latr+1)
           lats_nodes_rl=tmp(lonr,latr+2)
         else
           ipt_lats_node_rl=tmp(lonr,lats_node_r_max+1)
           lats_nodes_rl=tmp(lonr,lats_node_r_max+2)
         endif
         do j=1,lats_nodes_rl
           lat=global_lats_r(ipt_lats_node_rl-1+j)
           do i=1,lonr
              x(i,lat)=tmp(i,j)
           enddo
         enddo
         endif   !(proc.ne.ioproc+1)
        enddo
!!
      ENDIF
         ncc=ncc+1
 
!!
      return
      end
c
c***********************************************************************
c
      subroutine uninterpred(iord,kmsk,f,fi,global_lats_r,lonsperlar)
!!
      use resol_def
      use layout1
      implicit none
!!
      integer              global_lats_r(latr)
      integer,intent(in):: iord
      integer,intent(in):: kmsk(lonr,lats_node_r)
      integer,intent(in):: lonsperlar(latr)
      real(kind=kind_io8),intent(out):: f(lonr,lats_node_r)
      real(kind=kind_io8),intent(in):: fi(lonr,lats_node_r)
      integer j,lons,lat
!!
      do j=1,lats_node_r
          lat=global_lats_r(ipt_lats_node_r-1+j)
          lons=lonsperlar(lat)
          if(lons.ne.lonr) then
            call intlon(iord,1,1,lons,lonr,
     &                  kmsk(1,j),fi(1,j),f(1,j))
          else
            f(:,j)=fi(:,j)
          endif
        enddo
      end subroutine
      subroutine uninterprez(iord,kmsk,f,fi,global_lats_r,lonsperlar)
!!
      use resol_def
      use mod_state
      use layout1
      implicit none
!!
      integer              global_lats_r(latr)
      integer,intent(in):: iord
      integer,intent(in):: kmsk(lonr,lats_node_r)
      integer,intent(in):: lonsperlar(latr)
      real(kind=kind_io8),intent(out):: f(lonr,lats_node_r)
      real(kind=kind_io8),intent(in):: fi(lonr,lats_node_r)
       real(kind=4) f4(lonr,lats_node_r)
      integer j,lons,lat
      integer i,ubound
!!
      do j=1,lats_node_r
          lat=global_lats_r(ipt_lats_node_r-1+j)
          lons=lonsperlar(lat)
          if(lons.ne.lonr) then
            call intlon(iord,1,1,lons,lonr,
     &                  kmsk(1,j),fi(1,j),f(1,j))
          f4(:,j)=fi(:,j)
          else
            f(:,j)=fi(:,j)
            f4(:,j)=fi(:,j)
          endif
        enddo
      do j=1,lats_node_r
      do i=1,lonr
      buff_mult_piecea(i,ngrid,j)=f (i,j)
      end do
      end do
      ngrid=ngrid+1
      end subroutine
       subroutine unsplit2z(ioproc,x,global_lats_r)
c
c***********************************************************************
c
      use resol_def
      use mod_state
      use layout1
      use mpi_def
      implicit none
!!
      real(kind=kind_io4) x(lonr,latr)
      real(kind=kind_io4) tmp(lonr,latr+2)
      integer global_lats_r(latr),ipt_lats_node_rl,nodesr
      integer lats_nodes_rl
      integer maxfld,ioproc,nproct
      integer proc,j,lat,msgtag,nproc,i,msgtag1,buff,startlat,ierr
      integer ifldu/0/
      save ifldu
      integer illen
       character*8 cna
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
!!
      write(cna,985)600+ngrid
 985   format('fort.',i3)
      X=0.
      maxfld=50
      ifldu=ifldu+1
!!
      IF (me.ne.ioproc) THEN
            continue
      ELSE
!!
!!     for pes ioproc
c        if (.NOT.LIOPE) then
c            continue
c        else
c          nproct=nodes-1
c        endif
           nproct=nodes_comp
        DO proc=1,nproct
c         if (proc.ne.ioproc+1) then
c         if (.NOT.LIOPE) then
c             continue
c         else
            ipt_lats_node_rl=ivar_global_a(1,proc)
            lats_nodes_rl=ivar_global_a(2,proc)
c         endif
         do j=1,lats_nodes_rl
           lat=global_lats_r(ipt_lats_node_rl-1+j)
           do i=1,lonr
c              x(i,lat)=tmp(i,j)
              x(i,lat)=buff_mult_piecesa(i,ngrid,j,proc)
           enddo
         enddo
c         endif   !(proc.ne.ioproc+1)
        enddo
!!
c        call baclose(563,i)
c         print *,cna,' UNSPLITFCLOSE  ',i
c        call baopenw(563,cna,i)
c         print *,cna,' UNSPLITF OPEN  ',i
      ENDIF
        ngrid=ngrid+1
!!
      return
      end
 
c
c***********************************************************************
c
      subroutine unsplit2d_r(ioproc,x,xl,global_lats_r)
c
c***********************************************************************
c
      use resol_def
      use layout1
      use mpi_def
      implicit none
!!
      real(kind=kind_ior) x(lonr,latr)
      real (kind=kind_io8) xl(lonr,lats_node_r)
      real(kind=kind_ior) tmp(lonr,latr+2)
      integer global_lats_r(latr),ipt_lats_node_rl,nodesr
      integer lats_nodes_rl
      integer maxfld,ioproc,nproct
      integer proc,j,lat,msgtag,nproc,i,msgtag1,buff,startlat,ierr
      integer ifldu/0/
      save ifldu
      integer illen,ncc
      data ncc/0/
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
!!
!     X=0.               ! commented by moorthi on 20051117
      maxfld=50
      ifldu=ifldu+1
!!
      IF (me.ne.ioproc) THEN
c
c         Sending the data
c         ----------------
         tmp=0.
         tmp(lonr,latr+1)=ipt_lats_node_r
         tmp(lonr,latr+2)=lats_node_r
         do j=1,lats_node_r
            do i=1,lonr
              tmp(i,j)=XL(i,j)
            enddo
         enddo
         if (.NOT.LIOPE) then
           nodesr=nodes
         else
           nodesr=nodes+1
         endif
         msgtag=1000+(me+1)*nodesr*maxfld+ifldu
          call MPI_SEND(tmp(lonr,latr+1),1,MPI_R_IO_R,ioproc,
     &                  msgtag,MPI_COMM_ALL,info)
          call MPI_SEND(tmp(lonr,latr+2),1,MPI_R_IO_R,ioproc,
     &                  msgtag,MPI_COMM_ALL,info)
         illen=tmp(lonr,latr+2)
c send the local grid domain
         CALL mpi_send(tmp(1,1),illen*lonr,MPI_R_IO_R,ioproc,
     &                  msgtag,MPI_COMM_ALL,info)
      ELSE
!!
!!     for pes ioproc
        x = 0.0               ! added by Moorthi on 2005111700
        if (.NOT.LIOPE) then
          nproct=nodes
          do j=1,lats_node_r
             lat=global_lats_r(ipt_lats_node_r-1+j)
             do i=1,lonr
                x(i,lat)=XL(i,j)
             enddo
          enddo
        else
          nproct=nodes-1
        endif
        DO proc=1,nproct
         if (proc.ne.ioproc+1) then
         msgtag=1000+proc*nodes*maxfld+ifldu
          CALL mpi_recv(tmp(lonr,latr+1),1,MPI_R_IO_R,proc-1,
     &                msgtag,MPI_COMM_ALL,stat,info)
          CALL mpi_recv(tmp(lonr,latr+2),1,MPI_R_IO_R,proc-1,
     &                msgtag,MPI_COMM_ALL,stat,info)
         illen=tmp(lonr,latr+2)
          CALL mpi_recv(tmp(1,1),illen*lonr ,MPI_R_IO_R,proc-1,
     &                msgtag,MPI_COMM_ALL,stat,info)
         if (.NOT.LIOPE) then
           ipt_lats_node_rl=tmp(lonr,latr+1)
           lats_nodes_rl=tmp(lonr,latr+2)
         else
           ipt_lats_node_rl=tmp(lonr,lats_node_r_max+1)
           lats_nodes_rl=tmp(lonr,lats_node_r_max+2)
         endif
         do j=1,lats_nodes_rl
           lat=global_lats_r(ipt_lats_node_rl-1+j)
           do i=1,lonr
              x(i,lat)=tmp(i,j)
           enddo
         enddo
         endif   !(proc.ne.ioproc+1)
        enddo
!!
      ENDIF
         ncc=ncc+1
 
!!
      return
      end
c
c***********************************************************************
c
      subroutine split2d_r(x,xl,global_lats_r)
c
c***********************************************************************
c
      use resol_def
      use layout1
      use mpi_def
      implicit none
!!
      real(kind=kind_ior) x(lonr,latr)
      real (kind=kind_io8) xl(lonr,lats_node_r)
      real(kind=kind_ior) tmp(lonr,latr)
      integer global_lats_r(latr)
      integer nprocf,nodesr
!     integer maxfld,nprocf,nodesr
      integer proc,j,lat,nproc,i,buff,startlat,ierr
!     integer proc,j,lat,msgtag,nproc,i,msgtag1,buff,startlat,ierr
      integer ifld/0/
      save ifld
      real t1,t2,t3,t4,timef,ta,tb
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
!!
      XL=0.
!     maxfld=50
      ifld=ifld+1
!!
      IF (icolor.eq.2.and.me.eq.nodes-1) THEN
        ta=timef()
        t3=ta
c        DO proc=1,nodes-1
         do proc=1,1
c
c         Sending the data
c         ----------------
         tmp=0.
         do j=1,latr
            do i=1,lonr
              tmp(i,j)=X(i,j)
            enddo
         enddo
!Moor    msgtag=1000+proc*nodes*maxfld+ifld
         t1=timef()
!sela    print *,' GWVX BROADCASTING FROM ',nodes-1
         call mpi_bcast
     1 (tmp,lonr*latr,MPI_R_IO_R,nodes-1,MPI_COMM_ALL,info)
         call mpi_comm_rank(MPI_COMM_ALL,i,info)
c         CALL mpi_send(tmp,lonr*latr,MPI_R_IO_R,proc-1,msgtag,
c     &                  MPI_COMM_ALL,info)
         t2=timef()
!sela    print 102,t2-t1
 
 102    format(' SEND TIME ',f10.5)
        enddo
        t4=timef()
      ELSE
        if (.NOT.LIOPE) then
          nodesr=nodes
        else
          nodesr=nodes+1
        endif
!Moor   msgtag=1000+(me+1)*nodesr*maxfld+ifld
!sela    print *,' GWVX BROADCASTREC  FROM ',nodesr-1
         call mpi_bcast
     1 (tmp,lonr*latr,MPI_R_IO_R,nodesr-1,MPI_COMM_ALL,info)
         call mpi_comm_rank(MPI_COMM_ALL,i,info)
!sela    print *,'GWVX IPT ',ipt
c        CALL mpi_recv(tmp,lonr*latr,MPI_R_IO_R,nodesr-1,
c     &                msgtag,MPI_COMM_ALL,stat,info)
        do j=1,lats_node_r
           lat=global_lats_r(ipt_lats_node_r-1+j)
           do i=1,lonr
              xl(i,j)=tmp(i,lat)
           enddo
        enddo
!!
      ENDIF
!!
!!     for pes nodes-1
      if (.NOT.LIOPE) then
        if (me.eq.nodes-1) then
          do j=1,lats_node_r
             lat=global_lats_r(ipt_lats_node_r-1+j)
             do i=1,lonr
                xl(i,j)=X(i,lat)
             enddo
          enddo
        endif
      endif
!!
      tb=timef()
         call mpi_comm_rank(MPI_COMM_ALL,i,info)
 
!sela  if(icolor.eq.2.and.me.eq.nodes-1)print 103,tb-ta,t4-t3
 103  format(' GLOBAL AND SEND TIMES  SPLIT2D',2f10.5)
      return
      end
      SUBROUTINE read_sfc_r(sfc_fld,NEEDORO,nread,
     &                    cfile,global_lats_r,lonsperlar)
!
!***********************************************************************
!
      use sfcio_module
      use resol_def
      use layout1
      use mpi_def
      use Sfc_Flx_ESMFMod
      use namelist_soilveg , only : salp_data, snupx
      use physcons, only : tgice => con_tice
      implicit none
!
      TYPE(Sfc_Var_Data)        :: sfc_fld
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)

      integer jump
      integer needoro

      real(kind=kind_ior) buff1(lonr,latr)
!    &,    buff4(lonr,latr,4)
!     real(kind=kind_io4) xhour
c$$$     &     buff4(lonr,latr,4),slmskful(lonr,latr),xhour
      real(kind=kind_io8) buffo(lonr,lats_node_r)
      real(kind=kind_io8) buff3(lonr,lats_node_r)
      integer nread,i,j,k,ij,idate(4),lonsfc,latsfc,lplsfc(latr2)
      character*(*) cfile
      integer kmsk(lonr,latr)
      CHARACTER*8 labfix(4)
c$$$      common /comfixio/slmskful
      real t1,t2,timef,rsnow
      type(sfcio_head) head
      type(sfcio_dbta) data
      integer iret, vegtyp
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
      t1=timef()

      IF (icolor.eq.2.and.me.eq.nodes-1) THEN
        call sfcio_srohdc(nread,cfile,head,data,iret)

        PRINT 99,nread,head%fhour,head%idate,
     &           head%lonb,head%latb,head%lsoil,head%ivs,iret
99      FORMAT(1H ,'in fixio nread=',i3,2x,'HOUR=',f8.2,3x,'IDATE=',
     &  4(1X,I4),4x,'lonsfc,latsfc,lsoil,ivssfc,iret=',5i8)

        if(iret.ne.0) goto 5000
!       if(head%ivs.ne.200412.and.head%ivs.ne.200501) goto 5000
        if(head%lonb.ne.lonr) goto 5000
        if(head%latb.ne.latr) goto 5000
        if(head%lsoil.ne.lsoil) goto 5000

      ENDIF

      kmsk=0

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%tsea
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%TSEA,global_lats_r,lonsperlar)

      DO K=1, LSOIL
        if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%smc(:,:,k)
        call split2d_r(buff1, buffo,global_lats_r)
        CALL interpred(1,kmsk,buffo,buff3,global_lats_r,lonsperlar)
        sfc_fld%SMC(k,:,:) = buff3(:,:)
      ENDDO

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%sheleg
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%SHELEG,
     &               global_lats_r,lonsperlar)

      DO K = 1, LSOIL
        if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%stc(:,:,k)
        call split2d_r(buff1, buffo,global_lats_r)
        CALL interpred(1,kmsk,buffo,buff3,global_lats_r,lonsperlar)
        sfc_fld%STC(k,:,:) = buff3(:,:)
      ENDDO

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%tg3
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%TG3,global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%zorl
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%ZORL,global_lats_r,lonsperlar)

      sfc_fld%cv  = 0
      sfc_fld%cvb = 0
      sfc_fld%cvt = 0

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%alvsf
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%ALVSF,
     &               global_lats_r,lonsperlar)
      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%alvwf
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%ALVWF,
     &               global_lats_r,lonsperlar)
      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%alnsf
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%ALNSF,
     &               global_lats_r,lonsperlar)
      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%alnwf
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%ALNWF,
     &               global_lats_r,lonsperlar)

!     The mask cannot be interpolated
      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%slmsk
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%SLMSK,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%vfrac
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%VFRAC,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%canopy
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%CANOPY,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%f10m
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%F10M,global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%vtype
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%VTYPE,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%stype
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%STYPE,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%facsf
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%FACSF,
     &               global_lats_r,lonsperlar)
      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%facwf
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%FACWF,
     &               global_lats_r,lonsperlar)

!szunyogh 06/16/99
      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%uustar
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%UUSTAR,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%ffmm
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%FFMM,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%ffhh
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%FFHH,
     &               global_lats_r,lonsperlar)

!c-- XW: FOR SEA-ICE Nov04
!    Sea-ice (hice/fice) was added to the surface files.

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%hice
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%HICE,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%fice
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%FICE,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%tisfc
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%tisfc,
     &               global_lats_r,lonsperlar)
      if (lats_node_r > 0 )  then
        if (sfc_fld%tisfc(1,1) < 0.0) then
          DO j=1,lats_node_r
            DO i=1,LONR
              sfc_fld%TISFC(i,j) = sfc_fld%TSEA(i,j)
              IF(sfc_fld%SLMSK(i,j) >=  2. .AND.
     &          sfc_fld%FICE(i,j)  >= 0.5) THEN
                sfc_fld%TISFC(i,j) = (sfc_fld%TSEA(i,j)
     &         -tgice*(1.-sfc_fld%FICE(i,j))) / sfc_fld%FICE(i,j)
                sfc_fld%TISFC(i,j) = MIN(sfc_fld%TISFC(i,j),tgice)
              ENDIF
            ENDDO
          ENDDO
        endif
      endif

!c-- XW: END SEA-ICE

!lu   11/10/2004
!*     surface files for GFS/Noah contain 8 additional records:
!*     tprcp, srflag, snwdph, slc, shdmin, shdmax, slope, snoalb

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%tprcp
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%TPRCP,
     &               global_lats_r,lonsperlar)

!* srflag
      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%srflag
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%SRFLAG,
     &               global_lats_r,lonsperlar)

!* snwdph
      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%snwdph
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%SNWDPH,
     &               global_lats_r,lonsperlar)

!* slc
      DO K=1, LSOIL
        if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%slc(:,:,k)
        call split2d_r(buff1, buffo,global_lats_r)
        CALL interpred(1,kmsk,buffo,buff3,global_lats_r,lonsperlar)
        sfc_fld%SLC(k,:,:) = buff3(:,:)
      ENDDO

!* shdmin
      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%shdmin
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%SHDMIN,
     &               global_lats_r,lonsperlar)

!* shdmax
      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%shdmax
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%SHDMAX,
     &               global_lats_r,lonsperlar)

!* slope
      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%slope
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%SLOPE,
     &               global_lats_r,lonsperlar)

!* snoalb
      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%snoalb
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,sfc_fld%SNOALB,
     &               global_lats_r,lonsperlar)
!lu [+67L]: the addition of 8 Noah records ends here .........................

      if(needoro.eq.1) then
        if(icolor.eq.2.and.me.eq.nodes-1) then
          buff1=data%orog
          needoro=1
          if(all(data%orog.ne.sfcio_realfill)) needoro=0
          print *,'read sfc orography'
        endif
        call split2d(buff1, buffo,global_lats_r)
        CALL interpred(1,kmsk,buffo,sfc_fld%ORO,
     &                 global_lats_r,lonsperlar)
        call skip(needoro)
      endif
!
!Wei initialize snow fraction(sheleg is in mm)
      DO j=1,lats_node_r
        DO i=1,LONR
          sfc_fld%SNCOVR(i,j) = 0.0
          if (sfc_fld%slmsk(i,j) > 0.001) then
            vegtyp = sfc_fld%VTYPE(i,j)
            RSNOW  = 0.001*sfc_fld%SHELEG(i,j)/SNUPX(vegtyp)
            IF (0.001*sfc_fld%SHELEG(i,j) < SNUPX(vegtyp)) THEN
              sfc_fld%SNCOVR(i,j) = 1.0 - ( EXP(-SALP_DATA*RSNOW)
     &                                    - RSNOW*EXP(-SALP_DATA))
            ELSE
              sfc_fld%SNCOVR(i,j) = 1.0
            ENDIF
!          print*,SNUPX(vegtyp2d(i,j)),SALP_DATA,sfc_fld%SNCOVR(i,j),
!    & '************debug',SHELEG(i,j),vegtyp2d(i,j)
          endif
        ENDDO
       ENDDO
!

       IF (icolor.eq.2.and.me.eq.nodes-1) then
         call sfcio_axdbta(data,iret)
         t2=timef()
         print *,'FIXIO TIME ',t2-t1,t1,t2
       endif
!
      RETURN
 5000 PRINT *, ' ERROR IN INPUT IN FIXIO'
      STOP
      END
      SUBROUTINE read_nsst_r(nsst_fld, nread, cfile,
     &                      global_lats_r, lonsperlar)
!
!***********************************************************************
!
      use namelist_def
      use nsstio_module
      use resol_def
      use layout1
      use mpi_def
      use Nsstm_ESMFMod
      implicit none
!
      TYPE(Nsst_Var_Data)       :: nsst_fld
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)

!     real (kind=kind_io8) slmsk(lonr,lats_node_r),

      real(kind=kind_ior) buff1(lonr,latr)
      real(kind=kind_io8) buffo(lonr,lats_node_r)
      integer nread,i,j,k,ij,idate(4),lonnsst,latnsst,lplnsst(latr2)
      character*(*) cfile
      integer kmsk(lonr,latr)
      CHARACTER*8 labfix(4)
      real t1,t2,timef
      type(nsstio_head) head
      type(nsstio_dbta) data
      integer iret

c
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      t1=timef()

      IF (icolor.eq.2.and.me.eq.nodes-1) THEN
        call nsstio_srohdc(nread,cfile,head,data,iret)

        PRINT 99,nread,head%fhour,head%idate,
     &     head%lonb,head%latb,head%lsea,head%ivo,iret,lats_node_r
99      FORMAT(1H ,'in fixio nread=',i3,2x,'HOUR=',f8.2,3x,'IDATE=',
     &  4(1X,I4),4x,'lonnsst,latnsst,lsea,ivsnsst,iret=',6i8)

        if(iret.ne.0) goto 5000
        if(head%lonb.ne.lonr) goto 5000
        if(head%latb.ne.latr) goto 5000
        if(head%lsea.ne.lsea) goto 5000

      ENDIF

      kmsk=0
!
!     Assign ocnf(lonr,lats_node_r,nf_ocn)
!
      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%tref
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%TREF,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%dt_cool
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%DT_COOL,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%z_c
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%Z_C,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%dt_warm
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%DT_WARM,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%z_w
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%Z_W,
     &               global_lats_r,lonsperlar)
      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%c_0
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%C_0,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%c_d
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%C_D,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%w_0
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%W_0,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%w_d
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%W_D,
     &               global_lats_r,lonsperlar)

!
!     Assign ocnr(lonr,lats_node_r,nr_ocn)
!
!?    if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%slmsk
!?    call split2d_r(buff1, buffo,global_lats_r)
!?    CALL interpred(1,kmsk,buffo,SLMSK,
!?   &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%ifd
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%IFD,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%time_old
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%TIME_OLD,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%time_ins
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%TIME_INS,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%I_Sw
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%I_Sw,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%I_Q
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%I_Q,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%I_Qrain
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%I_QRAIN,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%I_M
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%I_M,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%I_Tau
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%I_TAU,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%I_Sw_Zw
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%I_Sw_Zw,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%I_Q_Ts
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%I_Q_Ts,
     &               global_lats_r,lonsperlar)

      if(icolor.eq.2.and.me.eq.nodes-1) buff1=data%I_M_Ts
      call split2d_r(buff1, buffo,global_lats_r)
      CALL interpred(1,kmsk,buffo,nsst_fld%I_M_Ts,
     &               global_lats_r,lonsperlar)

       IF (icolor.eq.2.and.me.eq.nodes-1) then
         call nsstio_axdbta(data,iret)
         t2=timef()
         print *,'FIXIO for NSST TIME ',t2-t1,t1,t2
       endif
!
      RETURN
 5000 PRINT *, ' ERROR IN INPUT IN FIXIO NSST'
      STOP
      END
!
!***********************************************************************
!
