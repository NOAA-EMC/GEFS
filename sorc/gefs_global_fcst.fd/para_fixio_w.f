       SUBROUTINE PARA_FIXIO_W(IOPROC,sfc_fld, nw,cfile,xhour,idate,
     &                         global_lats_r,lonsperlar)
!!
      use resol_def
      use layout1
      use sfcio_module
      use Sfc_Flx_ESMFMod
      implicit none
!!
      TYPE(Sfc_Var_Data)        :: sfc_fld
!
      integer nw,IOPROC
      character*(*) cfile
      real(kind=kind_io8) xhour
      INTEGER              GLOBAL_LATS_R(latr)
      INTEGER              lonsperlar(latr)
!!
!!
!mi   real(kind=kind_io4) buff4(lonr,latr,4)
      real(kind=kind_ior) buff4(lonr,latr)
      real(kind=kind_io8) bfo(lonr,lats_node_r)
      real(kind=kind_io8) buffi(lonr,lats_node_r)
      integer kmsk(lonr,lats_node_r),kmskcv(lonr,lats_node_r)
      integer idate(4),k,il
!!
!     CHARACTER*8 labfix(4)
!     real(kind=kind_io4) yhour
      integer,save:: version
!CluX data version/200004/
!mi   data version/200412/
!
      type(sfcio_head) head
      type(sfcio_dbta) data
      integer iret
      logical first
      save head, first
      data first /.true./
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
      if (me.eq.ioproc) then
        if (first) then
          head%clabsfc = CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//
     &                   CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)
          head%latb    = latr
          head%lonb    = lonr
          head%ivs     = ivssfc_restart
          head%irealf  = 2
          head%lsoil   = lsoil
          call sfcio_alhead(head,iret)
          head%lpl     = lonsperlar(1:latr/2)
          if (lsoil .eq. 4) then
            head%zsoil   = (/-0.1,-0.4,-1.0,-2.0/)
          elseif (lsoil .eq. 2) then
            head%zsoil   = (/-0.1,-2.0/)
          endif
          first = .false.
        endif
        head%fhour   = xhour
        head%idate   = idate
!
        PRINT 99,nw,xhour,IDATE
99      FORMAT(1H ,'in fixio nw=',i7,2x,'HOUR=',f8.2,3x,'IDATE=',
     &  4(1X,I4))
        call sfcio_aldbta(head,data,iret)
      ENDIF
!!
      kmsk= nint(sfc_fld%slmsk)
!
      CALL uninterpred(1,kmsk,bfo,sfc_fld%tsea,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%tsea=buff4
!
      DO k=1,LSOIL
      buffi(:,:) = sfc_fld%SMC(k,:,:)
      CALL uninterpred(1,kmsk,bfo,buffi,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%smc(:,:,k)=buff4
      ENDDO
!
      CALL uninterpred(1,kmsk,bfo,sfc_fld%SHELEG,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%sheleg=buff4
!
      DO k=1,LSOIL
      buffi(:,:) = sfc_fld%STC(k,:,:)
      CALL uninterpred(1,kmsk,bfo,buffi,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%stc(:,:,k)=buff4
      ENDDO
!
      CALL uninterpred(1,kmsk,bfo,sfc_fld%TG3,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%tg3=buff4
!
      CALL uninterpred(1,kmsk,bfo,sfc_fld%ZORL,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%zorl=buff4
!
      CALL uninterpred(1,kmsk,bfo,sfc_fld%ALVSF,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%alvsf=buff4
!
      CALL uninterpred(1,kmsk,bfo,sfc_fld%ALVWF,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%alvwf=buff4
!
      CALL uninterpred(1,kmsk,bfo,sfc_fld%ALNSF,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%alnsf=buff4
!
      CALL uninterpred(1,kmsk,bfo,sfc_fld%ALNWF,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%alnwf=buff4
!
      CALL uninterpred(1,kmsk,bfo,sfc_fld%SLMSK,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%slmsk=buff4
!
      CALL uninterpred(1,kmsk,bfo,sfc_fld%VFRAC,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%vfrac=buff4
!
      CALL uninterpred(1,kmsk,bfo,sfc_fld%CANOPY,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%canopy=buff4
!
      CALL uninterpred(1,kmsk,bfo,sfc_fld%F10M,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%f10m=buff4

      CALL uninterpred(1,kmsk,bfo,sfc_fld%T2M,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%t2m=buff4

      CALL uninterpred(1,kmsk,bfo,sfc_fld%Q2M,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%q2m=buff4

!
      CALL uninterpred(1,kmsk,bfo,sfc_fld%VTYPE,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%vtype=buff4
!
      CALL uninterpred(1,kmsk,bfo,sfc_fld%STYPE,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%stype=buff4
!
      CALL uninterpred(1,kmsk,bfo,sfc_fld%FACSF,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%facsf=buff4
!
      CALL uninterpred(1,kmsk,bfo,sfc_fld%FACWF,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%facwf=buff4
!
      CALL uninterpred(1,kmsk,bfo,sfc_fld%UUSTAR,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%uustar=buff4
!
      CALL uninterpred(1,kmsk,bfo,sfc_fld%FFMM,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%ffmm=buff4
!
      CALL uninterpred(1,kmsk,bfo,sfc_fld%FFHH,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%ffhh=buff4
!
!c-- XW: FOR SEA-ICE Nov04
      CALL uninterpred(1,kmsk,bfo,sfc_fld%HICE,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%hice=buff4
!
      CALL uninterpred(1,kmsk,bfo,sfc_fld%FICE,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%fice=buff4

      CALL uninterpred(1,kmsk,bfo,sfc_fld%TISFC,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%tisfc=buff4
!c-- XW: END SEA-ICE Nov04
!
!lu: the addition of 8 Noah-related records starts here ........................
!tprcp
      CALL uninterpred(1,kmsk,bfo,sfc_fld%TPRCP,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%tprcp=buff4
!srflag
      CALL uninterpred(1,kmsk,bfo,sfc_fld%SRFLAG,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%srflag=buff4
!snwdph
      CALL uninterpred(1,kmsk,bfo,sfc_fld%SNWDPH,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%snwdph=buff4
!slc
      DO k=1,LSOIL
      buffi(:,:) = sfc_fld%SLC(k,:,:)
      CALL uninterpred(1,kmsk,bfo,buffi,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%slc(:,:,k)=buff4
      ENDDO
!shdmin
      CALL uninterpred(1,kmsk,bfo,sfc_fld%SHDMIN,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%shdmin=buff4
!shdmax
      CALL uninterpred(1,kmsk,bfo,sfc_fld%SHDMAX,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%shdmax=buff4
!slope
      CALL uninterpred(1,kmsk,bfo,sfc_fld%SLOPE,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%slope=buff4
!snoalb
      CALL uninterpred(1,kmsk,bfo,sfc_fld%SNOALB,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%snoalb=buff4
!lu: the addition of 8 Noah records ends here .........................

      CALL uninterpred(1,kmsk,bfo,sfc_fld%ORO,global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%orog=buff4

      if(me.eq.ioproc) then
        call sfcio_swohdc(nw,cfile,head,data,iret)
        call sfcio_axdbta(data,iret)
      endif
!
      return
      end
