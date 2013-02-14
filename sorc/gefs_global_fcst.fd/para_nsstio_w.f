       SUBROUTINE PARA_NSSTIO_W(IOPROC,nsst_fld,nw,cfile,
     &                         xhour,idate,global_lats_r,lonsperlar)
!!
      use resol_def
      use layout1
      use namelist_def
      use nsstio_module
      use Nsstm_ESMFMod
      implicit none
!!
      TYPE(Nsst_Var_Data)        :: nsst_fld
!
      integer nw,IOPROC
      character*(*) cfile
      real(kind=kind_io8) xhour
      INTEGER              GLOBAL_LATS_R(latr)
      INTEGER              lonsperlar(latr)
!!
!!
      real(kind=kind_ior) buff4(lonr,latr)
      real(kind=kind_io8) bfo(lonr,lats_node_r)
      real(kind=kind_io8) buffi(lonr,lats_node_r)
      integer kmsk(lonr,lats_node_r)
      integer idate(4),k,il
!!
!     CHARACTER*8 labfix(4)
!     real(kind=kind_io4) yhour
      integer,save:: version
!
      type(nsstio_head) head
      type(nsstio_dbta) data
      integer iret
      logical first
      save head, first
      data first /.true./
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
      if (me.eq.ioproc) then
        if (first) then
          head%clabnsst= CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)//
     &                   CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)
          head%latb    = latr
          head%lonb    = lonr
          head%ivo     = ivsnsst
          head%irealf  = 2
          head%lsea    = lsea
          call nsstio_alhead(head,iret)
          head%lpl     = lonsperlar(1:latr/2)
!         if (lsea == 0) then
!           head%zsea   = (/-0.1,-2.0/)
!         else
!         endif
          first = .false.
        endif
        head%fhour   = xhour
        head%idate   = idate
!
        PRINT 99,nw,xhour,IDATE
99      FORMAT(1H ,'in fixio nw=',i7,2x,'HOUR=',f8.2,3x,'IDATE=',
     &  4(1X,I4))
        call nsstio_aldbta(head,data,iret)
      ENDIF
!!
      kmsk= nint(nsst_fld%slmsk)
!
      CALL uninterpred(1,kmsk,bfo,nsst_fld%slmsk,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%slmsk=buff4
!
      CALL uninterpred(1,kmsk,bfo,nsst_fld%tref,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%tref=buff4
!
      CALL uninterpred(1,kmsk,bfo,nsst_fld%dt_cool,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%dt_cool=buff4
!
      CALL uninterpred(1,kmsk,bfo,nsst_fld%z_c,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%z_c=buff4
!
      CALL uninterpred(1,kmsk,bfo,nsst_fld%dt_warm,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%dt_warm=buff4
!
      CALL uninterpred(1,kmsk,bfo,nsst_fld%z_w,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%z_w=buff4
!
      CALL uninterpred(1,kmsk,bfo,nsst_fld%C_0,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%C_0=buff4
!
      CALL uninterpred(1,kmsk,bfo,nsst_fld%C_d,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%C_d=buff4
!
      CALL uninterpred(1,kmsk,bfo,nsst_fld%W_0,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%W_0=buff4
!
      CALL uninterpred(1,kmsk,bfo,nsst_fld%W_d,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%W_d=buff4
!
      CALL uninterpred(1,kmsk,bfo,nsst_fld%ifd,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%ifd=buff4
!
      CALL uninterpred(1,kmsk,bfo,nsst_fld%time_old,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%time_old=buff4
!
      CALL uninterpred(1,kmsk,bfo,nsst_fld%time_ins,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%time_ins=buff4
!
      CALL uninterpred(1,kmsk,bfo,nsst_fld%I_Sw,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%I_Sw=buff4
!
      CALL uninterpred(1,kmsk,bfo,nsst_fld%I_Q,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%I_Q=buff4
!
      CALL uninterpred(1,kmsk,bfo,nsst_fld%I_Qrain,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%I_Qrain=buff4
!
      CALL uninterpred(1,kmsk,bfo,nsst_fld%I_Tau,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%I_Tau=buff4
!
      CALL uninterpred(1,kmsk,bfo,nsst_fld%I_Sw_Zw,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%I_Sw_Zw=buff4
!
      CALL uninterpred(1,kmsk,bfo,nsst_fld%I_Q_Ts,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%I_Q_Ts=buff4
!
      CALL uninterpred(1,kmsk,bfo,nsst_fld%I_M_Ts,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%I_M_Ts=buff4
!
      if(me.eq.ioproc) then
        print *,' calling nsstio_swohdc with nw=',nw,' cfile=',cfile
     &,' ivo=',head%ivo,' idate=',head%idate
        call nsstio_swohdc(nw,cfile,head,data,iret)
        call nsstio_axdbta(data,iret)
      endif
!
      return
      end
