      SUBROUTINE WRTSFC(IOPROC,noflx,ZHOUR,FHOUR,IDATE,colat1,SECSWR,
     &                  SECLWR,sfc_fld, flx_fld, fluxr,
     &                  global_lats_r,lonsperlar)
!!
      use machine
      use resol_def
      use layout1
      use sig_io
      use namelist_def
      use Sfc_Flx_ESMFMod
      implicit none
!!
      TYPE(Sfc_Var_Data)        :: sfc_fld
      TYPE(Flx_Var_Data)        :: flx_fld

      INTEGER              GLOBAL_LATS_R(LATR)
      INTEGER              lonsperlar(LATR)
      integer   IOPROC
!!
      integer   IPRS,ITEMP,IZNLW,IMERW,ISPHUM,IPWAT,
     $          IPCPR,ISNOWD,ICLDF,ICCLDF,
     $          ISLMSK,IZORL,IALBDO,ISOILM,ISNOHF,
     $          ISMCWLT,ISMCREF,ICEMSK,
     $          ILHFLX,ISHFLX,IZWS,IMWS,IGHFLX,
     $          IUSWFC,IDSWFC,IULWFC,IDLWFC,
     $          INSWFC,INLWFC,
     $          IDSWVB,IDSWVD,IDSWNB,IDSWND,
     $          ITMX,ITMN,IRNOF,IEP,
!jwang add IQMX,IQMN
     &          IQMX,IQMN,
     &          ICLDWK,IZGW,IMGW,IHPBL,
     $          IDSWF,IDLWF,IUSWF,IULWF,ICPCPR,
     $          ISFC,ITOA,IELEV,
     $          ISGLEV,IDBLS,I2DBLS,ICOLMN,
     $          IBLBL,IBLTL,IBLLYR,
     $          ILCBL,ILCTL,ILCLYR,
     $          IMCBL,IMCTL,IMCLYR,
     $          IHCBL,IHCTL,IHCLYR,
     $          ICVBL,ICVTL,ICVLYR,
     $          INST,IWIN,IAVG,IACC,
     $          IFHOUR,IFDAY,
!    $          LEN,NFLD,
     $          NFLD,
     $          IUVBF,IUVBFC,
!yth add ISUNTM for sunshine time sep/08
     &          ISUNTM,
     $   j,i,k,itop,ibot,k4,l,noflx
     &,         ISIK                                    ! FOR SEA-ICE - XW Nov04
!Clu [+1L]: declare additional parameter index
     +,         ISLC,ISNOD,ICNP
     &,  iveg, ivtp, istp, islo,iust,ihgt,irst,ichh
     &,  icmm,isrf,ievbs,ievcw,itran,isbs,isnc,istc
!*RADFLX*
     +,  ICSUSW,ICSDSW,ICSULW,ICSDLW

!*RADFLX
!*    PARAMETER(NFLD=18)
      PARAMETER(NFLD=25)
       integer ilpds,iyr,imo,ida,ihr,ifhr,ithr,lg,ierr
       real (kind=kind_io8) RTIMER(NFLD),rtime,rtimsw,rtimlw
       real (kind=kind_io8) colat1
       real (kind=kind_io8) cl1,secswr,zhour,fhour,seclwr
!
      PARAMETER(IPRS=1,ITEMP=11,IZNLW=33,IMERW=34,ISPHUM=51,IPWAT=54,
     $          IPCPR=59,ISNOWD=65,ICLDF=71,ICCLDF=72,
     $          ISLMSK=81,IZORL=83,IALBDO=84,ISOILM=144,ICEMSK=91,
     $          ISIK=92,                                ! FOR SEA-ICE - XW Nov04
     $          ILHFLX=121,ISHFLX=122,IZWS=124,IMWS=125,IGHFLX=155,
     $          IUSWFC=160,IDSWFC=161,IULWFC=162,IDLWFC=163,
     $          INSWFC=164,INLWFC=165,
     $          IDSWVB=166,IDSWVD=167,IDSWNB=168,IDSWND=169,
     $          ITMX=15,ITMN=16,IRNOF=90,IEP=145,
!jwang add IQMX IQMN
     &          IQMX=204,IQMN=205,
     &          ICLDWK=146,IZGW=147,IMGW=148,IHPBL=221,
     $          IDSWF=204,IDLWF=205,IUSWF=211,IULWF=212,ICPCPR=214,
!*RADFLX
!*   &          IUVBF=200,IUVBFC=201)
!    +          IUVBF=200,IUVBFC=201,
!yth  add ISUNTM for sunshine time  sep/08
     &          IUVBF=200,IUVBFC=201,ISUNTM=191,
     +          ICSUSW=160,ICSDSW=161,ICSULW=162,ICSDLW=163)
      PARAMETER(ISFC=1,ITOA=8,IELEV=105,
     $          ISGLEV=109,IDBLS=111,I2DBLS=112,ICOLMN=200,
!Cwei    $          ISGLEV=107,IDBLS=111,I2DBLS=112,ICOLMN=200,
     $          IBLBL=209,IBLTL=210,IBLLYR=211,
     $          ILCBL=212,ILCTL=213,ILCLYR=214,
     $          IMCBL=222,IMCTL=223,IMCLYR=224,
     $          IHCBL=232,IHCTL=233,IHCLYR=234,
     $          ICVBL=242,ICVTL=243,ICVLYR=244)

!Clu [+1L]: define parameter index, using Table 130
      PARAMETER(ISLC=160,ISNOD=66)
!Cwei
      PARAMETER(ISLO=222,ISBS=198,ISNC=238,ICMM=179)
      PARAMETER(ISNOHF=229,ISMCWLT=219,ISMCREF=220)
!Clu [+1L]: define parameter index, using Table 2
      PARAMETER(ICNP=223)
!Cwei
      PARAMETER(IVEG=87,IVTP=225,ISTP=224,IUST=253,IHGT=7,
     $          IRST=140,ICHH=208,ISRF=235,IEVBS=199,
     $          IEVCW=200,ITRAN=210,ISTC=86)

      PARAMETER(INST=10,IWIN=2,IAVG=3,IACC=4)
      PARAMETER(IFHOUR=1,IFDAY=2)
!     PARAMETER(LEN=lonr*latr)
      real(kind=kind_io4) wrkga(lonr*latr),wrkgb(lonr*latr)
      real(kind=kind_io8) slmskful(lonr*latr)
      real(kind=kind_io8) slmskloc(LONR,LATS_NODE_R)
!
      LOGICAL(1) LBM(lonr*latr)
      CHARACTER G(200+lonr*latr*(16+1)/8)
      INTEGER   IPUR(NFLD),ITLR(NFLD)
      DATA      IPUR/IULWF , IUSWF , IUSWF , IDSWF ,  ICLDF,   IPRS,
     $                 IPRS, ITEMP ,  ICLDF,   IPRS,   IPRS, ITEMP ,
!*RADFLX
!*   $                ICLDF,   IPRS,   IPRS, ITEMP ,  IUVBF, IUVBFC /
     +                ICLDF,   IPRS,   IPRS, ITEMP ,  IUVBF, IUVBFC ,
     +                IDSWF, ICSULW, ICSUSW, ICSDLW, ICSUSW, ICSDSW,
     +                ICSULW /
      DATA      ITLR/ITOA  , ITOA  , ISFC  , ISFC  , IHCLYR, IHCTL ,
     $               IHCBL , IHCTL , IMCLYR, IMCTL , IMCBL , IMCTL ,
!*RADFLX
!*   $               ILCLYR, ILCTL , ILCBL , ILCTL , ISFC  , ISFC /
     +               ILCLYR, ILCTL , ILCBL , ILCTL , ISFC  , ISFC ,
     +               ITOA  ,  ITOA ,  ITOA ,  ISFC , ISFC  , ISFC,
     +               ISFC /
!    $               ILCLYR, ILCTL , ILCBL , ILCTL /
      INTEGER     IDATE(4), IDS(255)
!     INTEGER     IDATE(4), IDS(255),IENS(5)
      real (kind=kind_io8) SI(LEVP1)
!
!sela..................................................................
*RADFLX
!*    real (kind=kind_io8)   rflux(lonr,LATS_NODE_R,27)
      real (kind=kind_io8)   rflux(lonr,LATS_NODE_R,NFXR)
      real (kind=kind_io8)   glolal(lonr,LATS_NODE_R)
      real (kind=kind_io8)   buffo(lonr,LATS_NODE_R)
      real (kind=kind_io4)   buff1(lonr,latr)
      real (kind=kind_io4)   buff1l(lonr*latr)
!sela..................................................................
      real (kind=kind_io8)  FLUXR(nfxr,LONR,LATS_NODE_R)
!sela..................................................................
      integer kmsk(lonr,lats_node_r),kmsk0(lonr,lats_node_r)
      integer kmskcv(lonr,LATS_NODE_R),il
!jfe
      IDS=0
      G=' '
!jfe
!!
      kmsk  = nint(sfc_fld%slmsk)
      kmsk0 = 0
      CALL uninterpred(1,kmsk,glolal,sfc_fld%slmsk,
     &                 global_lats_r,lonsperlar)
      slmskloc = glolal
      call unsplit2d(ioproc,buff1l,glolal,global_lats_r)
      slmskful = buff1l
c
!*    do k=1,27       !*RADFLX
      do k=1,NFXR
       do j=1,LATS_NODE_R
        do i=1,lonr
         rflux(i,j,k)=fluxr(k,i,j)
        enddo
       enddo
      enddo
!!
      CALL IDSDEF(1,IDS)
!jwang add spfhmax/spfhmin
      ids(IQMX)   = 5
      ids(IQMN)   = 5
! UV-B scaling factor, if set up already, comment the next 2 lines out
      ids(IUVBF)  = 2
      ids(IUVBFC) = 2
! Ice conentration and thickness scaling factor
      ids(icemsk) = 3      ! ICE CONCENTRATION ()
      ids(isik)   = 2      ! ICE THICKNESS (M)
!
!wei added 10/24/2006
      ids(IZORL)  = 4
      ids(IHGT)   = 3
      ids(IVEG)   = 2
      ids(IUST)   = 3
      ids(ICHH)   = 4
      ids(ICMM)   = 4
      ids(ISRF)   = 5
      ids(ITEMP)  = 3
      ids(ISPHUM) = 6
      ids(IZNLW)  = 2
      ids(IMERW)  = 2
      ids(ISNC)   = 3
      ids(ISTC)   = 4
      ids(ISOILM) = 4
      ids(ISNOD)  = 6
      ids(ISNOWD) = 5
      ids(ICNP)   = 5
      ids(IPCPR)  = 6
      ids(ICPCPR) = 6
      ids(IRNOF)  = 5                                                                                                                          
      ids(ISMCWLT)  = 4
      ids(ISMCREF)  = 4

      ILPDS   = 28
      IF(ICEN2.EQ.2) ILPDS=45
      IENS(1) = 1
      IENS(2) = IENST
      IENS(3) = IENSI
      IENS(4) = 1
      IENS(5) = 255
      IYR     = IDATE(4)
      IMO     = IDATE(2)
      IDA     = IDATE(3)
      IHR     = IDATE(1)
      IFHR    = NINT(ZHOUR)
      ITHR     = NINT(FHOUR)
      IF(FHOUR.GT.ZHOUR) THEN
        RTIME = 1./(3600.*(FHOUR-ZHOUR))
      ELSE
        RTIME = 0.
      ENDIF
      IF(SECSWR.GT.0.) THEN
        RTIMSW = 1./SECSWR
      ELSE
        RTIMSW = 1.
      ENDIF
      IF(SECLWR.GT.0.) THEN
        RTIMLW = 1./SECLWR
      ELSE
        RTIMLW = 1.
      ENDIF
      RTIMER    = RTIMSW
      RTIMER(1) = RTIMLW
!*RADFLX*
      RTIMER(20)=RTIMLW       ! CSULF_TOA
      RTIMER(22)=RTIMLW       ! CSDLF_SFC
      RTIMER(25)=RTIMLW       ! CSULF_SFC
!*RADFLX*
      CL1       = colat1         
!!
!..........................................................
      glolal = flx_fld%DUSFC*RTIME
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
!
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IZWS,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IZWS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '01)Zonal compt of momentum flux (N/m**2) land and sea surface '
      endif

      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      glolal = flx_fld%DVSFC*RTIME
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IMWS,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IMWS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '02)Merid compt of momentum flux (N/m**2) land and sea surface '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      glolal = flx_fld%DTSFC*RTIME
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ISHFLX,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ISHFLX),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '03)Sensible heat flux (W/m**2) land and sea surface           '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      glolal = flx_fld%DQSFC*RTIME
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ILHFLX,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ILHFLX),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '04)Latent heat flux (W/m**2) land and sea surface             '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      CALL uninterpred(2,kmsk0,buffo,sfc_fld%tsea,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ITEMP,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '05)Temperature (K) land and sea surface                       '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      glolal(:,:) = sfc_fld%SMC(1,:,:)
      CALL uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM = slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISOILM,I2DBLS,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '06)Volumetric soil moist content (frac) layer 10cm and 0cm    '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
!..........................................................
!lu [-3L/+3L]: change 10-200cm to 10-40cm for smc(2)
      glolal(:,:) = sfc_fld%SMC(2,:,:)
!lu   CALL uninterpred(1,kmsk,buffo,glolal,
      CALL uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM = slmskful.EQ.1._kind_io8
      if(lsoil.gt.2)then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
!lu  &              1,ISOILM,I2DBLS,10,200,IYR,IMO,IDA,IHR,
     +              1,ISOILM,I2DBLS,10,40,IYR,IMO,IDA,IHR,
     &              IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!lu  x '07)Volumetric soil moist content (frac) layer 200cm and 10cm  '
     + '07)Volumetric soil moist content (frac) layer 40cm and 10cm  '
      else
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              1,ISOILM,I2DBLS,10,200,IYR,IMO,IDA,IHR,
     &              IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '07)Volumetric soil moist content (frac) layer 200cm and 10cm  '
      endif
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      glolal(:,:) = sfc_fld%STC(1,:,:)
      CALL uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ITEMP,I2DBLS,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '08)Temp (K) layer betw two depth below land sfc 10cm and 0cm  '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
!lu [-2L/+2L]: change 10-200 to 10-40 for stc(2)
      glolal(:,:) = sfc_fld%STC(2,:,:)
      CALL uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM = slmskful.EQ.1._kind_io8
      if(lsoil.gt.2)then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
!lu  &              1,ITEMP,I2DBLS,10,200,IYR,IMO,IDA,IHR,
     +              1,ITEMP,I2DBLS,10,40,IYR,IMO,IDA,IHR,
     &              IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!lu  x '09)Temp (K) layer betw two depth below land sfc 200cm and 10cm'
     + '09)Temp (K) layer betw two depth below land sfc 40cm and 10cm'
      else
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              1,ITEMP,I2DBLS,10,200,IYR,IMO,IDA,IHR,
     &              IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '09)Temp (K) layer betw two depth below land sfc 200cm and 10cm'
      endif
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      CALL uninterpred(2,kmsk,buffo,sfc_fld%sheleg,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ISNOWD,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISNOWD),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '10)Water equiv of accum snow depth (kg/m**2) land sea surface '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
c..........................................................
      glolal = flx_fld%DLWSFC*RTIME
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IDLWF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IDLWF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '11)Downward long wave radiation flux (W/m**2) land sea surface'
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      glolal = flx_fld%ULWSFC*RTIME
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IULWF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IULWF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '12)Upward long wave radiation flux (W/m**2) land sea surface  '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
!.......  FIX FLUXES FOR APPROX DIURNAL CYCLE
      DO 113 K=1,4
       do j=1,LATS_NODE_R
        do i=1,lonr
         glolal(i,j) = rflux(i,j,k)*RTIMER(k)
        enddo
       enddo
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              0,IPUR(K),ITLR(K),0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IPUR(K)),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0.and.k.eq.1)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '13)Upward long wave radiation flux (W/m**2) top of atmosphere '
      if(ierr.ne.0.and.k.eq.2)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '14)Upward solar radiation flux (W/m**2) top of atmosphere     '
      if(ierr.ne.0.and.k.eq.3)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '15)Upward solar radiation flux (W/m**2) land and sea surface  '
      if(ierr.ne.0.and.k.eq.4)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '16)Downward solar radiation flux (W/m**2) land and sea surface'
      endif
        IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
  113 CONTINUE
!..........................................................
!
!     For UV-B fluxes
!
      do j=1,LATS_NODE_R
        do i=1,lonr
          glolal(i,j) = rflux(i,j,21)*rtimsw
        enddo
      enddo
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,129,ICEN,IGEN,
     &            0,IUVBF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IUVBF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '17)UV-B Downward solar flux (W/m**2) land sea surface'
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
      do j=1,LATS_NODE_R
        do i=1,lonr
          glolal(i,j) = rflux(i,j,22)*rtimsw
        enddo
      enddo
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,129,ICEN,IGEN,
     &            0,IUVBFC,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IUVBFC),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '18)clear sky UV-B Downward solar flux (W/m**2) land sea surface'
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!
!     End UV-B fluxes
!
!..........................................................
!..........................................................
      DO 813 K=5,7
!
       do j=1,LATS_NODE_R
        do i=1,lonr
         glolal(i,j) = rflux(i,j,k)*100.*rtimsw      
        enddo
       enddo
      where(glolal.ge.0.5)
        kmskcv = 1
      elsewhere
        kmskcv = 0
      endwhere
!!
      CALL uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
!
        K4=4+(K-5)*4
        L=K4+1
        LBM=wrkga.Ge.0.5_kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              0,IPUR(L),ITLR(L),0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IPUR(L)),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0.and.k.eq.5)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '19)Total cloud cover (percent) high cloud layer               '
      if(ierr.ne.0.and.k.eq.6)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '23)Total cloud cover (percent) middle cloud layer             '
      if(ierr.ne.0.and.k.eq.7)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '27)Total cloud cover (percent) low cloud layer                '
      endif
        IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!
       do j=1,LATS_NODE_R
        do i=1,lonr
         if(rflux(i,j,k).gt.0.)then
          glolal(i,j) = rflux(i,j,k+3)*1000./rflux(i,j,k)      
         else
          glolal(i,j) = 0.
         endif
        enddo
       enddo
      CALL uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
!
        L=K4+2
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              1,IPUR(L),ITLR(L),0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IPUR(L)),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0.and.k.eq.5)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '20)Pressure (Pa) high cloud top level                         '
      if(ierr.ne.0.and.k.eq.6)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '24)Pressure (Pa) middle cloud top level                       '
      if(ierr.ne.0.and.k.eq.7)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '28)Pressure (Pa) low cloud top level                          '
      endif
        IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!
       do j=1,LATS_NODE_R
        do i=1,lonr
         if(rflux(i,j,k).gt.0.)then
          glolal(i,j) = rflux(i,j,k+6)*1000./rflux(i,j,k)      
         else
          glolal(i,j) = 0.
         endif
        enddo
       enddo
      CALL uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
!
        L=K4+3
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              1,IPUR(L),ITLR(L),0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IPUR(L)),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0.and.k.eq.5)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '21)Pressure (Pa) high cloud bottom level                      '
      if(ierr.ne.0.and.k.eq.6)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '25)Pressure (Pa) middle cloud bottom level                    '
      if(ierr.ne.0.and.k.eq.7)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '29)Pressure (Pa) low cloud bottom level                       '
      endif
        IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!
       do j=1,LATS_NODE_R
        do i=1,lonr
         if(rflux(i,j,k).gt.0.)then
          glolal(i,j) = rflux(i,j,k+9)/rflux(i,j,k)      
         else
          glolal(i,j) = 0.
         endif
        enddo
       enddo
      CALL uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
        L=K4+4
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              1,IPUR(L),ITLR(L),0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IPUR(L)),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0.and.k.eq.5)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '22)Temperature (K) high cloud top level                       '
      if(ierr.ne.0.and.k.eq.6)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '26)Temperature (K) middle cloud top level                     '
      if(ierr.ne.0.and.k.eq.7)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '30)Temperature (K) low cloud top level                        '
      endif
        IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!
  813 CONTINUE
!!
!...................................................................
      glolal = flx_fld%GESHEM*1.E3*RTIME
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IPCPR,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IPCPR),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '31)Precipitation rate (kg/m**2/s) land and sea surface        '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!...................................................................
      glolal = flx_fld%BENGSH*1.E3*RTIME
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ICPCPR,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ICPCPR),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '32)Convective precipitation rate (kg/m**2/s) land sea surface '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!...................................................................
      glolal = flx_fld%GFLUX*RTIME
      CALL uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.NE.0._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IGHFLX,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IGHFLX),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '33)Ground heat flux (W/m**2) land and sea surface             '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!...................................................................
      buffo = MOD(slmskloc,2._kind_io8)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ISLMSK,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISLMSK),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '34)Land-sea mask (1=land; 0=sea) (integer) land sea surface   '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!...................................................................
!c-- XW: FOR SEA-ICE Nov04
!     buffo=MAX(slmskloc-1._kind_io8,0._kind_io8)
!     call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
!     if(me.eq.ioproc) then
!     call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
!    &            0,ICEMSK,ISFC,0,0,IYR,IMO,IDA,IHR,
!    &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ICEMSK),IENS,
!    &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '35)Ice concentration (ice=1; no ice=0) (1/0) land sea surface '
!     endif
      CALL uninterpred(2,kmsk0,buffo,sfc_fld%fice,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ICEMSK,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ICEMSK),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '35)Ice concentration (ice>0; no ice=0) (1/0) land sea surface '
      endif
!c-- XW: END SEA-ICE
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!...................................................................
      CALL uninterpred(2,kmsk0,buffo,flx_fld%u10m,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IZNLW,IELEV,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IZNLW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '36)u wind (m/s) height above ground                           '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!...................................................................
      CALL uninterpred(2,kmsk0,buffo,flx_fld%v10m,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IMERW,IELEV,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IMERW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '37)v wind (m/s) height above ground                           '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!...................................................................
      CALL uninterpred(2,kmsk0,buffo,sfc_fld%t2m,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ITEMP,IELEV,0,2,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '38)Temperature (K) height above ground                        '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!...................................................................
      CALL uninterpred(2,kmsk0,buffo,sfc_fld%q2m,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ISPHUM,IELEV,0,2,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISPHUM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '39)Specific humidity (kg/kg) height above ground              '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!...................................................................
      glolal = flx_fld%PSURF*1.E3
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IPRS,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IPRS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '40)Pressure (Pa) land and sea surface                         '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!...................................................................
      CALL uninterpred(2,kmsk0,buffo,flx_fld%tmpmax,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ITMX,IELEV,0,2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IWIN,0,0,ICEN2,IDS(ITMX),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '41)Maximum temperature (K) height above ground                '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!...................................................................
      CALL uninterpred(2,kmsk0,buffo,flx_fld%tmpmin,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ITMN,IELEV,0,2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IWIN,0,0,ICEN2,IDS(ITMN),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '42)Minimum temperature (K) height above ground                '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!...................................................................
!jwang add spfhmax/spfhmin
      CALL uninterpred(2,kmsk0,buffo,flx_fld%spfhmax,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,133,ICEN,IGEN,
     &            0,IQMX,IELEV,0,2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IWIN,0,0,ICEN2,IDS(IQMX),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '41a)Maximum specific humidity (kg/kg) height above ground      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!...................................................................
      CALL uninterpred(2,kmsk0,buffo,flx_fld%spfhmin,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,133,ICEN,IGEN,
     &            0,IQMN,IELEV,0,2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IWIN,0,0,ICEN2,IDS(IQMN),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '42a)Minimum specific humidity (kg/kg) height above ground      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!...................................................................
      glolal = flx_fld%RUNOFF * 1.E3
      CALL uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.NE.0._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IRNOF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IACC,0,0,ICEN2,IDS(IRNOF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '43)Runoff (kg/m**2) land and sea surface                      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!...................................................................
      glolal = flx_fld%EP * RTIME
      CALL uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.NE.0._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IEP,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IEP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '44)Potential evaporation rate (w/m**/) land and sea surface   '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!...................................................................
      glolal = flx_fld%CLDWRK * RTIME
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ICLDWK,ICOLMN,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ICLDWK),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '45)Cloud work function (J/Kg) total atmospheric column        '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!...................................................................
      glolal = flx_fld%DUGWD*RTIME
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IZGW,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IZGW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '46)Zonal gravity wave stress (N/m**2) land and sea surface    '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!...................................................................
      glolal = flx_fld%DVGWD*RTIME
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IMGW,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IMGW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '47)Meridional gravity wave stress (N/m**2) land sea surface   '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!...................................................................
      CALL uninterpred(2,kmsk0,buffo,flx_fld%hpbl,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IHPBL,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IHPBL),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '48)Boundary layer height '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!...................................................................
      CALL uninterpred(2,kmsk0,buffo,flx_fld%pwat,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IPWAT,ICOLMN,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IPWAT),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '49)Precipitable water (kg/m**2) total atmospheric column      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!...................................................................
!
       do j=1,LATS_NODE_R
        do i=1,lonr
         if (rflux(i,j,4).GT.0.) then
          glolal(i,j) = rflux(i,j,3)/rflux(i,j,4) * 100.
          if (glolal(i,j).GT.100.) glolal(i,j) = 100.
         else
          glolal(i,j) = 0.
         endif
        enddo
       enddo
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
!
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IALBDO,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IALBDO),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '50)Albedo (percent) land and sea surface                      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!
       do j=1,LATS_NODE_R
        do i=1,lonr
         glolal(i,j) = rflux(i,j,26)*100.*rtimsw
        enddo
       enddo
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
!
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              0,ICLDF,ICOLMN,0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ICLDF),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '51)Total cloud cover (percent) total atmospheric column       '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!
! CONVECTIVE CLOUDS
! LABELED INSTANTANEOUS BUT ACTUALLY AVERAGED OVER FHSWR HOURS
!
      glolal = sfc_fld%CV*1.E2
      where(glolal.ge.0.5)
        kmskcv = 1
      elsewhere
        kmskcv = 0
      endwhere
      CALL uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=wrkga.Ge.0.5_kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ICLDF,ICVLYR,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ICLDF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '52)Total cloud cover (percent) convective cloud layer         '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
c.................................................
       do j=1,LATS_NODE_R
        do i=1,lonr
        glolal(i,j) = 0.
        IF(sfc_fld%CV(i,j).GT.0.) THEN
!        ITOP=NINT(CVT(i,j))
!        IF(ITOP.GE.1.AND.ITOP.LE.LEVS)
!    &   glolal(i,j)=SI(ITOP+1)*PSURF(i,j)*1.E3
!...      cvt already a pressure (cb)...convert to Pa
         glolal(i,j) = sfc_fld%CVT(i,j)*1.E3
        END IF
       ENDDO
      ENDDO
      CALL uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IPRS,ICVTL,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IPRS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '53)Pressure (Pa) convective cloud top level                   '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!.................................................
       do j=1,LATS_NODE_R
        do i=1,lonr
        glolal(i,j) = 0.
        IF(sfc_fld%CV(i,j).GT.0.) THEN
!        Ibot=NINT(CVB(i,j))
!        IF(Ibot.GE.1.AND.Ibot.LE.LEVS)
!    &   glolal(i,j)=SI(IBOT)*PSURF(i,j)*1.E3
!...      cvb already a pressure (cb)...convert to Pa
         glolal(i,j) = SFC_fld%CVB(i,j)*1.E3
        END IF
       ENDDO
      ENDDO
      CALL uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IPRS,ICVBL,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IPRS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '54)Pressure (Pa) convective cloud bottom level                '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!.................................................
!...   SAVE B.L. CLOUD AMOUNT
!
       do j=1,LATS_NODE_R
        do i=1,lonr
         glolal(i,j) = rflux(i,j,27)*100.*rtimsw
        enddo
       enddo
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
!
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              0,ICLDF,IBLLYR,0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ICLDF),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '55)Total cloud cover (percent) boundary layer cloud layer     '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!c-- XW: FOR SEA-ICE Nov04
      CALL uninterpred(2,kmsk0,buffo,sfc_fld%hice,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.NE.1._kind_io8
!     LBM=slmskful.EQ.2._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISIK,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISIK),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '56)Sea ice thickness (m) category 1'
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!c-- XW: END SEA-ICE
!.................................................
!lu: add smc(3:4), stc(3:4), slc(1:4), snwdph, canopy
!lu: addition of 10 records starts here -------------------------------
      if(lsoil.gt.2)then
        glolal(:,:) = sfc_fld%SMC(3,:,:)
        CALL uninterpred(2,kmsk,buffo,glolal,
     &                   global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
        if(me.eq.ioproc) then
        LBM=slmskful.EQ.1._kind_io8
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              1,ISOILM,I2DBLS,40,100,IYR,IMO,IDA,IHR,
     &              IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x   '57)Volumetric soil moist content (frac) layer 100cm and 40cm '
        endif
        IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
        glolal(:,:) = sfc_fld%SMC(4,:,:)
        CALL uninterpred(2,kmsk,buffo,glolal,
     &                   global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
        if(me.eq.ioproc) then
        LBM=slmskful.EQ.1._kind_io8
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              1,ISOILM,I2DBLS,100,200,IYR,IMO,IDA,IHR,
     &              IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '58)Volumetric soil moist content (frac) layer 200cm and 100cm '
        endif
        IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
        glolal(:,:) = sfc_fld%STC(3,:,:)
        CALL uninterpred(2,kmsk,buffo,glolal,
     &                   global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
        if(me.eq.ioproc) then
        LBM=slmskful.EQ.1._kind_io8
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              1,ITEMP,I2DBLS,40,100,IYR,IMO,IDA,IHR,
     &              IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '59)Temp (K) layer betw two depth below land sfc 100cm and 40cm'
        endif
        IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
        glolal(:,:) = sfc_fld%STC(4,:,:)
        CALL uninterpred(2,kmsk,buffo,glolal,
     &                   global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
        if(me.eq.ioproc) then
        LBM=slmskful.EQ.1._kind_io8
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              1,ITEMP,I2DBLS,100,200,IYR,IMO,IDA,IHR,
     &              IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '60)Temp (K) layer betw two depth below land sfc 200cm and 100cm'
        endif
        IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
      endif
!..........................................................
      glolal(:,:) = sfc_fld%SLC(1,:,:)
      CALL uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISLC,I2DBLS,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '61)Liquid soil moist content (frac) layer 10cm and 0cm  '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      glolal(:,:) = sfc_fld%SLC(2,:,:)
      CALL uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      if(lsoil.gt.2)then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &              1,ISLC,I2DBLS,10,40,IYR,IMO,IDA,IHR,
     &              IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '62)Liquid soil moist content (frac) layer 40cm and 10cm '
      else
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &              1,ISLC,I2DBLS,10,200,IYR,IMO,IDA,IHR,
     &              IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '62)Liquid soil moist content (frac) layer 200cm and 10cm '
      endif
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      if(lsoil.gt.2)then
        glolal(:,:) = sfc_fld%SLC(3,:,:)
        CALL uninterpred(2,kmsk,buffo,glolal,
     &                   global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
        if(me.eq.ioproc) then
        LBM=slmskful.EQ.1._kind_io8
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &              1,ISLC,I2DBLS,40,100,IYR,IMO,IDA,IHR,
     &              IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '63)Liquid soil moist content (frac) layer 100cm and 40cm'
        endif
        IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
        glolal(:,:) = sfc_fld%SLC(4,:,:)
        CALL uninterpred(2,kmsk,buffo,glolal,
     &                   global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
        if(me.eq.ioproc) then
        LBM=slmskful.EQ.1._kind_io8
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &              1,ISLC,I2DBLS,100,200,IYR,IMO,IDA,IHR,
     &              IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
        if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '64)Liquid soil moist content (frac) layer 200cm and 100cm'
        endif
        IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
      endif
!..........................................................
      glolal = sfc_fld%SNWDPH / 1.E3       !! convert from mm to m
      CALL uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
!     LBM=slmskful.EQ.1._kind_io8
      LBM=slmskful.EQ.1._kind_io8 .or. slmskful.EQ.2._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISNOD,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISNOD),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '65)Snow depth (m) land surface                  '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      CALL uninterpred(2,kmsk,buffo,sfc_fld%canopy,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ICNP,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ICNP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '66)Canopy water content (kg/m^2) land surface      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!lu: addition of 10 records ends here -------------------------------
!
!wei: addition of 30 records starts here -------------------------------
!..........................................................
      glolal = sfc_fld%ZORL / 1.E2       !! convert from cm to m
      CALL uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IZORL,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IZORL),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '67)Surface roughness (m)    '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      glolal = sfc_fld%vfrac*100.
      CALL uninterpred(1,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IVEG,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IVEG),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '68)Vegetation fraction (fractional) land surface      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      CALL uninterpred(1,kmsk,buffo,sfc_fld%vtype,
     &                 global_lats_r,lonsperlar)
!     buffo=MOD(glolal,2._kind_io8)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IVTP,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IVTP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '69)Vegetation type land surface      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      CALL uninterpred(1,kmsk,buffo,sfc_fld%stype,
     &                 global_lats_r,lonsperlar)
!     buffo=MOD(glolal,2._kind_io8)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISTP,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISTP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '70)Soil type land surface      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      CALL uninterpred(1,kmsk,buffo,sfc_fld%slope,
     &                 global_lats_r,lonsperlar)
!     buffo = MOD(glolal,2._kind_io8)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM = slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISLO,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISLO),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '71)Slope type land surface      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      CALL uninterpred(2,kmsk0,buffo,sfc_fld%uustar,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IUST,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IUST),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '72)Frictional velocity (m/s)'
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      CALL uninterpred(1,kmsk0,buffo,sfc_fld%oro,
     &                global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IHGT,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IHGT),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '73)Surface height (m)       '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      CALL uninterpred(1,kmsk,buffo,sfc_fld%srflag,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IRST,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IRST),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '74)Freezing precip flag land surface      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      CALL uninterpred(2,kmsk0,buffo,flx_fld%chh,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ICHH,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ICHH),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '75)Exchange coefficient CH(m/s)       '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      CALL uninterpred(2,kmsk0,buffo,flx_fld%cmm,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            0,ICMM,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ICMM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '76)Exchange coefficient CM(m/s)      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      CALL uninterpred(2,kmsk,buffo,flx_fld%epi,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IEP,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IEP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '77)Potential evaporation rate (w/m**2) land and sea surface   '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      CALL uninterpred(2,kmsk0,buffo,flx_fld%DLWSFCI,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IDLWF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IDLWF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '78)Downward long wave radiation flux (W/m**2) '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      CALL uninterpred(2,kmsk0,buffo,flx_fld%ULWSFCI,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IULWF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IULWF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '79)Upward long wave radiation flux (W/m**2)  '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      CALL uninterpred(2,kmsk0,buffo,flx_fld%USWSFCI,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IUSWF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IUSWF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '80)Upward short wave radiation flux (W/m**2)  '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      CALL uninterpred(2,kmsk0,buffo,flx_fld%DSWSFCI,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IDSWF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IDSWF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '81)Downward short wave radiation flux (W/m**2)  '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      CALL uninterpred(2,kmsk0,buffo,flx_fld%DTSFCI,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ISHFLX,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISHFLX),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '82)Sensible heat flux (W/m**2) land and sea surface           '
        endif
      endif
!..........................................................
      CALL uninterpred(2,kmsk0,buffo,flx_fld%DQSFCI,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ILHFLX,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ILHFLX),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '83)Latent heat flux (W/m**2) land and sea surface             '
        endif
      endif
!..........................................................
      CALL uninterpred(2,kmsk,buffo,flx_fld%GFLUXI,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
        LBM=slmskful.NE.0._kind_io8
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IGHFLX,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IGHFLX),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(IERR.EQ.0) then
          CALL WRYTE(noflx,LG,G)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '84)Ground heat flux (W/m**2) land and sea surface             '
        endif
      endif
!..........................................................
      glolal = flx_fld%SRUNOFF * 1.E3
      CALL uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISRF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISRF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '85)Surface runoff (kg/m^2) land surface      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      CALL uninterpred(2,kmsk0,buffo,flx_fld%t1,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ITEMP,isglev,1,1,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '86)Lowest model level Temp (K)       '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      CALL uninterpred(2,kmsk0,buffo,flx_fld%q1,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,ISPHUM,isglev,1,1,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISPHUM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '87)Lowest model specific humidity (kg/kg)      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      CALL uninterpred(2,kmsk0,buffo,flx_fld%u1,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IZNLW,isglev,1,1,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IZNLW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '88)Lowest model u wind (m/s)      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      CALL uninterpred(2,kmsk0,buffo,flx_fld%v1,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IMERW,isglev,1,1,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IMERW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '89)Lowest model v wind (m/s)      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      CALL uninterpred(2,kmsk,buffo,flx_fld%zlvl,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IHGT,isglev,1,1,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(IHGT),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '90)Lowest model level height (m) land surface      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      glolal = flx_fld%EVBSA*RTIME
      CALL uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IEVBS,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IEVBS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '91)Direct evaporation from bare soil(W/m^2) land surface      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      glolal = flx_fld%EVCWA*RTIME
      CALL uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IEVCW,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IEVBS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '92)Canopy water evaporation(W/m^2) land surface      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      glolal = flx_fld%TRANSA*RTIME
      CALL uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ITRAN,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ITRAN),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '93)Transpiration (W/m^2) land surface      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      glolal = flx_fld%SBSNOA*RTIME
      CALL uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISBS,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ISBS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '94)Snow Sublimation (W/m^2) land surface      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      glolal = flx_fld%SNOWCA*RTIME*100.
      CALL uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISNC,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ISNC),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '95)Snow Cover (fraction) land surface      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      glolal = flx_fld%soilm*1.E3       !! convert from m to (mm)kg/m^2
      CALL uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISTC,I2DBLS,0,200,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISTC),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '96)Total column soil moisture (Kg/m^2) land surface      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)


!*RADFLX*
!Clu: Addition of 7 records starts here -------------------------------
!dswrf_toa, csulf_toa, csusf_toa, csdlf_sfc, csusf_sfc, csdsf_sfc, csulf_sfc

      DO 115 K=19, 25
       if (k .eq. 19) then
          L = 18
        else
          L = k + 8
       endif
       do j=1,LATS_NODE_R
        do i=1,lonr
         glolal(i,j)=rflux(i,j,L)*RTIMER(K)
        enddo
       enddo
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              0,IPUR(K),ITLR(K),0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IPUR(K)),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0.and.k.eq.19)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '97)Downward solar radiation flux (W/m**2) TOA '
      if(ierr.ne.0.and.k.eq.20)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '98)CS upward long wave radiation flux (W/m**2) TOA '
      if(ierr.ne.0.and.k.eq.21)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '99)CS upward solar radiation flux (W/m**2) TOA     '
      if(ierr.ne.0.and.k.eq.22)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '100)CS downward long radiation flux (W/m**2) SFC  '
      if(ierr.ne.0.and.k.eq.23)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '101)CS upward solar radiation flux (W/m**2)  SFC '
      if(ierr.ne.0.and.k.eq.24)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '102)CS downward solar radiation flux (W/m**2) SFC'
      if(ierr.ne.0.and.k.eq.25)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '103)CS upward long wave radiation flux (W/m**2) SFC '
      endif

        IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
 115  CONTINUE

!..........................................................
      glolal = flx_fld%SNOHFA*RTIME
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISNOHF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ISNOHF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '104)Snow phase-change heat flux [W/m^2] land surface      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)

!..........................................................
      glolal = flx_fld%smcwlt2
      CALL uninterpred(1,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
       if(me.eq.ioproc) then
       LBM=slmskful.EQ.1._kind_io8
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISMCWLT,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISMCWLT),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '105)Wilting point [fraction] land surface   '
        endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      glolal = flx_fld%smcref2
      CALL uninterpred(1,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
       if(me.eq.ioproc) then
       LBM=slmskful.EQ.1._kind_io8
        call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISMCREF,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISMCREF),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
        IF(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '106)Field capacity [fraction] land surface   '
        endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................


Clu: Addition of 7 records ends here ---------------------------------
!..........................................................
!
!     Sunshine duration time
!
      glolal = flx_fld%suntim
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,133,ICEN,IGEN,
     &            0,ISUNTM,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IACC,0,0,ICEN2,IDS(ISUNTM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '107)Accumulated sunshine duration time (sec) '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!
!     end sunshine duration time
!
!...................................................................
! Output additional variable (averaged quantity) for GOCART
! If LGGFS3D = .TRUE.
!
      IF ( LGGFS3D ) THEN
!
!hchuang code change [+16L] 11/12/2007 :
!..........................................................
      glolal=flx_fld%gsoil**rtime     !! fractional
      CALL uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISOILM,I2DBLS,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '108)Average VOL soil moist content(frac) layer 10cm -> 0cm'
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      glolal=flx_fld%gtmp2m*rtime
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ITEMP,IELEV,0,2,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ITEMP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '109)Average temperature at 2 meter (K)                    '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      glolal=flx_fld%gustar*rtime
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IUST,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IUST),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '110)Average Frictional Velocity (m/s)                     '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      glolal=flx_fld%gpblh*rtime
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IHPBL,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IHPBL),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '111)Average Boundary layer height                        '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      glolal=flx_fld%gu10m*rtime
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IZNLW,IELEV,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IZNLW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '112)Average u wind (m/s) height 10m above ground         '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      glolal=flx_fld%gv10m*rtime
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IMERW,IELEV,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IMERW),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '113)Average v wind (m/s) height 10m above ground         '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      glolal=flx_fld%gzorl*1.0E-2*rtime   !! convert from cm to m
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,IZORL,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IZORL),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '114)Average Surface roughness (m)                        '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!..........................................................
      glolal=flx_fld%goro*rtime
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISLMSK,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ISLMSK),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '115)Average Land-sea surface (fraction)                  '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!
      END IF

!!
      if(me.eq.ioproc)
     &   PRINT *,'GRIB FLUX FILE WRITTEN ',FHOUR,IDATE,noflx  
!!
      RETURN
      END
