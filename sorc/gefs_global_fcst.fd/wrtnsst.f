      SUBROUTINE WRTSFC(IOPROC,noflx,ZHOUR,FHOUR,IDATE,colat1,SECSWR,
     &                  SECLWR,slmsk,
     &                  HICE ,FICE ,                                    ! FOR SEA-ICE - XW Nov04
     &                  dusfc,dvsfc,dtsfc,dqsfc,tsea,smc,stc,
     &                  gflux,fluxr,dlwsfc,ulwsfc,
     &                  sheleg,geshem,bengsh,cldwrk,u10m,v10m,
     &                  t2m,q2m,psurf,
     &                  tmpmax,tmpmin,runoff,ep,dugwd,dvgwd,
     &                  hpbl,pwat,cv,cvt,cvb,
!Clu [+1L]: add additional state variables (canopy,slc,snwdph)
     &                  canopy,slc,snwdph,
!Cwei: additional 30 fields
     &                  zorl,vfrac,vtype,stype,slope,uustar,oro,
     &                  srflag,chh,cmm,epi,dlwsfci,ulwsfci,uswsfci,
     &                  dswsfci,dtsfci,dqsfci,gfluxi,srunoff,tt1,
     &                  q1,u1,v1,zlvl,evbsa,evcwa,transa,sbsnoa,
     &                  snowca,soilm,
     &           global_lats_r,lonsperlar)
!!
      use machine
      use resol_def
      use layout1
      use sig_io
      use namelist_def
!     USE module_nsst_parameters, ONLY : nj_nsst
      implicit none
!!
      INTEGER              GLOBAL_LATS_R(LATR)
      INTEGER              lonsperlar(LATR)
      integer   IOPROC
!!
      integer   IPRS,ITEMP,IZNLW,IMERW,ISPHUM,IPWAT,
     $          IPCPR,ISNOWD,ICLDF,ICCLDF,
     $          ISLMSK,IZORL,IALBDO,ISOILM,ICEMSK,
     $          ILHFLX,ISHFLX,IZWS,IMWS,IGHFLX,
     $          IUSWFC,IDSWFC,IULWFC,IDLWFC,
     $          INSWFC,INLWFC,
     $          IDSWVB,IDSWVD,IDSWNB,IDSWND,
     $          ITMX,ITMN,IRNOF,IEP,
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
     $   j,i,k,itop,ibot,k4,l,noflx
     &,         ISIK                                    ! FOR SEA-ICE - XW Nov04
!Clu [+1L]: declare additional parameter index
     +,         ISLC,ISNOD,ICNP
     &,  iveg, ivtp, istp, islo,iust,ihgt,irst,ichh
     &,  icmm,isrf,ievbs,ievcw,itran,isbs,isnc,istc
      PARAMETER(NFLD=18)
       integer ilpds,iyr,imo,ida,ihr,ifhr,ithr,lg,ierr
       real (kind=kind_io8) RTIMER(NFLD),rtime,rtimsw,rtimlw
       real (kind=kind_io8) colat1
       real (kind=kind_io8) cl1,secswr,zhour,fhour,seclwr
C
      PARAMETER(IPRS=1,ITEMP=11,IZNLW=33,IMERW=34,ISPHUM=51,IPWAT=54,
     $          IPCPR=59,ISNOWD=65,ICLDF=71,ICCLDF=72,
     $          ISLMSK=81,IZORL=83,IALBDO=84,ISOILM=144,ICEMSK=91,
     $          ISIK=92,                                ! FOR SEA-ICE - XW Nov04
     $          ILHFLX=121,ISHFLX=122,IZWS=124,IMWS=125,IGHFLX=155,
     $          IUSWFC=160,IDSWFC=161,IULWFC=162,IDLWFC=163,
     $          INSWFC=164,INLWFC=165,
     $          IDSWVB=166,IDSWVD=167,IDSWNB=168,IDSWND=169,
     $          ITMX=15,ITMN=16,IRNOF=90,IEP=145,
     &          ICLDWK=146,IZGW=147,IMGW=148,IHPBL=221,
     $          IDSWF=204,IDLWF=205,IUSWF=211,IULWF=212,ICPCPR=214,
     &          IUVBF=200,IUVBFC=201)
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
c
      LOGICAL(1) LBM(lonr*latr)
      CHARACTER G(200+lonr*latr*(16+1)/8)
      INTEGER   IPUR(NFLD),ITLR(NFLD)
      DATA      IPUR/IULWF , IUSWF , IUSWF , IDSWF ,  ICLDF,   IPRS,
     $                 IPRS, ITEMP ,  ICLDF,   IPRS,   IPRS, ITEMP ,
     $                ICLDF,   IPRS,   IPRS, ITEMP ,  IUVBF, IUVBFC /
      DATA      ITLR/ITOA  , ITOA  , ISFC  , ISFC  , IHCLYR, IHCTL ,
     $               IHCBL , IHCTL , IMCLYR, IMCTL , IMCBL , IMCTL ,
     $               ILCLYR, ILCTL , ILCBL , ILCTL , ISFC  , ISFC /
!    $               ILCLYR, ILCTL , ILCBL , ILCTL /
      INTEGER     IDATE(4), IDS(255),IENS(5)
      real (kind=kind_io8) SI(LEVP1)
C
csela..................................................................
      real (kind=kind_io8)   rflux(lonr,LATS_NODE_R,27)
      real (kind=kind_io8)   glolal(lonr,LATS_NODE_R)
      real (kind=kind_io8)   buffo(lonr,LATS_NODE_R)
      real (kind=kind_io4)   buff1(lonr,latr)
      real (kind=kind_io4)   buff1l(lonr*latr)
csela..................................................................
      real (kind=kind_io8)  FLUXR(nfxr,LONR,LATS_NODE_R)
      REAL (KIND=KIND_IO8) SLMSK (LONR,LATS_NODE_R),
     &    SHELEG(LONR,LATS_NODE_R),TSEA  (LONR,LATS_NODE_R),
     &     CV    (LONR,LATS_NODE_R),CVT   (LONR,LATS_NODE_R),
     &     CVB   (LONR,LATS_NODE_R),
     &     TMPMIN (LONR,LATS_NODE_R),TMPMAX (LONR,LATS_NODE_R),
     &     GESHEM (LONR,LATS_NODE_R),
     &     DUSFC (LONR,LATS_NODE_R),DVSFC (LONR,LATS_NODE_R),
     &     DTSFC (LONR,LATS_NODE_R),DQSFC(LONR,LATS_NODE_R),
     &     DLWSFC (LONR,LATS_NODE_R),ULWSFC(LONR,LATS_NODE_R),
     &     GFLUX (LONR,LATS_NODE_R),RUNOFF(LONR,LATS_NODE_R),
     &     EP (LONR,LATS_NODE_R),CLDWRK(LONR,LATS_NODE_R),
     &     DUGWD (LONR,LATS_NODE_R),DVGWD(LONR,LATS_NODE_R),
     &     BENGSH (LONR,LATS_NODE_R),PSURF(LONR,LATS_NODE_R),
     &     U10M (LONR,LATS_NODE_R),V10M(LONR,LATS_NODE_R),
     &     T2M (LONR,LATS_NODE_R),Q2M(LONR,LATS_NODE_R),
     &     HPBL (LONR,LATS_NODE_R),PWAT(LONR,LATS_NODE_R),
     &     SMC (LSOIL,LONR,LATS_NODE_R),
     &     STC (LSOIL,LONR,LATS_NODE_R)
!Clu [+2L]: add slc and snwdph
     +,    SLC (LSOIL,LONR,LATS_NODE_R)
     +,    SNWDPH(LONR,LATS_NODE_R)
     +,    CANOPY(LONR,LATS_NODE_R)
!Cwei: additional 30 fields
     +,    ZORL(LONR,LATS_NODE_R),VFRAC(LONR,LATS_NODE_R)
     +,    VTYPE(LONR,LATS_NODE_R),STYPE(LONR,LATS_NODE_R)
     +,    SLOPE(LONR,LATS_NODE_R),UUSTAR(LONR,LATS_NODE_R)
     +,    ORO(LONR,LATS_NODE_R),SRFLAG(LONR,LATS_NODE_R)
     +,    CHH(LONR,LATS_NODE_R),CMM(LONR,LATS_NODE_R)
     +,    EPI(LONR,LATS_NODE_R),DLWSFCI(LONR,LATS_NODE_R)
     +,    ULWSFCI(LONR,LATS_NODE_R),USWSFCI(LONR,LATS_NODE_R)
     +,    DSWSFCI(LONR,LATS_NODE_R),DTSFCI(LONR,LATS_NODE_R)
     +,    DQSFCI(LONR,LATS_NODE_R),GFLUXI(LONR,LATS_NODE_R)
     +,    SRUNOFF(LONR,LATS_NODE_R)
     +,    TT1(LONR,LATS_NODE_R),Q1(LONR,LATS_NODE_R)
     +,    U1(LONR,LATS_NODE_R),V1(LONR,LATS_NODE_R)
     +,    ZLVL(LONR,LATS_NODE_R),EVBSA(LONR,LATS_NODE_R)
     +,    EVCWA(LONR,LATS_NODE_R),TRANSA(LONR,LATS_NODE_R)
     +,    SBSNOA(LONR,LATS_NODE_R),SNOWCA(LONR,LATS_NODE_R)
     +,    SOILM(LONR,LATS_NODE_R)
!c-- XW: FOR SEA-ICE Nov04
      REAL (KIND=KIND_IO8) HICE(LONR,LATS_NODE_R),
     &     FICE(LONR,LATS_NODE_R)
!c-- XW: END SEA-ICE
csela..................................................................
      integer kmsk(lonr,lats_node_r),kmsk0(lonr,lats_node_r)
      integer kmskcv(lonr,LATS_NODE_R),il
cjfe
      IDS=0
      G=' '
cjfe
!!
      kmsk=nint(slmsk)
      kmsk0=0
      CALL uninterpred(1,kmsk,glolal,slmsk,global_lats_r,lonsperlar)
      slmskloc=glolal
      call unsplit2d(ioproc,buff1l,glolal,global_lats_r)
      slmskful=buff1l
c
      do k=1,27
       do j=1,LATS_NODE_R
        do i=1,lonr
         rflux(i,j,k)=fluxr(k,i,j)
        enddo
       enddo
      enddo
!!
      CALL IDSDEF(1,IDS)
! UV-B scaling factor, if set up already, comment the next 2 lines out
      ids(IUVBF)  = 2
      ids(IUVBFC) = 2
! Ice conentration and thickness scaling factor
      ids(icemsk) = 3      ! ICE CONCENTRATION ()
      ids(isik)   = 2      ! ICE THICKNESS (M)
!
!wei added 10/24/2006
      ids(IZORL)=4
      ids(IHGT)=3
      ids(IVEG)=2
      ids(IUST)=3
      ids(ICHH)=4
      ids(ICMM)=4
      ids(ISRF)=5
      ids(ITEMP)=3
      ids(ISPHUM)=6
      ids(IZNLW)=2
      ids(IMERW)=2
      ids(ISNC)=3
      ids(ISTC)=4
      ids(ISOILM)=4
      ids(ISNOD)=6
      ids(ISNOWD)=5
      ids(ICNP)=5
      ids(IPCPR)=6
      ids(ICPCPR)=6
      ids(IRNOF)=5                                                                                                                          

      ILPDS=28
      IF(ICEN2.EQ.2) ILPDS=45
      IENS(1)=1
      IENS(2)=IENST
      IENS(3)=IENSI
      IENS(4)=1
      IENS(5)=255
      IYR=IDATE(4)
      IMO=IDATE(2)
      IDA=IDATE(3)
      IHR=IDATE(1)
      IFHR=NINT(ZHOUR)
      ITHR=NINT(FHOUR)
      IF(FHOUR.GT.ZHOUR) THEN
        RTIME=1./(3600.*(FHOUR-ZHOUR))
      ELSE
        RTIME=0.
      ENDIF
      IF(SECSWR.GT.0.) THEN
        RTIMSW=1./SECSWR
      ELSE
        RTIMSW=1.
      ENDIF
      IF(SECLWR.GT.0.) THEN
        RTIMLW=1./SECLWR
      ELSE
        RTIMLW=1.
      ENDIF
      RTIMER=RTIMSW
      RTIMER(1)=RTIMLW
      CL1=colat1         
CC
c..........................................................
      glolal=DUSFC*RTIME
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
c..........................................................
      glolal=DVSFC*RTIME
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
c..........................................................
      glolal=DTSFC*RTIME
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
c..........................................................
      glolal=DQSFC*RTIME
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
c..........................................................
      CALL uninterpred(2,kmsk0,buffo,tsea,global_lats_r,lonsperlar)
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
c..........................................................
      glolal(:,:)=SMC(1,:,:)
      CALL uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISOILM,I2DBLS,0,10,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '06)Volumetric soil moist content (frac) layer 10cm and 0cm    '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
c..........................................................
c..........................................................
Clu [-3L/+3L]: change 10-200cm to 10-40cm for smc(2)
      glolal(:,:)=SMC(2,:,:)
Clu   CALL uninterpred(1,kmsk,buffo,glolal,
      CALL uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      if(lsoil.gt.2)then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
Clu  &            1,ISOILM,I2DBLS,10,200,IYR,IMO,IDA,IHR,
     +            1,ISOILM,I2DBLS,10,40,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
Clu  x '07)Volumetric soil moist content (frac) layer 200cm and 10cm  '
     + '07)Volumetric soil moist content (frac) layer 40cm and 10cm  '
      else
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISOILM,I2DBLS,10,200,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '07)Volumetric soil moist content (frac) layer 200cm and 10cm  '
      endif
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
c..........................................................
      glolal(:,:)=STC(1,:,:)
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
c..........................................................
Clu [-2L/+2L]: change 10-200 to 10-40 for stc(2)
      glolal(:,:)=STC(2,:,:)
      CALL uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      if(lsoil.gt.2)then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
Clu  &            1,ITEMP,I2DBLS,10,200,IYR,IMO,IDA,IHR,
     +            1,ITEMP,I2DBLS,10,40,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
Clu  x '09)Temp (K) layer betw two depth below land sfc 200cm and 10cm'
     + '09)Temp (K) layer betw two depth below land sfc 40cm and 10cm'
      else
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ITEMP,I2DBLS,10,200,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '09)Temp (K) layer betw two depth below land sfc 200cm and 10cm'
      endif
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
c..........................................................
      CALL uninterpred(2,kmsk,buffo,sheleg,global_lats_r,lonsperlar)
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
      glolal=DLWSFC*RTIME
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
c..........................................................
      glolal=ULWSFC*RTIME
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
c..........................................................
C.......  FIX FLUXES FOR APPROX DIURNAL CYCLE
      DO 113 K=1,4
       do j=1,LATS_NODE_R
        do i=1,lonr
         glolal(i,j)=rflux(i,j,k)*RTIMER(k)
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
c..........................................................
!
!     For UV-B fluxes
!
      do j=1,LATS_NODE_R
        do i=1,lonr
          glolal(i,j)=rflux(i,j,21)*rtimsw
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
          glolal(i,j)=rflux(i,j,22)*rtimsw
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
c..........................................................
c..........................................................
      DO 813 K=5,7
cxxxxxxxxxxxxxxxx
       do j=1,LATS_NODE_R
        do i=1,lonr
         glolal(i,j)=rflux(i,j,k)*100.*rtimsw      
        enddo
       enddo
      where(glolal.ge.0.5)
        kmskcv=1
      elsewhere
        kmskcv=0
      endwhere
!!
      CALL uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
cxxxxxxxxxxxxxxxx
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
cxxxxxxxxxxxxxxxx
       do j=1,LATS_NODE_R
        do i=1,lonr
         if(rflux(i,j,k).gt.0.)then
          glolal(i,j)=rflux(i,j,k+3)*1000./rflux(i,j,k)      
         else
          glolal(i,j)=0.
         endif
        enddo
       enddo
      CALL uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
cxxxxxxxxxxxxxxxx
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
cxxxxxxxxxxxxxxxx
       do j=1,LATS_NODE_R
        do i=1,lonr
         if(rflux(i,j,k).gt.0.)then
          glolal(i,j)=rflux(i,j,k+6)*1000./rflux(i,j,k)      
         else
          glolal(i,j)=0.
         endif
        enddo
       enddo
      CALL uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
cxxxxxxxxxxxxxxxx
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
Cxxxxxxxxxxxxxxxx
       do j=1,LATS_NODE_R
        do i=1,lonr
         if(rflux(i,j,k).gt.0.)then
          glolal(i,j)=rflux(i,j,k+9)/rflux(i,j,k)      
         else
          glolal(i,j)=0.
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
C
  813 CONTINUE
CC
c...................................................................
      glolal=GESHEM*1.E3*RTIME
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
c...................................................................
      glolal=BENGSH*1.E3*RTIME
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
c...................................................................
      glolal=GFLUX*RTIME
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
c...................................................................
      buffo=MOD(slmskloc,2._kind_io8)
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
c...................................................................
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
      CALL uninterpred(2,kmsk0,buffo,fice,global_lats_r,lonsperlar)
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
c...................................................................
      CALL uninterpred(2,kmsk0,buffo,u10m,global_lats_r,lonsperlar)
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
c...................................................................
      CALL uninterpred(2,kmsk0,buffo,v10m,global_lats_r,lonsperlar)
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
c...................................................................
      CALL uninterpred(2,kmsk0,buffo,t2m,global_lats_r,lonsperlar)
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
c...................................................................
      CALL uninterpred(2,kmsk0,buffo,q2m,global_lats_r,lonsperlar)
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
c...................................................................
      glolal=PSURF*1.E3
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
c...................................................................
      CALL uninterpred(2,kmsk0,buffo,tmpmax,global_lats_r,lonsperlar)
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
c...................................................................
      CALL uninterpred(2,kmsk0,buffo,tmpmin,global_lats_r,lonsperlar)
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
c...................................................................
      glolal=RUNOFF * 1.E3
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
c...................................................................
      glolal=EP * RTIME
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
c...................................................................
      glolal=CLDWRK * RTIME
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
c...................................................................
      glolal=DUGWD*RTIME
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
c...................................................................
      glolal=DVGWD*RTIME
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
c...................................................................
      CALL uninterpred(2,kmsk0,buffo,hpbl,global_lats_r,lonsperlar)
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
c...................................................................
      CALL uninterpred(2,kmsk0,buffo,pwat,global_lats_r,lonsperlar)
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
c...................................................................
cxxxxxxxxxxxxxxxx
       do j=1,LATS_NODE_R
        do i=1,lonr
         if (rflux(i,j,4).GT.0.) then
          glolal(i,j)=rflux(i,j,3)/rflux(i,j,4) * 100.
          if (glolal(i,j).GT.100.) glolal(i,j)=100.
         else
          glolal(i,j)=0.
         endif
        enddo
       enddo
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
cxxxxxxxxxxxxxxxx
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            0,IALBDO,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(IALBDO),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '50)Albedo (percent) land and sea surface                      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
cxxxxxxxxxxxxxxxx
       do j=1,LATS_NODE_R
        do i=1,lonr
         glolal(i,j)=rflux(i,j,26)*100.*rtimsw
        enddo
       enddo
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
cxxxxxxxxxxxxxxxx
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              0,ICLDF,ICOLMN,0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ICLDF),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '51)Total cloud cover (percent) total atmospheric column       '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
C
C CONVECTIVE CLOUDS
C LABELED INSTANTANEOUS BUT ACTUALLY AVERAGED OVER FHSWR HOURS
C
      glolal=CV*1.E2
      where(glolal.ge.0.5)
        kmskcv=1
      elsewhere
        kmskcv=0
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
        IF(CV(i,j).GT.0.) THEN
!        ITOP=NINT(CVT(i,j))
!        IF(ITOP.GE.1.AND.ITOP.LE.LEVS)
!    &   glolal(i,j)=SI(ITOP+1)*PSURF(i,j)*1.E3
c...      cvt already a pressure (cb)...convert to Pa
         glolal(i,j)=CVT(i,j)*1.E3
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
c.................................................
       do j=1,LATS_NODE_R
        do i=1,lonr
        glolal(i,j) = 0.
        IF(CV(i,j).GT.0.) THEN
!        Ibot=NINT(CVB(i,j))
!        IF(Ibot.GE.1.AND.Ibot.LE.LEVS)
!    &   glolal(i,j)=SI(IBOT)*PSURF(i,j)*1.E3
c...      cvb already a pressure (cb)...convert to Pa
         glolal(i,j)=CVB(i,j)*1.E3
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
c.................................................
C...   SAVE B.L. CLOUD AMOUNT
cxxxxxxxxxxxxxxxx
       do j=1,LATS_NODE_R
        do i=1,lonr
         glolal(i,j)=rflux(i,j,27)*100.*rtimsw
        enddo
       enddo
      CALL uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
cxxxxxxxxxxxxxxxx
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &              0,ICLDF,IBLLYR,0,0,IYR,IMO,IDA,IHR,
     &              IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ICLDF),IENS,
     &              0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '55)Total cloud cover (percent) boundary layer cloud layer     '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!c-- XW: FOR SEA-ICE Nov04
      CALL uninterpred(2,kmsk0,buffo,hice,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.2._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISIK,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISIK),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '56)Sea ice thickness (m) category 1'
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
!c-- XW: END SEA-ICE
c.................................................
Clu: add smc(3:4), stc(3:4), slc(1:4), snwdph, canopy
Clu: addition of 10 records starts here -------------------------------
      if(lsoil.gt.2)then
      glolal(:,:)=SMC(3,:,:)
      CALL uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISOILM,I2DBLS,40,100,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '57)Volumetric soil moist content (frac) layer 100cm and 40cm '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
c..........................................................
      glolal(:,:)=SMC(4,:,:)
      CALL uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISOILM,I2DBLS,100,200,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '58)Volumetric soil moist content (frac) layer 200cm and 100cm '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
c..........................................................
      glolal(:,:)=STC(3,:,:)
      CALL uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ITEMP,I2DBLS,40,100,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '59)Temp (K) layer betw two depth below land sfc 100cm and 40cm'
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
c..........................................................
      glolal(:,:)=STC(4,:,:)
      CALL uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ITEMP,I2DBLS,100,200,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ITEMP),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '60)Temp (K) layer betw two depth below land sfc 200cm and 100cm'
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
      endif
c..........................................................
      glolal(:,:)=SLC(1,:,:)
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
c..........................................................
      glolal(:,:)=SLC(2,:,:)
      CALL uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      if(lsoil.gt.2)then
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISLC,I2DBLS,10,40,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '62)Liquid soil moist content (frac) layer 40cm and 10cm '
      else
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISLC,I2DBLS,10,200,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '62)Liquid soil moist content (frac) layer 200cm and 10cm '
      endif
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
c..........................................................
      if(lsoil.gt.2)then
      glolal(:,:)=SLC(3,:,:)
      CALL uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISLC,I2DBLS,40,100,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '63)Liquid soil moist content (frac) layer 100cm and 40cm'
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
c..........................................................
      glolal(:,:)=SLC(4,:,:)
      CALL uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,130,ICEN,IGEN,
     &            1,ISLC,I2DBLS,100,200,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISOILM),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '64)Liquid soil moist content (frac) layer 200cm and 100cm'
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
      endif
c..........................................................
      glolal=SNWDPH / 1.E3       !! convert from mm to m
      CALL uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISNOD,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISNOD),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '65)Snow depth (m) land surface                  '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
c..........................................................
      CALL uninterpred(2,kmsk,buffo,canopy,global_lats_r,lonsperlar)
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
Clu: addition of 10 records ends here -------------------------------
C
Cwei: addition of 30 records starts here -------------------------------
c..........................................................
      glolal=ZORL / 1.E3       !! convert from mm to m
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
c..........................................................
      glolal=vfrac*100.
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
c..........................................................
      CALL uninterpred(1,kmsk,glolal,vtype,global_lats_r,lonsperlar)
      buffo=MOD(glolal,2._kind_io8)
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
c..........................................................
      CALL uninterpred(1,kmsk,glolal,stype,global_lats_r,lonsperlar)
      buffo=MOD(glolal,2._kind_io8)
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
c..........................................................
      CALL uninterpred(1,kmsk,glolal,slope,global_lats_r,lonsperlar)
      buffo=MOD(glolal,2._kind_io8)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISLO,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,ITHR,0,INST,0,0,ICEN2,IDS(ISLO),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '71)Slope type land surface      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
c..........................................................
      CALL uninterpred(2,kmsk0,buffo,uustar,global_lats_r,lonsperlar)
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
c..........................................................
      CALL uninterpred(1,kmsk0,buffo,oro,global_lats_r,lonsperlar)
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
c..........................................................
      CALL uninterpred(1,kmsk,buffo,srflag,global_lats_r,lonsperlar)
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
c..........................................................
      CALL uninterpred(2,kmsk0,buffo,chh,global_lats_r,lonsperlar)
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
c..........................................................
      CALL uninterpred(2,kmsk0,buffo,cmm,global_lats_r,lonsperlar)
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
c..........................................................
      CALL uninterpred(2,kmsk,buffo,epi,global_lats_r,lonsperlar)
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
c..........................................................
      CALL uninterpred(2,kmsk0,buffo,DLWSFCI,global_lats_r,lonsperlar)
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
c..........................................................
      CALL uninterpred(2,kmsk0,buffo,ULWSFCI,global_lats_r,lonsperlar)
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
c..........................................................
      CALL uninterpred(2,kmsk0,buffo,USWSFCI,global_lats_r,lonsperlar)
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
c..........................................................
      CALL uninterpred(2,kmsk0,buffo,DSWSFCI,global_lats_r,lonsperlar)
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
c..........................................................
      CALL uninterpred(2,kmsk0,buffo,DTSFCI,global_lats_r,lonsperlar)
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
c..........................................................
      CALL uninterpred(2,kmsk0,buffo,DQSFCI,global_lats_r,lonsperlar)
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
c..........................................................
      CALL uninterpred(2,kmsk,buffo,GFLUXI,global_lats_r,lonsperlar)
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
c..........................................................
      glolal=SRUNOFF * 1.E3
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
c..........................................................
      CALL uninterpred(2,kmsk0,buffo,tt1,global_lats_r,lonsperlar)
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
c..........................................................
      CALL uninterpred(2,kmsk0,buffo,q1,global_lats_r,lonsperlar)
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
c..........................................................
      CALL uninterpred(2,kmsk0,buffo,u1,global_lats_r,lonsperlar)
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
c..........................................................
      CALL uninterpred(2,kmsk0,buffo,v1,global_lats_r,lonsperlar)
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
c..........................................................
      CALL uninterpred(2,kmsk,buffo,zlvl,global_lats_r,lonsperlar)
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
c..........................................................
      glolal=EVBSA*RTIME
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
c..........................................................
      glolal=EVCWA*RTIME
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
c..........................................................
      glolal=TRANSA*RTIME
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
c..........................................................
      glolal=SBSNOA*RTIME
      CALL uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      LBM=slmskful.EQ.1._kind_io8
      call gribit(wrkga,LBM,4,lonr,latr,16,CL1,ILPDS,2,ICEN,IGEN,
     &            1,ISBS,ISFC,0,0,IYR,IMO,IDA,IHR,
     &            IFHOUR,IFHR,ITHR,IAVG,0,0,ICEN2,IDS(ISBS),IENS,
     &            0.,0.,0.,0.,0.,0.,G,LG,IERR)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '94)Snow Sublimation (W/m^2) land surface      '
      endif
      IF(IERR.EQ.0 .and. me.eq.ioproc) CALL WRYTE(noflx,LG,G)
c..........................................................
      glolal=SNOWCA*RTIME*100.
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
c..........................................................
      glolal=soilm*1.E3       !! convert from m to (mm)kg/m^2
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


CC
      if(me.eq.ioproc)
     &   PRINT *,'GRIB FLUX FILE WRITTEN ',FHOUR,IDATE,noflx  
!!
      RETURN
      END
