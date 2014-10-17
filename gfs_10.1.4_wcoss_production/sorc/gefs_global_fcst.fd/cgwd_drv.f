      subroutine cgwd_drv(im,ix,iy,levs,lat,dlength, rcs
     &,                   uin, vin, tin, cuhr,PRSL, PRSI, DEL
     &,                   ktop, kbot, kuo, gwdcu, gwdcv
     &                    DUSFCG,DVSFCG,lprnt,ipr, fhour)

!
!        CONVECTIVE GRAVITY WAVE DRAG DRIVER
!
      use machine , only : kind_phys

      use physcons, ROCP  => con_rocp,  CP => con_cp, FV => con_fvirt
     &,             grav  => con_g,     RD => con_RD
     &,             RV    => con_RV,    HVAP => con_HVAP
     &,             HFUS  => con_HFUS
     &,             rerth => con_rerth, pi => con_pi
      implicit none

!
      logical lprnt
      integer im, ix, iy, levs, lat, ipr
      real    dtf, fhour

      integer               kbot(im), ktop(im), kuo(im)
      real(kind=kind_phys)  UIN(IX,LEVS),   VIN(IX,LEVS), TIN(IX,LEVS)
      real(kind=kind_phys)  PRSL(IX,LEVS),  PRSI(IX,LEVS+1),
     &                      DEL(IX,LEVS),   CUHR(IM,LEVS)
     &                      GWDCU(IM,LEVS), GWDCV(IM,LEVS),
     &,                     dlength(im),    rcs(im)
!
!
       real(kind=kind_phys) cumchr(IM,levs),cumabs(IM)
       real(kind=kind_phys) qmax(IM)
     &                      DIAGN1(IM,LEVS),     DIAGN2(IM,LEVS)

      integer i, k1
!-----------------------------------------------------------------------
!        Calculate Maximum Convective Heating Rate            qmax [K/s]
!        cuhr = Temperature change due to deep convection
!-----------------------------------------------------------------------

      do i=1,IM
        qmax(i)   = 0.
        cumabs(i) = 0.
      enddo
      do k=1,LEVS
        do i=1,IM
          cumchr(i,k) = 0.
          GWDCU(i,k)  = 0.
          GWDCV(i,k)  = 0.
!         DIAGN1(i,k)=0.
!         DIAGN2(i,k)=0.
        enddo
      enddo
      do k=1,levs
        do i=1,IM
          qmax(i)   = max(qmax(i),cuhr(i,k))
          cumabs(i) = cuhr(i,k) + cumabs(i)
        enddo
      enddo

      do i=1,IM
        do k=KBOT(i),KTOP(i)
          do k1=KBOT(i),k
            cumchr(i,k) = cuhr(i,k1) + cumchr(i,k)
          enddo
          cumchr(i,k) = cumchr(i,k) / cumabs(i)
        enddo
      enddo
      if (lprnt) then
      if (KBOT(ipr).le.KTOP(ipr)) then
       lonipr=xlon(ipr)*57.29578
       latipr=xlat(ipr)*57.29578
       write(*,*) 'KBOT <= KTOP     for (lat,lon) = ',latipr,lonipr
       write(*,*) 'KUO KBOT KTOP QMAX DLENGTH  ',
     + kuo(ipr),kbot(ipr),ktop(ipr),(86400.*qmax(ipr)),dlength(ipr)
       write(*,9000) kdt
       do k=KTOP(ipr),KBOT(ipr),-1
         write(*,9010) k,(86400.*chr(ipr,k)),(100.*cumchr(ipr,k))
       enddo
      endif
      endif

 9000 format(/,3x,'K',5x,'CHR(K)',4x,'CUMCHR(K)',5x,'at KDT = ',i4,/)
 9010 format(2x,i2,2x,f8.2,5x,f6.0)

C-----------------------------------------------------------------------
C        Call gwdc routine
C-----------------------------------------------------------------------
      fhourpr = 0.

      if (lprnt) then
        print *,' Before GWDC in GBPHYS fhour ',fhour
        if (fhour.ge.fhourpr) then
          print *,' Before GWDC in GBPHYS start print'
          write(*,*) 'FHOUR IX IM LEVS = ',fhour,ix,im,levs
          print *,'dtp  dtf  RCS = ',dtp,dtf,RCS(ipr)

          write(*,9100)
          ilev=LEVS+1
          write(*,9110) ilev,(10.*prsi(ipr,ilev))
          do ilev=LEVS,1,-1
            write(*,9120) ilev,(10.*prsl(ipr,ilev)),(10.*del(ipr,ilev))
            write(*,9110) ilev,(10.*prsi(ipr,ilev))
          enddo

 9100 format(//,14x,'PRESSURE LEVELS',//,
     +' ILEV',7x,'PRSI',8x,'PRSL',8x,'DELP',/)
 9110 format(i4,2x,f10.3)
 9120 format(i4,12x,2(2x,f10.3))

          write(*,9130)
          do ilev=LEVS,1,-1
            write(*,9140) ilev,UGRS(ipr,ilev),GU0(ipr,ilev),
     +      VGRS(ipr,ilev),GV0(ipr,ilev),
     +      TGRS(ipr,ilev),GT0(ipr,ilev),GT0B(ipr,ilev),
     +      dudt(ipr,ilev),dvdt(ipr,ilev)
          enddo

 9130 format(//,10x,'Before GWDC in GBPHYS',//,' ILEV',6x,
     +'UGRS',9x,'GU0',8x,'VGRS',9x,'GV0',8x,
     +'TGRS',9x,'GT0',8x,'GT0B',8x,'DUDT',8x,'DVDT',/)
 9140 format(i4,9(2x,f10.3))

          print *,' Before GWDC in GBPHYS end print'

        endif
      endif
!
      CALL GWDC(IM, IX, IM, LEVS, LAT, UGRS, VGRS, TGRS,
     &          RCS, PRSL, PRSI, DEL, QMAX, CUMCHR, KTOP, KBOT, KUO,
     &          GWDCU, GWDCV, grav, CP, RD, dlength, lprnt, ipr, fhour,
     &          DUSFCG,DVSFCG,DIAGN1,DIAGN2)
!
      if (lprnt) then
        if (fhour.ge.fhourpr) then
          print *,' After GWDC in GBPHYS start print'

          write(*,9131)
          do ilev=LEVS,1,-1
            write(*,9141) ilev,UGRS(ipr,ilev),GU0(ipr,ilev),
     +      VGRS(ipr,ilev),GV0(ipr,ilev),
     +      TGRS(ipr,ilev),GT0(ipr,ilev),GT0B(ipr,ilev),
     +      GWDCU(ipr,ilev),GWDCV(ipr,ilev)
          enddo

 9131 format(//,10x,'After GWDC in GBPHYS',//,' ILEV',6x,
     +'UGRS',9x,'GU0',8x,'VGRS',9x,'GV0',8x,
     +'TGRS',9x,'GT0',8x,'GT0B',7x,'GWDCU',7x,'GWDCV',/)
 9141 format(i4,9(2x,f10.3))

          print *,' After GWDC in GBPHYS end print'
        endif
      endif
!
      return
      end

