      Program main 
      integer yyi,mmi,ddi,hhi,fhr,yyc,mmc,ddc,hhc
      CALL INITDATE(1008,03,02,12,84,yyi,mmi,ddi,hhi)
      CALL CURRDATE(yyi,mmi,ddi,hhi,84,yyc,mmc,ddc,hhc)
      CALL INITDATE(1008,03,02,12,804,yyi,mmi,ddi,hhi)
      CALL CURRDATE(yyi,mmi,ddi,hhi,804,yyc,mmc,ddc,hhc)
      CALL INITDATE(1008,03,02,12,7284,yyi,mmi,ddi,hhi)
      CALL CURRDATE(yyi,mmi,ddi,hhi,7284,yyc,mmc,ddc,hhc)
      CALL INITDATE(1008,03,02,12,72084,yyi,mmi,ddi,hhi)
      CALL CURRDATE(yyi,mmi,ddi,hhi,72084,yyc,mmc,ddc,hhc)
      stop
      end
      SUBROUTINE CURRDATE(yy,mm,dd,hh,fhr,yyc,mmc,ddc,hhc)
      integer yy,mm,dd,hh,fhr,yyc,mmc,ddc,hhc
      integer ndpm(12),mdpm,fhrd,fhrh,itest
      data ndpm/31,28,31,30,31,30,31,31,30,31,30,31/

      yyc=yy
      mmc=mm
      ddc=dd
      hhc=hh

! Adgust dd and hh, while mm and yy unchanged
      fhrh=mod(fhr,24)
      fhrd=fhr/24
      hhc=hhc+fhrh
      ddc=ddc+fhrd
      if (hhc.ge.24) then
       hhc=hhc-24
       ddc=ddc+1
      endif

! Adjust mm and yy, and dd, Month by Month
      do itest=1,100
      mdpm=ndpm(mmc)
      if (mmc.eq.2.and.mod(yyc,4).eq.0) mdpm=mdpm+1
      if (mmc.eq.2.and.(mod(yyc,100).eq.0.and.mod(yyc,400).ne.0)) mdpm=mdpm-1
      if (mmc.le.12.and.ddc.le.mdpm) then
       print *, 'STPS INIT',yy,mm,dd,hh,fhr,yyc,mmc,ddc,hhc
       return
      endif
      if (ddc.gt.mdpm) then
!Month advancement
        ddc=ddc-mdpm
        mmc=mmc+1
      endif
      if (mmc.gt.12) then
!Year advancement
           mmc=1
           yyc=yyc+1
      endif
      enddo

      print *, 'Forced stop in INITDATE subrountine'
      print *, yy,mm,dd,hh,fhr,yyc,mmc,ddc,hhc
      stop
      end

      SUBROUTINE INITDATE(yy,mm,dd,hh,fhr,yyi,mmi,ddi,hhi)
      integer yy,mm,dd,hh,fhr,yyi,mmi,ddi,hhi
      integer ndpm(12),mdpm,fhrd,fhrh,itest
      data ndpm/31,28,31,30,31,30,31,31,30,31,30,31/

      yyi=yy
      mmi=mm
      ddi=dd
      hhi=hh

! Adjust dd and hh, while mm and yy unchanged
      fhrh=mod(fhr,24)
      fhrd=fhr/24
      hhi=hhi-fhrh
      ddi=ddi-fhrd
      if (hhi.lt.0) then
       hhi=hhi+24
       ddi=ddi-1
      endif

!Adjust mm and yy, and dd, Month by Month
      do itest=1,100
      if (mmi.ge.1.and.ddi.ge.1) then
       print *, 'STPS INIT',yy,mm,dd,hh,fhr,yyi,mmi,ddi,hhi
       return
      endif
      mdpm=ndpm(mmi-1)
      if (mmi.eq.1) mdpm=ndpm(12)
      if (mmi.eq.3.and.mod(yyi,4).eq.0) mdpm=mdpm+1
      if (mmi.eq.3.and.(mod(yyi,100).eq.0.and.mod(yyI,400).ne.0)) mdpm=mdpm-1
      if (ddi.lt.1) then
!Month advancement
        ddi=ddi+mdpm
        mmi=mmi-1
      endif
      if (mmi.lt.1) then
!Year advancement
           mmi=12
           yyi=yyi-1
      endif
      enddo

      print *, 'Forced stop in INITDATE subrountine'
      print *, yy,mm,dd,hh,fhr,yyi,mmi,ddi,hhi
      stop
      end

