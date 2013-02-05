      subroutine gpxs
      use machine , only : kind_phys
      implicit none
 
      integer nx
      parameter(nx=7501)
      real(kind=kind_phys) tbpxs(nx), tbpxs0(nx), tbpxsi(nx),
     &                     c1xpxs, c2xpxs
      common/compxs/ c1xpxs,c2xpxs,tbpxs,tbpxs0,tbpxsi
      real(kind=kind_phys) xmin, xmax, xinc, t, x, fpxsx, fpxsx0, fpxsxi
      integer jx
      xmin   = 180.0
      xmax   = 330.0
      xinc   = (xmax-xmin) / (nx-1)
      c2xpxs = 1.0 / xinc
      c1xpxs = 1.0 - xmin*c2xpxs
      do jx=1,nx
        x          = xmin + (jx-1)*xinc
        t          = x
        tbpxs(jx)  = fpxsx(t)
        tbpxs0(jx) = fpxsx0(t)
        tbpxsi(jx) = fpxsxi(t)
      enddo
      return
      end
      function fpxs0(t)
      use machine , only : kind_phys
      implicit none
 
      integer nx
      parameter(nx=7501)
      real(kind=kind_phys) tbpxs(nx), tbpxs0(nx), tbpxsi(nx),
     &                     c1xpxs, c2xpxs, t
      common/compxs/ c1xpxs,c2xpxs,tbpxs,tbpxs0,tbpxsi
      real(kind=kind_phys) xj, fpxs0
      integer jx
      xj    = min(max(c1xpxs+c2xpxs*t,1.),float(nx))
      jx    = min(xj,nx-1.)
      fpxs0 = tbpxs0(jx) + (xj-jx)*(tbpxs0(jx+1)-tbpxs0(jx))
      return
      end
      function fpxsi(t)
      use machine , only : kind_phys
      implicit none
      integer nx
      parameter(nx=7501)
      real(kind=kind_phys) tbpxs(nx), tbpxs0(nx), tbpxsi(nx), c1xpxs,
     &                     c2xpxs, t
      common/compxs/ c1xpxs,c2xpxs,tbpxs,tbpxs0,tbpxsi
      real(kind=kind_phys) xj, fpxsi
      integer jx
      xj    = min(max(c1xpxs+c2xpxs*t,1.),float(nx))
      jx    = min(xj,nx-1.)
      fpxsi = tbpxsi(jx) + (xj-jx)*(tbpxsi(jx+1)-tbpxsi(jx))
      return
      end
      function fpxsq(t)
      use machine , only : kind_phys
      implicit none
      integer             jx,nx
      real(kind=kind_phys) c1xpxs,c2xpxs,dxj,fj1,fj2,fj3,fpxsq
      real(kind=kind_phys) rnx,rone,t,xj
      parameter(nx=7501)
      real(kind=kind_phys) tbpxs(nx), tbpxs0(nx), tbpxsi(nx)
      common/compxs/ c1xpxs,c2xpxs,tbpxs,tbpxs0,tbpxsi
      xj=min(max(c1xpxs+c2xpxs*t,1.),float(nx))
      jx=min(max(nint(xj),2),nx-1)
      dxj=xj-jx
      fj1=tbpxs(jx-1)
      fj2=tbpxs(jx)
      fj3=tbpxs(jx+1)
      fpxsq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2
      return
      end
      function fpxsx(t)
      use machine , only : kind_phys
      use physcons, cvap => con_cvap, csol => con_csol, rv => con_rv
     &,             ttp => con_ttp, hvap => con_hvap, hfus =>con_hfus
      implicit none
      real(kind=kind_phys) psatk, dldt, dldti, xa, xai, xb, xbi,
     &     t, tr, fpxsx,hsub
      parameter(psatk=psat*1.e-3)
      parameter(dldt=cvap-cliq,xa=-dldt/rv,xb=xa+hvap/(rv*ttp))
      parameter(hsub=hvap+hfus)
      parameter(dldti=cvap-csol,xai=-dldti/rv,xbi=xai+hsub/(rv*ttp))
      real(kind=kind_phys) tmix, w
      parameter (tmix=ttp-20.0)
      tr = ttp/t
      if(t.ge.ttp) then
        fpxsx = psatk * (tr**xa)  * exp(xb*(1.0-tr))
      elseif (t .lt. tmix) then
        fpxsx = psatk * (tr**xai) * exp(xbi*(1.0-tr))
      else
        w     = (t - tmix) / (ttp - tmix)
        fpxsx =        w  * psatk * (tr**xa)  * exp(xb*(1.0-tr))
     &        + (1.0 - w) * psatk * (tr**xai) * exp(xbi*(1.0-tr))
      endif
      return
      end
      function fpxsx0(t)
      use machine , only : kind_phys
      use physcons, cvap => con_cvap, cliq => con_cliq, rv => con_rv
     &,             ttp => con_ttp, hvap => con_hvap
      implicit none
      real(kind=kind_phys) psatk,dldt,tr,t,fpxsx0,xa,xb
      parameter(psatk=psat*1.e-3)
      parameter(dldt=cvap-cliq,xa=-dldt/rv,xb=xa+hvap/(rv*ttp))
      tr = ttp/t
      fpxsx0 = psatk * (tr**xa) * exp(xb*(1.-tr))
      return
      end
      function fpxsxi(t)
      use machine , only : kind_phys
      use physcons, cvap => con_cvap, csol => con_csol, rv => con_rv
     &,             ttp => con_ttp, hvap => con_hvap, hfus =>con_hfus
      implicit none
      real(kind=kind_phys) hsub
      real(kind=kind_phys) psatk,dldti,tr,t,fpxsxi,xai,xbi
      parameter(psatk=psat*1.e-3)
      parameter(hsub=hvap+hfus)
      parameter(dldti=cvap-csol,xai=-dldti/rv,xbi=xai+hsub/(rv*ttp))
      tr = ttp/t
      fpxsxi = psatk * (tr**xai) * exp(xbi*(1.0-tr))
      return
      end
      subroutine gtdp
      use machine , only : kind_phys
      implicit none
      integer             jx,nx
      real(kind=kind_phys) c1xtdp,c2xtdp,ftdpxg,pv,t,x,xinc,xmax,xmin
      parameter(nx=5001)
      real(kind=kind_phys) tbtdp(nx)
      common/comtdp/ c1xtdp,c2xtdp,tbtdp
      xmin= 0.001
      xmax=10.001
      xinc=(xmax-xmin)/(nx-1)
      c1xtdp=1.-xmin/xinc
      c2xtdp=1./xinc
      t=208.0
      do jx=1,nx
        x=xmin+(jx-1)*xinc
        pv=x
        t=ftdpxg(t,pv)
        tbtdp(jx)=t
      enddo
      return
      end
      function ftdp(pv)
      use machine , only : kind_phys
      implicit none
      integer             jx,nx
      real(kind=kind_phys) c1xtdp,c2xtdp,ftdp,pv,rnx,rone,xj
      parameter(nx=5001)
      real(kind=kind_phys) tbtdp(nx)
      common/comtdp/ c1xtdp,c2xtdp,tbtdp
      xj=min(max(c1xtdp+c2xtdp*pv,1.),float(nx))
      jx=min(xj,nx-1.)
      ftdp=tbtdp(jx)+(xj-jx)*(tbtdp(jx+1)-tbtdp(jx))
      return
      end
      function ftdpq(pv)
      use machine , only : kind_phys
      implicit none
      integer             jx,nx
      real(kind=kind_phys) c1xtdp,c2xtdp,dxj,fj1,fj2,fj3,ftdpq
      real(kind=kind_phys) pv,rnx,rone,xj
      parameter(nx=5001)
      real(kind=kind_phys) tbtdp(nx)
      common/comtdp/ c1xtdp,c2xtdp,tbtdp
      xj=min(max(c1xtdp+c2xtdp*pv,1.),float(nx))
      jx=min(max(nint(xj),2),nx-1)
      dxj=xj-jx
      fj1=tbtdp(jx-1)
      fj2=tbtdp(jx)
      fj3=tbtdp(jx+1)
      ftdpq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2
      return
      end
      function ftdpx(pv)
      use machine , only : kind_phys
      implicit none
      real(kind=kind_phys) ftdp,ftdpx,ftdpxg,pv,tg
      tg=ftdp(pv)
      ftdpx=ftdpxg(tg,pv)
      return
      end
      function ftdpxg(tg,pv)
      use machine , only : kind_phys
      use physcons, cvap => con_cvap, cliq => con_cliq, rv => con_rv
     &,             ttp => con_ttp, hvap => con_hvap, hfus =>con_hfus
      implicit none
      real(kind=kind_phys) dldt,dpvt,el,ftdpxg,psatk,pv,pvt,t
      real(kind=kind_phys) terr,terrm,tg,tr,xa,xb
      parameter(psatk=psat*1.e-3)
      parameter(dldt=cvap-cliq,xa=-dldt/rv,xb=xa+hvap/(rv*ttp))
      parameter(terrm=1.e-6)
      t=tg
      tr=ttp/t
      pvt=psatk*(tr**xa)*exp(xb*(1.-tr))
      el=hvap+dldt*(t-ttp)
      dpvt=el*pvt/(rv*t**2)
      terr=(pvt-pv)/dpvt
      t=t-terr
      dowhile(abs(terr).gt.terrm)
        tr=ttp/t
        pvt=psatk*(tr**xa)*exp(xb*(1.-tr))
        el=hvap+dldt*(t-ttp)
        dpvt=el*pvt/(rv*t**2)
        terr=(pvt-pv)/dpvt
        t=t-terr
      enddo
      ftdpxg=t
      return
      end
      subroutine gthe
      use machine , only : kind_phys
      use physcons, rocp => con_rocp, ttp => con_ttp
      implicit none
      integer             nx,ny
      real(kind=kind_phys) c1xthe,c1ythe,c2xthe,c2ythe,rocp,xinc
      real(kind=kind_phys) xmax,xmin,yinc,ymax,ymin
      parameter(rocp=rd/cp)
      parameter(nx=241,ny=151)
      real(kind=kind_phys) tbthe(nx,ny)
      real(kind=kind_phys) y,pk,x,t,fthex
      integer jy,jx
      common/comthe/ c1xthe,c2xthe,c1ythe,c2ythe,tbthe
      xmin=ttp-90.
      xmax=ttp+30.
      xinc=(xmax-xmin)/(nx-1)
      c1xthe=1.-xmin/xinc
      c2xthe=1./xinc
      ymin=0.04**rocp
      ymax=1.10**rocp
      yinc=(ymax-ymin)/(ny-1)
      c1ythe=1.-ymin/yinc
      c2ythe=1./yinc
 
      do jy=1,ny
        y=ymin+(jy-1)*yinc
        pk=y
        do jx=1,nx
          x=xmin+(jx-1)*xinc
          t=x
          tbthe(jx,jy)=fthex(t,pk)
        enddo
      enddo
      return
      end
      function fthe(t,pk)
      use machine , only : kind_phys
      implicit none
      integer             jx,jy,nx,ny
      real(kind=kind_phys) c1xthe,c1ythe,c2xthe,c2ythe,fthe,ftx1
      real(kind=kind_phys) ftx2,pk,rnx,rny,rone,t,xj,yj
      parameter(nx=241,ny=151)
      real(kind=kind_phys) tbthe(nx,ny)
      common/comthe/ c1xthe,c2xthe,c1ythe,c2ythe,tbthe
      xj=min(c1xthe+c2xthe*t,float(nx))
      yj=min(c1ythe+c2ythe*pk,float(ny))
      if(xj.ge.1..and.yj.ge.1.) then
        jx=min(xj,nx-1.)
        jy=min(yj,ny-1.)
        ftx1=tbthe(jx,jy)+(xj-jx)*(tbthe(jx+1,jy)-tbthe(jx,jy))
        ftx2=tbthe(jx,jy+1)+(xj-jx)*(tbthe(jx+1,jy+1)-tbthe(jx,jy+1))
        fthe=ftx1+(yj-jy)*(ftx2-ftx1)
      else
        fthe=0.
      endif
      return
      end
      function ftheq(t,pk)
      use machine , only : kind_phys
      implicit none
      integer             jx,jy,nx,ny
      real(kind=kind_phys) c1xthe,c1ythe,c2xthe,c2ythe,dxj,dyj
      real(kind=kind_phys) ft11,ft12,ft13,ft21,ft22,ft23,ft31
      real(kind=kind_phys) ft32,ft33,ftheq,ftx1,ftx2,ftx3,pk,rnx
      real(kind=kind_phys) rny,t,xj,yj
      parameter(nx=241,ny=151)
      real(kind=kind_phys) tbthe(nx,ny)
      common/comthe/ c1xthe,c2xthe,c1ythe,c2ythe,tbthe
      xj=min(c1xthe+c2xthe*t,float(nx))
      yj=min(c1ythe+c2ythe*pk,float(ny))
      if(xj.ge.1..and.yj.ge.1.) then
        jx=min(max(nint(xj),2),nx-1)
        jy=min(max(nint(yj),2),ny-1)
        dxj=xj-jx
        dyj=yj-jy
        ft11=tbthe(jx-1,jy-1)
        ft12=tbthe(jx-1,jy)
        ft13=tbthe(jx-1,jy+1)
        ft21=tbthe(jx,jy-1)
        ft22=tbthe(jx,jy)
        ft23=tbthe(jx,jy+1)
        ft31=tbthe(jx+1,jy-1)
        ft32=tbthe(jx+1,jy)
        ft33=tbthe(jx+1,jy+1)
        ftx1=(((ft31+ft11)/2-ft21)*dxj+(ft31-ft11)/2)*dxj+ft21
        ftx2=(((ft32+ft12)/2-ft22)*dxj+(ft32-ft12)/2)*dxj+ft22
        ftx3=(((ft33+ft13)/2-ft23)*dxj+(ft33-ft13)/2)*dxj+ft23
        ftheq=(((ftx3+ftx1)/2-ftx2)*dyj+(ftx3-ftx1)/2)*dyj+ftx2
      else
        ftheq=0.
      endif
      return
      end
      function fthex(t,pk)
      use machine , only : kind_phys
      use physcons, cvap => con_cvap, cliq => con_cliq, rv => con_rv
     &,             rd => con_rd, cp => con_cp, ttp => con_ttp
     &,             hvap => con_hvap
      implicit none
      real(kind=kind_phys) cpor,dldt,el,eps,expo,fthex,p,pd,pk
      real(kind=kind_phys) psatb,psatk,pv,rocp,t,tr,xa,xb
      real(kind=kind_phys) maxexp
      parameter(psatk=psat*1.e-3)
      parameter(rocp=rd/cp,cpor=cp/rd,psatb=psatk*1.e-2,eps=rd/rv,
     &          dldt=cvap-cliq,xa=-dldt/rv,xb=xa+hvap/(rv*ttp))
      maxexp=701.
      p=pk**cpor
      tr=ttp/t
      pv=psatb*(tr**xa)*exp(xb*(1.-tr))
      pd=p-pv
      if(pd.gt.0.) then
        el=hvap+dldt*(t-ttp)
        expo=el*eps*pv/(cp*t*pd)
        if (expo.gt.maxexp) then
           fthex=t*pd**(-rocp)*exp(maxexp)
        else
           fthex=t*pd**(-rocp)*exp(expo)
        endif
      else
        fthex=0.
      endif
      return
      end
      subroutine gtma
      use machine , only : kind_phys
      use physcons, rocp => con_rocp
      implicit none
      integer             jx,jy,nx,ny
      real(kind=kind_phys) c1xma,c1yma,c2xma,c2yma,ftmaxg,pk,q,rocp
      real(kind=kind_phys) t,the,x,xinc,xmax,xmin,y,yinc,ymax,ymin
!     parameter(rocp=rd/cp)
      parameter(nx=151,ny=121)
      real(kind=kind_phys) tbtma(nx,ny),tbqma(nx,ny)
      common/comma/ c1xma,c2xma,c1yma,c2yma,tbtma,tbqma
      xmin=200.
      xmax=500.
      xinc=(xmax-xmin)/(nx-1)
      c1xma=1.-xmin/xinc
      c2xma=1./xinc
      ymin=0.01**rocp
      ymax=1.10**rocp
      yinc=(ymax-ymin)/(ny-1)
      c1yma=1.-ymin/yinc
      c2yma=1./yinc
      do jy=1,ny
        y=ymin+(jy-1)*yinc
        pk=y
        t=xmin*y
        do jx=1,nx
          x=xmin+(jx-1)*xinc
          the=x
          t=ftmaxg(t,the,pk,q)
          tbtma(jx,jy)=t
          tbqma(jx,jy)=q
        enddo
      enddo
      return
      end
      function ftma(the,pk,qma)
      use machine , only : kind_phys
      implicit none
      integer             jx,jy,nx,ny
      real(kind=kind_phys) c1xma,c1yma,c2xma,c2yma,ftma,ftx1,ftx2
      real(kind=kind_phys) pk,qma,qx1,qx2,rnx,rny,rone,the,xj,yj
      parameter(nx=151,ny=121)
      real(kind=kind_phys) tbtma(nx,ny),tbqma(nx,ny)
      common/comma/ c1xma,c2xma,c1yma,c2yma,tbtma,tbqma
      xj=min(max(c1xma+c2xma*the,1.),float(nx))
      yj=min(max(c1yma+c2yma*pk,1.),float(ny))
      jx=min(xj,nx-1.)
      jy=min(yj,ny-1.)
      ftx1=tbtma(jx,jy)+(xj-jx)*(tbtma(jx+1,jy)-tbtma(jx,jy))
      ftx2=tbtma(jx,jy+1)+(xj-jx)*(tbtma(jx+1,jy+1)-tbtma(jx,jy+1))
      ftma=ftx1+(yj-jy)*(ftx2-ftx1)
      qx1=tbqma(jx,jy)+(xj-jx)*(tbqma(jx+1,jy)-tbqma(jx,jy))
      qx2=tbqma(jx,jy+1)+(xj-jx)*(tbqma(jx+1,jy+1)-tbqma(jx,jy+1))
      qma=qx1+(yj-jy)*(qx2-qx1)
      return
      end
      function ftmaq(the,pk,qma)
      use machine , only : kind_phys
      implicit none
      integer             jx,jy,nx,ny
      real(kind=kind_phys) c1xma,c1yma,c2xma,c2yma,dxj,dyj,ft11
      real(kind=kind_phys) ft12,ft13,ft21,ft22,ft23,ft31,ft32
      real(kind=kind_phys) ft33,ftmaq,ftx1,ftx2,ftx3,pk,q11,q12
      real(kind=kind_phys) q13,q21,q22,q23,q31,q32,q33,qma,qx1
      real(kind=kind_phys) qx2,qx3,rnx,rny,rone,the,xj,yj
      real(kind=kind_phys) tbqma,tbtma
      parameter(nx=151,ny=121)
      dimension tbtma(nx,ny),tbqma(nx,ny)
      common/comma/ c1xma,c2xma,c1yma,c2yma,tbtma,tbqma
      xj=min(max(c1xma+c2xma*the,1.),float(nx))
      yj=min(max(c1yma+c2yma*pk,1.),float(ny))
      jx=min(max(nint(xj),2),nx-1)
      jy=min(max(nint(yj),2),ny-1)
      dxj=xj-jx
      dyj=yj-jy
      ft11=tbtma(jx-1,jy-1)
      ft12=tbtma(jx-1,jy)
      ft13=tbtma(jx-1,jy+1)
      ft21=tbtma(jx,jy-1)
      ft22=tbtma(jx,jy)
      ft23=tbtma(jx,jy+1)
      ft31=tbtma(jx+1,jy-1)
      ft32=tbtma(jx+1,jy)
      ft33=tbtma(jx+1,jy+1)
      ftx1=(((ft31+ft11)/2-ft21)*dxj+(ft31-ft11)/2)*dxj+ft21
      ftx2=(((ft32+ft12)/2-ft22)*dxj+(ft32-ft12)/2)*dxj+ft22
      ftx3=(((ft33+ft13)/2-ft23)*dxj+(ft33-ft13)/2)*dxj+ft23
      ftmaq=(((ftx3+ftx1)/2-ftx2)*dyj+(ftx3-ftx1)/2)*dyj+ftx2
      q11=tbqma(jx-1,jy-1)
      q12=tbqma(jx-1,jy)
      q13=tbqma(jx-1,jy+1)
      q21=tbqma(jx,jy-1)
      q22=tbqma(jx,jy)
      q23=tbqma(jx,jy+1)
      q31=tbqma(jx+1,jy-1)
      q32=tbqma(jx+1,jy)
      q33=tbqma(jx+1,jy+1)
      qx1=(((q31+q11)/2-q21)*dxj+(q31-q11)/2)*dxj+q21
      qx2=(((q32+q12)/2-q22)*dxj+(q32-q12)/2)*dxj+q22
      qx3=(((q33+q13)/2-q23)*dxj+(q33-q13)/2)*dxj+q23
      qma=(((qx3+qx1)/2-qx2)*dyj+(qx3-qx1)/2)*dyj+qx2
      return
      end
      function ftmax(the,pk,qma)
      use machine , only : kind_phys
      implicit none
      real(kind=kind_phys) ftma,ftmax,ftmaxg,pk,qg,qma,tg,the
      tg=ftma(the,pk,qg)
      ftmax=ftmaxg(tg,the,pk,qma)
      return
      end
      function ftmaxg(tg,the,pk,qma)
      use machine , only : kind_phys
      use physcons, cvap => con_cvap, cliq => con_cliq, rv => con_rv
     &,             rd => con_rd, cp => con_cp, ttp => con_ttp
     &,             hvap => con_hvap
      implicit none
      real(kind=kind_phys) cpor,dldt,dthet,el,eps,expo,ftmaxg
      real(kind=kind_phys) p,pd,pk,psatb,psatk,pv,qma,rocp,t,terr
      real(kind=kind_phys) terrm,tg,the,thet,tr,xa,xb
      parameter(psatk=psat*1.e-3)
      parameter(rocp=rd/cp,cpor=cp/rd,psatb=psatk*1.e-2,eps=rd/rv,
     &          dldt=cvap-cliq,xa=-dldt/rv,xb=xa+hvap/(rv*ttp))
      parameter(terrm=1.e-4)
      t=tg
      p=pk**cpor
      tr=ttp/t
      pv=psatb*(tr**xa)*exp(xb*(1.-tr))
      pd=p-pv
      el=hvap+dldt*(t-ttp)
      expo=el*eps*pv/(cp*t*pd)
      thet=t*pd**(-rocp)*exp(expo)
      dthet=thet/t*(1.+expo*(dldt*t/el+el*p/(rv*t*pd)))
      terr=(thet-the)/dthet
      t=t-terr
      dowhile(abs(terr).gt.terrm)
        tr=ttp/t
        pv=psatb*(tr**xa)*exp(xb*(1.-tr))
        pd=p-pv
        el=hvap+dldt*(t-ttp)
        expo=el*eps*pv/(cp*t*pd)
        thet=t*pd**(-rocp)*exp(expo)
        dthet=thet/t*(1.+expo*(dldt*t/el+el*p/(rv*t*pd)))
        terr=(thet-the)/dthet
        t=t-terr
      enddo
      ftmaxg=t
      tr=ttp/t
      pv=psatb*(tr**xa)*exp(xb*(1.-tr))
      pd=p-pv
      qma=eps*pv/(pd+eps*pv)
      return
      end
      subroutine gpkap
      use machine , only : kind_phys
      implicit none
      real(kind=kind_phys) cd0,cd1,cd2,cd3,cd4,cn0,cn1,cn2
      common/compkap/ cn0,cn1,cn2,cd0,cd1,cd2,cd3,cd4
      external fpkapx,fident
      write(*,*)'error ratch not available'
      stop
      return
      end
      function fident(x)
      use machine , only : kind_phys
      implicit none
      real(kind=kind_phys) fident,x
      fident=x
      return
      end
      block data bdpkap
      use machine , only : kind_phys
      implicit none
      real(kind=kind_phys) cd0,cd1,cd2,cd3,cd4,cn0,cn1,cn2
      common/compkap/ cn0,cn1,cn2,cd0,cd1,cd2,cd3,cd4
      data cn0,cn1,cn2
     & /   3.13198449e-1,5.78544829e-2, 8.35491871e-4/
      data cd0,cd1,cd2,cd3,cd4
     & /1.,8.15968401e-2,5.72839518e-4,-4.86959812e-7,5.24459889e-10/
      end
      function fpkap(p)
 
      use machine , only : kind_phys
      implicit none
      real(kind=kind_phys) cd0,cd1,cd2,cd3,cd4,cn0,cn1,cn2,fpkap,p
      common/compkap/ cn0,cn1,cn2,cd0,cd1,cd2,cd3,cd4
      fpkap=(cn0+p*(cn1+p*cn2))/(cd0+p*(cd1+p*(cd2+p*(cd3+p*cd4))))
      return
      end
      function fpkapx(p)
      use machine , only : kind_phys
      use physcons, rocp => con_rocp
      implicit none
      real(kind=kind_phys) fpkapx,p,rocp
!     parameter(rocp=rd/cp)
      fpkapx=(p/100.)**rocp
      return
      end
      function ftlcl(t,tdpd)
      use machine , only : kind_phys
      implicit none
      real(kind=kind_phys) clcl1,clcl2,clcl3,clcl4,ftlcl,t,tdpd
      parameter(clcl1= 0.954442e+0,clcl2= 0.967772e-3,
     &          clcl3=-0.710321e-3,clcl4=-0.270742e-5)
      ftlcl=t-tdpd*(clcl1+clcl2*t+tdpd*(clcl3+clcl4*t))
      return
      end
 
 
 
 
      subroutine fpxs(tin,ixi,kmi,tout,ixo,kmo)
      use machine , only : kind_phys
      implicit none
 
      integer nx,ixi,kmi,i,k,ixo,kmo
      parameter(nx=7501)
      real(kind=kind_phys) tbpxs(nx), tbpxs0(nx), tbpxsi(nx),
     &                     c1xpxs, c2xpxs, tin(ixi,kmi),tout(ixo,kmo)
      common/compxs/ c1xpxs,c2xpxs,tbpxs,tbpxs0,tbpxsi
      real(kind=kind_phys) xj
      integer jx
      do k=1,kmo
        do i=1,ixo
         xj   = min(max(c1xpxs+c2xpxs*tin(i,k),1.),float(nx))
         jx   = min(xj,nx-1.)
         tout(i,k) = tbpxs(jx) + (xj-jx)*(tbpxs(jx+1)-tbpxs(jx))
        enddo
      enddo
      return
      end
