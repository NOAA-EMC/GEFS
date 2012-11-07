      subroutine gfidi_sig(lon_dim,lons_lat,lat,
     1 dg,tg,zg,ug,vg,rqg,dphi,dlam,qg,
     1 rcl,del,rdel2,ci,tov,spdmax,deltim,sl,nvcn,xvcn,
     2 dtdf,dtdl,drdf,drdl,dudl,dvdl,dudf,dvdf,
     2 dqdt,dtdt,drdt,dudt,dvdt)
cc
      USE MACHINE , ONLY : kind_phys

cc
      use resol_def
      use physcons, rerth => con_rerth, rd => con_rd, cp => con_cp
     &,             omega => con_omega
      implicit none
cc
      integer              j,k,n,nvcn
      real(kind=kind_evod) fnor,rcl,rcl2,rk,sinra,deltim,xvcn
c
c input variables
c
      integer              lon_dim,lat
      integer              lons_lat
c
      real(kind=kind_evod)
     1  dg(lon_dim,levs),tg(lon_dim,levs), zg(lon_dim,levs),
     2  ug(lon_dim,levs),vg(lon_dim,levs),rqg(lon_dim,levs,ntrac),
     3          dphi(lon_dim),dlam(lon_dim),qg(lon_dim)
c
      real(kind=kind_evod)
     1  dtdf(lon_dim,levs),dtdl(lon_dim,levs),
     1  drdf(lon_dim,levs,ntrac),drdl(lon_dim,levs,ntrac),
     1  dudl(lon_dim,levs),dvdl(lon_dim,levs),
     1  dudf(lon_dim,levs),dvdf(lon_dim,levs)
c
c output variables
c
      real(kind=kind_evod) spdmax(levs),
     1  dudt(lon_dim,levs),dvdt(lon_dim,levs),
     1  dtdt(lon_dim,levs),drdt(lon_dim,levs,ntrac),
     1  dqdt(lon_dim)
c
c constant arrays
c
       real(kind=kind_evod)
     1 sl(levs),
     1 del(levs),rdel2(levs),
     2 ci(levp1),tov(levs)
c
c local variables
c
      real(kind=kind_evod)
     1     cg (lon_dim,levs), db(lon_dim,levs),cb(lon_dim,levs),
     2     dot(lon_dim,levp1),dup(lon_dim,levs),dvp(lon_dim,levs),
     3     dum(lon_dim,levs ),dvm(lon_dim,levs), ek(lon_dim,levs),
     4     rmu(levs ),rnu(levs),rho(levs),si(levp1),
     5      x1(levs ), x2(levs), x3(levs),x4(levs)
c
      real(kind=kind_evod) cons0,cons0p5,cons1,cons2     !constant
c
      cons0   = 0.d0      !constant
      cons0p5 = 0.5d0     !constant
      cons1   = 1.d0      !constant
      cons2   = 2.d0      !constant
c
      rk= rd /cp
      sinra=sqrt(cons1-cons1/rcl)     !constant
      fnor=cons2*omega*sinra          !constant
      sinra=sinra/rerth
 
      if(lat.gt.latg2) then
      fnor=-fnor
      sinra=-sinra
      endif
c
      si(1)=cons1     !constant
      do 4 k=1,levs
      si(k+1)=si(k)-del(k)
4     continue
c
      do 1 k=1,levm1
      rho(k)= log(si(k)/si(k+1))
1     continue
      rho(levs)=cons0     !constant
c
      do 2 k=1,levs
      rmu(k)=cons1-si(k+1)*rho(k)/del(k)     !constant
2     continue
c
      do 3 k=1,levm1
      rnu(k+1)=-cons1+si(k)*rho(k)/del(k)     !constant
3     continue
      rnu(1)=cons0     !constant
c
      do 20 k=1,levs
      x1(k)=rmu(k)*(cons1-rk*rnu(k))/(rmu(k)+rnu(k))     !constant
      x2(k)=cons1-x1(k)                                  !constant
      x3(k)=(cons1+rk*rmu(k))/(cons1-rk*rnu(k))          !constant
      x4(k)=cons1/x3(k)                                  !constant
20    continue
c
      do 1234 k=1,levs
      spdmax(k)=cons0      !constant
1234  continue
c
      rcl2=cons0p5*rcl     !constant
c
      do 140 k=1,levs
      do 140 j=1,lons_lat
      ek(j,k)=(ug(j,k)*ug(j,k)+vg(j,k)*vg(j,k))*rcl
  140 continue
c
      do 10 k=1,levs
      do 10 j=1,lons_lat
      if (ek(j,k) .gt. spdmax(k))  spdmax(k)=ek(j,k)
   10 continue
c
c     compute c=v(true)*del(ln(ps)).divide by cos for del, cos for v
c
      do 150 j=1,lons_lat
      dphi(j)=dphi(j)*rcl
      dlam(j)=dlam(j)*rcl
  150 continue
c
      do 180 k=1,levs
      do 180 j=1,lons_lat
      cg(j,k)=ug(j,k)*dlam(j)+vg(j,k)*dphi(j)
  180 continue
c
      do 190 j=1,lons_lat
      db(j,1)=del(1)*dg(j,1)
      cb(j,1)=del(1)*cg(j,1)
  190 continue
c
      do 210 k=1,levm1
      do 210 j=1,lons_lat
      db(j,k+1)=db(j,k)+del(k+1)*dg(j,k+1)
      cb(j,k+1)=cb(j,k)+del(k+1)*cg(j,k+1)
  210 continue
c
c   store integral of cg in dlax
c
      do 220 j=1,lons_lat
      dqdt(j)= -cb(j,levs)
  220 continue
c
c   sigma dot computed only at interior interfaces.
c
      do 230 j=1,lons_lat
      dot(j,1)=cons0         !constant
      dvm(j,1)=cons0         !constant
      dum(j,1)=cons0         !constant
      dot(j,levp1)=cons0     !constant
      dvp(j,levs )=cons0     !constant
      dup(j,levs )=cons0     !constant
c
  230 continue
c
      do 240 k=1,levm1
      do 240 j=1,lons_lat
      dot(j,k+1)=dot(j,k)+
     1                 del(k)*(db(j,levs)+cb(j,levs)-
     2                 dg(j,k)-cg(j,k))
  240 continue
c
c
c  implicitly filter input profiles
c  if vertical advection would be numerically unstable
c
      call vcnfil(lons_lat,lon_dim,levs,ntrac,
     x            deltim,del,sl,si,tov,dot(1,1),
     x            ug(1,1),vg(1,1),tg(1,1),rqg(1,1,1),nvcn,xvcn)
c
      do 260 k=1,levm1
      do 260 j=1,lons_lat
      dvp(j,k  )=vg(j,k+1)-vg(j,k)
      dup(j,k  )=ug(j,k+1)-ug(j,k)
      dvm(j,k+1)=vg(j,k+1)-vg(j,k)
      dum(j,k+1)=ug(j,k+1)-ug(j,k)
  260 continue
c
      do j=1,lons_lat
       dphi(j)=dphi(j)/rcl
       dlam(j)=dlam(j)/rcl
      enddo
c
      do k=1,levs
       do j=1,lons_lat
        dudt(j,k)=-ug(j,k)*dudl(j,k)-vg(j,k)*dudf(j,k)
     1 -rdel2(k)*(dot(j,k+1)*dup(j,k)+dot(j,k)*dum(j,k))
     2 -rd*tg(j,k)*dlam(j)
c
        dvdt(j,k)=-ug(j,k)*dvdl(j,k)-vg(j,k)*dvdf(j,k)
     1 -rdel2(k)*(dot(j,k+1)*dvp(j,k)+dot(j,k)*dvm(j,k))
     2 -rd*tg(j,k)*dphi(j)
       enddo
      enddo
c
      do k=1,levs
       do j=1,lons_lat
        dudt(j,k)=dudt(j,k)+vg(j,k)*fnor
c
        dvdt(j,k)=dvdt(j,k)-ug(j,k)*fnor-sinra*ek(j,k)
       enddo
      enddo
c
      do k=1,levs
       do j=1,lons_lat
        dudt(j,k)=dudt(j,k)*rcl
        dvdt(j,k)=dvdt(j,k)*rcl
       enddo
      enddo
c
c
      do 280 k=1,levm1
      do 280 j=1,lons_lat
cecmwf:
      dup(j,k  )=tg(j,k+1)+tov(k+1)-tg(j,k)-tov(k)
     x          +cons2*rk*rnu(k+1)*(tg(j,k)+tov(k))       !constant
cecmwf:
      dum(j,k+1)=tg(j,k+1)+tov(k+1)-tg(j,k)-tov(k)
     x        +cons2*rk*rmu(k+1)*(tg(j,k+1)+tov(k+1))     !constant
cecmwf:
cecmwf:
  280 continue
c
c
      do k=1,levs
       do j=1,lons_lat
c       dtdt(j,k)=
        dtdt(j,k)=-ug(j,k)*dtdl(j,k)-vg(j,k)*dtdf(j,k)
     1 -rdel2(k)*(dot(j,k+1)*dup(j,k)+dot(j,k)*dum(j,k))
       enddo
      enddo
c
      do k=1,levs
       do j=1,lons_lat
        dtdt(j,k)=dtdt(j,k)
     1  +rk*(tov(k)+tg(j,k))*(cg(j,k)-cb(j,levs)-db(j,levs))
       enddo
      enddo
c
      do 330 n=1,ntrac
      do 300 k=1,levm1
      do 300 j=1,lons_lat
      dup(j,k  )=rqg(j,k+1,n)-rqg(j,k,n)
      dum(j,k+1)=rqg(j,k+1,n)-rqg(j,k,n)
  300 continue
c
      do 310 j=1,lons_lat
      dup(j,levs)=cons0     !constant
  310 continue
c
      do 320 k=1,levs
      do 320 j=1,lons_lat
      drdt(j,k,n)=-ug(j,k)*drdl(j,k,n)-vg(j,k)*drdf(j,k,n)
     1 -rdel2(k)*(dot(j,k+1)*dup(j,k)+dot(j,k)*dum(j,k))
  320 continue
  330 continue
c
      return
      end
c-----------------------------------------------------------------------
      subroutine vcnfil(im,ix,km,nt,deltim,del,sl,si,tov,dot,
     &                  u,v,t,q,nvcn,xvcn)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    vcnfil      vertical advection instability filter
c   prgmmr: iredell          org: w/nmc23    date: 91-05-07
c
c abstract: prefilters fields appearing in the nonlinear terms
c   in the dynamics tendency equation in order to ensure stability
c   when the vertical velocity exceeds the cfl criterion.
c   the vertical velocity in this case is sigmadot.
c   for simple second-order centered eulerian advection,
c   filtering is needed when vcn=sigmadot*deltim/dsigma>1.
c   the maximum eigenvalue of the linear advection equation
c   with second-order implicit filtering on the tendencies
c   is less than one for all resolvable wavenumbers (i.e. stable)
c   if the nondimensional filter parameter is nu=(vcn**2-1)/4.
c
c program history log:
c   97-07-30  iredell
c   98-02-20  iredell  increase margin of error in filter
c                      by starting filter when vcn>0.9.
c
c usage:  call vcnfil(im,ix,km,nt,deltim,del,sl,si,tov,dot,
c    &                u,v,t,q,nvcn,xvcn)
c
c   input argument list:
c     im       - integer number of gridpoints to filter
c     ix       - integer first dimension of dot,u,v,t,q
c     km       - integer number of vertical levels
c     nt       - integer number of tracers in q
c     deltim   - real timestep in seconds
c     del      - real (km) sigma thicknesses
c     sl       - real (km) full sigma values
c     si       - real (km+1) interface sigma values
c     tov      - real (km) temperature base
c     dot      - real (ix,km+1) sigmadot in 1/seconds
c     u        - real (ix,km) zonal wind
c     v        - real (ix,km) meridional wind
c     t        - real (ix,km) virtual temperature deviation
c     q        - real (ix,km,nt) tracers
c
c   output argument list:
c     u        - real (ix,km) zonal wind
c     v        - real (ix,km) meridional wind
c     t        - real (ix,km) virtual temperature deviation
c     q        - real (ix,km,nt) tracers
c     nvcn     - integer number of points requiring filtering
c     xvcn     - real maximum vertical courant number
c
c   subprograms called:
c     tridim   - tridiagonal matrix solver
c
c$$$
cfpp$ noconcur r
cc
      USE MACHINE , ONLY : kind_phys

      use resol_def
      use physcons, rerth => con_rerth, rocp => con_rocp
      implicit none
cc
      integer              i,im,ix,j,k,km,n,nt,nvcn
      real(kind=kind_evod) cvcn,deli,deltim,rnu,t0term,t1term
!     real(kind=kind_evod) cvcn,deli,deltim,rnu,rocp,t0term,t1term
      real(kind=kind_evod) tterm,xvcn
cc
!     parameter(rocp=rd/cp)
      parameter(cvcn=0.9d0)     !constant
      real(kind=kind_evod) del(km),sl(km),si(km+1),tov(km)
      real(kind=kind_evod) dot(ix,km+1)
      real(kind=kind_evod) u(ix,km),v(ix,km),t(ix,km),q(ix,km,nt)
      logical              lvcn(im)
      integer              ivcn(im)
      real(kind=kind_evod) vcn(ix,km-1)
      real(kind=kind_evod) cm(ix,km),cu(ix,km-1),cl(ix,km-1)
      real(kind=kind_evod) rr(ix,km,3+nt)
cc
      real(kind=kind_evod) cons0,cons0p5,cons1,cons4     !constant
cc
      cons0   =  0.d0         !constant
      cons0p5 = 0.5d0         !constant
      cons1   =  1.d0         !constant
      cons4   =  4.d0         !constant
cc
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c  compute vertical courant number
      nvcn=0
      xvcn=cons0     !constant
      do i=1,im
        lvcn(i)=.false.
      enddo
      do k=1,km-1
        do i=1,im
          deli=sl(k)-sl(k+1)
          vcn(i,k)=abs(dot(i,k+1))*deltim/deli
          lvcn(i)=lvcn(i).or.vcn(i,k).gt.cvcn
          xvcn=max(xvcn,vcn(i,k))
        enddo
      enddo
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c  determine points requiring filtering
      if(xvcn.gt.cvcn) then
        do i=1,im
          if(lvcn(i)) then
            ivcn(nvcn+1)=i
            nvcn=nvcn+1
          endif
        enddo
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c  compute tridiagonal matrix
        do j=1,nvcn
          cm(j,1)=cons1                           !constant
        enddo
        do k=1,km-1
          deli=sl(k)-sl(k+1)
          do j=1,nvcn
            i=ivcn(j)
            if(vcn(i,k).gt.cvcn) then
              rnu=(vcn(i,k)**2-cvcn**2)/cons4     !constant
              cu(j,k)=-rnu*deli/del(k)
              cl(j,k)=-rnu*deli/del(k+1)
              cm(j,k)=cm(j,k)-cu(j,k)
              cm(j,k+1)=cons1-cl(j,k)             !constant
            else
              cu(j,k)=cons0                       !constant
              cl(j,k)=cons0                       !constant
              cm(j,k+1)=cons1                     !constant
            endif
          enddo
        enddo
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c  fill fields to be filtered
        do k=1,km
          do j=1,nvcn
            i=ivcn(j)
            rr(j,k,1)=u(i,k)
            rr(j,k,2)=v(i,k)
            rr(j,k,3)=t(i,k)+tov(k)
          enddo
          do n=1,nt
            do j=1,nvcn
              i=ivcn(j)
              rr(j,k,3+n)=q(i,k,n)
            enddo
          enddo
        enddo
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c  adjust temperature for adiabatic change
        do k=1,km-1
          deli=sl(k)-sl(k+1)
          t1term=cons0p5*rocp*deli/si(k+1)     !constant
          t0term=t1term*(tov(k)+tov(k+1))
          do j=1,nvcn
            i=ivcn(j)
            tterm=t0term+t1term*(t(i,k)+t(i,k+1))
            rr(j,k,3)=rr(j,k,3)-cu(j,k)*tterm
            rr(j,k+1,3)=rr(j,k+1,3)+cl(j,k)*tterm
          enddo
        enddo
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c  solve tridiagonal system
        call tridim(nvcn,ix,km,km,3+nt,cl,cm,cu,rr,cu,rr)
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c  replace filtered fields
        do k=1,km
          do j=1,nvcn
            i=ivcn(j)
            u(i,k)=rr(j,k,1)
            v(i,k)=rr(j,k,2)
            t(i,k)=rr(j,k,3)-tov(k)
          enddo
          do n=1,nt
            do j=1,nvcn
              i=ivcn(j)
              q(i,k,n)=rr(j,k,3+n)
            enddo
          enddo
        enddo
      endif
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end
c-----------------------------------------------------------------------
      subroutine tridim(l,lx,n,nx,m,cl,cm,cu,r,au,a)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    tridim      solves tridiagonal matrix problems.
c   prgmmr: iredell          org: w/nmc23    date: 91-05-07
c
c abstract: this routine solves multiple tridiagonal matrix problems
c   with multiple right-hand-side and solution vectors for every matrix.
c   the solutions are found by eliminating off-diagonal coefficients,
c   marching first foreward then backward along the matrix diagonal.
c   the computations are vectorized around the number of matrices.
c   no checks are made for zeroes on the diagonal or singularity.
c
c program history log:
c   97-07-30  iredell
c
c usage:    call tridim(l,lx,n,nx,m,cl,cm,cu,r,au,a)
c
c   input argument list:
c     l        - integer number of tridiagonal matrices
c     lx       - integer first dimension (lx>=l)
c     n        - integer order of the matrices
c     nx       - integer second dimension (nx>=n)
c     m        - integer number of vectors for every matrix
c     cl       - real (lx,2:n) lower diagonal matrix elements
c     cm       - real (lx,n) main diagonal matrix elements
c     cu       - real (lx,n-1) upper diagonal matrix elements
c                (may be equivalent to au if no longer needed)
c     r        - real (lx,nx,m) right-hand-side vector elements
c                (may be equivalent to a if no longer needed)
c
c   output argument list:
c     au       - real (lx,n-1) work array
c     a        - real (lx,nx,m) solution vector elements
c
c attributes:
c   language: fortran 77.
c   machine:  cray.
c
c$$$
cfpp$ noconcur r
cc
      use machine
      implicit none
c
      integer              i,j,k,l,lx,m,n,nx
      real(kind=kind_evod) fk
cc
      real(kind=kind_evod) cl(lx,2:n),cm(lx,n),cu(lx,n-1),r(lx,nx,m),
     &                                         au(lx,n-1),a(lx,nx,m)
cc
      real(kind=kind_evod) cons1                        !constant
cc
      cons1   =  1.d0                                   !constant
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c  march up
      do i=1,l
        fk=cons1/cm(i,1)                                !constant
        au(i,1)=fk*cu(i,1)
      enddo
      do j=1,m
        do i=1,l
          fk=cons1/cm(i,1)                              !constant
          a(i,1,j)=fk*r(i,1,j)
        enddo
      enddo
      do k=2,n-1
        do i=1,l
          fk=cons1/(cm(i,k)-cl(i,k)*au(i,k-1))          !constant
          au(i,k)=fk*cu(i,k)
        enddo
        do j=1,m
          do i=1,l
            fk=cons1/(cm(i,k)-cl(i,k)*au(i,k-1))        !constant
            a(i,k,j)=fk*(r(i,k,j)-cl(i,k)*a(i,k-1,j))
          enddo
        enddo
      enddo
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c  march down
      do j=1,m
        do i=1,l
          fk=cons1/(cm(i,n)-cl(i,n)*au(i,n-1))          !constant
          a(i,n,j)=fk*(r(i,n,j)-cl(i,n)*a(i,n-1,j))
        enddo
      enddo
      do k=n-1,1,-1
        do j=1,m
          do i=1,l
            a(i,k,j)=a(i,k,j)-au(i,k)*a(i,k+1,j)
          enddo
        enddo
      enddo
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end
