      subroutine gwdc(im,ix,iy,km,lat,u1,v1,t1,q1,
     &                rcs,pmid1,pint1,dpmid1,qmax,cumchr1,ktop,kbot,kuo,
     &                fu1,fv1,g,cp,rd,fv,dlength,lprnt,ipr,fhour,
     &                tauctx,taucty,brunm1,rhom1)
!    &                gwdcloc,critic,brunm1,rhom1)

!***********************************************************************
!        ORIGINAL CODE FOR PARAMETERIZATION OF CONVECTIVELY FORCED
!        GRAVITY WAVE DRAG FROM YONSEI UNIVERSITY, KOREA
!        BASED ON THE THEORY GIVEN BY CHUN AND BAIK (JAS, 1998)
!        MODIFIED FOR IMPLEMENTATION INTO THE GFS/CFS BY
!        AKE JOHANSSON  --- AUG 2005
!***********************************************************************

      USE MACHINE , ONLY : kind_phys
      implicit none

!---------------------------- Arguments --------------------------------
!
!  Input variables
!
!  u        : midpoint zonal wind
!  v        : midpoint meridional wind
!  t        : midpoint temperatures
!  pmid     : midpoint pressures
!  pint     : interface pressures
!  dpmid    : midpoint delta p ( pi(k)-pi(k-1) )
!  rcs      : reciprocal of cosine latitude         rcs=1/cos(lat)
!  rcsi     : cosine latitude                       rcsi=cos(lat)
!  lat      : latitude index
!  qmax     : deep convective heating
!  kcldtop  : Vertical level index for cloud top    ( mid level ) 
!  kcldbot  : Vertical level index for cloud bottom ( mid level )
!  kuo      : (0,1) dependent on whether convection occur or not
!
!  Output variables
!
!  fu1     : zonal wind tendency
!  fv1     : meridional wind tendency
!
!-----------------------------------------------------------------------

      integer im, ix, iy, km, lat, ipr, ilev
      integer ktop(im),kbot(im),kuo(im)
      integer kcldtop(im),kcldbot(im)

      real(kind=kind_phys) g,cp,rd,fv,dlength(im),rcs(im),rcsi(im)
      real(kind=kind_phys) qmax(ix),cumchr1(ix,km),cumchr(ix,km)
      real(kind=kind_phys) fhour,fhourpr
      real(kind=kind_phys) u1(ix,km),v1(ix,km),t1(ix,km),q1(ix,km),
     &                     pmid1(ix,km),dpmid1(ix,km),pint1(ix,km+1),
     &                     fu1(iy,km),fv1(iy,km)
      real(kind=kind_phys) u(im,km),v(im,km),t(im,km),spfh(im,km),
     &                     pmid(im,km),dpmid(im,km),pint(im,km+1)

      logical lprnt

!------------------------- Local workspace -----------------------------
!
!  i, k     : Loop index
!  ii,kk    : Loop index
!  cldbar   : Deep convective cloud coverage at the cloud top.
!  ugwdc    : Zonal wind after GWDC paramterization
!  vgwdc    : Meridional wind after GWDC parameterization
!  plnmid   : Log(pmid) ( mid level )
!  plnint   : Log(pint) ( interface level )
!  dpint    : Delta pmid ( interface level )
!  tauct    : Wave stress at the cloud top calculated using basic-wind
!             parallel to the wind vector at the cloud top ( mid level )
!  tauctx   : Wave stress at the cloud top projected in the east
!  taucty   : Wave stress at the cloud top projected in the north
!  qmax     : Maximum deep convective heating rate ( K s-1 ) in a  
!             horizontal grid point calculated from cumulus para-
!             meterization. ( mid level )
!  wtgwc    : Wind tendency in direction to the wind vector at the cloud top level
!             due to convectively generated gravity waves ( mid level )
!  utgwc    : Zonal wind tendency due to convectively generated 
!             gravity waves ( mid level )
!  vtgwc    : Meridional wind tendency due to convectively generated
!             gravity waves ( mid level )
!  taugwci  : Profile of wave stress calculated using basic-wind
!             parallel to the wind vector at the cloud top 
!  taugwcxi : Profile of zonal component of gravity wave stress
!  taugwcyi : Profile of meridional component of gravity wave stress 
!
!  taugwci, taugwcxi, and taugwcyi are defined at the interface level
!
!  bruni    : Brunt-Vaisala frequency ( interface level )
!  brunm    : Brunt-Vaisala frequency ( mid level )
!  rhoi     : Air density ( interface level )
!  rhom     : Air density ( mid level )
!  ti       : Temperature ( interface level )
!  basicum  : Basic-wind profile. Basic-wind is parallel to the wind
!             vector at the cloud top level. (mid level) 
!  basicui  : Basic-wind profile. Basic-wind is parallel to the wind
!             vector at the cloud top level. ( interface level )
!  riloc    : Local Richardson number ( interface level )
!  rimin    : Minimum Richardson number including both the basic-state
!             and gravity wave effects ( interface level )
!  gwdcloc  : Horizontal location where the GWDC scheme is activated.
!  break    : Horizontal location where wave breaking is occurred.
!  critic   : Horizontal location where critical level filtering is
!             occurred.
!  dogwdc   : Logical flag whether the GWDC parameterization is           
!             calculated at a grid point or not.
!  
!  dogwdc is used in order to lessen CPU time for GWDC calculation.
!
!-----------------------------------------------------------------------

      integer i,ii,k,k1,k2,kk,kb

      real(kind=kind_phys) cldbar(im),
     &     ugwdc(im,km),vgwdc(im,km),
     &     plnmid(im,km),plnint(im,km+1),dpint(im,km+1),
     &     tauct(im),tauctx(im),taucty(im),
     &     wtgwc(im,km),utgwc(im,km),vtgwc(im,km),
     &     taugwci(im,km+1),taugwcxi(im,km+1),taugwcyi(im,km+1),
     &     bruni(im,km+1),rhoi(im,km+1),ti(im,km+1),
     &     brunm(im,km),rhom(im,km),brunm1(im,km),rhom1(im,km),
     &     basicum(im,km),basicui(im,km+1),
     &     riloc(km+1),rimin(km+1)

      real(kind=kind_phys) gwdcloc(im),break(im),critic(im)
      real(kind=kind_phys) tem1, tem2, qtem

      logical dogwdc(im)

!-----------------------------------------------------------------------
!
!  ucltop    : Zonal wind at the cloud top ( mid level )
!  vcltop    : Meridional wind at the cloud top ( mid level )
!  windcltop : Wind speed at the cloud top ( mid level )
!  shear     : Vertical shear of basic wind 
!  cosphi    : Cosine of angle of wind vector at the cloud top
!  sinphi    : Sine   of angle of wind vector at the cloud top
!  c1        : Tunable parameter
!  c2        : Tunable parameter
!  dlength   : Grid spacing in the direction of basic wind at the cloud top
!  nonlinct  : Nonlinear parameter at the cloud top
!  nonlin    : Nonlinear parameter above the cloud top
!  nonlins   : Saturation nonlinear parameter
!  taus      : Saturation gravity wave drag
!  n2        : Square of Brunt-Vaisala frequency
!  dtdp      : dT/dp
!  xstress   : Vertically integrated zonal momentum change due to GWDC
!  ystress   : Vertically integrated meridional momentum change due to GWDC
!  crit1     : Variable 1 for checking critical level
!  crit2     : Variable 2 for checking critical level
!  sum1      : Temporary variable
!
!-----------------------------------------------------------------------

      real(kind=kind_phys) ucltop, vcltop, windcltop, shear, kcldtopi
      real(kind=kind_phys) cosphi, sinphi, angle
      real(kind=kind_phys) nonlinct, nonlin, nonlins, taus 

!-----------------------------------------------------------------------
      real(kind=kind_phys), parameter ::
     &                      c1=1.41,          c2=-0.38,     ricrit=0.25
     &,                     n2min=1.e-32,     zero=0.0,     one=1.0
     &,                     taumin=1.0e-20,   tauctmax=-20.
     &,                     qmin=1.0e-10,     shmin=1.0e-20
     &,                     rimax=1.0e+20,    rimaxm=0.99e+20
     &,                     rimaxp=1.01e+20,  rilarge=0.9e+20
     &,                     riminx=-1.0e+20,  riminm=-1.01e+20
     &,                     riminp=-0.99e+20, rismall=-0.9e+20

      real(kind=kind_phys) n2, dtdp, sum1, xstress, ystress
      real(kind=kind_phys) crit1, crit2
      real(kind=kind_phys) pi,p1,p2

!-----------------------------------------------------------------------
!        Write out incoming variables
!-----------------------------------------------------------------------

      fhourpr = zero
      if (lprnt) then
        if (fhour.ge.fhourpr) then
          print *,' '
          write(*,*) 'Inside GWDC raw input start print at fhour = ',
     &               fhour
          write(*,*) 'IX  IM  KM  ',ix,im,km
          write(*,*) 'KBOT KTOP QMAX DLENGTH KUO  ',
     +     kbot(ipr),ktop(ipr),qmax(ipr),dlength(ipr),kuo(ipr)
          write(*,*) 'g  cp  rd  RCS  ',g,cp,rd,RCS(ipr)

!-------- Pressure levels ----------
          write(*,9100)
          ilev=km+1
          write(*,9110) ilev,(10.*pint1(ipr,ilev))
          do ilev=km,1,-1
            write(*,9120) ilev,(10.*pmid1(ipr,ilev)),
     &                         (10.*dpmid1(ipr,ilev))
            write(*,9110) ilev,(10.*pint1(ipr,ilev))
          enddo

!-------- U1 V1 T1 ----------
          write(*,9130)
          do ilev=km,1,-1
            write(*,9140) ilev,U1(ipr,ilev),V1(ipr,ilev),T1(ipr,ilev)
          enddo

          print *,' '
          print *,' Inside GWDC raw input end print'
        endif
      endif

 9100 format(//,14x,'PRESSURE LEVELS',//,
     +' ILEV',6x,'PINT1',7x,'PMID1',6x,'DPMID1',/)
 9110 format(i4,2x,f10.3)
 9120 format(i4,12x,2(2x,f10.3))
 9130 format(//,' ILEV',7x,'U1',10x,'V1',10x,'T1',/)
 9140 format(i4,3(2x,f10.3))

!-----------------------------------------------------------------------
!        Create local arrays with reversed vertical indices
!        Make regular (U,V) by using array RCS=1/cos(lat)
!          Incoming (U1,V1)=cos(lat)*(U,V)
!        Make pressure have unit of Pa [Multiply by 1000]
!          Incoming pressures in kPa
!-----------------------------------------------------------------------

      do k=1,km
        k1 = km - k + 1
        do i=1,im
          u(i,k)      = u1(i,k1)*rcs(i)
          v(i,k)      = v1(i,k1)*rcs(i)
          t(i,k)      = t1(i,k1)
          spfh(i,k)   = max(q1(i,k1),qmin)
          pmid(i,k)   = pmid1(i,k1)*1000.
          dpmid(i,k)  = dpmid1(i,k1)*1000.
          cumchr(i,k) = cumchr1(i,k1)
        enddo
      enddo

      do k=1,km+1
        k1 = km - k + 2
        do i=1,im
          pint(i,k) = pint1(i,k1)*1000.
        enddo
      enddo

      do i = 1, im
        kcldtop(i) = km - ktop(i) + 1
        kcldbot(i) = km - kbot(i) + 1
      enddo

      if (lprnt) then
        if (fhour.ge.fhourpr) then
          write(*,9200)
          do i=1,im
            write(*,9201) kuo(i),kcldbot(i),kcldtop(i)
          enddo
        endif
      endif

 9200 format(//,'  Inside GWDC local variables start print',//,
     +2x,'KUO',2x,'KCLDBOT',2x,'KCLDTOP',//)
 9201 format(i4,2x,i5,4x,i5)

!***********************************************************************
!
!  Begin GWDC
!
!***********************************************************************

      pi     = 2.*asin(1.)

!-----------------------------------------------------------------------
!
!  Initialize local variables
!
!-----------------------------------------------------------------------
!                              PRESSURE VARIABLES
!
!  Interface 1 ======== pint(1)           *********
!  Mid-Level 1 --------          pmid(1)            dpmid(1)
!            2 ======== pint(2)           dpint(2)
!            2 --------          pmid(2)            dpmid(2)
!            3 ======== pint(3)           dpint(3)
!            3 --------          pmid(3)            dpmid(3)
!            4 ======== pint(4)           dpint(4)
!            4 --------          pmid(4)            dpmid(4)
!              ........
!           17 ======== pint(17)          dpint(17) 
!           17 --------          pmid(17)           dpmid(17)
!           18 ======== pint(18)          dpint(18)
!           18 --------          pmid(18)           dpmid(18)
!           19 ======== pint(19)          *********
!
!-----------------------------------------------------------------------

      do k = 1, km+1
        do i = 1, im
          plnint(i,k)   = log(pint(i,k))
          taugwci(i,k)  = zero
          taugwcxi(i,k) = zero
          taugwcyi(i,k) = zero
          bruni(i,k)    = zero
          rhoi(i,k)     = zero
          ti(i,k)       = zero
          basicui(i,k)  = zero
          riloc(k)      = zero
          rimin(k)      = zero
        enddo
      enddo

      do k = 1, km
        do i = 1, im
          plnmid(i,k)  = log(pmid(i,k))
          wtgwc(i,k)   = zero
          utgwc(i,k)   = zero
          vtgwc(i,k)   = zero
          ugwdc(i,k)   = zero
          vgwdc(i,k)   = zero
          brunm(i,k)   = zero
          rhom(i,k)    = zero
          basicum(i,k) = zero
        enddo
      enddo

      do k = 2, km
        do i = 1, im
          dpint(i,k) = pmid(i,k) - pmid(i,k-1)
        enddo
      enddo

      do i = 1, im
        dpint(i,1)    = zero
        dpint(i,km+1) = zero
        tauct(i)      = zero
        tauctx(i)     = zero
        taucty(i)     = zero
        gwdcloc(i)    = zero
        break(i)      = zero
        critic(i)     = zero
      enddo

!-----------------------------------------------------------------------
!                              THERMAL VARIABLES
!
!  Interface 1 ========       TI(1)           RHOI(1)            BRUNI(1)
!            1 -------- T(1)         RHOM(1)           BRUNM(1)
!            2 ========       TI(2)           RHOI(2)            BRUNI(2)
!            2 -------- T(2)         RHOM(2)           BRUNM(2)
!            3 ========       TI(3)           RHOI(3)            BRUNI(3)
!            3 -------- T(3)         RHOM(3)           BRUNM(3)
!            4 ========       TI(4)           RHOI(4)            BRUNI(4)
!            4 -------- T(4)         RHOM(4)           BRUNM(4)
!              ........
!           17 ========
!           17 -------- T(17)        RHOM(17)          BRUNM(17)
!           18 ========       TI(18)          RHOI(18)           BRUNI(18)
!           18 -------- T(18)        RHOM(18)          BRUNM(18)
!           19 ========       TI(19)          RHOI(19)           BRUNI(19)
!
!-----------------------------------------------------------------------

      do k = 1, km
        do i = 1, im
          rhom(i,k) = pmid(i,k) / (rd*t(i,k)*(1.0+fv*spfh(i,k)))
        enddo
      enddo

!-----------------------------------------------------------------------
!
!  Top interface temperature is calculated assuming an isothermal 
!  atmosphere above the top mid level.
!
!-----------------------------------------------------------------------

      do i = 1, im
        ti(i,1)    = t(i,1)
        rhoi(i,1)  = pint(i,1)/(rd*ti(i,1))
        bruni(i,1) = sqrt ( g*g / (cp*ti(i,1)) )
      enddo

!-----------------------------------------------------------------------
!
!  Calculate interface level temperature, density, and Brunt-Vaisala
!  frequencies based on linear interpolation of Temp in ln(Pressure)
!
!-----------------------------------------------------------------------

      do k = 2, km
        do i = 1, im
          tem1 = (plnmid(i,k)-plnint(i,k)) / (plnmid(i,k)-plnmid(i,k-1))
          tem2 = one - tem1
          ti(i,k)    = t(i,k-1)    * tem1 + t(i,k)    * tem2
          qtem       = spfh(i,k-1) * tem1 + spfh(i,k) * tem2
          rhoi(i,k)  = pint(i,k) / ( rd * ti(i,k)*(1.0+fv*qtem) )
          dtdp       = (t(i,k)-t(i,k-1)) / (pmid(i,k)-pmid(i,k-1))
          n2         = g*g/ti(i,k)*( 1./cp - rhoi(i,k)*dtdp ) 
          bruni(i,k) = sqrt (max (n2min, n2))
        enddo
      enddo

!-----------------------------------------------------------------------
!
!  Bottom interface temperature is calculated assuming an isothermal
!  atmosphere below the bottom mid level
!
!-----------------------------------------------------------------------
   
      do i = 1, im
        ti(i,km+1)    = t(i,km)
        rhoi(i,km+1)  = pint(i,km+1)/(rd*ti(i,km+1)*(1.0+fv*spfh(i,km)))
        bruni(i,km+1) = sqrt ( g*g / (cp*ti(i,km+1)) )
      enddo
 
!-----------------------------------------------------------------------
!
!  Determine the mid-level Brunt-Vaisala frequencies.
!             based on interpolated interface Temperatures [ ti ]
!
!-----------------------------------------------------------------------

      do k = 1, km
        do i = 1, im
          dtdp       = (ti(i,k+1)-ti(i,k)) / (pint(i,k+1)-pint(i,k))
          n2         = g*g/t(i,k)*( 1./cp - rhom(i,k)*dtdp ) 
          brunm(i,k) = sqrt (max (n2min, n2))
        enddo
      enddo

!-----------------------------------------------------------------------
!        PRINTOUT
!-----------------------------------------------------------------------

      if (lprnt) then
        if (fhour.ge.fhourpr) then

!-------- Pressure levels ----------
          write(*,9101)
          do ilev=1,km
            write(*,9111) ilev,(0.01*pint(ipr,ilev)),
     &                         (0.01*dpint(ipr,ilev)),plnint(ipr,ilev)
            write(*,9121) ilev,(0.01*pmid(ipr,ilev)),
     &                         (0.01*dpmid(ipr,ilev)),plnmid(ipr,ilev)
          enddo
          ilev=km+1
          write(*,9111) ilev,(0.01*pint(ipr,ilev)),
     &                       (0.01*dpint(ipr,ilev)),plnint(ipr,ilev)

!                2
!-------- U V T N  ----------
          write(*,9102)
          do ilev=1,km
            write(*,9112) ilev,ti(ipr,ilev),(100.*bruni(ipr,ilev))
            write(*,9122) ilev,u(ipr,ilev),v(ipr,ilev),
     +                    t(ipr,ilev),(100.*brunm(ipr,ilev))
          enddo
          ilev=km+1
          write(*,9112) ilev,ti(ipr,ilev),(100.*bruni(ipr,ilev))

        endif
      endif

 9101 format(//,14x,'PRESSURE LEVELS',//,
     +' ILEV',4x,'PINT',4x,'PMID',4x,'DPINT',3x,'DPMID',5x,'LNP',/)
 9111 format(i4,1x,f8.2,9x,f8.2,9x,f8.2)
 9121 format(i4,9x,f8.2,9x,f8.2,1x,f8.2)
 9102 format(//' ILEV',5x,'U',7x,'V',5x,'TI',7x,'T',
     +5x,'BRUNI',3x,'BRUNM',//)
 9112 format(i4,16x,f8.2,8x,f8.3)
 9122 format(i4,2f8.2,8x,f8.2,8x,f8.3)

!-----------------------------------------------------------------------
!
!  Set switch for no convection present
!
!-----------------------------------------------------------------------

      do i = 1, im
        dogwdc(i) =.true.
        if (kuo(i) == 0 .or. qmax(i) <= zero) dogwdc(i) =.false.
      enddo

!***********************************************************************
!
!        Big loop over grid points                    ONLY done if KUO=1
!
!***********************************************************************

      do i = 1, im

      if ( dogwdc(i) ) then                 !  For fast GWDC calculation

      kk        = kcldtop(i)
      kb        = kcldbot(i)
      cldbar(i) = 0.1

!-----------------------------------------------------------------------
!
!  Determine cloud top wind component, direction, and speed.
!  Here, ucltop, vcltop, and windcltop are wind components and 
!  wind speed at mid-level cloud top index
!
!-----------------------------------------------------------------------

      ucltop    = u(i,kcldtop(i))
      vcltop    = v(i,kcldtop(i))
      windcltop = sqrt( ucltop*ucltop + vcltop*vcltop )
      cosphi    = ucltop/windcltop
      sinphi    = vcltop/windcltop
      angle     = acos(cosphi)*180./pi

!-----------------------------------------------------------------------
!
!  Calculate basic state wind projected in the direction of the cloud 
!  top wind.
!  Input u(i,k) and v(i,k) is defined at mid level
!
!-----------------------------------------------------------------------

      do k=1,km
        basicum(i,k) = u(i,k)*cosphi + v(i,k)*sinphi
      enddo

!-----------------------------------------------------------------------
!
!  Basic state wind at interface level is also calculated
!  based on linear interpolation in ln(Pressure)
!
!  In the top and bottom boundaries, basic-state wind at interface level
!  is assumed to be vertically uniform.
!
!-----------------------------------------------------------------------

      basicui(i,1)   = basicum(i,1)
      do k=2,km
        tem1 = (plnmid(i,k)-plnint(i,k)) / (plnmid(i,k)-plnmid(i,k-1))
        tem2 = one - tem1
        basicui(i,k) = basicum(i,k)*tem2 + basicum(i,k-1)*tem2
      enddo
      basicui(i,km+1) = basicum(i,km)

!-----------------------------------------------------------------------
!
!  Calculate local richardson number 
!
!  basicum   : U at mid level
!  basicui   : UI at interface level
!
!  Interface 1 ========       UI(1)            rhoi(1)  bruni(1)  riloc(1)
!  Mid-level 1 -------- U(1)
!            2 ========       UI(2)  dpint(2)  rhoi(2)  bruni(2)  riloc(2)
!            2 -------- U(2)
!            3 ========       UI(3)  dpint(3)  rhoi(3)  bruni(3)  riloc(3)
!            3 -------- U(3)
!            4 ========       UI(4)  dpint(4)  rhoi(4)  bruni(4)  riloc(4)
!            4 -------- U(4)
!              ........
!           17 ========       UI(17) dpint(17) rhoi(17) bruni(17) riloc(17)
!           17 -------- U(17)
!           18 ========       UI(18) dpint(18) rhoi(18) bruni(18) riloc(18)
!           18 -------- U(18)
!           19 ========       UI(19)           rhoi(19) bruni(19) riloc(19)
!
!-----------------------------------------------------------------------     

      do k=2,km
         shear     =  (basicum(i,k) - basicum(i,k-1))/dpint(i,k) *
     &                ( rhoi(i,k)*g )
         if ( abs(shear) .lt. shmin ) then
           riloc(k) = rimax
         else
           riloc(k)  = (bruni(i,k)/shear) ** 2 
           if (riloc(k) .ge. rimax ) riloc(k) = rilarge
         end if 
      enddo
 
      riloc(1)    = riloc(2)
      riloc(km+1) = riloc(km)

      if (lprnt.and.(i.eq.ipr)) then
        if (fhour.ge.fhourpr) then
          write(*,9104) ucltop,vcltop,windcltop,angle,kk
          do ilev=1,km
            write(*,9114) ilev,basicui(ipr,ilev),dpint(ipr,ilev),
     +      rhoi(ipr,ilev),(100.*bruni(ipr,ilev)),riloc(ilev)
            write(*,9124) ilev,(basicum(ipr,ilev))
          enddo
          ilev=km+1
          write(*,9114) ilev,basicui(ipr,ilev),dpint(ipr,ilev),
     +      rhoi(ipr,ilev),(100.*bruni(ipr,ilev)),riloc(ilev)
        endif
      endif

 9104 format(//,'WIND VECTOR AT CLOUDTOP = (',f6.2,' , ',f6.2,' ) = ',
     +f6.2,' IN DIRECTION ',f6.2,4x,'KK = ',i2,//,
     +' ILEV',2x,'BASICUM',2x,'BASICUI',4x,'DPINT',6x,'RHOI',5x,
     +'BRUNI',6x,'RI',/)
 9114 format(i4,10x,f8.2,4(2x,f8.2))
 9124 format(i4,1x,f8.2)

!-----------------------------------------------------------------------
!
!  Calculate gravity wave stress at the interface level cloud top
!      
!  kcldtopi  : The interface level cloud top index
!  kcldtop   : The midlevel cloud top index
!  kcldbot   : The midlevel cloud bottom index
!
!  A : Find deep convective heating rate maximum
!
!      If kcldtop(i) is less than kcldbot(i) in a horizontal grid point,
!      it can be thought that there is deep convective cloud. However,
!      deep convective heating between kcldbot and kcldtop is sometimes
!      zero in spite of kcldtop less than kcldbot. In this case,
!      maximum deep convective heating is assumed to be 1.e-30. 
!
!  B : kk is the vertical index for interface level cloud top
!
!  C : Total convective fractional cover (cldbar) is used as the
!      convective cloud cover for GWDC calculation instead of   
!      convective cloud cover in each layer (concld).
!                       a1 = cldbar*dlength
!      You can see the difference between cldbar(i) and concld(i)
!      in (4.a.2) in Description of the NCAR Community Climate    
!      Model (CCM3).
!      In NCAR CCM3, cloud fractional cover in each layer in a deep
!      cumulus convection is determined assuming total convective
!      cloud cover is randomly overlapped in each layer in the 
!      cumulus convection.
!
!  D : Wave stress at cloud top is calculated when the atmosphere
!      is dynamically stable at the cloud top
!
!  E : Cloud top wave stress and nonlinear parameter are calculated 
!      using density, temperature, and wind that are defined at mid
!      level just below the interface level in which cloud top wave
!      stress is defined.
!      Nonlinct is defined at the interface level.
!  
!  F : If the atmosphere is dynamically unstable at the cloud top,
!      GWDC calculation in current horizontal grid is skipped.  
!
!  G : If mean wind at the cloud top is less than zero, GWDC
!      calculation in current horizontal grid is skipped.
!
!  H : Maximum cloud top stress, tauctmax =  -20 N m^(-2),
!      in order to prevent numerical instability.
!
!-----------------------------------------------------------------------
!D
      if ( basicui(i,kcldtop(i)) > zero ) then 
!E
        if ( riloc(kcldtop(i)) > ricrit ) then
          nonlinct  = ( g*qmax(i)*cldbar(i)*dlength(i) )/
     &  (bruni(i,kcldtop(i))*t(i,kcldtop(i))*(basicum(i,kcldtop(i))**2))
          tauct(i)  = - (rhom(i,kcldtop(i))*(basicum(i,kcldtop(i))**2))
     &              /   (bruni(i,kcldtop(i))*dlength(i))
     &              * basicum(i,kcldtop(i))*c1*c2*c2*nonlinct*nonlinct
          tauctx(i) = tauct(i)*cosphi
          taucty(i) = tauct(i)*sinphi
        else
!F
          tauct(i)  = zero
          tauctx(i) = zero 
          taucty(i) = zero
          go to 1000
        end if
      else
!G
        tauct(i)  = zero
        tauctx(i) = zero 
        taucty(i) = zero
        go to 1000

      end if 
!H
      if ( tauct(i) .lt. tauctmax ) then
        tauct(i)  = tauctmax
        tauctx(i) = tauctmax*cosphi
        taucty(i) = tauctmax*sinphi
      end if

      if (lprnt.and.(i.eq.ipr)) then
        if (fhour.ge.fhourpr) then
           write(*,9210) tauctx(ipr),taucty(ipr),tauct(ipr),angle,kk
        endif
      endif

 9210 format(/,5x,'STRESS VECTOR = ( ',f8.3,' , ',f8.3,' ) = ',f8.3,
     +' IN DIRECTION ',f6.2,4x,'KK = ',i2,/)

!-----------------------------------------------------------------------
!
!  At this point, mean wind at the cloud top is larger than zero and
!  local RI at the cloud top is larger than ricrit (=0.25)
!
!  Calculate minimum of Richardson number including both basic-state
!  condition and wave effects.
!
!          g*Q_0*alpha*dx                  RI_loc*(1 - mu*|c2|)
!  mu  =  ----------------  RI_min =  -----------------------------
!           c_p*N*T*U^2                (1 + mu*RI_loc^(0.5)*|c2|)^2
!
!  Minimum RI is calculated for the following two cases
!
!  (1)   RIloc < 1.e+20  
!  (2)   Riloc = 1.e+20  ----> Vertically uniform basic-state wind
!
!  RIloc cannot be smaller than zero because N^2 becomes 1.E-32 in the
!  case of N^2 < 0.. Thus the sign of RINUM is determined by 
!  1 - nonlin*|c2|.
!
!-----------------------------------------------------------------------

       do k=kcldtop(i),1,-1

         if ( k .ne. 1 ) then
           crit1 = ucltop*(u(i,k)+u(i,k-1))*0.5
           crit2 = vcltop*(v(i,k)+v(i,k-1))*0.5
         else
           crit1 = ucltop*u(i,1)
           crit2 = vcltop*v(i,1)
         end if

         if((basicui(i,k) > zero).and.(crit1 > zero).and.
     &                                (crit2 > zero)) then
           nonlin   = ( g*qmax(i)*cldbar(i)*dlength(i) )/
     &                ( bruni(i,k)*ti(i,k)*(basicui(i,k)**2) )        
           if ( riloc(k)  <  rimaxm ) then
             rimin(k) = riloc(k)*( 1 - nonlin*abs(c2) ) /
     &                ( 1 + nonlin*sqrt(riloc(k))*abs(c2) )**2
           else if((riloc(k) > rimaxm).and.
     &             (riloc(k) < rimaxp))then
             rimin(k) = ( 1 - nonlin*abs(c2) ) /
     &                  ( (nonlin**2)*(c2**2) ) 
           end if
           if ( rimin(k) <= riminx ) then
             rimin(k) = rismall
           end if
         else
           rimin(k) = riminx
         end if
       end do              

!-----------------------------------------------------------------------
!
!  If minimum RI at interface cloud top is less than or equal to 1/4,
!  GWDC calculation for current horizontal grid is skipped 
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!
!  Calculate gravity wave stress profile using the wave saturation
!  hypothesis of Lindzen (1981).   
!
!  Assuming kcldtop(i)=10 and kcldbot=16,
!
!                             TAUGWCI  RIloc  RImin   UTGWC
!
!  Interface 1 ========       - 0.001         -1.e20
!            1 --------                               0.000
!            2 ========       - 0.001         -1.e20
!            2 --------                               0.000
!            3 ========       - 0.001         -1.e20
!            3 --------                               -.xxx 
!            4 ========       - 0.001  2.600  2.000
!            4 --------                               0.000
!            5 ========       - 0.001  2.500  2.000
!            5 --------                               0.000
!            6 ========       - 0.001  1.500  0.110
!            6 --------                               +.xxx 
!            7 ========       - 0.005  2.000  3.000
!            7 --------                               0.000
!            8 ========       - 0.005  1.000  0.222
!            8 --------                               +.xxx
!            9 ========       - 0.010  1.000  2.000
!            9 --------                               0.000
! kcldtopi  10 ========  $$$  - 0.010 
! kcldtop   10 --------  $$$                          yyyyy
!           11 ========  $$$  0
!           11 --------  $$$
!           12 ========  $$$  0
!           12 --------  $$$
!           13 ========  $$$  0
!           13 --------  $$$
!           14 ========  $$$  0
!           14 --------  $$$
!           15 ========  $$$  0
!           15 --------  $$$
!           16 ========  $$$  0
! kcldbot   16 --------  $$$
!           17 ========       0
!           17 -------- 
!           18 ========       0
!           18 -------- 
!           19 ========       0
!
!-----------------------------------------------------------------------
!
!   Even though the cloud top level obtained in deep convective para-
!   meterization is defined in mid-level, the cloud top level for
!   the GWDC calculation is assumed to be the interface level just 
!   above the mid-level cloud top vertical level index.
!
!-----------------------------------------------------------------------
 
      taugwci(i,kcldtop(i)) = tauct(i)                          !  *1

      do k=kcldtop(i)-1,2,-1
        if ( abs(taugwci(i,k+1)) > taumin ) then                ! TAUGWCI
          if ( riloc(k) > ricrit ) then                         ! RIloc
            if ( rimin(k) > ricrit ) then                       ! RImin
              taugwci(i,k) = taugwci(i,k+1)
            else if ((rimin(k) > riminp) .and.
     &               (rimin(k) <= ricrit)) then
              nonlins = (1.0/abs(c2))*( 2.*sqrt(2. + 1./sqrt(riloc(k)) )
     &                             - ( 2. + 1./sqrt(riloc(k)) )    )
              taus    =  - ( rhoi(i,k)*( basicui(i,k)**2 ) )/
     &                     ( bruni(i,k)*dlength(i) ) *
     &                       basicui(i,k)*c1*c2*c2*nonlins*nonlins
              taugwci(i,k) = taus
            else if((rimin(k) > riminm) .and.
     &              (rimin(k) < riminp)) then
              taugwci(i,k) = zero 
            end if                                              ! RImin
          else

!!!!!!!!!! In the dynamically unstable environment, there is no gravity 
!!!!!!!!!! wave stress

            taugwci(i,k) = zero    
          end if                                                ! RIloc
        else
           taugwci(i,k) = zero
        end if                                                  ! TAUGWCI

        if ( (basicum(i,k+1)*basicum(i,k) ) .lt. 0. ) then
           taugwci(i,k+1) = zero
           taugwci(i,k)   = zero
        endif

        if (abs(taugwci(i,k)) .gt. abs(taugwci(i,k+1))) then
           taugwci(i,k) = taugwci(i,k+1)
        end if

      end do 

!!!!!! Upper boundary condition to permit upward propagation of gravity  
!!!!!! wave energy at the upper boundary 

       taugwci(i,1) = taugwci(i,2)

!-----------------------------------------------------------------------
!
!  Calculate zonal and meridional wind tendency 
!
!-----------------------------------------------------------------------

       do k=1,km+1
         taugwcxi(i,k) = taugwci(i,k)*cosphi
         taugwcyi(i,k) = taugwci(i,k)*sinphi
       end do

!!!!!! Vertical differentiation
!!!!!!
       do k=1,kcldtop(i)-1
         tem1 = g / dpmid(i,k)
         wtgwc(i,k) = tem1 * (taugwci(i,k+1)  - taugwci(i,k))
         utgwc(i,k) = tem1 * (taugwcxi(i,k+1) - taugwcxi(i,k))
         vtgwc(i,k) = tem1 * (taugwcyi(i,k+1) - taugwcyi(i,k))
       end do

       do k=kcldtop(i),km
         wtgwc(i,k) = zero
         utgwc(i,k) = zero
         vtgwc(i,k) = zero
       end do

!-----------------------------------------------------------------------
!
!  Calculate momentum flux = stress deposited above cloup top
!  Apply equal amount with opposite sign within cloud
!
!-----------------------------------------------------------------------

      xstress = zero
      ystress = zero
      do k=1,kcldtop(i)-1
        xstress = xstress + utgwc(i,k)*dpmid(i,k)/g 
        ystress = ystress + vtgwc(i,k)*dpmid(i,k)/g  
      end do

!-----------------------------------------------------------------------
!        ALT 1      ONLY UPPERMOST LAYER
!-----------------------------------------------------------------------

C     kk = kcldtop(i)
C     tem1 = g / dpmid(i,kk)
C     utgwc(i,kk) = - tem1 * xstress
C     vtgwc(i,kk) = - tem1 * ystress

!-----------------------------------------------------------------------
!        ALT 2      SIN(KT-KB)
!-----------------------------------------------------------------------

      kk = kcldtop(i)
      kb = kcldbot(i)
      do k=kk,kb
      p1=pi/2.*(pint(i,k)-pint(i,kk))/
     +         (pint(i,kb+1)-pint(i,kk))
      p2=pi/2.*(pint(i,k+1)-pint(i,kk))/
     +         (pint(i,kb+1)-pint(i,kk))
      utgwc(i,k) = - g*xstress*(sin(p2)-sin(p1))/dpmid(i,k)
      vtgwc(i,k) = - g*ystress*(sin(p2)-sin(p1))/dpmid(i,k)
      enddo

!-----------------------------------------------------------------------
!        ALT 3      FROM KT to KB  PROPORTIONAL TO CONV HEATING
!-----------------------------------------------------------------------

!     do k=kcldtop(i),kcldbot(i)
!     p1=cumchr(i,k)
!     p2=cumchr(i,k+1)
!     utgwc(i,k) = - g*xstress*(p1-p2)/dpmid(i,k)
!     enddo

!-----------------------------------------------------------------------
!
!  The GWDC should accelerate the zonal and meridional wind in the   
!  opposite direction of the previous zonal and meridional wind, 
!  respectively
!
!-----------------------------------------------------------------------

!     do k=1,kcldtop(i)-1

!      if (utgwc(i,k)*u(i,k) .gt. 0.0) then

!-------------------- x-component-------------------

!       write(6,'(a)')   
!    +  '(GWDC) WARNING: The GWDC should accelerate the zonal wind '
!       write(6,'(a,a,i3,a,i3)')   
!    +  'in the opposite direction of the previous zonal wind', 
!    +  ' at I = ',i,' and J = ',lat
!       write(6,'(4(1x,e17.10))') u(i,kk),v(i,kk),u(i,k),v(i,k)
!       write(6,'(a,1x,e17.10))') 'Vcld . V =',
!    +  u(i,kk)*u(i,k)+v(i,kk)*v(i,k)

!       if(u(i,kcldtop(i))*u(i,k)+v(i,kcldtop(i))*v(i,k).gt.0.0)then
!       do k1=1,km
!         write(6,'(i2,36x,2(1x,e17.10))')
!    +             k1,taugwcxi(i,k1),taugwci(i,k1)
!         write(6,'(i2,2(1x,e17.10))') k1,utgwc(i,k1),u(i,k1) 
!       end do
!       write(6,'(i2,36x,1x,e17.10)') (km+1),taugwcxi(i,km+1)
!       end if

!-------------------- Along wind at cloud top -----

!       do k1=1,km
!         write(6,'(i2,36x,2(1x,e17.10))')
!    +             k1,taugwci(i,k1)
!         write(6,'(i2,2(1x,e17.10))') k1,wtgwc(i,k1),basicum(i,k1) 
!       end do
!       write(6,'(i2,36x,1x,e17.10)') (km+1),taugwci(i,km+1)

!      end if

!      if (vtgwc(i,k)*v(i,k) .gt. 0.0) then
!       write(6,'(a)')
!    +  '(GWDC) WARNING: The GWDC should accelerate the meridional wind'
!       write(6,'(a,a,i3,a,i3)')
!    +  'in the opposite direction of the previous meridional wind',
!    +  ' at I = ',i,' and J = ',lat
!       write(6,'(4(1x,e17.10))') u(i,kcldtop(i)),v(i,kcldtop(i)),
!    +                            u(i,k),v(i,k)
!       write(6,'(a,1x,e17.10))') 'Vcld . V =',
!    +                    u(i,kcldtop(i))*u(i,k)+v(i,kcldtop(i))*v(i,k)
!       if(u(i,kcldtop(i))*u(i,k)+v(i,kcldtop(i))*v(i,k).gt.0.0)then
!       do k1=1,km
!         write(6,'(i2,36x,2(1x,e17.10))')
!    +                        k1,taugwcyi(i,k1),taugwci(i,k1)
!         write(6,'(i2,2(1x,e17.10))') k1,vtgwc(i,k1),v(i,k1) 
!       end do
!       write(6,'(i2,36x,1x,e17.10)') (km+1),taugwcyi(i,km+1)
!       end if
!      end if

!     enddo

 1000 continue

       end if   ! DO GWDC CALCULATION
   
      end do   ! I-LOOP 

!***********************************************************************

      if (lprnt) then
        if (fhour.ge.fhourpr) then
!-------- UTGWC VTGWC ----------
          write(*,9220)
          do ilev=1,km
            write(*,9221) ilev,(86400.*utgwc(ipr,ilev)),
     +                         (86400.*vtgwc(ipr,ilev))
          enddo
        endif
      endif

 9220 format(//,14x,'TENDENCY DUE TO GWDC',//,
     +' ILEV',6x,'UTGWC',7x,'VTGWC',/)
 9221 format(i4,2(2x,f10.3))

!-----------------------------------------------------------------------
!
!  For GWDC performance analysis        
!
!-----------------------------------------------------------------------

      do i = 1, im
      kk=kcldtop(i)

       if ( dogwdc(i) .and. (abs(taugwci(i,kk)).gt.taumin) ) then

        gwdcloc(i) = one

        do k = 1, kk-1
         if ( abs(taugwci(i,k)-taugwci(i,kk)).gt.taumin ) then
          break(i) = 1.0
          go to 2000
         endif 
        enddo
 2000   continue

        do k = 1, kk-1

         if ( ( abs(taugwci(i,k)).lt.taumin ) .and.
     &        ( abs(taugwci(i,k+1)).gt.taumin ) .and.
     &        ( basicum(i,k+1)*basicum(i,k) .lt. 0. ) ) then
          critic(i) = 1.0
!         print *,i,k,' inside GWDC  taugwci(k) = ',taugwci(i,k)
!         print *,i,k+1,' inside GWDC  taugwci(k+1) = ',taugwci(i,k+1)
!         print *,i,k,' inside GWDC  basicum(k) = ',basicum(i,k)
!         print *,i,k+1,' inside GWDC  basicum(k+1) = ',basicum(i,k+1)
!         print *,i,' inside GWDC  critic = ',critic(i)
          goto 2010
         endif
        enddo
 2010   continue

       endif

      enddo

!-----------------------------------------------------------------------
!        Convert back local GWDC Tendency arrays to GFS model vertical indices
!        Make output Tendencies cos-weighted by using array RCS=1/cos(lat)
!        Outgoing (FU1,FV1)=cos(lat)*(utgwc,vtgwc)
!-----------------------------------------------------------------------

      do i=1,im
        rcsi(i) = one / rcs(i)
      enddo
      do k=1,km
        k1=km-k+1
        do i=1,im
          fu1(i,k1)    = utgwc(i,k)*rcsi(i)
          fv1(i,k1)    = vtgwc(i,k)*rcsi(i)
          brunm1(i,k1) = brunm(i,k)
          rhom1(i,k1)  = rhom(i,k)
        enddo
      enddo

      if (lprnt) then
        if (fhour.ge.fhourpr) then
!-------- UTGWC VTGWC ----------
          write(*,9225)
          do ilev=km,1,-1
            write(*,9226) ilev,(86400.*fu1(ipr,ilev)),
     +                         (86400.*fv1(ipr,ilev))
          enddo
        endif
      endif

 9225 format(//,14x,'TENDENCY DUE TO GWDC - TO GBPHYS',//,
     +' ILEV',6x,'UTGWC',7x,'VTGWC',/)
 9226 format(i4,2(2x,f10.3))

      return
      end
