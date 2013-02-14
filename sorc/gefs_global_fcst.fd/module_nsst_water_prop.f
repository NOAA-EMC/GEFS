MODULE module_nsst_water_prop
  USE machine, ONLY : kind_phys
  USE module_nsst_parameters, ONLY : t0K
  !
  PRIVATE
  PUBLIC :: rhocoef,density,sw_rad,sw_rad_Aw,sw_rad_upper,sw_rad_upper_Aw,sw_rad_skin,grv,solar_time_from_Julian,compjd
      
  !
  INTERFACE sw_rad
     MODULE PROCEDURE sw_fairall_6exp_v1  ! sw_wick_v1
  END INTERFACE
  INTERFACE sw_rad_Aw
     MODULE PROCEDURE sw_fairall_6exp_v1_Aw
  END INTERFACE
  INTERFACE sw_rad_upper
     MODULE PROCEDURE sw_soloviev_3exp_v2
  END INTERFACE
  INTERFACE sw_rad_upper_Aw
     MODULE PROCEDURE sw_soloviev_3exp_v2_Aw
  END INTERFACE
  INTERFACE sw_rad_skin
     MODULE PROCEDURE sw_ohlmann_v1
  END INTERFACE
CONTAINS
  ! ------------------------------------------------------
  SUBROUTINE rhocoef(t, s, rhoref, alpha, beta)
    ! ------------------------------------------------------

    !  compute thermal expansion coefficient (alpha) 
    !  and saline contraction coefficient (beta) using 
    !  the international equation of state of sea water 
    !  (1980). Ref: pond and pickard, introduction to 
    !  dynamical oceanography, pp310.  
    !  note: compression effects are not included

    IMPLICIT NONE
    REAL(kind=kind_phys), INTENT(in)  :: t, s, rhoref 
    REAL(kind=kind_phys), INTENT(out) :: alpha, beta  
    REAL(kind=kind_phys)              :: tc, sqrts, rhoinv
!
    real (kind=kind_phys), parameter ::                                       &
          ac1=6.793952e-2,       ac2=-2.0*9.095290e-3, ac3=3.0*1.001685e-4,   & 
          ac4=-4.0*1.120083e-6,  ac5=5.0*6.536332e-9,  ac6=-4.0899e-3,        &
          ac7=2.0*7.6438e-5,     ac8=-3.0*8.2467e-7,   ac9=4.0*5.3875e-9,     &
          ac10=1.0227e-4,        ac11=-2.0*1.6546e-6

    real (kind=kind_phys), parameter ::                                       &
          bc1=8.24493e-1,      bc2=-4.0899e-3,     bc3=7.6438e-5,             &
          bc4=-8.2467e-7,      bc5=5.3875e-9,      bc6=-1.5*5.72466e-3,       &
          bc7=1.5*1.0227e-4,   bc8=-1.5*1.6546e-6, bc9=2.0*4.8314e-4
!
    tc     = t - t0K
    sqrts  = sqrt(s)
    rhoinv = 1.0 /  rhoref

    alpha = ac1 + tc*(ac2 + tc*(ac3 + tc*(ac4 + tc*ac5)))                     &
          + s * (ac6 + tc*(ac7 + tc*(ac8 + tc*ac9)) + sqrts*(ac10 + tc*ac11))  

    ! NOTE: rhoref - specify 
    !
    alpha =  -alpha * rhoinv

    beta = bc1 + tc*(bc2 + tc*(bc3 + tc*(bc4 + tc*bc5)))                      &
         + sqrts * (bc6 + tc*(bc7 + tc*bc8)) + bc9*s

    beta = beta * rhoinv

  END SUBROUTINE rhocoef
  ! ----------------------------------------
  SUBROUTINE density(t, s, rho)
    ! ----------------------------------------
    IMPLICIT NONE

    ! input
    REAL(kind=kind_phys), INTENT(in)  :: t     !unit, K
    REAL(kind=kind_phys), INTENT(in)  :: s     !unit, 1/1000
    ! output
    REAL(kind=kind_phys), INTENT(out) :: rho   !unit, kg/m^3 
    ! local
    REAL(kind=kind_phys)              :: tc, sqrts
    real (kind=kind_phys), save       :: rc(15)
    data rc /999.842594,      6.793952e-2, - 9.095290e-3,   1.001685e-4, &
             - 1.120083e-6,   6.536332e-9,   8.24493e-1,  - 4.0899e-3,   &
               7.6438e-5,   - 8.2467e-7,     5.3875e-9,   - 5.72466e-3,  &
               1.0227e-4,   - 1.6546e-6,     4.8314e-4/

    ! compute density using the international equation 
    ! of state of sea water 1980, (pond and pickard, 
    ! introduction to dynamical oceanography, pp310). 
    ! compression effects are not included

    rho   = 0.0
    tc    = t - t0K
    sqrts = sqrt(s)

    !  effect of temperature on density (line 1)
    !  effect of temperature and salinity on density (lines 2-3)

    rho = rc(1) + tc*(rc(2) + tc*(rc(3) + tc*(rc(4) + tc*(rc(5) + tc*rc(6))))) &
        + s * (rc(7) + tc*(rc(8) + tc*(rc(9) + tc*(rc(10) + tc*rc(11))))       &
        + sqrts * (rc(12) + tc*(rc(13) + tc*rc(14))) + s*rc(15))

  END SUBROUTINE density
  !
  !======================
  !
  elemental SUBROUTINE sw_fairall_6exp_v1(z,fxp)
    !
    ! Fraction of the Solar radiation absorbed by the ocean at the depth z (Fairall et all, 1996, p. 1298)
    ! following Paulson and Simpson, 1981
    !
    ! INPUT:
    ! z:       depth (m)
    !
    ! OUTPUT:
    ! fxp: Fraction of the solar radiation absorbed by the ocean at depth z (W/m^2)
    !
    IMPLICIT NONE
    REAL(kind=kind_phys),INTENT(in):: z
    REAL(kind=kind_phys),INTENT(out):: fxp
    REAL(kind=kind_phys), DIMENSION(9), PARAMETER :: F=(/0.237,0.36,0.179,0.087,0.08,0.0246,0.025,0.007,0.0004/) &
         ,gamma=(/34.8,2.27,3.15e-2,5.48e-3,8.32e-4,1.26e-4,3.13e-4,7.82e-5,1.44e-5/)
    REAL(kind=kind_phys),DIMENSION(9) :: zgamma
    REAL(kind=kind_phys),DIMENSION(9) :: f_c
    !
    IF(z>0) THEN
       zgamma=z/gamma
       f_c=F*(1.-1./zgamma*(1-EXP(-zgamma)))
       fxp=SUM(f_c)
    ELSE
       fxp=0.
    ENDIF
    !
  END SUBROUTINE sw_fairall_6exp_v1
  !
  !======================
  !
  !
  elemental SUBROUTINE sw_fairall_6exp_v1_Aw(z,Aw)
    !
    ! Fraction of the Solar radiation absorbed by the ocean at the depth z (Fairall et all, 1996, p. 1298)
    ! following Paulson and Simpson, 1981
    !
    ! INPUT:
    ! z:       depth (m)
    !
    ! OUTPUT:
    ! Aw: d(fxp)/d(z)
    !
    ! fxp: Fraction of the solar radiation absorbed by the ocean at depth z (W/m^2)
    !
    IMPLICIT NONE
    REAL(kind=kind_phys),INTENT(in):: z
    REAL(kind=kind_phys),INTENT(out):: Aw
    REAL(kind=kind_phys) :: fxp
    REAL(kind=kind_phys), DIMENSION(9), PARAMETER :: F=(/0.237,0.36,0.179,0.087,0.08,0.0246,0.025,0.007,0.0004/) &
         ,gamma=(/34.8,2.27,3.15e-2,5.48e-3,8.32e-4,1.26e-4,3.13e-4,7.82e-5,1.44e-5/)
    REAL(kind=kind_phys),DIMENSION(9) :: zgamma
    REAL(kind=kind_phys),DIMENSION(9) :: f_Aw
    !
    IF(z>0) THEN
       zgamma=z/gamma
       f_Aw=(F/z)*((gamma/z)*(1-EXP(-zgamma))-EXP(-zgamma))
       Aw=SUM(f_Aw)

!      write(*,'(a,F6.2,F12.6,9F10.4)') 'z,Aw in sw_rad_Aw : ',z,Aw,f_Aw

    ELSE
       Aw=0.
    ENDIF
    !
  END SUBROUTINE sw_fairall_6exp_v1_Aw
  !
  !======================

  elemental SUBROUTINE sw_fairall_simple_v1(F_sol_0,z,dF_sol_z)
    !
    ! Solar radiation absorbed by the ocean at the depth z (Fairall et all, 1996, p. 1298)
    ! 
    ! INPUT: 
    ! F_sol_0: solar radiation at the ocean surface (W/m^2)
    ! z:       depth (m)
    !
    ! OUTPUT:
    ! dF_sol_z: solar radiation absorbed by the ocean at depth z (W/m^2)
    !
    IMPLICIT NONE
    REAL(kind=kind_phys),INTENT(in):: z,F_sol_0
    REAL(kind=kind_phys),INTENT(out):: dF_sol_z
    !
    IF(z>0) THEN
       dF_sol_z=F_sol_0*(0.137+11.0*z-6.6e-6/z*(1.-EXP(-z/8.e-4)))
    ELSE
       dF_sol_z=0.
    ENDIF
    !
  END SUBROUTINE sw_fairall_simple_v1
  !
  !======================
  !
  elemental SUBROUTINE sw_wick_v1(F_sol_0,z,dF_sol_z)
    !
    ! Solar radiation absorbed by the ocean at the depth z (Zeng and Beljaars, 2005, p.5)
    ! 
    ! INPUT: 
    ! F_sol_0: solar radiation at the ocean surface (W/m^2)
    ! z:       depth (m)
    !
    ! OUTPUT:
    ! dF_sol_z: solar radiation absorbed by the ocean at depth z (W/m^2)
    !
    IMPLICIT NONE
    REAL(kind=kind_phys),INTENT(in):: z,F_sol_0
    REAL(kind=kind_phys),INTENT(out):: dF_sol_z
    !
    IF(z>0) THEN
       dF_sol_z=F_sol_0*(0.065+11.0*z-6.6e-5/z*(1.-EXP(-z/8.e-4)))
    ELSE
       dF_sol_z=0.
    ENDIF
    !
  END SUBROUTINE sw_wick_v1
  !
  !======================
  !
  elemental SUBROUTINE sw_soloviev_3exp_v1(F_sol_0,z,dF_sol_z)
    !
    ! Solar radiation absorbed by the ocean at the depth z (Fairall et all, 1996, p. 1301)
    ! following Soloviev, 1982
    ! 
    ! INPUT: 
    ! F_sol_0: solar radiation at the ocean surface (W/m^2)
    ! z:       depth (m)
    !
    ! OUTPUT:
    ! dF_sol_z: solar radiation absorbed by the ocean at depth z (W/m^2)
    !
    IMPLICIT NONE
    REAL(kind=kind_phys),INTENT(in):: z,F_sol_0
    REAL(kind=kind_phys),INTENT(out):: dF_sol_z
    REAL(kind=kind_phys),DIMENSION(3) :: f_c
    REAL(kind=kind_phys), DIMENSION(3), PARAMETER :: f=(/0.45,0.27,0.28/) &
         ,gamma=(/12.8,0.357,0.014/)
    !
    IF(z>0) THEN
       f_c=f*gamma(1-EXP(-z/gamma))
       dF_sol_z=F_sol_0*(1.0-SUM(f_c)/z)
    ELSE
       dF_sol_z=0.
    ENDIF
    !
  END SUBROUTINE sw_soloviev_3exp_v1
  !
  !======================
  !
  elemental SUBROUTINE sw_soloviev_3exp_v2(F_sol_0,z,dF_sol_z)
    !
    ! Solar radiation absorbed by the ocean at the depth z (Fairall et all, 1996, p. 1301)
    ! following Soloviev, 1982
    ! 
    ! INPUT: 
    ! F_sol_0: solar radiation at the ocean surface (W/m^2)
    ! z:       depth (m)
    !
    ! OUTPUT:
    ! dF_sol_z: solar radiation absorbed by the ocean at depth z (W/m^2)
    !
    IMPLICIT NONE
    REAL(kind=kind_phys),INTENT(in):: z,F_sol_0
    REAL(kind=kind_phys),INTENT(out):: dF_sol_z
    !
    IF(z>0) THEN
       dF_sol_z=F_sol_0*(1.0 &
            -(0.28*0.014*(1.-exp(-z/0.014)) &
            +0.27*0.357*(1.-exp(-z/0.357)) &        
            +.45*12.82*(1.-exp(-z/12.82)))/z &
            )
    ELSE
       dF_sol_z=0.
    ENDIF
    !
  END SUBROUTINE sw_soloviev_3exp_v2

  elemental SUBROUTINE sw_soloviev_3exp_v2_Aw(z,Aw)
    !
    ! Aw = d(fxp)/d(z)
    ! following Soloviev, 1982
    !
    ! INPUT:
    ! z:       depth (m)
    !
    ! OUTPUT:
    ! Aw: d(fxp)/d(z)
    !
    IMPLICIT NONE
    REAL(kind=kind_phys),INTENT(in):: z
    REAL(kind=kind_phys),INTENT(out):: Aw
    REAL(kind=kind_phys):: fxp
    !
    IF(z>0) THEN
       fxp=(1.0 &
            -(0.28*0.014*(1.-exp(-z/0.014)) &
            + 0.27*0.357*(1.-exp(-z/0.357)) &
            + 0.45*12.82*(1.-exp(-z/12.82)))/z &
            )
       Aw=1.0-fxp-(0.28*exp(-z/0.014)+0.27*exp(-z/0.357)+0.45*exp(-z/12.82))
    ELSE
       Aw=0.
    ENDIF
  END SUBROUTINE sw_soloviev_3exp_v2_Aw
  !
  !
  !======================
  !
  elemental SUBROUTINE sw_ohlmann_v1(z,fxp)
    !
    ! Fraction of the Solar radiation absorbed by the ocean at the depth z
    !
    ! INPUT:
    ! z:       depth (m)
    !
    ! OUTPUT:
    ! fxp: Fraction of the solar radiation absorbed by the ocean at depth z (W/m^2)
    !
    IMPLICIT NONE
    REAL(kind=kind_phys),INTENT(in):: z
    REAL(kind=kind_phys),INTENT(out):: fxp
    !
    IF(z>0) THEN
       fxp=.065+11.*z-6.6e-5/z*(1.-EXP(-z/8.0e-4))
    ELSE
       fxp=0.
    ENDIF
    !
  END SUBROUTINE sw_ohlmann_v1
  !

function grv(lat)
  real(kind=kind_phys) :: lat
  real(kind=kind_phys) :: gamma,c1,c2,c3,c4,pi,phi,x
  gamma=9.7803267715
  c1=0.0052790414
  c2=0.0000232718
  c3=0.0000001262
  c4=0.0000000007
  pi=3.141593
                                                                                                                                                             
  phi=lat*pi/180
  x=sin(phi)
  grv=gamma*(1+(c1*x**2)+(c2*x**4)+(c3*x**6)+(c4*x**8))
  !print *,'grav=',grv,lat
end function grv

SUBROUTINE solar_time_from_Julian(jday,xlon,soltim)
  !
  ! Calculate solar time from the Julian date
  !
  IMPLICIT NONE
  REAL(kind=kind_phys), INTENT(in)  :: jday
  REAL(kind=kind_phys), INTENT(in)  :: xlon
  REAL(kind=kind_phys), INTENT(out) :: soltim
  REAL(kind=kind_phys)                            :: fjd,xhr,xmin,xsec,intime
  INTEGER                                        :: nn
  !
  fjd=jday-FLOOR(jday)
  fjd=jday
  xhr=FLOOR(fjd*24.0)-SIGN(12.0,fjd-0.5)
  xmin=NINT(fjd*1440.0)-(xhr+SIGN(12.0,fjd-0.5))*60
  xsec=0
  intime=xhr+xmin/60.0+xsec/3600.0+24.0
  soltim=mod(xlon/15.0+intime,24.0)*3600.0
END SUBROUTINE solar_time_from_Julian

!
!***********************************************************************
!
      subroutine compjd(jyr,jmnth,jday,jhr,jmn,jd,fjd)
!fpp$ noconcur r
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    compjd      computes julian day and fraction
!   prgmmr: kenneth campana  org: w/nmc23    date: 89-07-07
!
! abstract: computes julian day and fraction
!   from year, month, day and time utc.
!
! program history log:
!   77-05-06  ray orzol,gfdl
!   98-05-15  iredell   y2k compliance
!
! usage:    call compjd(jyr,jmnth,jday,jhr,jmn,jd,fjd)
!   input argument list:
!     jyr      - year (4 digits)
!     jmnth    - month
!     jday     - day
!     jhr      - hour
!     jmn      - minutes 
!   output argument list:
!     jd       - julian day.
!     fjd      - fraction of the julian day.
!
! subprograms called:
!   iw3jdn     compute julian day number
!
! attributes:
!   language: fortran.
!
!$$$
      use machine , only :kind_phys
      implicit none
!
      integer jyr,jmnth,jday,jhr,jmn,jd
      integer iw3jdn
      real (kind=kind_phys) fjd
      jd=iw3jdn(jyr,jmnth,jday)
      if(jhr.lt.12) then
        jd=jd-1
        fjd=0.5+jhr/24.+jmn/1440.
      else
        fjd=(jhr-12)/24.+jmn/1440.
      endif
      end subroutine compjd

END MODULE module_nsst_water_prop
