!!!!!  ==========================================================  !!!!!
!!!!!          'module_radiation_astronomy'  description           !!!!!
!!!!!  ==========================================================  !!!!!
!                                                                      !
!   set up astronomy quantities for solar radiation calculations.      !
!                                                                      !
!   in module 'module_radiation_astronomy', externally accessable      !
!   subroutines are listed below:                                      !
!                                                                      !
!      'solinit'    -- read in solar constant                          !
!         input:                                                       !
!           ( ISOL, iyear, me )                                        !
!         output:                                                      !
!           ( none )                                                   !
!                                                                      !
!      'astronomy'  -- get astronomy related quantities                !
!         input:                                                       !
!           ( lons_lar,glb_lats_r,sinlat,coslat,xlon,                  !
!!            fhswr,jdate,deltim,                                      !
!             fhswr,jdate,                                             !
!             LON2,LATD,LATR,IPT_LATR, lsswr, me)                      !
!         output:                                                      !
!           ( solcon,slag,sdec,cdec,coszen,coszdg)                     !
!                                                                      !
!                                                                      !
!   external modules referenced:                                       !
!       'module machine'                    in 'machine.f'             !
!       'module physcons'                   in 'physcons.f             !
!                                                                      !
!   program history log:                                               !
!     may-06-1977  ---  ray orzol,      created at gfdl                !
!     jul-07-1989  ---  kenneth campana                                !
!     may-15-1998  ---  mark iredell    y2k compliance                 !
!     dec-15-2003  ---  yu-tai hou      combined compjd and fcstim and !
!                       rewrite in fortran 90 compatable form          !
!     feb-15-2006  ---  yu-tai hou      add 11-yr solar constant cycle !
!                                                                      !
!!!!!  ==========================================================  !!!!!
!!!!!                       end descriptions                       !!!!!
!!!!!  ==========================================================  !!!!!



!========================================!
      module module_radiation_astronomy  !
!........................................!
!
      use machine,                 only : kind_phys
      use physcons,                only : con_solr, con_pi
      use module_iounitdef,        only : NIRADSF
!
      implicit   none
!
      private

!  ---  parameter constants
      real (kind=kind_phys), parameter :: degrad = 180.0/con_pi
      real (kind=kind_phys), parameter :: tpi    = 2.0 * con_pi
      real (kind=kind_phys), parameter :: hpi    = 0.5 * con_pi

!  ---  module variables:
      real (kind=kind_phys), public    :: solc0

      public  solinit, astronomy


! =================
      contains
! =================

!-----------------------------------
      subroutine solinit                                                &
!...................................

!  ---  inputs:
     &     ( ISOL, iyear, me )
!  ---  outputs: ( none )

!  ===================================================================  !
!                                                                       !
!  read in solar constant value for a given year                        !
!                                                                       !
!  inputs:                                                              !
!     ISOL    - =0: use fixed solar constant in "physcon"               !
!               =1: use 11-year cycle solar constant from table         !
!     iyear   - year of the recorded data             1                 !
!     me      - print message control flag            1                 !
!                                                                       !
!  outputs:  (to module variable)                                       !
!     ( none )                                                          !
!                                                                       !
!  module variable:                                                     !
!     solc0   - solar constant  (w/m**2)              1                 !
!                                                                       !
!  usage:    call solinit                                               !
!                                                                       !
!  subprograms called:  none                                            !
!                                                                       !
!  ===================================================================  !
!
      implicit none

!  ---  input:
      integer,  intent(in) :: ISOL, iyear, me

!  ---  output: ( none )

!  ---  local:
      real (kind=kind_phys):: smean, solc1
      integer       :: i, iyr, iyr1, iyr2, jyr
      logical       :: file_exist
      character     :: cline*60, cfile0*26

      data  cfile0 / 'solarconstantdata.txt' /

!===>  ...  begin here

      if ( ISOL == 0 ) then
        solc0 = con_solr

        if ( me == 0 ) then
          print *,' - Using fixed solar constant =', solc0
        endif

        return
      endif

!  --- ... check to see if solar constant data file existed

      inquire (file=cfile0, exist=file_exist)
      if ( .not. file_exist ) then
        solc0 = con_solr

        if ( me == 0 ) then
          print *,' - Using varying solar constant with 11-year cycle'
          print *,'   Requested solar data file "',cfile0,              &
     &            '" not found!'
          print *,'   Using the default solar constant value =',solc0,  &
     &            ' instead!!'
        endif
      else
        iyr = iyear

        open (NIRADSF,file=cfile0,form='formatted',status='old')
        rewind NIRADSF

        read (NIRADSF, 24) iyr1, iyr2, smean, cline
  24    format(i4,2x,i4,f8.2,a60)

        if ( me == 0 ) then
          print *,' - Using varying solar constant with 11-year cycle'
          print *,'   Opened solar constant data file: ',cfile0
!check    print *, iyr1, iyr2, smean, cline
        endif

        if ( iyr < iyr1 ) then
          Lab_dowhile1 : do while ( iyr < iyr1 )
            iyr = iyr + 11
          enddo Lab_dowhile1

          if ( me == 0 ) then
            print *,'   *** Year',iyear,' out of table range!'
            print *,'       Using the 11-cycle year (',iyr,' ) value.'
          endif
        elseif ( iyr > iyr2 ) then
          Lab_dowhile2 : do while ( iyr > iyr2 )
            iyr = iyr - 11
          enddo Lab_dowhile2

          if ( me == 0 ) then
            print *,'   *** Year',iyear,' out of table range!'
            print *,'       Using the 11-cycle year (',iyr,' ) value.'
          endif
        endif

        i = iyr2
        Lab_dowhile3 : do while ( i >= iyr1 )
!         read (NIRADSF,26) jyr, solc1
! 26      format(i4,f8.2)
          read (NIRADSF,*) jyr, solc1

          if ( i == iyr .and. iyr == jyr ) then
            solc0  = smean + solc1
            if (me == 0) then
              print *,' CHECK: Solar constant data for year',iyr,       &
     &                 solc1, solc0
            endif
            exit Lab_dowhile3
          else
!check      if (me == 0) print *,'   Skip solar const data for year',i
            i = i - 1
          endif
        enddo   Lab_dowhile3

        close ( NIRADSF )
      endif      ! end if_file_exist_block

!
      return
!...................................
      end subroutine solinit
!-----------------------------------


!-----------------------------------
      subroutine astronomy                                              &
!...................................

!  ---  inputs:
     &     ( lons_lar,glb_lats_r,sinlat,coslat,xlon,                    &
!    &       fhswr,jdate,deltim,                                        &
     &       fhswr,jdate,                                               &
     &       LON2,LATD,LATR,IPT_LATR, lsswr, me,                        &
!  ---  outputs:
     &       solcon,slag,sdec,cdec,coszen,coszdg                        &
     &      )

!  ===================================================================  !
!                                                                       !
!  astronomy computes solar parameters at forecast time                 !
!                                                                       !
!  inputs:                                                   dimension  !
!    lons_lar      - num of grid pts on a given lat circle        (LATR)!
!    glb_lats_r    - index for global latitudes                   (LATR)!
!    sinlat,coslat - sin and cos of latitude                      (LATR)!
!    xlon          - longitude in radians                    (LON2*LATD)!
!    fhswr         - sw radiation calling interval in hour              !
!    jdate         - current forecast date and time               (8)   !
!                    (yr, mon, day, t-zone, hr, min, sec, mil-sec)      !
!!   deltim        - duration of model integration time step in seconds !
!    LON2,LATD,LATR- dimensions for longitude/latitude directions       !
!    IPT_LATR      - latitude index location indecator                  !
!    lsswr         - logical control flag for sw radiation call         !
!    me            - integer control flag for diagnostic print out      !
!                                                                       !
!  outputs:                                                             !
!    solcon        - sun-earth distance adjusted solar constant (w/m2)  !
!    slag          - equation of time in radians                        !
!    sdec, cdec    - sin and cos of the solar declination angle         !
!    coszen        - avg of cosz for daytime only            (LON2,LATD)!
!    coszdg        - avg of cosz over entire sw call interval(LON2,LATD)!
!                                                                       !
!                                                                       !
!  external functions called: iw3jdn                                    !
!                                                                       !
!  ===================================================================  !
!
      implicit none
      
!  ---  input:
      integer,  intent(in) :: LON2, LATD, LATR, IPT_LATR, me
      integer,  intent(in) :: lons_lar(:), glb_lats_r(:), jdate(:)

      logical, intent(in) :: lsswr

      real (kind=kind_phys), intent(in) :: sinlat(:), coslat(:),        &
     &       xlon(:,:), fhswr
!    &       xlon(:,:), fhswr, deltim

!  ---  output:
      real (kind=kind_phys), intent(out) :: solcon, slag, sdec, cdec,   &
     &       coszen(:,:), coszdg(:,:)

!  ---  locals:
      real (kind=kind_phys), parameter :: f24   = 24.0     ! hours/day
      real (kind=kind_phys), parameter :: f1440 = 1440.0   ! minutes/day

      real (kind=kind_phys) :: solhr, fjd, fjd1, dlt, r1, alp, solc

      integer :: jd, jd1, iyear, imon, iday, ihr, imin
      integer :: iw3jdn

!===>  ...  begin here

      iyear = jdate(1)
      imon  = jdate(2)
      iday  = jdate(3)
      ihr   = jdate(5)
      imin  = jdate(6)

!  --- ...  calculate forecast julian day and fraction of julian day

      jd1 = iw3jdn(iyear,imon,iday)

!  --- ...  unlike in normal applications, where day starts from 0 hr,
!           in astronomy applications, day stats from noon.

      if (ihr < 12) then
        jd1 = jd1 - 1
!       fjd1= 0.5 + float(ihr)/f24                     ! use next line if imin > 0
        fjd1= 0.5 + float(ihr)/f24 + float(imin)/f1440
      else
!       fjd1= float(ihr - 12)/f24                      ! use next line if imin > 0
        fjd1= float(ihr - 12)/f24 + float(imin)/f1440
      endif

      fjd1  = fjd1 + jd1

      jd  = int(fjd1)
      fjd = fjd1 - jd

      if (lsswr) then

!  --- ...  hour of forecast time

        solhr = mod( float(ihr), f24 )

        call solar                                                      &
!  ---  inputs:
     &     ( jd,fjd,                                                    &
!  ---  outputs:
     &       r1,dlt,alp,slag,sdec,cdec                                  &
     &     )

!       if (me == 0) print*,'in astronomy completed sr solar'

        call coszmn                                                     &
!  ---  inputs:
     &     ( lons_lar,glb_lats_r,xlon,sinlat,coslat,                    &
!    &       fhswr,deltim,solhr,sdec,cdec,slag,                         &
     &       fhswr,solhr,sdec,cdec,slag,                                &
     &       LON2,LATD,IPT_LATR,                                        &
!  ---  outputs:
     &       coszen,coszdg                                              &
     &     )

!       if (me == 0) print*,'in astronomy completed sr coszmn'

!  --- ...  calculate sun-earth distance adjustment factor appropriate for date

        solcon = solc0 / (r1*r1)

      endif

!  --- ...  diagnostic print out

      if (me == 0) then

        call prtime                                                     &
!  ---  inputs:
     &     ( jd, fjd, dlt, alp, r1, slag, solcon                        &
!  ---  outputs: ( none )
     &     )

      endif

!
      return
!...................................
      end subroutine astronomy
!-----------------------------------


!-----------------------------------
      subroutine solar                                                  &
!...................................

!  ---  inputs:
     &     ( jd,fjd,                                                    &
!  ---  outputs:
     &       r1,dlt,alp,slag,sdec,cdec                                  &
     &     )

!  ===================================================================  !
!                                                                       !
!  solar computes radius vector, declination and right ascension of     !
!  sun, and equation of time.                                           !
!                                                                       !
!  inputs:                                                              !
!    jd       - julian day                                              !
!    fjd      - fraction of the julian day                              !
!                                                                       !
!  outputs:                                                             !
!    r1       - earth-sun radius vector                                 !
!    dlt      - declination of sun in radians                           !
!    alp      - right ascension of sun in radians                       !
!    slag     - equation of time in radians                             !
!    sdec     - sine of declination angle                               !
!    cdec     - cosine of declination angle                             !
!                                                                       !
!  usage:    call solar                                                 !
!                                                                       !
!  external subroutines called: none                                    !
!                                                                       !
!  program history log:                                                 !
!    mar-xx-1989  ---  kenneth campana, patterner after original gfdl   !
!                          code but no calculation of latitude mean cos !
!                          solar zenith angle.                          !
!    fall  -1988  ---  hualu pan,  updated to limit iterations in newton!
!                          method and also ccr reduced to avoid non-    !
!                          convergence.                                 !
!    dec-15-2003  ---  yu-tai hou, updated to make fortran 90 compatable!
!                                                                       !
!  ===================================================================  !
!
      implicit none

!  ---  inputs:
      real (kind=kind_phys), intent(in) :: fjd
      integer,               intent(in) :: jd

!  ---  outputs:
      real (kind=kind_phys), intent(out) :: r1, dlt,alp,slag,sdec,cdec

!  ---  locals:
      real (kind=kind_phys), parameter :: cyear = 365.25   ! days of year
      real (kind=kind_phys), parameter :: ccr   = 1.3e-6   ! iteration limit
      real (kind=kind_phys), parameter :: tpp   = 1.55     ! days between epoch and
                                                           ! perihelion passage of 1900
      real (kind=kind_phys), parameter :: svt6  = 78.035   ! days between perihelion passage
                                                           ! and march equinox of 1900
      integer,               parameter :: jdor  = 2415020  ! jd of epoch which is january
                                                           ! 0, 1900 at 12 hours ut

      real (kind=kind_phys) :: dat, t1, year, tyear, ec, angin, ador,   &
     &       deleqn, sni, tini, er, qq, e1, ep, cd, eq, date, em,       &
     &       cr, w1, tst, sun

      integer               :: jdoe, iter

!===>  ...  begin here

! --- ...  computes time in julian centuries after epoch

      t1 = float(jd - jdor) / 36525.0

! --- ...  computes length of anomalistic and tropical years (minus 365 days)

      year = 0.25964134e0 + 0.304e-5 * t1
      tyear= 0.24219879E0 - 0.614e-5 * t1

! --- ...  computes orbit eccentricity and angle of earth's inclination from t

      ec   = 0.01675104e0 - (0.418e-4 + 0.126e-6 * t1) * t1
      angin= 23.452294e0 - (0.0130125e0 + 0.164e-5 * t1) * t1

      ador = jdor
      jdoe = ador + (svt6 * cyear) / (year - tyear)

! --- ...  deleqn is updated svt6 for current date

      deleqn= float(jdoe - jd) * (year - tyear) / cyear
      year  = year + 365.0
      sni   = sin( angin / degrad )
      tini  = 1.0 / tan( angin / degrad )
      er    = sqrt( (1.0 + ec) / (1.0 - ec) )
      qq    = deleqn * tpi / year

! --- ...  determine true anomaly at equinox

      e1    = 1.0
      cd    = 1.0
      iter  = 0

      lab_do_1 : do while ( cd > ccr )

        ep    = e1 - (e1 - ec*sin(e1) - qq) / (1.0 - ec*cos(e1))
        cd    = abs(e1 - ep)
        e1    = ep
        iter  = iter + 1

        if (iter > 10) then
          write(6,*) ' ITERATION COUNT FOR LOOP 32 =', iter
          write(6,*) ' E, EP, CD =', e1, ep, cd
          exit lab_do_1
        endif

      enddo  lab_do_1

      eq   = 2.0 * atan( er * tan( 0.5*e1 ) )

! --- ...  date is days since last perihelion passage

      dat  = float(jd - jdor) - tpp + fjd
      date = mod(dat, year)

! --- ...  solve orbit equations by newton's method

      em   = tpi * date / year
      e1   = 1.0
      cr   = 1.0
      iter = 0

      lab_do_2 : do while ( cr > ccr )

        ep   = e1 - (e1 - ec*sin(e1) - em) / (1.0 - ec*cos(e1))
        cr   = abs(e1 - ep)
        e1   = ep
        iter = iter + 1

        if (iter > 10) then
          write(6,*) ' ITERATION COUNT FOR LOOP 31 =', iter
          exit lab_do_2
        endif

      enddo  lab_do_2

      w1   = 2.0 * atan( er * tan( 0.5*e1 ) )

      r1   = 1.0 - ec*cos(e1)

      sdec = sni * sin(w1 - eq)
      cdec = sqrt( 1.0 - sdec*sdec )

      dlt  = asin( sdec )
      alp  = asin( tan(dlt)*tini )

      tst  = cos( w1 - eq )
      if (tst < 0.0) alp = con_pi - alp
      if (alp < 0.0) alp = alp + tpi

      sun  = tpi * (date - deleqn) / year
      if (sun < 0.0) sun = sun + tpi
      slag = sun - alp - 0.03255e0

!
      return
!...................................
      end subroutine solar
!-----------------------------------


!-----------------------------------
      subroutine coszmn                                                 &
!...................................

!  ---  inputs:
     &     ( lons_lar,glb_lats_r,xlon,sinlat,coslat,                    &
!    &       dtswav,deltim,solhr,sdec,cdec,slag,                        &
     &       dtswav,solhr,sdec,cdec,slag,                               &
     &       NLON2,LATD,IPT_LATR,                                       &
!  ---  outputs:
     &       coszen,coszdg                                              &
     &     )

!  ===================================================================  !
!                                                                       !
!  coszmn computes mean cos solar zenith angle over 'dtswav' hours.     !
!                                                                       !
!  inputs:                                                              !
!    lons_lar      - num of grid pts on a given lat circle              !
!    glb_lats_r    - index for global latitude                          !
!    xlon          - longitude in radians                               !
!    sinlat,coslat - sin and cos of latitude                            !
!    dtswav        - sw radiation calling interval in hour              !
!!   deltim        - duration of model integration time step in second  !
!    solhr         - time after 00z in hours                            !
!    sdec, cdec    - sin and cos of the solar declination angle         !
!    slag          - equation of time                                   !
!    NLON2,LATD    - dimensions for longitude/latitude directions       !
!    IPT_LATR      - latitude index location indecator                  !
!                                                                       !
!  outputs:                                                             !
!    coszen        - average of cosz for daytime only in sw call interval
!    coszdg        - average of cosz over entire sw call interval       !
!                                                                       !
!  usage:    call comzmn                                                !
!                                                                       !
!  external subroutines called: none                                    !
!                                                                       !
!  program history log:                                                 !
!     05-28-2004   yu-tai hou     - modified for gfs hybrid model       !
!                                                                       !
!  ===================================================================  !
!
      implicit none

!  ---  inputs:
      integer, intent(in) :: NLON2, LATD, IPT_LATR
      integer, intent(in) :: lons_lar(:), glb_lats_r(:)

      real (kind=kind_phys), intent(in) :: sinlat(:), coslat(:),        &
     &       xlon(:,:), dtswav, solhr, sdec, cdec, slag
!    &       xlon(:,:), dtswav, deltim, solhr, sdec, cdec, slag

!  ---  outputs:
      real (kind=kind_phys), intent(out) :: coszen(:,:), coszdg(:,:)

!  ---  locals:
      real (kind=kind_phys) :: coszn(NLON2), pid12, cns, ss, cc

      integer :: istsun(NLON2), nstp, istp, nlon, nlnsp, i, it, j, lat

!===>  ...  begin here

      nlon = NLON2 / 2

      nstp = 6                               ! number of cosz calc per fcst hour
!     nstp = max(6, min(10, nint(3600.0/deltim) ))  ! for better time step sync
      istp = nint( dtswav*nstp )             ! total num of calc in dtswav interval

!     pid12 = con_pi / 12.0                  ! angle per hour
      pid12 = (2.0 * asin(1.0)) / 12.0

      do j = 1, LATD
        lat   = glb_lats_r(IPT_LATR-1+j)
        nlnsp = lons_lar(lat)

        do i = 1, NLON2
          coszen(i,j) = 0.0
          istsun(i) = 0
        enddo

        do it = 1, istp
          cns = pid12 * (solhr - 12.0 + float(it-1)/float(nstp)) + slag
          ss  = sinlat(lat) * sdec
          cc  = coslat(lat) * cdec

          do i = 1, nlnsp
            coszn(i) = ss + cc * cos(cns + xlon(i,j))
            coszen(i,j) = coszen(i,j) + max(0.0, coszn(i))
            if (coszn(i) > 0.0001) istsun(i) = istsun(i) + 1
          enddo
        enddo

!  --- ...  compute time averages

        do i = 1, NLON2
          coszdg(i,j) = coszen(i,j) / float(istp)
          if (istsun(i) > 0) coszen(i,j) = coszen(i,j) / istsun(i)
        enddo
      enddo

!
      return
!...................................
      end subroutine coszmn
!-----------------------------------


!-----------------------------------
      subroutine prtime                                                 &
!...................................

!  ---  inputs:
     &     ( jd, fjd, dlt, alp, r1, slag, solc                          &
!  ---  outputs: ( none )
     &     )

!  ===================================================================  !
!                                                                       !
!  prtime prints out forecast date, time, and astronomy quantities.     !
!                                                                       !
!  inputs:                                                              !
!    jd       - forecast julian day                                     !
!    fjd      - forecast fraction of julian day                         !
!    dlt      - declination angle of sun in radians                     !
!    alp      - right ascension of sun in radians                       !
!    r1       - earth-sun radius vector in meter                        !
!    slag     - equation of time in radians                             !
!    solc     - solar constant in w/m^2                                 !
!                                                                       !
!  outputs:   ( none )                                                  !
!                                                                       !
!  usage:    call prtime                                                !
!                                                                       !
!  external subroutines called: w3fs26                                  !
!                                                                       !
!  program history log:                                                 !
!    jun-07-1977  ---  robert white (gfdl)                              !
!    jul-07-1989  ---  kenneth campana                                  !
!    may-15-1998  ---  mark iredell    y2k compliance                   !
!    dec-18-2003  ---  yu-tai hou      combine cdate and prtime and     !
!                           rewrite in fortran 90 compatable form       !
!                                                                       !
!  ===================================================================  !
!
      implicit none

!  ---  inputs:
      integer, intent(in) :: jd

      real (kind=kind_phys), intent(in) :: fjd, dlt, alp, r1, slag, solc

!  ---  outputs: ( none )

!  ---  locals:
      real (kind=kind_phys), parameter :: sixty  = 60.0

      character(LEN=1),     parameter :: sign   = '-'
      character(LEN=1),     parameter :: sigb   = ' '

      character(LEN=1)     :: dsig
      character(LEN=4)     :: month(12)

      data month / 'JAN.','FEB.','MAR.','APR.','MAY ','JUNE',           &
     &             'JULY','AUG.','SEP.','OCT.','NOV ','DEC.' /

      integer               :: iday, imon, iyear, ihr, ltd, ltm,        &
     &                         ihalp, iyy, jda, mfjd, idaywk, idayyr
      real (kind=kind_phys) :: xmin, dltd, dltm, dlts, halp, ymin,      &
     &                         asec, eqt, eqsec

!===>  ...  begin here

!  --- ...  get forecast hour and minute from fraction of julian day

      if (fjd >= 0.5) then
        jda = jd + 1
        mfjd= nint( fjd*1440.0 )
        ihr = mfjd / 60 - 12
        xmin= float(mfjd) - (ihr + 12)*sixty
      else
        jda = jd
        mfjd= nint( fjd*1440.0 )
        ihr = mfjd / 60 + 12
        xmin= float(mfjd) - (ihr - 12)*sixty
      endif

!  --- ...  get forecast year, month, and day from julian day

      call w3fs26(jda, iyear,imon,iday, idaywk,idayyr)

!  -- ...  compute solar parameters

      dltd = degrad * dlt
      ltd  = dltd
      dltm = sixty * (abs(dltd) - abs(float(ltd)))
      ltm  = dltm
      dlts = sixty * (dltm - float(ltm))

      if ((dltd < 0.0) .and. (ltd == 0.0)) then
        dsig = sign
      else
        dsig = sigb
      endif

      halp = 6.0 * alp / hpi
      ihalp= halp
      ymin = abs(halp - float(ihalp)) * sixty
      iyy  = ymin
      asec = (ymin - float(iyy)) * sixty

      eqt  = 228.55735 * slag
      eqsec= sixty * eqt

      print 101, iday, month(imon), iyear, ihr, xmin, jd, fjd
 101  format('0 FORECAST DATE',9x,i3,a5,i6,' AT',i3,' HRS',f6.2,' MINS'/&
     &       '  JULIAN DAY',12x,i8,2x,'PLUS',f11.6)

      print 102, r1, halp, ihalp, iyy, asec
 102  format('  RADIUS VECTOR',9x,f10.7/'  RIGHT ASCENSION OF SUN',     &
     &       f12.7,' HRS, OR',i4,' HRS',i4,' MINS',f6.1,' SECS')

      print 103, dltd, dsig, ltd, ltm, dlts, eqt, eqsec, slag, solc
 103  format('  DECLINATION OF THE SUN',f12.7,' DEGS, OR ',a1,i3,       &
     &       ' DEGS',i4,' MINS',f6.1,' SECS'/'  EQUATION OF TIME',6x,   &
     &       f12.7,' MINS, OR',f10.2,' SECS, OR',f9.6,' RADIANS'/       &
     &       '  SOLAR CONSTANT',8X,F12.7,' (DISTANCE AJUSTED)'//)

!
      return
!...................................
      end subroutine prtime
!-----------------------------------

!
!...........................................!
      end module module_radiation_astronomy !
!===========================================!
