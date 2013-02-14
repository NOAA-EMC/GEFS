!!!!!  ==========================================================  !!!!!
!!!!!              'module_radiation_gases'  description           !!!!!
!!!!!  ==========================================================  !!!!!
!                                                                      !
!   set up ozone climatological profiles and other constant gas        !
!   profiles, such as co2, ch4, n2o, o2, and those of cfc gases.  All  !
!   data are entered as mixing ratio by volume, except ozone which is  !
!   mass mixing ratio (g/g).                                           !
!                                                                      !
!   in the module, the externally callabe subroutines are :            !
!                                                                      !
!      'gasinit'    -- initialization                                  !
!         input:                                                       !
!           ( iyear, month, ICTM, ICO2, me )                           !
!         output:                                                      !
!           ( none )                                                   !
!                                                                      !
!      'getozn'     -- setup climatological ozone profile              !
!         input:                                                       !
!           ( prslk,xlat,k1oz,k2oz,facoz,                              !
!             IMAX, LM, iflip )                                        !
!         output:                                                      !
!           ( o3mmr )                                                  !
!                                                                      !
!      'getgases'   -- setup constant gas profiles for LW and SW       !
!         input:                                                       !
!           ( plvl, xlon, xlat,                                        !
!             IMAX, LMAX, iflip )                                      !
!         output:                                                      !
!           ( gasdat )                                                 !
!                                                                      !
!   external modules referenced:                                       !
!       'module machine'                    in 'machine.f'             !
!       'module funcphys'                   in 'funcphys.f'            !
!       'module physcons'                   in 'physcons.f             !
!       'module module_iounitdef'           in 'iounitdef.f'           !
!                                                                      !
!   unit used for radiative active gases:                              !
!      ozone : mass mixing ratio                     (g/g)             !
!      co2   : volume mixing ratio                   (p/p)             !
!      n2o   : volume mixing ratio                   (p/p)             !
!      ch4   : volume mixing ratio                   (p/p)             !
!      o2    : volume mixing ratio                   (p/p)             !
!      co    : volume mixing ratio                   (p/p)             !
!      cfc11 : volume mixing ratio                   (p/p)             !
!      cfc12 : volume mixing ratio                   (p/p)             !
!      cfc22 : volume mixing ratio                   (p/p)             !
!      ccl4  : volume mixing ratio                   (p/p)             !
!      cfc113: volume mixing ratio                   (p/p)             !
!                                                                      !
!!!!!  ==========================================================  !!!!!
!!!!!                       end descriptions                       !!!!!
!!!!!  ==========================================================  !!!!!



!========================================!
      module module_radiation_gases      !
!........................................!
!
      use machine ,                only : kind_phys, kind_io4
      use funcphys,                only : fpkap
      use physcons,                only : con_pi
      use ozne_def,                only : jmr => latsozc, loz => levozc &
     &,                                   blte => blatc, dlte=> dphiozc &
     &,                                   timeozc => timeozc
      use module_iounitdef,        only : NIO3CLM, NICO2CN
!
      implicit   none
!
      private

!  ---  parameter constants

      integer, parameter, public :: NF_VGAS = 10     ! number of gas species
      integer, parameter         :: IMXCO2  = 24     ! input co2 data lon points
      integer, parameter         :: JMXCO2  = 12     ! input co2 data lat points
      integer, parameter         :: MINYEAR = 1957   ! earlist year 2-d co2 data
                                                     ! available

      real (kind=kind_phys), parameter :: resco2=15.0         ! horiz res in degree
      real (kind=kind_phys), parameter :: raddeg=180.0/con_pi ! rad->deg conversion
      real (kind=kind_phys), parameter :: prsco2=788.0        ! pres lim for 2-d co2 (mb)

!  ---  parameter constants for gas volume mixing ratioes

      real (kind=kind_phys), parameter :: co2vmr_def = 350.0e-6
      real (kind=kind_phys), parameter :: n2ovmr_def = 0.31e-6
      real (kind=kind_phys), parameter :: ch4vmr_def = 1.50e-6
      real (kind=kind_phys), parameter :: o2vmr_def  = 0.209
      real (kind=kind_phys), parameter :: covmr_def  = 1.50e-8
      real (kind=kind_phys), parameter :: f11vmr_def = 3.520e-10   ! aer 2003 value
      real (kind=kind_phys), parameter :: f12vmr_def = 6.358e-10   ! aer 2003 value
      real (kind=kind_phys), parameter :: f22vmr_def = 1.500e-10   ! aer 2003 value
      real (kind=kind_phys), parameter :: cl4vmr_def = 1.397e-10   ! aer 2003 value
      real (kind=kind_phys), parameter :: f113vmr_def= 8.2000e-11  ! gfdl 1999 value

!  ---  co2 2-d monthly data and global mean from observed data

      real (kind=kind_phys), allocatable :: co2vmr_sav(:,:,:)
      real (kind=kind_phys)              :: co2_glb

!  ---  ico2flg  - control flag for co2 data sources set by 'gasinit'
!          =0: use prescribed global mean value
!          =1: use observed co2 global annual mean value
!          =2: use obs co2 monthly data with 2-d variation

      integer :: ico2flg = 0
      integer :: kyrsav  = 0
      integer :: kmonsav = 0

!  ---  public interfaces

      public  gasinit, getgases, getozn


! =================
      contains
! =================

!-----------------------------------
      subroutine gasinit                                                &
!...................................

!  ---  inputs:
     &     ( iyear, month, ICTM, ICO2, me )
!  ---  outputs: ( none )

!  ===================================================================  !
!                                                                       !
!  gasinit reads in recorded global 2-d monthly co2 data stored in      !
!  15 degree lat/lon horizontal resolution.                             !
!                                                                       !
!  inputs:                                               dimemsion      !
!     iyear   - year of the requested data for fcst         1           !
!     month   - month of the year                           1           !
!     ICTM    - =yyyy#, external data time/date control flag            !
!               =    0: use data at initial cond time, if not available,!
!                       then use latest, without extrapolation.         !
!               =    1: use data at the forecast time, if not available,!
!                       then use latest and extrapolate to fcst time.   !
!               =yyyy0: use yyyy data for the forecast time, no further !
!                       data extrapolation.                             !
!               =yyyy1: use yyyy data for the fcst. if needed, do       !
!                       extrapolation to match the fcst time.           !
!     ICO2    - input data control flag                     1           !
!               =0: use prescribed global mean co2 (old opernl)         !
!               =1: use observed co2 global annual mean value           !
!               =2: use obs co2 monthly data with 2-d variation         !
!     me      - print message control flag                  1           !
!                                                                       !
!  outputs: (to the module variables)                                   !
!    ( none )                                                           !
!                                                                       !
!  module variables:                                                    !
!     ico2flg    - control flag as ICO2                     1           !
!     co2vmr_sav - monthly co2 volume mixing ratio     IMXCO2*JMXCO2*12 !
!     co2_glb    - global annual mean co2 mixing ratio      1           !
!                                                                       !
!  usage:    call gasinit                                               !
!                                                                       !
!  subprograms called:  none                                            !
!                                                                       !
!  ===================================================================  !
!
      implicit none

!  ---  inputs:
      integer, intent(in) :: iyear, month, ICTM, ICO2, me

!  ---  output: ( none )

!  ---  locals:
      real (kind=kind_phys), dimension(IMXCO2,JMXCO2) :: co2dat
      real (kind=kind_phys):: co2g1, co2g2, rate
      integer    :: i, iyr, imo, iyr1, iyr2, jyr, idyr
      logical    :: file_exist, lextpl
      character  :: cline*94, cform*8, cfile0*26, cfile1*26

      data  cfile0 / 'co2historicaldata_glob.txt' /
      data  cfile1 / 'co2historicaldata_2004.txt' /
      data  cform  / '(24f7.2)' /       !! data format in IMXCO2*f7.2

!===>  ...  begin here

      ico2flg = ICO2

      if ( ICO2 == 0 ) then
!  --- ...  use prescribed global mean co2 data

        co2_glb = co2vmr_def

        if ( me == 0 ) then
          print *,' - Using prescribed co2 global mean value=',         &
     &              co2vmr_def
        endif

        return
      endif

      lextpl = ( mod(ICTM,10) == 1 )     ! flag for data extrapolation
      idyr   = ICTM / 10                 ! year of data source used
      if ( idyr == 0 ) idyr = iyear      ! not specified, use model year

      if ( ICO2 == 1 .or. ICO2 == 2 ) then
!  --- ...  auto select co2 2-d data table for required year

        kmonsav = month
        if ( kyrsav == iyear ) return
        kyrsav = iyear
        iyr    = iyear

!  --- ...  allocate data space
        if ( ICO2==2 .and. .not. allocated(co2vmr_sav) ) then
          allocate ( co2vmr_sav(IMXCO2,JMXCO2,12) )
        endif

!  --- ...  set up input data format
!       write(cform,22) IMXCO2
! 22    format('(',i2,'f7.2)')

!  --- ...  for data earlier than MINYEAR (1957), the data are in
!           the form of semi-yearly global mean values.  otherwise,
!           data are monthly mean in horizontal 2-d map.

        Lab_if_idyr : if ( idyr < MINYEAR ) then

          if ( me == 0 ) then
            print *,' - Using Historical Co2 Data Table'
            print *,'   Requested CO2 data year',iyear,' earlier than', &
     &              MINYEAR
            print *,'   Which is the earliest monthly observation',     &
     &              ' data available.'
            print *,'   Thus, historical global mean data is used'
          endif

!  --- ... check to see if requested co2 data file existed

          inquire (file=cfile0, exist=file_exist)
          if ( .not. file_exist ) then
            if ( me == 0 ) then
              print *,'   Requested co2 data file "',cfile0,            &
     &                '" not found!'
              print *,'   *** Stopped in subroutine GASINIT !!'
            endif
            stop
          else
            open (NICO2CN,file=cfile0,form='formatted',status='old')
            rewind NICO2CN

            read (NICO2CN, 24) iyr1, iyr2, cline
  24        format(i4,4x,i4,a48)

            if ( me == 0 ) then
              print *,'   Opened co2 data file: ',cfile0
!check        print *, iyr1, iyr2, cline(1:48)
            endif

            if ( idyr < iyr1 ) then
              iyr = iyr1
!check        if ( me == 0 ) then
!               print *,'   Using earlist available co2 data, year=',   &
!    &                  iyr1
!check        endif
            endif

            i = iyr2
            Lab_dowhile1 : do while ( i >= iyr1 )
!             read (NICO2CN,26) jyr, co2g1, co2g2
! 26          format(i4,4x,2f7.2)
              read (NICO2CN, *) jyr, co2g1, co2g2

              if ( i == iyr .and. iyr == jyr ) then
                co2_glb = (co2g1+co2g2) * 0.5e-6
                if ( ICO2 == 2 ) then
                  co2vmr_sav(:,:,1:6)  = co2g1 * 1.0e-6
                  co2vmr_sav(:,:,7:12) = co2g2 * 1.0e-6
                endif

                if ( me == 0 ) print *,'   Co2 data for year',iyear,    &
     &                                 co2_glb
                exit Lab_dowhile1
              else
!check          if ( me == 0 ) print *,'   Skip co2 data for year',i
                i = i - 1
              endif
            enddo  Lab_dowhile1

            close ( NICO2CN )
          endif   ! end if_file_exist_block

        else  Lab_if_idyr

!  --- ...  set up input data file name
          write(cfile1(19:22),34) idyr
  34      format(i4.4)

          if ( me == 0 ) then
            print *,' - Using Historical Co2 Data Table'
          endif

!  --- ... check to see if requested co2 data file existed

          inquire (file=cfile1, exist=file_exist)
          if ( .not. file_exist ) then
            Lab_if_ICTM : if ( ICTM > 10 ) then ! specified year of data not found

              if ( me == 0 ) then
                print *,'   Specified co2 data for year',idyr,          &
     &                 ' not found !!  Need to change namelist ICTM !!'
                print *,'   *** Stopped in subroutine GASINIT !!'
              endif
              stop

            else Lab_if_ICTM                    ! looking for latest available data

              if ( me == 0 ) then
                print *,'   Requested co2 data for year',idyr,          &
     &                ' not found, check for other available data set'
              endif

              Lab_dowhile2 : do while ( iyr >= MINYEAR )
                iyr = iyr - 1
                write(cfile1(19:22),34) iyr

                inquire (file=cfile1, exist=file_exist)
                if ( me == 0 ) then
                  print *,' Looking for CO2 file ',cfile1
                endif

                if ( file_exist ) then
                  exit Lab_dowhile2
                endif
              enddo   Lab_dowhile2

              if ( .not. file_exist ) then
                if ( me == 0 ) then
                  print *,'   Can not find co2 data source file'
                  print *,'   *** Stopped in subroutine GASINIT !!'
                endif
                stop
              endif

            endif  Lab_if_ICTM
          endif   ! end if_file_exist_block

!  --- ...  read in co2 2-d data for the requested month
          open (NICO2CN,file=cfile1,form='formatted',status='old')
          rewind NICO2CN
          read (NICO2CN, 36) iyr, cline, co2g1, co2g2
  36      format(i4,a94,f7.2,16x,f5.2)

          if ( me == 0 ) then
            print *,'   Opened co2 data file: ',cfile1
            print *, iyr, cline, co2g1, '  GROWTH RATE =', co2g2
          endif

!  --- ...  add growth rate if needed
          if ( lextpl ) then
!           rate = co2g2 * (iyear - iyr)   ! rate from early year
!           rate = 1.60  * (iyear - iyr)   ! avg rate over long period
            rate = 2.00  * (iyear - iyr)   ! avg rate for recent period
          else
            rate = 0.0
          endif

          co2_glb = (co2g1 + rate) * 1.0e-6
!check    if ( me == 0 ) print *,'   co2 data for year',iyear,co2_glb

          if ( ICO2 == 2 ) then
            do imo = 1, 12
              read (NICO2CN,cform) co2dat
!check        print cform, co2dat

              co2vmr_sav(:,:,imo) = (co2dat(:,:) + rate) * 1.0e-6
            enddo

            if ( me == 0 ) then
              print *,' CHECK: Sample CO2 data used for year:',iyear
              do imo = 1, 12, 3
                print *,'        Month =',imo
                print *, co2vmr_sav(1,:,imo)
              enddo
            endif
          endif

          close ( NICO2CN )
        endif  Lab_if_idyr

        return
      else
        print *,' !! ERROR in CO2 Scheme Setting, ICO2=',ICO2
        stop
      endif    ! end if_ICO2_block

!
!...................................
      end subroutine gasinit
!-----------------------------------


!-----------------------------------
      subroutine getgases                                               &
!...................................

!  ---  inputs:
     &     ( plvl, xlon, xlat,                                          &
     &       IMAX, LMAX, iflip,                                         &
!  ---  outputs:
     &       gasdat                                                     &
     &      )

!  ===================================================================  !
!                                                                       !
!  getgases set up global distribution of radiation absorbing  gases    !
!  in volume mixing ratio.  currently only co2 has the options from     !
!  observed values, all other gases are asigned to the climatological   !
!  values.                                                              !
!                                                                       !
!  inputs:                                                              !
!     plvl(IMAX,LMAX+1)- pressure at model layer interfaces (mb)        !
!     xlon,xlat(IMAX)  - grid longitude/latitude in radians             !
!     IMAX, LMAX       - horiz, vert dimensions for output data         !
!     iflip            - control flag for direction of vertical index   !
!                        =0: index from toa to surface                  !
!                        =1: index from surface to toa                  !
!                                                                       !
!  outputs:                                                             !
!     gasdat(IMAX,LMAX,NF_VGAS) - gases volume mixing ratioes           !
!               (:,:,1)           - co2                                 !
!               (:,:,2)           - n2o                                 !
!               (:,:,3)           - ch4                                 !
!               (:,:,4)           - o2                                  !
!               (:,:,5)           - co                                  !
!               (:,:,6)           - cfc11                               !
!               (:,:,7)           - cfc12                               !
!               (:,:,8)           - cfc22                               !
!               (:,:,9)           - ccl4                                !
!               (:,:,10)          - cfc113                              !
!                                                                       !
!  module variables used:                                               !
!     ico2flg    -  =0: use prescribed co2 global mean value            !
!                   =1: use input global mean co2 value (co2_glb)       !
!                   =2: use input 2-d monthly co2 value (co2vmr_sav)    !
!     co2vmr_sav - saved monthly co2 concentration from sub gasinit     !
!     co2_glb    - saved global annual mean co2 value from  gasinit     !
!                                                                       !
!  usage:    call getgases                                              !
!                                                                       !
!  subprograms called:  none                                            !
!                                                                       !
!  ===================================================================  !
!
      implicit none

!  ---  input:
      integer,  intent(in)  :: IMAX, LMAX, iflip
      real (kind=kind_phys), intent(in) :: plvl(:,:), xlon(:), xlat(:)

!  ---  output:
      real (kind=kind_phys), intent(out) :: gasdat(:,:,:)

!  ---  local:
      integer :: i, k, ilat, ilon

!===>  ...  begin here

!  --- ...  assign default values

      do k = 1, LMAX
      do i = 1, IMAX
        gasdat(i,k,1) = co2vmr_def
        gasdat(i,k,2) = n2ovmr_def
        gasdat(i,k,3) = ch4vmr_def
        gasdat(i,k,4) = o2vmr_def
        gasdat(i,k,5) = covmr_def
        gasdat(i,k,6) = f11vmr_def
        gasdat(i,k,7) = f12vmr_def
        gasdat(i,k,8) = f22vmr_def
        gasdat(i,k,9) = cl4vmr_def
        gasdat(i,k,10)= f113vmr_def
      enddo
      enddo

!  --- ...  co2 section

      if ( ico2flg == 1 ) then
!  ---  use obs co2 global annual mean value only

        do k = 1, LMAX
          do i = 1, IMAX
            gasdat(i,k,1) = co2_glb
          enddo
        enddo

      elseif ( ico2flg == 2 ) then
!  ---  use obs co2 monthly data with 2-d variation at lower atmos
!       otherwise use global mean value

        do i = 1, IMAX
          ilon = min( IMXCO2, int(      xlon(i)*raddeg /resco2) + 1 )
          ilat = min( JMXCO2, int((90.0-xlat(i)*raddeg)/resco2) + 1 )

          do k = 1, LMAX
            if ( plvl(i,k+1) >= prsco2 ) then
              gasdat(i,k,1) = co2vmr_sav(ilon,ilat,kmonsav)
            else
              gasdat(i,k,1) = co2_glb
            endif
          enddo
        enddo
      endif

!
      return
!...................................
      end subroutine getgases
!-----------------------------------


!-----------------------------------
      subroutine getozn                                                 &
!...................................

!  ---  inputs:
     &     ( prslk,xlat,k1oz,k2oz,facoz,                                &
     &       IMAX, LM, iflip,                                           &
!  ---  outputs:
     &       o3mmr                                                      &
     &     )

!  ===================================================================  !
!                                                                       !
!  getozn sets up climatological ozone profile for radiation calculation!
!                                                                       !
!  this code is originally written By Shrinivas Moorthi                 !
!                                                                       !
!  modified to make output o3mmr has same vertical index order as given !
!  by input flag 'iflip'   ---  Apr. 03,  y-t hou                       !
!                                                                       !
!  inputs:                                                              !
!     prslk (IMAX,LM)  - pressure in cb (kPa)                           !
!     xlat  (IMAX)     - latitude in radians                            !
!     k1oz, k2oz       - ozone data interpolation indices               !
!     facoz            - ozone data interpolation factor                !
!     IMAX, LM         - horizontal and vertical dimensions             !
!     iflip            - control flag for direction of vertical index   !
!                        =0: index from toa to surface                  !
!                        =1: index from surface to toa                  !
!                                                                       !
!  outputs:                                                             !
!     o3mmr (IMAX,LM)  - output ozone profile in mass mixing ratio (g/g)!
!                                                                       !
!  usage:    call getozn                                                !
!                                                                       !
!  external function called : fpkap                                     !
!                                                                       !
!  ===================================================================  !
!
      implicit none

!  ---  inputs:
      integer,  intent(in) :: IMAX, LM, k1oz, k2oz, iflip

      real (kind=kind_phys), intent(in) :: prslk(:,:), xlat(:), facoz

!  ---  outputs:
      real (kind=kind_phys), intent(out) :: o3mmr(:,:)

!  ---  locals:
!     integer :: JMR, blte, dlte, LOZ
!  4X5 ozone data
!     parameter (JMR=45, blte=-86.0, dlte=4.0)
! GEOS ozone data
!     parameter (JMR=18, blte=-85.0, dlte=10.0, LOZ=17)
!
      real (kind=kind_io4) :: o3clim4(JMR,LOZ,12), pstr4(LOZ)

      real (kind=kind_phys), allocatable :: pstr(:), pkstr(:),          &
     &                                     o3r(:,:,:)
      real (kind=kind_phys) ::  o3i(IMAX,LOZ), wk1(IMAX), deglat, elte, &
     &                         rdg, tem, tem1, tem2, tem3, tem4, temp
                                                                               
      integer :: imond(12), ilat(JMR,12)
      integer :: i, j, k, l, nm, j1, j2, ll
                                                                               
      logical :: first
!
      data  first / .true. /
                                                                               
      save first, pkstr, pstr, o3r, elte
!

      if (first) then

         if (timeozc .ne. 12) then
           print *,' timeozc=',timeozc, ' is not monthly mean'          &
     &,' - job aborting'
           call mpi_quit(999)
         endif
!
         allocate (pstr(LOZ), pkstr(LOZ), o3r(JMR,LOZ,timeozc))
         rewind NIO3CLM
         elte = blte + (JMR-1) * dlte
                                                                               
         if (LOZ == 17) then       ! For the operational ozone climatology
           do l = 1, LOZ
             read(NIO3CLM,15) pstr4(l)
   15        format(f10.3)
           enddo

           do nm = 1, 12
             do j = 1, JMR
              read(NIO3CLM,19) imond(nm),ilat(j,nm),                    &
     &                         (o3clim4(j,l,nm),l=1,10)
   19         format(i2,i4,10f6.2)
              read(NIO3CLM,20) (o3clim4(j,l,nm), l=11,LOZ)
   20         format(6x,10f6.2)
             enddo
           enddo
         else                      ! For newer ozone climatology
           read (NIO3CLM)
           do l=1,loz
             READ (NIO3CLM) pstr4(l)
           enddo

           do nm = 1, 12
             do l=1,loz
               read(NIO3CLM) (o3clim4(j,l,nm),j=1,jmr)
             enddo
           enddo
         endif
!
         pstr = pstr4
         o3r = o3clim4

         do  nm = 1, 12
           do l = 1, LOZ
             do j = 1, JMR
               o3r(j,l,nm) = o3r(j,l,nm) * 1.655e-6
             enddo
           enddo
         enddo

         print *,' FOUND OZONE DATA FOR LEVELS PSTR=',(pstr(l),l=1,LOZ)
!        print *,' O3=',(o3r(15,l,1),l=1,LOZ)

         do l = 1, LOZ
           pkstr(l) = fpkap(pstr(l)*100.0)
         enddo

         first  = .false.
      endif
!
      do i = 1, IMAX
        deglat = xlat(i) * raddeg

        if (deglat > blte .and. deglat < elte) then
          tem1 = (deglat - blte) / dlte + 1
          j1   = tem1
          j2   = j1 + 1
          tem1 = tem1 - j1
        elseif (deglat <= blte) then
          j1   = 1
          j2   = 1
          tem1 = 1.0
        elseif (deglat >= elte) then
          j1   = JMR
          j2   = JMR
          tem1 = 1.0
        endif

        tem2 = 1.0 - tem1
        do j = 1, LOZ
          tem3     = tem2*o3r(j1,j,k1oz) + tem1*o3r(j2,j,k1oz)
          tem4     = tem2*o3r(j1,j,k2oz) + tem1*o3r(j2,j,k2oz)
          o3i(i,j) = tem4*facoz          + tem3*(1.0 - facoz)
        enddo
      enddo

      do l = 1, LM
        ll = l
        if (iflip == 1) ll = LM -l + 1

        do i = 1, IMAX
          wk1(i) = prslk(i,ll)
        enddo

        do k = 1, LOZ-1
          temp = 1.0 / (pkstr(k+1) - pkstr(k))

          do i = 1, IMAX
            if (wk1(i) > pkstr(k) .and. wk1(i) <= pkstr(k+1)) then
              tem       = (pkstr(k+1) - wk1(i)) * temp
              o3mmr(I,ll) = tem * o3i(i,k) + (1.0 - tem) * o3i(i,k+1)
            endif
          enddo
        enddo

        do i = 1, IMAX
          if (wk1(i) > pkstr(LOZ)) o3mmr(i,ll) = o3i(i,LOZ)
          if (wk1(i) < pkstr(1))   o3mmr(i,ll) = o3i(i,1)
        enddo
      enddo
!
      return
!...................................
      end subroutine getozn
!-----------------------------------

!
!........................................!
      end module module_radiation_gases  !
!========================================!
