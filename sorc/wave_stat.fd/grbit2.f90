! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
subroutine grbit2 (IFL2,nx,ny,lcgrib,rdlon,rdlat,bmp,fld,ierr,ymdc,fhr,&
                   parcode,ensid,nprb,prbid,prblv,nensm) 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
! Generates grib2 output
!
! Vera Gerald, NCEP June 2011
! 
! Changes: 
! - Add parcode to read in discipline, category and parameter number
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Inputs
!
!  parcode  - three integers defining discipline, category and param number
!             from grib2 tables 
!  ensid    - 1, 2 or 3 ; where: 1=mean , 2=spread and 3=probability
!  nprb     - Total number of probability levels
!  prbid    - Index of probability value
!  prblv    - Value of probability threshold
!  nensm    - number of ensemble members
!  IFL2     - Unit number for output file 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      CHARACTER(len=1) :: cgrib(lcgrib),cin(lcgrib)
      real             :: FLD(nx*ny)
      real             :: coordlist(1)
      real             :: rdlon(3),rdlat(3)
      integer          :: pds(25),gds(22),ensid,prbid,prblv,nensm
      integer          :: listsec0(3),tmpln 
      integer          :: listsec1(13)
      integer          :: parcode(3)
      integer          :: igds(5),igdstmpl(200),ipdstmpl(200)
      integer          :: ymdc,ymd,ym,y4,mm,dd,cc,fhr,yy,cen
      integer          :: nx, ny, nxny, ibfl
      integer          :: ideflist,idefnum
      integer          :: idrstmpl(200)
      integer          :: intlon,intlat
      Logical*1        :: bmp(nx*ny) 
      character*11     :: envvar
      character(len=80):: g2file
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
! Initialize date parameters
!
      cc = mod(ymdc,100)
      ymd = ymdc/100
      dd = mod(ymd,100)
      ym = ymd/100
      mm = mod(ym,100)
      y4 = ym/100
!
!  Create grib message
!
!  Initialize new GRIB2 message and pack
!
!  Section 0 (Indicator Section) 
!
       listsec0 = 00
!
       listsec0(1) = parcode(1) ! Discipline-GRIB Master Table Number (see Code Table 0.0)
       listsec0(2) = 2 ! GRIB Edition Number (currently 2)
       listsec0(3) = 0
!
! Section 1 (Identification  Section)
!
       listsec1(1) = 7       ! 7  Id of orginating centre (Common Code Table C-1)
       listsec1(2) = 0 !"EMC"! Id of orginating sub-centre (local table)/Table C/on388
       listsec1(3) = 2       ! GRIB Master Tables Version Number (Code Table 1.0)
       listsec1(4) = 1       !per Brent! GRIB Local Tables Version Number (Code Table 1.
       listsec1(5) = 1       ! Significance of Reference Time (Code Table 1.2)
       listsec1(6) = y4          ! Reference Time - Year (4 digits)
       listsec1(7) = mm          ! Reference Time - Month
       listsec1(8) = dd          ! Reference Time - Day
       listsec1(9) = cc          ! Reference Time - Hour(cycle:0,6,12,18)
       listsec1(10) = 0          ! Reference Time - Minute
       listsec1(11) = 0          ! Reference Time - Second
       listsec1(12) = 0          ! Production status of data (Code Table 1.3)
       listsec1(13) = 1          ! Type of processed data (Code Table 1.4)

!
      call gribcreate(cgrib,lcgrib,listsec0,listsec1,ierr)
      if (ierr.ne.0) then
        write(6,*) ' ERROR creating new GRIB2 field = ',ierr
        return
      endif
!
!  Section 3 (Grid Definition Section) 
!

        nxny = nx * ny           ! NI # rows(lat) & NJ # rows long
!
           igds = 00
!
           igds(1) = 0                  !Source of grid definition (see Code Table 3.0)
           igds(2) = nxny              ! num of grid points
           igds(3) = 0                  !Number of octets needed for each additional grid points dfn
           igds(4) = 0                  !Interpretation of list for optional points definition (Code Table 3.11)
           igds(5) = 0                  !Grid Definition Template Number (Code Table 3.1)
!
           idefnum = 0
           ideflist=0       !Used if igds(3) .ne. 0. Dummy array otherwise
!
        igdstmpl = 00
!
!   Define geographical ranges from arguments
!
           iloni=INT(rdlon(1)*1.E6)
           ilone=INT(rdlon(2)*1.E6)
           idlon=INT(rdlon(3)*1.E6)
           ilati=INT(rdlat(1)*1.E6)
           ilate=INT(rdlat(2)*1.E6)
           idlat=INT(rdlat(3)*1.E6)

           igdstmpl(1) = 6    !Earth assumed spherical with radius of 6,371,229.0m 
           igdstmpl(2) = 0    !
           igdstmpl(3) = 0
           igdstmpl(4) = 0
           igdstmpl(5) = 0
           igdstmpl(6) = 0
           igdstmpl(7) = 0
           igdstmpl(8) = nx             ! num points along parallel
           igdstmpl(9) = ny             ! num points along meridian
           igdstmpl(10) = 0
           igdstmpl(11) = 0
           igdstmpl(12) = ilati         ! lat of first grid point
           igdstmpl(13) = iloni         ! long of first grid point
           igdstmpl(14) = 48            ! res and comp flags
           igdstmpl(15) = ilate         ! lat of last grid point
           igdstmpl(16) = ilone         ! long of last grid point
           igdstmpl(17) = idlat         ! Increment of lat
           igdstmpl(18) = idlon         ! Increment of long
           igdstmpl(19) = 0            ! scanning mode
           igdstmpl(21) = 0
           igdstmpl(22) = 0

!
      call addgrid(cgrib,lcgrib,igds,igdstmpl,200,ideflist, &
                  idefnum,ierr)
      if (ierr.ne.0) then
        write(6,*) ' ERROR adding GRIB2 grid = ',ierr
        return
      endif
!
!  Section 4 - Product Definition Section
!
!  Define production template number (4.tmpln)
!
         if(ensid.eq.3)then
            ipdsnum = 5  ! ensemble for probal template(4.5)
           else
            ipdsnum = 2  ! ensemble spread or mean template(4.2)
         endif
!
!  Parameter category (see Code Table 4.1)
!
       ipdstmpl = 00
!
        ipdstmpl(1) = parcode(2)     ! Category
!
!    Get parameter number (see Code Table 4.2)
!
        ipdstmpl(2) = parcode(3)  ! Parameter number
!
!  Type of generating process: analysis or forecast(see code Table 4.3)
!  For Wave Model/Forecast fields
!
        ipdstmpl(3) = 4     ! ensemb
        ipdstmpl(4) = 0     !background generating process identifier
!                         (defined by originating Center)
        ipdstmpl(5) = 10   ! :analysis or forecast generating process identifier
!                            (defined by originating Center)
        ipdstmpl(6) = 0     ! hours of observational data cutoff after reference time
        ipdstmpl(7) = 0     ! minutes of observational data cutoff after reference time
        ipdstmpl(8) = 1        !indicator of unit of time range (see Code Table 4.4) 
        ipdstmpl(9) = fhr      !forecast time in units defined by pdstmpl(8)
        ipdstmpl(10) = 1        ! type of level (see Code Table 4.5) 1st level
        ipdstmpl(11) = 0        ! scale factor of pdstmpl(10) 
        ipdstmpl(12) = 0        ! scaled value of pdstmpl(10)
        ipdstmpl(13) = 255   ! type of level (See Code Table 4.5) 2nd level
        ipdstmpl(14) = 0     ! scale factor of ipdstmpl(13)
        ipdstmpl(15) = 0     ! scaled value of ipdstmpl(13)
!
!   Choose proper parameters for given ensid (mean, spread or probab)
!
       if (ensid.eq.1) then       !   ensemble mean
          ipdstmpl(16) = 0        !
          ipdstmpl(17) = nensm    ! Number of esemble members
       elseif (ensid.eq.2) then   !  ensemble spread
           ipdstmpl(16) = 4
           ipdstmpl(17) = nensm   ! Number of ensemble members 
       elseif (ensid.eq.3) then   ! ensemble probability
            ipdstmpl(16) = prbid  ! Forecast probability number
            ipdstmpl(17) = 8      ! Total number of forecast probabilities 
            ipdstmpl(18) = 1      ! Probability type
            ipdstmpl(19) = 0      ! Scale factor of lower limit
            ipdstmpl(20) = 0      ! Scaled value of lower limit
            ipdstmpl(21) = 2      ! Scale factor of upper limit 
            ipdstmpl(22) = prblv  ! Scaled value of upper limit 
       endif
!
      numcoord=0
        coordlist= 0.0     !needed for hybrid vertical coordinate
! set bitmap flag
        ibfl = 192    ! GDS/BMS FLAG (RIGHT ADJ COPY OF OCTET 8)
        if (btest(ibfl,6)) then
          ibmap=0
        else
          ibmap=255    ! Bitmap indicator ( see Code Table 6.0 )
         bmp=.true.
        endif
      idrsnum=40     ! Data Rep Template Number ( see Code Table 5.0 )/Simple packing
!
      idrstmpl=0
!
!********************************************************************************
! idrstmpl(1): reference value(R) (IEEE 32-bit floating-point value)             *
! idrstmpl(2): binary scale factor (E)                                           *
! idrstmpl(3): decimal scale factor (D)                                          *
! idrstmpl(4): number of bits used for each packed value for simple packing      *
!              or for each group reference value for complex packing or          *
!              spatial differencing                                              *
! idrstmpl(5): type of original field values (See Code Table 5.1)                *
!********************************************************************************
!
      idrstmpl(1) = 0
      idrstmpl(2) = 0
 
      idrstmpl(3) = 2       ! binary scale factor (E)
      idrstmpl(4) = 0
      idrstmpl(5) = 0
!
      call addfield(cgrib,lcgrib,ipdsnum,ipdstmpl,200, &
                   coordlist,numcoord,idrsnum,idrstmpl,200,fld, &
                   nxny,ibmap,bmp,ierr)
      if (ierr.ne.0) then
          write(6,*) ' ERROR adding GRIB2 field = ',ierr
          return
      endif
!
!
       print*,'grid2 PDS ',ipdsnum,idrsnum,ipdstmpl(1:25)
!
       print*,'grib2 GDS',igds(1:5),igdstmpl(1:22)
!
!
! End GRIB2 field
!
      call gribend(cgrib,lcgrib,lengrib,ierr)
      if (ierr.ne.0) then
        write(6,*) ' ERROR ending new GRIB2 message = ',ierr
        return
      endif
      print *,' writing ',lengrib,' bytes...'
       call wryte(ifl2,lengrib,cgrib)
!
!..         encode next record
!
         return
!
      end
