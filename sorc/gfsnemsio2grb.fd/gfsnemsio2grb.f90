!- - - - -- - -- - -- - -- - - -- - --  -- - -- - -- - - -- - - - -- - --
!
! This program converts a GFS Gaussian Grid nemsio file into grib2
!	the program handles most of the atm and sfc files
!
! This program is based on mkgfsnemsioctl.f  (5/12/2015 Jun Wang)
!
! The conversion to grib is done using wgrib2api
!   http://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/wgrib2api.html
! The center, subcenter and grib packing type are easily changed but
! hard coded into the fortran code.
!
! Requirements:  
!       New Fortran: newer f90 or f95, intel circa 2012
!       wgrib2 library
!
!   April 20, 2018   Jun Wang, Wesley Ebisuzaki
! - - - - -- - -- - -- - -- - - -- - --  -- - -- - -- - - -- - - - -- - --
!
  use nemsio_module
  use wgrib2api
  implicit none
!
!---------------------------------------------------------------------------
  type(nemsio_gfile) :: gfile
  character(255) cin,cindx,cgrb
  real,allocatable  :: data(:,:), data1(:)
  character(100) :: glev, ftime, refdate, fcst, ave, min,max,acc, xdef, ydef,ftime1
  character(100) :: to_upper
  character(200) :: meta

!---------------------------------------------------------------------------
!--- nemsio meta data
  integer nrec,im,jm,lm,idate(7),nfhour,tlmeta,nsoil,fieldsize
  real,allocatable       :: slat(:),dx(:)
  real(8),allocatable    :: lat(:)
  character(16),allocatable:: recname(:),reclevtyp(:)
  integer,allocatable:: reclev(:)
!---------------------------------------------------------------------------
!--- local vars
  character(3) cmon
  character(16) reclevtyp_sht
  character(32) ctldate,varname
  character(35) sweep_blanks
  real(8) dxctl,radi
  integer i,n,j,krec,iret,io_unit,idrt,data_unit
  integer :: strlen
!---------------------------------------------------------------------------
!---------------------------------------------------------------------------
!
  call nemsio_init(iret=iret)
  if(iret/=0) print *,'ERROR: nemsio_init '
!
!---------------------------------------------------------------------------
!***  read nemsio grd header info
!---------------------------------------------------------------------------
!--- open gfile for reading
  call getarg(1,cin)
  print *,'filename is cin=',trim(cin)
  if(trim(cin)=='') then
    print *,'usage: gfsnemsio2grb input_nemsio_file_name'
    stop 1
  endif
  cindx=trim(cin)//'.ctl'
  
  call nemsio_open(gfile,trim(cin),'READ',iret=iret)
  if(iret/=0) print *,'Error: open nemsio file,',trim(cin),' iret=',iret

  call nemsio_getfilehead(gfile,iret=iret,nrec=nrec,dimx=im,dimy=jm, &
    dimz=lm,idate=idate,nfhour=nfhour,                               &
    nsoil=nsoil,tlmeta=tlmeta)
!
   fieldsize=im*jm
   allocate(recname(nrec),reclevtyp(nrec),reclev(nrec))
   allocate(lat(fieldsize),slat(jm),dx(fieldsize))
   call nemsio_getfilehead(gfile,iret=iret,recname=recname,          &
       reclevtyp=reclevtyp,reclev=reclev,idrt=idrt) 

!  check for Gaussian grid
   if (idrt /= 4) then
       write(*,*) 'Error: expected idrt=4, not ',idrt
       stop 2
   endif 
!  get Gaussian grid
   call splat4(idrt,jm,slat)
   radi=180.0/(4.*atan(1.0))
   do  j=1,jm
       lat(j) = asin(slat(j)) * radi
   enddo
   dxctl=360./(im*1.)

!   write grib output

    cgrb=trim(cin)//'.grb'		! grib file name

!   create template with Gaussian grid using wgrib2 -new_grid
    write(xdef,'(a,i,a,f10.6)') '0:', im,':',360.0/im
    write(ydef,'(f12.6,a,i)') lat(1),':',jm
    write(*,*) trim(xdef)
    write(*,*) 'ydef=',trim(ydef)

    i = wgrib2a('template_512.grb','-new_grid_winds','earth','-new_grid','gaussian',xdef,ydef,'@mem:0')
    if (i /= 0) stop 2

    write(refdate,'(i4.4,5i2.2)') idate(1:6)
    write(*,*) 'date=',idate(1:4), ' nfhour=',nfhour, trim(refdate)
    allocate (data(im,jm),data1(im*jm))

!   formats for ftime
    write(fcst,'(i,a)') nfhour,' hour fcst'
    fcst = adjustl(fcst)
    write(ave,'(i,a)') nfhour,' hour ave fcst'
    ave = '0-' // adjustl(ave)
    write(max,'(i,a)') nfhour,' hour max fcst'
    max = '0-' // adjustl(max)
    write(min,'(i,a)') nfhour,' hour min fcst'
    min = '0-' // adjustl(min)
    write(acc,'(i,a)') nfhour,' hour acc fcst'
    acc = '0-' // adjustl(acc)

!   loop over all records
    do i = 1, nrec
	call nemsio_readrec(gfile,i,data1,iret=iret)
	if (iret.ne.0) then
	    write(*,*) 'nemsio_readrec problem return = ', iret
            stop 3
	endif

	varname = recname(i)
	glev = reclevtyp(i)
        ftime = fcst
        ftime1 = 'fcst'

!	suffix to name

!	_ave
        strlen = len_trim(varname)
	if (varname(strlen-3:strlen) == '_ave') then
            ftime=ave
            ftime1='ave'
            varname = varname(1:strlen-4)
        endif

!	min_min
        strlen = len_trim(varname)
	if (varname(strlen-6:strlen) == 'min_min') then
            ftime=min
            ftime1='min'
            varname = varname(1:strlen-7)
	    if (varname == 't') varname='tmp'
        endif

!	max_max
        strlen = len_trim(varname)
	if (varname(strlen-6:strlen) == 'max_max') then
            ftime=max
            ftime1='max'
            varname = varname(1:strlen-7)
	    if (varname == 't') varname='tmp'
        endif

!	_acc
        strlen = len_trim(varname)
	if (varname(strlen-3:strlen) == '_acc') then
            ftime=acc
            ftime1='acc'
            varname = varname(1:strlen-4)
        endif

        strlen = len_trim(varname)
	if (varname(strlen-7:strlen) == '_hyblev1') then
            glev = '1 hybrid level'
            varname = varname(1:strlen-8)
        endif


!	do the levels .. can use else if (..) then  ..  ugly
	if (glev == 'sfc') glev='surface'
	if (glev == '0-10 cm down') glev='0-0.1 m below ground'
	if (glev == '10-40 cm down') glev='0.1-0.4 m below ground'
	if (glev == '10-40 cm down') glev='0.1-0.4 m below ground'
	if (glev == '40-100 cm down') glev='0.4-1 m below ground'
        if (glev == '100-200 cm down') glev='1-2 m below ground'
        if (glev == '0-200 cm down') glev='0-2 m below ground'
	if (glev == 'nom. top') glev='top of atmosphere'
	if (glev == '2 m above gnd') glev='2 m above ground'
	if (glev == '10 m above gnd') glev='10 m above ground'
	if (glev == 'bndary-layer cld') glev='boundary layer cloud layer'
	if (glev == 'convect-cld laye') glev='convective cloud layer'
	if (glev == 'low cld lay') glev='low cloud layer'
	if (glev == 'mid cld lay') glev='middle cloud layer'
	if (glev == 'high cld lay') glev='high cloud layer'
	if (glev == 'low cld top') glev='low cloud top level'
	if (glev == 'mid cld top') glev='middle cloud top level'
	if (glev == 'high cld top') glev='high cloud top level'
	if (glev == 'low cld bot') glev='low cloud bottom level'
	if (glev == 'mid cld bot') glev='middle cloud bottom level'
	if (glev == 'high cld bot') glev='high cloud bottom level'
	if (glev == 'convect-cld top') glev='convective cloud top level'
	if (glev == 'convect-cld bot') glev='convective cloud bottom level'

	if (glev == 'mid layer') write(glev,'(i,a)') reclev(i), ' hybrid level'

!	do varname
	varname = to_upper(varname)
	if (varname == 'OROG') varname = 'HGT'
	if (varname == 'DSWRF') then
           if (glev /= 'surface') then
               ftime1 = 'fcst'
           endif
!            write(16,*) glev
!            write(16,*) ftime1
        endif
        if (varname == 'TPRCP') then
           varname = 'APCP'
!           ftime = '0-9 hour acc'
!           ftime1 = 'acc'
           ftime1 = ''
!          don't know timing info
        endif
!      if (varname(strlen-3:strlen)  == "_ave") then 
      if ( ftime1== 'acc' .or. ftime1 == 'ave' .or. ftime1 == 'min' .or. ftime1 == 'max') then
!        write(15,*) ftime1,varname
        meta='d=' // trim(refdate) // ':' // trim(varname) // ':' // trim(glev) // ':' // trim(ftime) // ':'
	write(*,*) trim(meta)

	iret = grb2_wrt(trim(cgrb),'@mem:0',1,meta=meta,data1=data1, &
            order='we:ns', center=7, subcenter=0, packing='c2')
	if (iret /= 0) then
	    write(*,*) 'bad meta: ', trim(meta)
	endif
      endif
    enddo

!
   call nemsio_close(gfile,iret=iret)
!
   call nemsio_finalize()
!
!---------------------------------------------------------------------------
!****** write .ctl file
!---------------------------------------------------------------------------
!
!-- get date
   call cmonth(idate(2),cmon)
   write(ctldate,'(i2.2,a,i2.2,a3,i4.4)')idate(4),'Z',idate(3)       &
        ,cmon,idate(1)

   io_unit=650
   open(io_unit,file=trim(cindx),form='formatted')

   write(io_unit,105)trim(cin)
   write(io_unit,106)
   write(io_unit,107)
   write(io_unit,108)tlmeta
   write(io_unit,109)
   write(io_unit,111)im,dxctl
   write(io_unit,112)jm
   write(io_unit,113)(lat(i),i=jm,1,-1)
   write(io_unit,114)
   if(nfhour/=0) then
     write(io_unit,115)1,trim(ctldate),nfhour
   else
     write(io_unit,116)1,trim(ctldate)
   endif

!
 105  FORMAT('dset ^',A)
 106  FORMAT('undef 9.99E+20')
 107  FORMAT('options big_endian sequential yrev')
 108  FORMAT('fileheader',I12.0)
 109  FORMAT('title gfs nemsioi sfc file')

 111  FORMAT('xdef  ',I6,' linear  0.000000 ',f9.6)
 112  FORMAT('ydef  ',I6,' levels')
 113  FORMAT(10f11.6)

 114  FORMAT('zdef 1 linear 1 1 ')
 115  FORMAT('tdef ',I6,' linear ',A12,' ',I6,'hr')
 116  FORMAT('tdef ',I6,' linear ',A12,'  1yr')
!
   krec=0
   do n=1,nrec
     if(reclev(n)==1) then
       krec=krec+1
     endif
   enddo

   WRITE(IO_UNIT,'(A,I6)')'VARS ',krec

    n=1
    nrec=krec
   do while (n<=nrec)
     reclevtyp_sht=reclevtyp(n)
     if(trim(reclevtyp_sht) == "convect-cld bot") then
       reclevtyp_sht="cvb"
     elseif (trim(reclevtyp_sht) == "convect-cld top") then
       reclevtyp_sht="cvt"
     elseif (trim(reclevtyp_sht) == "high cld bot") then
       reclevtyp_sht="hcb"
     elseif (trim(reclevtyp_sht) == "high cld top") then
       reclevtyp_sht="hct"
     elseif (trim(reclevtyp_sht) == "mid cld bot") then
       reclevtyp_sht="mcb"
     elseif (trim(reclevtyp_sht) == "mid cld top") then
       reclevtyp_sht="mct"
     elseif (trim(reclevtyp_sht) == "low cld bot") then
       reclevtyp_sht="lcb"
     elseif (trim(reclevtyp_sht) == "low cld top") then
       reclevtyp_sht="lct"
     elseif (trim(reclevtyp_sht) == "convect-cld laye") then
       reclevtyp_sht="cvcl"
     elseif (trim(reclevtyp_sht) == "bndary-layer cld") then
       reclevtyp_sht="bdrlc"
     elseif (trim(reclevtyp_sht) == "atmos col") then
       reclevtyp_sht="acol"
     elseif (trim(reclevtyp_sht) == "high cld lay") then
       reclevtyp_sht="hcl"
     elseif (trim(reclevtyp_sht) == "mid cld lay") then
       reclevtyp_sht="mcl"
     elseif (trim(reclevtyp_sht) == "low cld lay") then
       reclevtyp_sht="lcl"
     elseif (trim(reclevtyp_sht) == "2 m above gnd") then
       reclevtyp_sht="2m"
     elseif (trim(reclevtyp_sht) == "10 m above gnd") then
       reclevtyp_sht="10m"
     endif
     varname=sweep_blanks(trim(recname(n))//trim(reclevtyp_sht))
  
     if(trim(reclevtyp(n))=='mid layer') then
       write(io_unit,'(a16,i3,a)')varname,lm,' 99 model layer'
       n=n+lm
     elseif(trim(reclevtyp(n))=='soil layer') then
        write(io_unit,'(a16,i3,a)')varname,nsoil,' 99 soil layer'
        n=n+nsoil
     elseif(trim(reclevtyp(n))=='2 m above gnd') then
       recname(n)=trim(recname(n))//'2m'
       write(io_unit,'(a16,a7,a)')varname,'  0 99 ',trim(reclevtyp(n))
       n=n+1
     elseif(trim(reclevtyp(n))=='10 m above gnd') then
       recname(n)=trim(recname(n))//'10m'
       write(io_unit,'(a16,a7,a)')varname,'  0 99 ',trim(reclevtyp(n))
       n=n+1
     else
       write(io_unit,'(a16,a7,a)')varname,'  0 99 ',trim(reclevtyp(n))
       n=n+1
     endif
   enddo

   write(io_unit,'(A8)')'endvars'
   close(io_unit)

!---------------------------------------------------------------------------
!****** clean up
!---------------------------------------------------------------------------
  deallocate(recname,reclevtyp,reclev,lat,slat,dx)
!---------------------------------------------------------------------------
!
! - - - - -- - -- - -- - -- - - -- - --  -- - -- - -- - - -- - - - -- - --
  stop

 end program
! - - - - -- - -- - -- - -- - - -- - --  -- - -- - -- - - -- - - - -- - --
      SUBROUTINE CMONTH(IMON,CMON)
!
!-----------------------------------------------------------------------
!***  Convert month
!-----------------------------------------------------------------------
!
      INTEGER,INTENT(IN) :: IMON
      CHARACTER(LEN=3)   :: CMON
!
!-----------------------------------------------------------------------
!
      SELECT CASE (IMON)
        CASE(1)
            CMON='Jan'
        CASE(2)
            CMON='Feb'
        CASE(3)
            CMON='Mar'
        CASE(4)
            CMON='Apr'
        CASE(5)
            CMON='May'
        CASE(6)
            CMON='Jun'
        CASE(7)
            CMON='Jul'
        CASE(8)
            CMON='Aug'
        CASE(9)
            CMON='Sep'
        CASE(10)
            CMON='Oct'
        CASE(11)
            CMON='Nov'
        CASE(12)
            CMON='Dec'
      END SELECT
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE CMONTH

!
      SUBROUTINE splat4(IDRT,JMAX,ASLAT)
!
      implicit none
      integer,intent(in) :: idrt,jmax
      real(4),intent(out) :: ASLAT(JMAX)
      INTEGER,PARAMETER:: KD=SELECTED_REAL_KIND(15,45)
      REAL(KIND=KD):: PK(JMAX/2),PKM1(JMAX/2),PKM2(JMAX/2)
      REAL(KIND=KD):: ASLATD(JMAX/2),SP,SPMAX,EPS=10.d0*EPSILON(SP)
      integer,PARAMETER:: JZ=50
      REAL(8) BZ(JZ)
      DATA BZ        / 2.4048255577d0,  5.5200781103d0, &
       8.6537279129d0, 11.7915344391d0, 14.9309177086d0, 18.0710639679d0, &
      21.2116366299d0, 24.3524715308d0, 27.4934791320d0, 30.6346064684d0, &
      33.7758202136d0, 36.9170983537d0, 40.0584257646d0, 43.1997917132d0, &
      46.3411883717d0, 49.4826098974d0, 52.6240518411d0, 55.7655107550d0, &
      58.9069839261d0, 62.0484691902d0, 65.1899648002d0, 68.3314693299d0, &
      71.4729816036d0, 74.6145006437d0, 77.7560256304d0, 80.8975558711d0, &
      84.0390907769d0, 87.1806298436d0, 90.3221726372d0, 93.4637187819d0, &
      96.6052679510d0, 99.7468198587d0, 102.888374254d0, 106.029930916d0, &
      109.171489649d0, 112.313050280d0, 115.454612653d0, 118.596176630d0, &
      121.737742088d0, 124.879308913d0, 128.020877005d0, 131.162446275d0, &
      134.304016638d0, 137.445588020d0, 140.587160352d0, 143.728733573d0, &
      146.870307625d0, 150.011882457d0, 153.153458019d0, 156.295034268d0 /
      REAL(8):: DLT,D1=1.d0
      INTEGER(4):: JHE,JHO,J0=0
      real(8),PARAMETER :: PI=3.14159265358979d0,C=(1.d0-(2.d0/PI)**2)*0.25d0
      real(8) r
      integer jh,js,n,j
!C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!C  GAUSSIAN LATITUDES
      IF(IDRT.EQ.4) THEN
        JH=JMAX/2
        JHE=(JMAX+1)/2
        R=1.d0/SQRT((JMAX+0.5d0)**2+C)
        DO J=1,MIN(JH,JZ)
          ASLATD(J)=COS(BZ(J)*R)
        ENDDO
        DO J=JZ+1,JH
          ASLATD(J)=COS((BZ(JZ)+(J-JZ)*PI)*R)
        ENDDO
        SPMAX=1.d0
        DO WHILE(SPMAX.GT.EPS)
          SPMAX=0.d0
          DO J=1,JH
            PKM1(J)=1.d0
            PK(J)=ASLATD(J)
          ENDDO
          DO N=2,JMAX
            DO J=1,JH
              PKM2(J)=PKM1(J)
              PKM1(J)=PK(J)
              PK(J)=((2*N-1)*ASLATD(J)*PKM1(J)-(N-1)*PKM2(J))/N
            ENDDO
          ENDDO
          DO J=1,JH
            SP=PK(J)*(1.d0-ASLATD(J)**2)/(JMAX*(PKM1(J)-ASLATD(J)*PK(J)))
            ASLATD(J)=ASLATD(J)-SP
            SPMAX=MAX(SPMAX,ABS(SP))
          ENDDO
        ENDDO
!CDIR$ IVDEP
        DO J=1,JH
          ASLAT(J)=ASLATD(J)
          ASLAT(JMAX+1-J)=-ASLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          ASLAT(JHE)=0.d0
        ENDIF
!C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!C  EQUALLY-SPACED LATITUDES INCLUDING POLES
      ELSEIF(IDRT.EQ.0) THEN
        JH=JMAX/2
        JHE=(JMAX+1)/2
        JHO=JHE-1
        DLT=PI/(JMAX-1)
        ASLAT(1)=1.d0
        DO J=2,JH
          ASLAT(J)=COS((J-1)*DLT)
        ENDDO
!CDIR$ IVDEP
        DO J=1,JH
          ASLAT(JMAX+1-J)=-ASLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          ASLAT(JHE)=0.d0
        ENDIF
!C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!C  EQUALLY-SPACED LATITUDES EXCLUDING POLES
      ELSEIF(IDRT.EQ.256) THEN
        JH=JMAX/2
        JHE=(JMAX+1)/2
        JHO=JHE
        DLT=PI/JMAX
        ASLAT(1)=1.d0
        DO J=1,JH
          ASLAT(J)=COS((J-0.5)*DLT)
        ENDDO
!CDIR$ IVDEP
        DO J=1,JH
          ASLAT(JMAX+1-J)=-ASLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          ASLAT(JHE)=0.d0
        ENDIF
      ENDIF
!C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     end subroutine splat4

 character(35) function sweep_blanks(in_str)
!
   implicit none
!
   character(*), intent(in) :: in_str
   character(35) :: out_str
   character :: ch
   integer :: j

   out_str = " "
   do j=1, len_trim(in_str)
     ! get j-th char
     ch = in_str(j:j)
     if (ch .eq. "-") then
       out_str = trim(out_str) // "_"
     else if (ch .ne. " ") then
       out_str = trim(out_str) // ch
     endif
     sweep_blanks = out_str
   end do
 end function sweep_blanks

character(100) Function to_upper (str)

!   ==============================
!   Changes a string to upper case
!   ==============================

    Implicit None
    Character(*), Intent(In) :: str
    Character(LEN(str))      :: string

    Integer :: ic, i

    Character(26), Parameter :: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    Character(26), Parameter :: low = 'abcdefghijklmnopqrstuvwxyz'

!   Capitalize each letter if it is lowecase
    to_upper = str
    do i = 1, LEN_TRIM(str)
        ic = INDEX(low, to_upper(i:i))
        if (ic > 0) to_upper(i:i) = cap(ic:ic)
    end do
    
End Function to_upper

