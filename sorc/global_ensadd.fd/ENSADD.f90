!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: ENSADD_G2 
!   PRGMMR: ZHU              ORG: NP23        DATE: 1999-08-31
!
! ABSTRACT: THIS PROGRAM WILL EXTEND PDS MESSAGE WHICH WILL  
!           INCLUDE ENS(5) MESSAGE
!
! PROGRAM HISTORY LOG:
!   96-10-??   MARK IREDELL     - Originator
!   97-03-17   YUEJIAN ZHU      - Added DOCBLOACK
!   99-07-26   YUEJIAN ZHU      - Modified to IBM-SP
!   14-10-06   BO Cui           - Modified to Decode/Incode GRIB2 Data
!
! USAGE:
!
!   INPUT FILES:
!     UNIT  11  GRIB FILE 
!     UNIT   5  READ *, IENST,IENSI (For ensemble message)
!
!   OUTPUT FILES:
!     UNIT  51  GRIB FILE 
!
!   SUBPROGRAMS CALLED:
!     GETGB2 -- W3LIB ROUTINE
!     PUTGB2 -- W3LIB ROUTINE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN
!
!$$$

program      ensadd_g2

use grib_mod
use params

implicit none

type(gribfield) :: gfldo

integer :: currlen=0
logical :: unpack=.true.
logical :: expand=.false.

integer,dimension(200) :: kids,kpdt,kgdt
integer kskp,kdisc,kpdtn,kgdtn,i

integer,dimension(200) :: jids,jpdt,jgdt,iids,ipdt,igdt
integer jskp,jdisc,jpdtn,jgdtn,idisc,ipdtn,igdtn

integer temp(200)

integer ienst,iensi,lpgb,lpgi,lpge,icount,iretb,ireti,irete,iret,jret,ipdtnum_out                                        
character*255 cpgb,cpge
namelist     /namin/ ienst,iensi,cpgb,cpge      

CALL W3TAGB('ENSADD',1999,0243,0068,'NP23')

read (5,namin)
lpgb=len_trim(cpgb)
lpge=len_trim(cpge)

print *, cpgb(1:lpgb),' ',cpge(1:lpge)
print *, ' '  

call baopenr(11,cpgb(1:lpgb),iretb)
call baopen (51,cpge(1:lpge),irete)

! loop over variables

kids=-9999;kpdt=-9999; kgdt=-9999
kdisc=-1;  kpdtn=-1;   kgdtn=-1

icount=0
kskp=0
do

  print *, '----- Read Ensemble Forecast ------'
  print *, ' '  

  call getgb2(11,0,kskp,kdisc,kids,kpdtn,kpdt,kgdtn,kgdt,unpack,kskp,gfldo,iret)

  if(iret.ne.0) then
    if(iret.eq.99 ) exit
    print *,' getgb2 error = ',iret
    cycle
    !call errexit(17)
  endif

  icount=icount+1

  if(iret.eq.0) then
    call printinfr(gfldo,icount)
  else
    print*, 'there is no fcst for ens member'
  endif

  ! gfldo%idsect(2): Identification of originating
  ! gfldo%idsect(2)=2: NCEP Ensemble Products
  ! Original GFS has gfldo%idsect(2)=0, change it to be 2

  gfldo%idsect(2)=2
  gfldo%idsect(13)=3

  ! when product difinition template 4.0 change to 4.1
  ! when product difinition template 4.8 change to 4.11
  ! ipdtlen aslo change, need do modification for output

! print*, '1 gfldo%ipdtmpl=',gfldo%ipdtmpl   

  if(gfldo%ipdtnum.eq.0) then

    temp=-9999

    temp(1:gfldo%ipdtlen)=gfldo%ipdtmpl(1:gfldo%ipdtlen)
    if(gfldo%ipdtmpl(3).eq.2) then
     temp(3)=4
    endif 

    deallocate (gfldo%ipdtmpl)

    gfldo%ipdtnum=1
    if(gfldo%ipdtnum.eq.1) gfldo%ipdtlen=18
    if(gfldo%ipdtnum.eq.1) allocate (gfldo%ipdtmpl(gfldo%ipdtlen))

    gfldo%ipdtmpl(1:15)=temp(1:15)
    gfldo%ipdtmpl(16)=ienst
    gfldo%ipdtmpl(17)=iensi
    gfldo%ipdtmpl(18)=10    

  endif

  if(gfldo%ipdtnum.eq.8) then

    temp=-9999

    temp(1:gfldo%ipdtlen)=gfldo%ipdtmpl(1:gfldo%ipdtlen)
    if(gfldo%ipdtmpl(3).eq.2) then
     temp(3)=4
    endif 

    deallocate (gfldo%ipdtmpl)

    gfldo%ipdtnum=11
    if(gfldo%ipdtnum.eq.11) gfldo%ipdtlen=32
    if(gfldo%ipdtnum.eq.11) allocate (gfldo%ipdtmpl(gfldo%ipdtlen))

    gfldo%ipdtmpl(1:15)=temp(1:15)
    gfldo%ipdtmpl(16)=ienst
    gfldo%ipdtmpl(17)=iensi
    gfldo%ipdtmpl(18)=10    
    gfldo%ipdtmpl(19:32)=temp(16:29)

  endif

! print*, '2 gfldo%ipdtmpl=',gfldo%ipdtmpl   

! print*, 'gfldo%ipdtnum=',gfldo%ipdtnum   
! print*, 'gfldo%ipdtlen=',gfldo%ipdtlen   

  print *, '----- Write Ensemble Forecast ------'
  print *, ' '  

  call printinfr(gfldo,icount)
  call putgb2(51,gfldo,jret)

  ! end of probability forecast calculation

  200 continue

  call gf_free(gfldo)

enddo

! end of ivar loop

! close files

call baclose(11,iretb)
call baclose(51,irete)

CALL W3TAGE('ENSADD')

stop
end

