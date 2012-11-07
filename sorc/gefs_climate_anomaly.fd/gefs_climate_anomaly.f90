!
!  Main program    gefs_climate_anomaly
!  Prgmmr: Yuejian Zhu           Org: np23                  Date: 2005-10-10
!          Bo Cui                Converted from f77 to f90        2006-12-10
!
! This is main program to generate climate anomaly forecasts.             
!
!   subroutine                                                    
!              IADDATE---> to add forecast hours to initial data    
!              GETGB  ---> to get GRIB format data                  
!              GRANGE ---> to calculate max. and min value of array
!
!   parameters:
!      ix    -- x-dimensional
!      iy    -- y-dimensional
!      ixy   -- ix*iy
!      iv    -- 19 variables
!   note:
!      if ibias = 1, no bias information available
!
!   Fortran 90 on IBMSP 
!
!--------+---------+---------+---------+---------+----------+---------+--

program ANOMALY

implicit none

real cdfnor

integer   iv,ixy,irwght,iret,index,j,ndata,icnt,jj,kf,k,idate,jdate
integer   ij,i,ii,kpds9,kpds10,kpds11,members,lwght,kdate,jpds9,jpds10,jpds11
integer   ibias,lfcst,lmean,lstdv,lbias,lanom,iranom,irfcst,irmean,irstdv,irbias 
real      dmin,dmax

parameter (iv=19)

real,      allocatable :: fcst(:),cavg(:),stdv(:),bias(:),anom(:)
logical(1),allocatable :: lb(:)

integer ipds(200),igds(200),iens(5)
integer jpds(200),jgds(200),jens(5)
integer kpds(200),kgds(200),kens(5)
integer ifld(iv),ityp(iv),ilev(iv)

real    fmon(2),opara(2)

character*80 cfcst,cmean,cstdv,cbias,canom
namelist /namin/ cfcst,cmean,cstdv,cbias,canom,ibias
data ifld/   7,   7,   7,   7,  11,  11,  11,  33,  34,  33,  34, 33,  34,   2,  11,  15,  16,  33,  34/
data ityp/ 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 102, 105, 105, 105, 105, 105/
data ilev/1000, 700, 500, 250, 850, 500, 250, 850, 850, 500, 500, 250, 250,   0,   2,   2,   2,  10,  10/

read (5,namin,end=100)
write(6,namin)

100  continue

lfcst = len_trim(cfcst)
lmean = len_trim(cmean)
lstdv = len_trim(cstdv)
lbias = len_trim(cbias)
lanom = len_trim(canom)

print *, 'Forecast      file is ',cfcst(1:lfcst)
print *, 'Climate  mean file is ',cmean(1:lmean)
print *, 'Climate  stdv file is ',cstdv(1:lstdv)
print *, 'Analysis bias file is ',cbias(1:lbias)
print *, 'Anomaly outpu file is ',canom(1:lanom)
print *, '    '
call baopen (81,canom(1:lanom),iranom)

! get the dimension of the variables
! index=0, to get index buffer from the grib file not the grib index file
! j=0, to search from beginning,  <0 to read index buffer and skip -1-j messages

index=0
j=0
iret=0
jpds=-1
jgds=-1
jens=-1

! find grib message. input: jpds,jgds and jens.  output: kpds,kgds,kens

call baopenr(11,cfcst(1:lfcst),irfcst)
call getgbeh(11,index,j,jpds,jgds,jens,ndata,ixy,j,kpds,kgds,kens,iret)
if (iret .ne. 0) then; print*,' getgbeh ,fort,index,iret =',11,index,iret; endif
if (iret .ne. 0) goto 882

allocate (fcst(ixy),cavg(ixy),stdv(ixy),bias(ixy),anom(ixy),lb(ixy))

icnt = 0
do ii = 1, iv
  call baopenr(11,cfcst(1:lfcst),irfcst)
  call baopenr(12,cmean(1:lmean),irmean)
  call baopenr(13,cstdv(1:lstdv),irstdv)
  call baopenr(14,cbias(1:lbias),irbias)
  if (irfcst.ne.0) goto 882
  if (irmean.ne.0) goto 882
  if (irstdv.ne.0) goto 882
  if (ibias.ne.1) then
    if (irbias.ne.0) goto 882
  endif

  ! get forecast

  jj      = 0
  jpds    = -1
  jgds    = -1
  jens    = -1
  jpds(5) = ifld(ii)
  jpds(6) = ityp(ii)
  jpds(7) = ilev(ii)
  call getgbe(11,0,ixy,jj,jpds,jgds,jens,kf,k,kpds,kgds,kens,lb,fcst,iret)
  if(iret.eq.0) then
    ipds = kpds
    igds = kgds
    iens = kens
    call grange(kf,lb,fcst,dmin,dmax)
    if (icnt.eq.0) then
      icnt=1
    endif
    print *, 'Forecast      file is ',cfcst(1:lfcst)
    write(*,886)
    write(*,888) k,(kpds(i),i=5,11),kpds(14),kf,dmax,dmin
    print *,'pds14=',kpds(14),' pds15=',kpds(15),'pds16=',kpds(16)
    print *, '   '
  else if (iret.eq.99) then
    goto 881
  else
    goto 991
  endif

  idate=ipds(8)*1000000 + ipds(9)*10000 + ipds(10)*100 + ipds(11)
  idate=idate + 2000000000
  if (ipds(16).eq.2) then
    call iaddate(idate,ipds(15),jdate)
  else
    call iaddate(idate,ipds(14),jdate)
  endif
  jpds9  = mod(jdate/10000,  100)
  jpds10 = mod(jdate/100,    100)
  jpds11 = mod(jdate,        100)
  call iaddate(jdate,-6,kdate)
  kpds9  = mod(kdate/10000,  100)
  kpds10 = mod(kdate/100,    100)
  kpds11 = mod(kdate,        100)

  ! get climate mean

  jj      = 0
  jpds    = -1
  jgds    = -1
  jens    = -1
  jpds(5) = ipds(5)
  jpds(6) = ipds(6)
  jpds(7) = ipds(7)
  jpds(8) = 59
  if (ii.le.14) then
    jpds(9) = jpds9  
    jpds(10)= jpds10 
    jpds(11)= jpds11 
  else
    jpds(9) = kpds9  
    jpds(10)= kpds10 
    jpds(11)= kpds11 
  endif

  call getgbe(12,0,ixy,jj,jpds,jgds,jens,kf,k,kpds,kgds,kens,lb,cavg,iret)
  if(iret.eq.0) then
    call grange(kf,lb,cavg,dmin,dmax)
    if (icnt.eq.0) then
      write(*,886)
      icnt=1
    endif
    print *, 'Climate  mean file is ',cmean(1:lmean)
    write(*,886)
    write(*,888) k,(kpds(i),i=5,11),kpds(14),kf,dmax,dmin
    print *, '  '
  else if (iret.eq.99) then
    goto 881
  else
    goto 991
  endif

  ! get climate standard deviation

  jj      = 0
  jpds    = -1
  jgds    = -1
  jens    = -1
  jpds(5) = ipds(5)
  jpds(6) = ipds(6)
  jpds(7) = ipds(7)
  jpds(8) = 59      
  if (ii.le.14) then
    jpds(9) = jpds9  
    jpds(10)= jpds10 
    jpds(11)= jpds11 
  else
    jpds(9) = kpds9  
    jpds(10)= kpds10 
    jpds(11)= kpds11 
  endif

  call getgbe(13,0,ixy,jj,jpds,jgds,jens,kf,k,kpds,kgds,kens,lb,stdv,iret)
  if(iret.eq.0) then
    call grange(kf,lb,stdv,dmin,dmax)
    if (icnt.eq.0) then
      write(*,886)
      icnt=1
    endif
    print *, 'Climate  stdv file is ',cstdv(1:lstdv)
    write(*,886)
    write(*,888) k,(kpds(i),i=5,11),kpds(14),kf,dmax,dmin
    print *, '   '
  else if (iret.eq.99) then
    goto 881
  else
    goto 991
  endif

  ! get bias (diff. between analysis and cdas)

  if (ibias.ne.1) then
    jj      = 0
    jpds    = -1
    jgds    = -1
    jpds(5) = ipds(5)
    jpds(6) = ipds(6)
    jpds(7) = ipds(7)
    jpds(8) = -1        
    jpds(9) = -1        
    jpds(10)= -1        
    jpds(11)= -1        
    jpds(14)= -1        
    !  jpds(8) = ipds(8)
    !  jpds(9) = ipds(9)
    !  jpds(10)= ipds(10)
    !  jpds(11)= ipds(11)
    !  jpds(14)= ipds(14)

    call getgb(14,0,ixy,jj,jpds,jgds,kf,k,kpds,kgds,lb,bias,iret)
    if(iret.eq.0) then
      call grange(kf,lb,bias,dmin,dmax)
      if (icnt.eq.0) then
        write(*,886)
        icnt=1
      endif
      print *, 'Analysis bias file is ',cbias(1:lbias)
      write(*,886)
      write(*,888) k,(kpds(i),i=5,11),kpds(14),kf,dmax,dmin
      print *, '   '
    else if (iret.eq.99) then
      goto 881
    else
      goto 991
    endif
  else
    bias=0.0
  endif

  ! to calculate anomaly forecast

  print *, 'Anomaly outpu file is ',canom(1:lanom)

  do ij = 1, ixy

    ! notes to use climate bias
    ! if bias = gdas - cdas, then fmon = cavg + bias
    ! if bias = cdas - gdas, then fmon = cavg - bias

    fmon(1) = cavg(ij) - bias(ij)
    fmon(2) = stdv(ij)

    ! fmon(1) = cavg(ij)
    ! protect when stdv = 0.0
    ! cdfnor accept two parameters directly (mean and std deviation)

    if (fmon(2).eq.0.0) fmon(2) = 0.01
      opara(1) = fmon(1)
      opara(2) = fmon(2)
      anom(ij)=cdfnor(fcst(ij),opara)*100.0
      if (ij.ge.10001.and.ij.le.10020) then
        write (*,883) ij,fmon(1),fmon(2),bias(ij),fcst(ij),anom(ij)
        ! print *, 'ij=',ij,' m1=',fmon(1),' m2=',fmon(2),' fc=',fcst(ij),' an=',anom(ij)
      endif
  enddo
  iens(4)=6
  call putgbe(81,ixy,ipds,igds,iens,lb,anom,iret)

  print *, '    '

  call baclose(11,iret)
  call baclose(12,iret)
  call baclose(13,iret)
  call baclose(14,iret)

enddo

call baclose(81,iret)

881 continue
991 continue
883 format('ij=',i5,'  m1=',e10.4,'  m2=',e10.4, ' bs=',e10.4,'  fc=',e10.4,'  an=',e10.4)
886 format('  Irec  pds5 pds6 pds7 pds8 pds9 pd10 pd11 pd14','  ndata  Maximun  Minimum')
888 format (i4,2x,8i5,i8,2f9.2)

stop   

882 print *, 'Missing input file, please check! stop!!!'

stop
end

SUBROUTINE IADDATE(IDATE,IHOUR,JDATE)

IMPLICIT NONE

INTEGER   MON(12),IC,IY,IM,ID,IHR,IDATE,JDATE,IHOUR
DATA MON/31,28,31,30,31,30,31,31,30,31,30,31/

IC = MOD(IDATE/100000000,100 )
IY = MOD(IDATE/1000000,100 )
IM = MOD(IDATE/10000  ,100 )
ID = MOD(IDATE/100    ,100 )
IHR= MOD(IDATE        ,100 ) + IHOUR

IF(MOD(IY,4).EQ.0) MON(2) = 29
1 IF(IHR.LT.0) THEN
    IHR = IHR+24
    ID = ID-1
    IF(ID.EQ.0) THEN
      IM = IM-1
      IF(IM.EQ.0) THEN
        IM = 12
        IY = IY-1
        IF(IY.LT.0) IY = 99
      ENDIF
      ID = MON(IM)
    ENDIF
    GOTO 1
  ELSEIF(IHR.GE.24) THEN
    IHR = IHR-24
    ID = ID+1
    IF(ID.GT.MON(IM)) THEN
      ID = 1
      IM = IM+1
        IF(IM.GT.12) THEN
          IM = 1
        IY = MOD(IY+1,100)
        ENDIF
     ENDIF
     GOTO 1
  ENDIF

JDATE = IC*100000000 + IY*1000000 + IM*10000 + ID*100 + IHR
RETURN
END

subroutine grange(n,ld,d,dmin,dmax)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: GRANGE(N,LD,D,DMIN,DMAX)
!   PRGMMR: YUEJIAN ZHU       ORG:NP23          DATE: 97-03-17
!
! ABSTRACT: THIS SUBROUTINE WILL ALCULATE THE MAXIMUM AND
!           MINIMUM OF A ARRAY
!
! PROGRAM HISTORY LOG:
!   97-03-17   YUEJIAN ZHU (WD20YZ)
!
! USAGE:
!
!   INPUT ARGUMENTS:
!     N        -- INTEGER
!     LD(N)    -- LOGICAL OF DIMENSION N
!     D(N)     -- REAL ARRAY OF DIMENSION N
!
!   OUTPUT ARGUMENTS:
!     DMIN     -- REAL NUMBER ( MINIMUM )
!     DMAX     -- REAL NUMBER ( MAXIMUM )
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN
!
!$$$

implicit none

logical(1) ld(n)
real d(n)
real dmin,dmax
integer i,n
dmin=1.e40
dmax=-1.e40
do i=1,n
  if(ld(i)) then
    dmin=min(dmin,d(i))
    dmax=max(dmax,d(i))
  endif
enddo
return
end
