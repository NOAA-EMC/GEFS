      program overenstr
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: OVERENSTR    REPLACE ensemble ID info IN A GRIB FILE
C   PRGMMR: IREDELL          ORG: NP23        DATE: 1998-01-01
C
C ABSTRACT: THIS PROGRAM READS AN ENTIRE ENSEMBLE GRIB1 FILE FROM UNIT 11
C   AND WRITES IT BACK OUT TO UNIT 51, REPLACING THE INTERNAL P1, P2
C   and TIME RANGE TO 10.    
C
C PROGRAM HISTORY LOG:
C   1998-01-01  IREDELL
C   1998-06-17  FARLEY   - MODIFIED OVERDATE ROUTINE
C   1999-05-24  Gilbert  - added calls to BAOPEN.
C   2015-08-07  Vuong    - CORRECT P1, P2 AND TIME RANGE
C
C INPUT FILES:
C   UNIT   11    INPUT GRIB FILE = "fort.11"
C
C OUTPUT FILES:
C   UNIT   51    OUTPUT GRIB FILE = "fort.51"
C
C SUBPROGRAMS CALLED:
C   SKGB     - Find next GRIB field 
C   BAREAD   - Read GRIB field
C   WRYTE    - Read GRIB field
C
C REMARKS:
C   ANY NON-GRIB INFORMATION IN THE INPUT GRIB FILE WILL BE LOST.
C   AN OUTPUT LINE WILL BE WRITTEN FOR EACH GRIB MESSAGE COPIED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      parameter(msk1=32000,msk2=4000,mgrib=999999)
      character cgrib(mgrib)
C
c     read *,id1   ! Type of ensemble (PDS octet 42)
c     read *,id2   ! Ensemble ID numer (PDS octet 43)
      call baopenr(11,"fort.11",iret1)
      call baopenw(51,"fort.51",iret2)
C
      n=0
      iseek=0
      call skgb(11,iseek,msk1,lskip,lgrib)
      dowhile(lgrib.gt.0.and.lgrib.le.mgrib)
        call baread(11,lskip,lgrib,ngrib,cgrib)
        if(ngrib.ne.lgrib) call exit(2)
        n=n+1
        id19 = mova2i(cgrib(8+19))
        id20 = mova2i(cgrib(8+20)) 
        id21 = mova2i(cgrib(8+21)) 
        if ( id19 .ne. 0 .and. id20 .eq. 0 .and. id21 .eq. 0) then
             cgrib(8+19) = char(0)
             cgrib(8+20) = char(id19)
             cgrib(8+21) = char(10)
             newid19 = mova2i(cgrib(8+19))
             newid20 = mova2i(cgrib(8+20))
             newid21 = mova2i(cgrib(8+21))
          print '("msg",i6,1x,"len",i8,2x," was  p1=",i3," p2=",i3,
     &       " TR=",i2,2x," now  p1=",i3," p2=",i3,
     &       " TR=",i2)',n,lgrib,id19,id20,id21,
     &       newid19,newid20,newid21

        end if
        call wryte(51,lgrib,cgrib)
        iseek=lskip+lgrib
        call skgb(11,iseek,msk2,lskip,lgrib)
      enddo
      end
