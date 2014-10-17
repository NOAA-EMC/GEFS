C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: ENSADD
C   PRGMMR: ZHU              ORG: NP23        DATE: 1999-08-31
C
C ABSTRACT: THIS PROGRAM WILL EXTEND PDS MESSAGE WHICH WILL  
C           INCLUDE ENS(5) MESSAGE
C
C PROGRAM HISTORY LOG:
C   96-10-??   MARK IREDELL     - Originator
C   97-03-17   YUEJIAN ZHU      - Added DOCBLOACK
C   99-07-26   YUEJIAN ZHU      - Modified to IBM-SP
C
C USAGE:
C
C   INPUT FILES:
C     UNIT  11  GRIB FILE ( maximum 512*256 )
C     UNIT  21  GRIB INDEX FILE
C     UNIT   5  READ *, IENST,IENSI (For ensemble message)
C
C   OUTPUT FILES:
C     UNIT  51  GRIB FILE ( maximum 512*256 )
C
C   SUBPROGRAMS CALLED:
C     GETGB  -- W3LIB ROUTINE
C     PUTGBE -- W3LIB ROUTINE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
      program      ensadd
      parameter    (jf=512*256)
      integer      jpds(200),jgds(200),kpds(200),kgds(200),kens(200)
      integer      ienst,iensi                                        
      real         f(jf)
      character*80 cpgb,cpgi,cpge
      logical*1      lb(jf)
      namelist     /namin/ ienst,iensi,cpgb,cpgi,cpge      
c
c
c     CALL W3LOG('$S97118.73','ENSADD')
      CALL W3TAGB('ENSADD',1999,0243,0068,'NP23')

      read (5,namin)
      lpgb=len_trim(cpgb)
      lpgi=len_trim(cpgi)
      lpge=len_trim(cpge)
      print *, cpgb(1:lpgb),'  ',cpgi(1:lpgi),'  ',cpge(1:lpge)
      call baopenr(11,cpgb(1:lpgb),iretb)
      call baopenr(21,cpgi(1:lpgi),ireti)
      call baopen (51,cpge(1:lpge),irete)

      j=0
      jpds=-1
      call getgb(11,21,jf,j,jpds,jgds,kf,j,kpds,kgds,lb,f,iret)
      dowhile(iret.eq.0)
        kpds(23)=2
        kens(1)=1
        kens(2)=ienst
        kens(3)=iensi
        kens(4)=1
        kens(5)=255
        if(kpds(6).eq.100.and.kpds(5).ne.222.and.kpds(5).ne.52)
     &  kens(5)=kgds(3)/2-1
        call putgbe(51,kf,kpds,kgds,kens,lb,f,iret)
        call getgb(11,21,jf,j,jpds,jgds,kf,j,kpds,kgds,lb,f,iret)
      enddo
 
      call baclose(11,iretb)
      call baclose(21,ireti)
      call baclose(51,irete)

c     CALL W3LOG('$E')
      CALL W3TAGE('ENSADD')

      stop
      end
