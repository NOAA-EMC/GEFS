      program CVT24H
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC   This program will convert input global ensemble precipitation CCCC
CCC   forecast to 24 hrs accumulation and ready for verification    CCCC
CCC   ( output gfs and ensemble ctl only )                          CCCC
CCC                                                                 CCCC
CCC    Notes:                                                       CCCC
CCC      for ensemble precipitation calibration using only          CCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C PROGRAM HISTORY LOG:
C   04-02-09  YUEJIAN ZHU 
C   06-02-06  YUEJIAN ZHU MODIFYINF FOR NEW CONFIGURATION 
C
C
C   Parameters:
C     1. jf--->    model resolution or total grid points ( default 10512 )
C     2. iem-->    numbers of ensember members
C     3. len-->    the length of 12 hours interval                 
C
C--------+---------+---------+---------+---------+---------+---------+--
      parameter(jf=10512,iem=16,len=34)

      dimension f(jf),ff(jf,iem),pp(jf,iem),fr(jf),aa(jf)
      dimension trf(jf),ctl(jf)
      dimension ipds(25),igds(22),iens(5)
      dimension jpds(25),jgds(22),jens(5)
      dimension kpds(25),kgds(22),kens(5)
      dimension kens2(iem),kens3(iem)
      dimension iprob(2),xprob(2),iclust(16),imembr(80)

      logical*1 lb(jf)
      character*80 cpgb,cpgi,pgb71,pgb72

      namelist /namin/ cpgb,cpgi,pgb71,pgb72

      data kens2/1,1,3,3,3,3,3,3,3,3,3, 3, 3, 3, 3, 3/
      data kens3/1,2,1,2,3,4,5,6,7,8,9,10,11,12,13,14/
C     data kens2/1,1,2,3,2,3,2,3,2,3,2,3/
C     data kens3/1,2,1,1,2,2,3,3,4,4,5,5/
 
      read (5,namin)
      write(6,namin)

      lpgb=len_trim(cpgb)
      lpgi=len_trim(cpgi)
      print *, cpgb(1:lpgb)
      print *, cpgi(1:lpgi)
      call baopenr(11,cpgb(1:lpgb),iretb)
      call baopenr(21,cpgi(1:lpgi),ireti)
 
      lg71=len_trim(pgb71)
      call baopen(71,pgb71(1:lg71),irt71)
      lg72=len_trim(pgb72)
      call baopen(72,pgb72(1:lg72),irt72)

      ncnt=0
c     do n=1,len  
      do n=2,len  
c      do m=1,iem
       do m=1,2  
c       j=n-1
        j=-1
        jpds=-1
        jgds=-1
        jens=-1
        jpds(23)=2
        jpds(11)=00
        jpds(14)=n-1
        jens(2)=kens2(m)
        jens(3)=kens3(m)
        call getgbe(11,21,jf,j,jpds,jgds,jens,
     &              kf,k,kpds,kgds,kens,lb,f,iret)
        if(iret.eq.0) then
         call grange(kf,lb,f,dmin,dmax)
c        print '(i4,i3,2i5,4i3,i4,4i2,i4,i7,2g12.4)',
c    &          n,(kpds(i),i=5,11),kpds(14),kens,kf,dmin,dmax
         print '(i4,i3,2i5,4i3,i4,i4,4i2,i4,i7,2d11.4)',
     &          n,(kpds(i),i=5,11),kpds(14),kpds(15),kens,kf,dmax,dmin
         do ii=1,jf    
          ff(ii,m)=f(ii)
         enddo
ccc
         do ii = 1, 25
          ipds(ii)=kpds(ii)
         enddo
         do ii = 1, 22
          igds(ii)=kgds(ii)
         enddo
         do ii = 1, 5 
          iens(ii)=kens(ii)
         enddo 
        else
         ncnt=ncnt+1
         if ( ncnt.le.1 ) then
          print *,' n=',n,' iret=',iret
         endif
        endif
       enddo
c
       if (n.ge.2.and.mod(n,2).eq.0) then

        ipds(5)  = 59         !: OCT 9
        ipds(13) = 1
        ipds(14) = (n-1)*12
        ipds(15) = (n+1)*12
        ipds(16) = 3          !: or 10
        ipds(22) = 6

        do ii = 1, jf
c        trf(ii) = (ff(ii,1)+pp(ii,1))/3600.00/24.00
c        ctl(ii) = (ff(ii,2)+pp(ii,2))/3600.00/24.00
         trf(ii) = ff(ii,1)/3600.00/24.00
         ctl(ii) = ff(ii,2)/3600.00/24.00
        enddo
        print *, '==== To write out the precip. of gfs run'
        call putgb(71,10512,ipds,igds,lb,trf,iret)
        print *, '==== To write out the precip. of ctl run'
        call putgb(72,10512,ipds,igds,lb,ctl,iret)
 
       endif          ! if (n.ge.4.and.mod(n,2).eq.0)

       do ii = 1, jf
        do k = 1, iem
         pp(ii,k) = ff(ii,k)
        enddo
       enddo
      enddo           ! for n loop

      call baclose(11,iretb)
      call baclose(21,ireti)
      call baclose(71,iret)
      call baclose(72,iret)
c
 666  format (10(1pe8.1))
      stop    
      end
c
      subroutine grange(n,ld,d,dmin,dmax)
      logical*1 ld
      dimension ld(n),d(n)
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
