      program decay_30d
C
C ABSTRACT: TO CALCULATE HISTORICAL STATISTICS BY USING 30-DAY
C           DECAYING FUNCTION
C           THIS PROGRAM COULD BE USED AS M ( NOT ONLY 30-DAY ) DAYS
C           DECAYING CALCULATION
C
C PROGRAM HISTORY LOG:
C   02-02-15  YUEJIAN ZHU 
C
      parameter (ncat=9,nreg=10,nfhr=15,nday=1)
      integer   hit(ncat,nreg+1,nfhr,nday)
      integer   obs(ncat,nreg+1,nfhr,nday)
      integer   fcs(ncat,nreg+1,nfhr,nday)
      integer   tot(ncat,nreg+1,nfhr,nday)
      dimension thit(ncat,nreg+1,nfhr)
      dimension tobs(ncat,nreg+1,nfhr)
      dimension tfcs(ncat,nreg+1,nfhr)
      dimension ttot(ncat,nreg+1,nfhr)
      dimension dets(ncat,nreg+1,nfhr,nday)
      dimension dbis(ncat,nreg+1,nfhr,nday)
      dimension ets(ncat,nreg+1,nfhr)
      dimension bis(ncat,nreg+1,nfhr)
      dimension ifile(2),index(nfhr)
      character*80 cfile(2),ofile(1)
      namelist/namin/ cfile,ifile,ofile,iymd,idday
      data iunit/11/,junit/12/,kunit/51/
ccc
      thit = 0.0
      tobs = 0.0
      tfcs = 0.0
      ttot = 0.0
      dets = 0.0
      dbis = 0.0
      ets  = 0.0
      bis  = 0.0
      index= 0
      obs  = -9999.99
      fcs  = -9999.99
      hit  = -9999.99
      tot  = -9999.99
ccc
      read  (5,namin,end=1000)
 1000 continue
      write (6,namin)
      dday=float(idday)
      ddaym1=float(idday-1)
ccc

      ii = 1

      if (ifile(ii).eq.1) then
       open(unit=iunit,file=cfile(ii),form='FORMATTED',
     1      status='OLD')
       do jj = 1, nfhr
        jfhr=jj*24+12
        read (iunit,902,end=101) ifhr
        if (ifhr.eq.jfhr) then
         index(jj) = 1
         backspace (iunit)
         read (iunit,900)
         read (iunit,901) obs(1,nreg+1,jj,ii),fcs(1,nreg+1,jj,ii),
     .                    hit(1,nreg+1,jj,ii),tot(1,nreg+1,jj,ii),
     .                    obs(2,nreg+1,jj,ii),fcs(2,nreg+1,jj,ii),
     .                    hit(2,nreg+1,jj,ii),tot(2,nreg+1,jj,ii),
     .                    obs(3,nreg+1,jj,ii),fcs(3,nreg+1,jj,ii),
     .                    hit(3,nreg+1,jj,ii),tot(3,nreg+1,jj,ii) 
         read (iunit,901) obs(4,nreg+1,jj,ii),fcs(4,nreg+1,jj,ii),
     .                    hit(4,nreg+1,jj,ii),tot(4,nreg+1,jj,ii),
     .                    obs(5,nreg+1,jj,ii),fcs(5,nreg+1,jj,ii),
     .                    hit(5,nreg+1,jj,ii),tot(5,nreg+1,jj,ii),
     .                    obs(6,nreg+1,jj,ii),fcs(6,nreg+1,jj,ii),
     .                    hit(6,nreg+1,jj,ii),tot(6,nreg+1,jj,ii) 
         read (iunit,911) obs(7,nreg+1,jj,ii),fcs(7,nreg+1,jj,ii),
     .                    hit(7,nreg+1,jj,ii),tot(7,nreg+1,jj,ii),
     .                    obs(8,nreg+1,jj,ii),fcs(8,nreg+1,jj,ii),
     .                    hit(8,nreg+1,jj,ii),tot(8,nreg+1,jj,ii)
         do kk = 1, nreg
          read (iunit,900)      
          read (iunit,901) obs(1,kk,jj,ii),fcs(1,kk,jj,ii),
     .                     hit(1,kk,jj,ii),tot(1,kk,jj,ii),
     .                     obs(2,kk,jj,ii),fcs(2,kk,jj,ii),
     .                     hit(2,kk,jj,ii),tot(2,kk,jj,ii),
     .                     obs(3,kk,jj,ii),fcs(3,kk,jj,ii),
     .                     hit(3,kk,jj,ii),tot(3,kk,jj,ii) 
          read (iunit,901) obs(4,kk,jj,ii),fcs(4,kk,jj,ii),
     .                     hit(4,kk,jj,ii),tot(4,kk,jj,ii),
     .                     obs(5,kk,jj,ii),fcs(5,kk,jj,ii),
     .                     hit(5,kk,jj,ii),tot(5,kk,jj,ii),
     .                     obs(6,kk,jj,ii),fcs(6,kk,jj,ii),
     .                     hit(6,kk,jj,ii),tot(6,kk,jj,ii) 
          read (iunit,901) obs(7,kk,jj,ii),fcs(7,kk,jj,ii),
     .                     hit(7,kk,jj,ii),tot(7,kk,jj,ii),
     .                     obs(8,kk,jj,ii),fcs(8,kk,jj,ii),
     .                     hit(8,kk,jj,ii),tot(8,kk,jj,ii),
     .                     obs(9,kk,jj,ii),fcs(9,kk,jj,ii),
     .                     hit(9,kk,jj,ii),tot(9,kk,jj,ii) 
         enddo
        else
         backspace(iunit)
        endif
 101   continue
       enddo
       close(iunit)
      else
       write (*,'("NO INPUT STAT FOR NEW STAT,QUIT!!!")')
       goto 9000
      endif
C--------+---------+---------+---------+---------+---------+---------+---------+
      ii = 2

      if (ifile(ii).eq.1) then
       open(unit=junit,file=cfile(ii),form='FORMATTED',
     1      status='OLD')
       do jj = 1, nfhr
        jfhr=jj*24+12
        read(junit,900)

C--------+---------+---------+---------+---------+----------+---------+---------+
        read(junit,900)
        read(junit,803) (tfcs(ijk,nreg+1,jj),ijk=1,ncat-1)
        read(junit,804) (tobs(ijk,nreg+1,jj),ijk=1,ncat-1)
        read(junit,808) (thit(ijk,nreg+1,jj),ijk=1,ncat-1)
        read(junit,809) (ttot(ijk,nreg+1,jj),ijk=1,ncat-1)
        read(junit,900)
        read(junit,803) (tfcs(ijk,1,jj),ijk=1,ncat)
        read(junit,804) (tobs(ijk,1,jj),ijk=1,ncat)
        read(junit,808) (thit(ijk,1,jj),ijk=1,ncat)
        read(junit,809) (ttot(ijk,1,jj),ijk=1,ncat)

       enddo
       close(junit)

       do ijk = 1, ncat-1
        do jj = 1, nfhr
         if (index(jj).eq.1) then
         ii = 1
         kk = nreg+1
         tfcs(ijk,kk,jj)=fcs(ijk,kk,jj,ii)+tfcs(ijk,kk,jj)*ddaym1/dday
         tobs(ijk,kk,jj)=obs(ijk,kk,jj,ii)+tobs(ijk,kk,jj)*ddaym1/dday
         thit(ijk,kk,jj)=hit(ijk,kk,jj,ii)+thit(ijk,kk,jj)*ddaym1/dday
         ttot(ijk,kk,jj)=tot(ijk,kk,jj,ii)+ttot(ijk,kk,jj)*ddaym1/dday
         endif
        enddo
       enddo
       do ijk = 1, ncat
        do jj = 1, nfhr
         if (index(jj).eq.1) then
         ii = 1
         kk = 1
         tfcs(ijk,kk,jj)=fcs(ijk,kk,jj,ii)+tfcs(ijk,kk,jj)*ddaym1/dday
         tobs(ijk,kk,jj)=obs(ijk,kk,jj,ii)+tobs(ijk,kk,jj)*ddaym1/dday
         thit(ijk,kk,jj)=hit(ijk,kk,jj,ii)+thit(ijk,kk,jj)*ddaym1/dday
         ttot(ijk,kk,jj)=tot(ijk,kk,jj,ii)+ttot(ijk,kk,jj)*ddaym1/dday
         endif
        enddo
       enddo
      endif

C
C SAFETY CHECK:
C
      do ijk = 1, ncat
       do jj = 1, nfhr
        if (ttot(ijk,1,jj).gt.500000) then
         ttot(ijk,1,jj)=307000.0
         write(6,*) "PLEASE CHECKING READ IN AND WRITE OUT FILE"
         write(6,*) "PROBLEM!!! PROBLEM!!! PROBLEM!!! *********"
        endif
       enddo
      enddo

      open(unit=kunit,file=ofile(1),form='FORMATTED',
     1     status='NEW')
      do jj = 1, nfhr
       write(*,'(2x)')
       write(*,907) jj*24-12,jj*24+12
       write(kunit,907) jj*24-12,jj*24+12
C--------+---------+---------+---------+---------+----------+---------+---------+
       write(*,905) 
       write(kunit,905) 
       write(*,903)    (tfcs(ijk,nreg+1,jj),ijk=1,ncat-1)
       write(*,904)    (tobs(ijk,nreg+1,jj),ijk=1,ncat-1)
       write(kunit,903)(tfcs(ijk,nreg+1,jj),ijk=1,ncat-1)
       write(kunit,904)(tobs(ijk,nreg+1,jj),ijk=1,ncat-1)
       write(kunit,908)(thit(ijk,nreg+1,jj),ijk=1,ncat-1)
       write(kunit,909)(ttot(ijk,nreg+1,jj),ijk=1,ncat-1)
       write(*,906) 
       write(kunit,906) 
       write(*,903)     (tfcs(ijk,1,jj),ijk=1,ncat)
       write(*,904)     (tobs(ijk,1,jj),ijk=1,ncat)
       write(kunit,903) (tfcs(ijk,1,jj),ijk=1,ncat)
       write(kunit,904) (tobs(ijk,1,jj),ijk=1,ncat)
       write(kunit,908) (thit(ijk,1,jj),ijk=1,ncat)
       write(kunit,909) (ttot(ijk,1,jj),ijk=1,ncat)
      enddo

      if (ifile(2).eq.1) then
       write(kunit,910)
       write(kunit,912) iymd
      else
       write(kunit,910)
       write(kunit,913) iymd
      endif
      close(kunit)
c
 803  format (5x,9(f7.0))
 804  format (5x,9(f7.0))
 808  format (5x,9(f7.0))
 809  format (5x,9(f7.0))
 900  format (1x)
 901  format (6x,4(i5),6x,4(i5),6x,4(i5))         
 911  format (6x,4(i5),6x,4(i5))         
 902  format (32x,i3)                             
 903  format ('FST: ',9(f7.0))
 904  format ('OBS: ',9(f7.0))
 908  format ('HIT: ',9(f7.0))
 909  format ('TOT: ',9(f7.0))
 910  format ('--------------------------------------------------')
 912  format (i8,' has     been added ')
 913  format (i8,' has not been added ')
 905  format ('THOLD  >.01   >0.1   >.25   >.50   >.75 ',
     *             '  >1.0   >1.5   >2.0 (inch/day)')
 906  format ('THOLD  >0.2   >2.0   >5.0   >10.   >15. ',
     *             '  >25.   >35.   >50.   >75.  (mm/day)')
 907  format (10x,'Leading Forecats ',i3,'-',i3,' hours')
      STOP
 1030 print *, " there is a problem to open unit kunit"
 9000 STOP
      END
