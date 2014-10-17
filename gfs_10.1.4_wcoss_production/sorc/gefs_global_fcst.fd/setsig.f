      subroutine setsig(si, ci, del, sl, cl,rdel2,tov,me)
      use machine , only : kind_phys,kind_rad
      use resol_def
      implicit none
!sela include 'constant.h'
      integer le,li,iprint,me
      real(kind=kind_evod) ci(levp1), si(levp1)
      real(kind=kind_evod) tov(levs)
      real(kind=kind_evod) del(levs), sl(levs), cl(levs),rdel2(levs)
      real(kind=kind_evod) cons1     !constant
      real(kind=kind_evod) cons0p5   !constant
      real(kind=kind_evod) cons300   !constant
 
      cons1 = 1.d0     !constant
      cons0p5  =    0.5d0     !constant
      cons300  = 300.d0       !constant
 
 
      if (me .eq. 0) print 98
98    format (1h0, 'begin setsig - called from main ' )
 
      do 1 li=1,levp1
      ci(li) = cons1 - si(li)      !constant
1     continue
 
      do 3 le=1,levs
      tov(le)=cons300
      cl(le) = cons1 - sl(le)      !constant
      del(le) = si(le) - si(le+1)
      rdel2(le) = cons0p5/del(le)
3     continue
 
      iprint=0
      if(iprint.ne.1) return
      do 5 le=1,levp1
      if(iprint.eq.1) print 100, le, ci(le), si(le)
100   format (1h , 'setsig level=',i2,2x,'ci=', f6.3,2x,'si=',f6.3)
5     continue
 
      if(iprint.eq.1) print 97
97    format (1h0)
      do 6 le=1,levs
      if(iprint.eq.1) print 101, le, cl(le), sl(le), del(le)
101   format (1h ,' setsig layer=',i2,2x,'cl=',f6.3,2x,'sl=',f6.3,2x,
     1 'del=', f6.3)
6     continue
      return
      end
