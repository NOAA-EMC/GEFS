      program global_sigzvd
c  this program checks and, optionally, zeroes out the 0,0
c  coeffiecents of vorticity and divergence in a gfs sigma
c  file
c program history log:
c   06-03-30  Wobus
c         
      use sigio_module
      implicit none
      type(sigio_head)::  headi
      type(sigio_head)::  heado
      type(sigio_data)::  datai
      type(sigio_data)::  datao
      character*8  cfnamei
      character*8  cfnameo
      character*8  cfnamel
      character*255 label
      integer   lui
      integer   luo
      integer   lul
      integer   iretn
      integer   iretp
      integer   ireti
      integer   ireto
      integer   ilev
      integer   levs
      integer   ic
      integer   nc
      integer   llabel
      integer   i
      real dp
      real dz
      real vp
      real vz
      real divmax
      real divavgmax
      real divrat
      real divratmin
      real vormax
      real voravgmax
      real vorrat
      real vorratmin
      character*1, allocatable:: labelt(:)

      label=' '
      lul=12
      cfnamel="sig_zvdl"
      open(unit=lul,file=cfnamel,err=100,iostat=iretp)
 100  continue
      if ( iretp .ne. 0 ) then 
	print *,' iretp = ',iretp
      endif

      read(lul,'(A)',err=200,end=200,iostat=iretn) label
 200  continue
      if ( iretn .ne. 0 ) then 
	print *,' iretn = ',iretn
      endif

      llabel=len_trim(label)
      allocate (labelt(1:llabel))
      do i=1,llabel
	labelt(i)=label(i:i)
      enddo

      cfnamei="sig_zvdi"
      cfnameo="sig_zvdo"
      lui=11
      luo=61

      call sigio_srohdc(lui,cfnamei,headi,datai,ireti)
      if ( ireti .ne. 0 ) then
        print *,labelt,' ireti = ',ireti
	stop
      endif

      heado=headi
      datao=datai

      levs=heado%levs
      nc=(heado%jcap+1)*(heado%jcap+2)

      voravgmax=0.
      vorratmin=999999999.
      do ilev=levs,1,-1
	if ( datao%z(1,ilev) .ne. (0.0,0.0) ) then
	  vp=datao%z(1,ilev) 
	  vormax=0.0
	  do ic=2,nc
	    vormax=max(vormax,abs(datao%z(ic,ilev)))
	  enddo
	  vorrat=abs(vormax/datao%z(1,ilev))
	  voravgmax=max(voravgmax,abs(datao%z(1,ilev)))
	  vorratmin=min(vorratmin,vorrat)
	  print *,'vor lev mean max ratio ',labelt,ilev,
     *      vp,vormax,vorrat
	  datao%z(1,ilev)=(0.0,0.0)
	endif
      enddo
      if ( voravgmax .gt. 0.0 ) then
	print *,'vor max avg, min ratio ',labelt,voravgmax,vorratmin
      endif

      divavgmax=0.
      divratmin=999999999.
      do ilev=levs,1,-1
	if ( datao%d(1,ilev) .ne. (0.0,0.0) ) then
	  dp=datao%d(1,ilev) 
	  divmax=0.0
	  do ic=2,nc
	    divmax=max(divmax,abs(datao%z(ic,ilev)))
	  enddo
	  divrat=abs(divmax/datao%d(1,ilev))
	  divavgmax=max(divavgmax,abs(datao%d(1,ilev)))
	  divratmin=min(divratmin,divrat)
	  print *,'div lev mean max ratio ',labelt,ilev,
     *      dp,divmax,divrat
	  datao%d(1,ilev)=(0.0,0.0)
	endif
      enddo
      if ( divavgmax .gt. 0.0 ) then
	print *,'div max avg, min ratio ',labelt,divavgmax,divratmin
      endif

      call sigio_swohdc(luo,cfnameo,heado,datao,ireto)
      if ( ireto .ne. 0 ) then
        print *,labelt,' ireto = ',ireti
	stop
      endif

      end
