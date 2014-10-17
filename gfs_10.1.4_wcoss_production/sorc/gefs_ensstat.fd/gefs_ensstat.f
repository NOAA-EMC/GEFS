C MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: ENSSTAT
C   PRGMMR: WOBUS            ORG: NP20        DATE: 2006-05-10
C
C ABSTRACT: THIS PROGRAM creates components of the mean and
C           spread members and of selected enspost and ensstat
C           files
C
C PROGRAM HISTORY LOG:
C   00-04-03   RICHARD WOBUS (WX20RW) NEW PROGRAM ON IBM-SP
C   00-04-21   RICHARD WOBUS (WX20RW) SET UP FOR VARIABLE NUMBERS
C                            OF 00Z AND/OR 12Z MEMBERS and add
c                            decaying averages in ensstat files
c   00-09-05   RICHARD WOBUS (WX20RW) move decaying averages to
c                            separate job step
c   01-03-14   RICHARD WOBUS (WX20RW) update diagnostic output
c   01-09-04   Richard Wobus (wx20rw) variable resolution, single cycle
c   01-11-09   Richard Wobus (wx20rw) add single-run output
c   03-09-10   Richard Wobus (wx20rw) add search for 6-hourly 
c   04-03-12   Richard Wobus (wx20rw) add interval-average
c   06-05-10   Richard Wobus (wx20rw) reorganize for pgrba files
c                                     and control by namelist
c   11-06-20   Richard Wobus (wx20rw) enable separate runs for
c                                     mean/spread and enspost
C
C USAGE:
C
C   INPUT FILES:
C     UNIT   5  NAMELIST
C     as assigned by namelist -- input grib and index files
C
C   OUTPUT FILES:
C     as assigned by namelist -- output grib and index files
C     as assigned by namelist -- output enspost files
C     as assigned by namelist -- output ensstat files
C
C   SUBPROGRAMS CALLED:
c     baopenr -- bacio routine
c     baopenwa-- bacio routine
c     baclose -- bacio routine
C     GETGBE  -- W3LIB ROUTINE
C     PUTGBEX -- W3LIB ROUTINE
C     PUTGBE  -- W3LIB ROUTINE
C     SRANGE  -- LOCAL ROUTINE ( included after main program )
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
      program ensstat                                  
      parameter(nmemd=100,nenspostd=100)
      allocatable f(:)
      allocatable ecnt(:)
      allocatable eatot(:)
      allocatable eavg(:)
      allocatable evtot(:)
      allocatable evar(:)
      allocatable estd(:)
      dimension xprob(2),imembr(80)
      dimension yprob(2),jmembr(80)
      dimension zprob(2),kmembr(80)
      dimension ipds(200),igds(200),iens(200),iprob(2),iclust(16)
      dimension jpds(200),jgds(200),jens(200),jprob(2),jclust(16)
      dimension kpds(200),kgds(200),kens(200),kprob(2),kclust(16)
      dimension lens(200)
      logical*1,allocatable::lb(:)
      logical fp

      dimension ivar(nenspostd)
      dimension ilvt(nenspostd)
      dimension ilev(nenspostd)
c     allocatable ivar(:)
c     allocatable ilvt(:)
c     allocatable ilev(:)

      character*120 cfipg(nmemd)
      character*120 cfipi(nmemd)
c     character*120,allocatable:: cfipg(:)
c     character*120,allocatable:: cfipi(:)
      character*120 cfoag
      character*120 cfosg
      character*120 cfopg(nenspostd)
      character*120 cfotg(nenspostd)
c     character*120,allocatable:: cfopg(:)
c     character*120,allocatable:: cfotg(:)

      dimension lfipg(nmemd)
      dimension lfipi(nmemd)
c     allocatable lfipg(:)
c     allocatable lfipi(:)
      dimension lfopg(nenspostd)
      dimension lfotg(nenspostd)
c     allocatable lfopg(:)
c     allocatable lfotg(:)

      dimension icfipg(nmemd)
      dimension icfipi(nmemd)
c     allocatable icfipg(:)
c     allocatable icfipi(:)
      dimension icfopg(nenspostd)
      dimension icfotg(nenspostd)
c     allocatable icfopg(:)
c     allocatable icfotg(:)

      dimension jpos(nmemd)
      dimension kpos(nmemd)
      dimension iskip(nmemd)
c     allocatable jpos(:)
c     allocatable kpos(:)
c     allocatable iskip(:)

      logical inindxf

      namelist /namdim/ nmemdim,nenspostdim,lfdim

      namelist /namens/ nfiles,iskip,cfipg,cfipi,
     ,     nenspost,ivar,ilev,ilvt,cfopg,cfotg,
     ,             cfoag,cfosg,inindxf

ccc
      CALL W3TAGB('ENSSTAT',2000,0243,0069,'NP21')

      read (5,namdim)
      write (6,namdim)

c     nmemd=nmemdim
c     nenspostd=nenspostdim
      lfm=lfdim

c     allocate(ivar(nenspostd))
c     allocate(ilvt(nenspostd))
c     allocate(ilev(nenspostd))
c     allocate(cfopg(nenspostd))
c     allocate(cfotg(nenspostd))
c     allocate(lfopg(nenspostd))
c     allocate(lfotg(nenspostd))
c     allocate(icfopg(nenspostd))
c     allocate(icfotg(nenspostd))

c     allocate(cfipg(nmemd))
c     allocate(cfipi(nmemd))
c     allocate(ifipg(nmemd))
c     allocate(ifipi(nmemd))
c     allocate(icfipg(nmemd))
c     allocate(icfipi(nmemd))
c     allocate(jpos(nmemd))
c     allocate(kpos(nmemd))
c     allocate(iskip(nmemd))

      allocate(f(lfm))
      allocate(ecnt(lfm))
      allocate(eatot(lfm))
      allocate(eavg(lfm))
      allocate(evtot(lfm))
      allocate(evar(lfm))
      allocate(estd(lfm))
      allocate(lb(lfm))

      inindxf=.true.
      read (5,namens)
      write (6,namens)

      lfipg=len_trim(cfipg)
      lfipi=len_trim(cfipi)
      lfoag=len_trim(cfoag)
      lfosg=len_trim(cfosg)
      lfotg=len_trim(cfopg)
      lfopg=len_trim(cfotg)

c open files

      iunit=9
      nskip=0
      do ifile=1,nfiles
	if ( iskip(ifile) .eq. 1 ) then
	  nskip=nskip+1
	endif

	iunit=iunit+1
	icfipg(ifile)=iunit
	print *, icfipg(ifile),cfipg(ifile)(1:lfipg(ifile))
	call baopenr(icfipg(ifile),cfipg(ifile)(1:lfipg(ifile)),iretipg)
	if ( iretipg .ne. 0 ) then
	  print *,'ifile,iretipg = ',ifile,iretipg
	endif

        if (inindxf) then
	  iunit=iunit+1
	  icfipi(ifile)=iunit
	  call baopenr(icfipi(ifile),cfipi(ifile)(1:lfipi(ifile)),iretipi)
	  if ( iretipi .ne. 0 ) then
	    print *,'ifile,iretipi = ',ifile,iretipi
	    icfipi(ifile)=0
	  endif
	else
	  icfipi(ifile)=0
	endif

	print *, icfipi(ifile),cfipi(ifile)(1:lfipi(ifile))

      enddo

      if ( iunit .lt. 50) then
	iunit=50
      endif

      if ( nfiles > (nskip+2) ) then

	iunit=iunit+1
	icfoag=iunit
	call baopenwa(icfoag,cfoag(1:lfoag),iretoag)
	if ( iretoag .ne. 0 ) then
	  print *,'iretoag = ',iretoag
	endif
	print *, icfoag,cfoag(1:lfoag)

	iunit=iunit+1
	icfosg=iunit
	call baopenwa(icfosg,cfosg(1:lfosg),iretosg)
	if ( iretosg .ne. 0 ) then
	  print *,'iretosg = ',iretosg
	endif
	print *, icfosg,cfosg(1:lfosg)

      endif

      do iep=1,nenspost
	iunit=iunit+1
	icfopg(iep)=iunit
	call baopenwa(icfopg(iep),cfopg(iep)(1:lfopg(iep)),iretopg)
	if ( iretopg .ne. 0 ) then
	  print *,'iep,iretopg = ',iep,iretopg
	endif
	print *, icfopg(iep),cfopg(iep)(1:lfopg(iep))
	iunit=iunit+1
	icfotg(iep)=iunit
	call baopenwa(icfotg(iep),cfotg(iep)(1:lfotg(iep)),iretotg)
	if ( iretotg .ne. 0 ) then
	  print *,'iep,iretotg = ',iep,iretotg
	endif
	print *, icfotg(iep),cfotg(iep)(1:lfotg(iep))
      enddo

c loop over variables
      jpos=0
      kpos=0

      lvar: do iv=1,100
        print *,'begin variable loop iv=',iv
	if ( 0 .eq. nfiles ) then
	  print *,' nfiles=',nfiles
	  print *,' exiting'
	  exit lvar
	endif

	ncnt=0
	iprob=0
	xprob=0.0
	iclust=0
	imembr=0
	kpds=0
	kgds=0
	kens=0
	kprob=0
	zprob=0.0
	kclust=0
	kmembr=0
	lens(1)=1
	lens(2)=0
	lens(3)=0
	lens(4)=1
	lens(5)=255

	icnt=0
	ecnt=0.0
	eatot=0.0
	eavg=0.0
	evtot=0.0
	evar=0.0
	estd=0.0

c loop over files

	lmem: do ifile=nfiles,1,-1
	  print *,'begin member input loop ifile=' ,ifile

c read a field
          
	  jpds=-1
	  jgds=-1
	  jens=-1
	  if ( ifile .ne. nfiles ) then
	    jpds(5)=kpds(5)
	    jpds(6)=kpds(6)
	    jpds(7)=kpds(7)
	  endif
	  kpds=0
	  kgds=0
	  kens=0
	  kprob=0
	  zprob=0.0
	  kclust=0
	  kmembr=0
	  if ((ifile.ge.nfiles-3).or.(ifile.le.3)) then
	    print *
	    print *,' before getgbex'
	    print *,' jpds=',(jpds(l),l=1,25)
	    print *,' jens=',(jens(l),l=1,5)
	    print *,' before getgbex'
	    print *,' kpds=',(kpds(l),l=1,25)
	    print *,' kens=',(kens(l),l=1,5)
	    print *,' kprob=',kprob
	    print *,' zprob=',zprob
	    print *,' kclust=',kclust
	    print *,' kmembr=',kmembr
	    print *,' jpos,kpos=',jpos(ifile),kpos(ifile)
	  endif
	  call getgbex(icfipg(ifile),icfipi(ifile),
     &              lfm,jpos(ifile),jpds,jgds,jens,
     &              kf,kpos(ifile),kpds,kgds,kens,
     &                     kprob,zprob,kclust,kmembr,
     &                      lb,f,iret)
	  if ((ifile.ge.nfiles-3).or.
     &        (ifile.le.3).or.
     &        (iret.ne.0)) then
	    print *,' after getgbex'
	    print *,' jpds=',(jpds(l),l=1,25)
	    print *,' jens=',(jens(l),l=1,5)
	    print *,' after getgbex'
	    print *,' kpds=',(kpds(l),l=1,25)
	    print *,' kens=',(kens(l),l=1,5)
	    print *,' kprob=',kprob
	    print *,' zprob=',zprob
	    print *,' kclust=',kclust
	    print *,' kmembr=',kmembr
	    print *,' jpos,kpos=',jpos(ifile),kpos(ifile)
	  endif
	  if (iret.eq.0) then
	    jpos(ifile)=kpos(ifile)
	    print *,' jpos,kpos=',jpos(ifile),kpos(ifile)
	    if ( ifile .eq. nfiles ) then
	      ipds=kpds
	      igds=kgds
	      iens=kens
	    endif

            call srange(kf,lb,f,fp,dmin,dmax,dmean,adev,sdev,skew)
            print '(i3,i3,2i5,4i3,i4,4i2,i4,i6,6g11.4)',
     &        n,(kpds(i),i=5,11),kpds(14),(kens(i),i=1,5),kf,dmin,
     &        dmax,dmean,adev,sdev,skew

c write the field to enspost if asked

	    do iep=1,nenspost
	      if (kpds(5).eq.ivar(iep)) then
		if (kpds(6).eq.ilvt(iep)) then
		  if (kpds(7).eq.ilev(iep)) then
		    print *
		    print *,' before putgbex enspost'
		    print *,' kpds=',(kpds(l),l=1,25)
		    print *,' kens=',(kens(l),l=1,5)
		    print *,' kprob=',kprob
		    print *,' zprob=',zprob
		    print *,' kclust=',kclust
		    print *,' kmembr=',kmembr
		    call putgbex(icfopg(iep),kf,kpds,kgds,kens,
     &                   kprob,zprob,kclust,kmembr,lb,f,iret)
		    if (iret.eq.0) then
		      print *,'putgbex enspost succeeded'
		    else
		      print *,'putgbex enspost failed, iret=',iret
		    endif
		  endif
		endif
	      endif
	    enddo

c add the field to the running totals if not skipped

            if (iskip(ifile).eq.0) then 
	      icnt=icnt + 1
	      do ii=1,kf
		ecnt(ii)=ecnt(ii)+1.0
		eatot(ii)=eatot(ii)+f(ii)
		evtot(ii)=evtot(ii)+f(ii)**2
	      enddo
	      lf=kf
	    endif

          else
	    if ( ifile .eq. nfiles ) then
	      print *,'iret=',iret
	      print *,' exiting'
	      exit lvar
	    endif
	  endif

	enddo lmem


c calculate the ensemble mean and spread

	if (icnt.ge.3) then
	  print *,'begin calculating mean and spread'
	  kpds=ipds
	  kgds=igds
          kf=lf
	  iens=0

	  print *,' before calculations'
	  print *,' ipds=',(ipds(l),l=1,25)
	  print *,' iens=',(iens(l),l=1,5)
	  print *,' iprob=',iprob
	  print *,' xprob=',xprob
	  print *,' iclust=',iclust
	  print *,' imembr=',imembr
	  print *
	  print *,'number of members'
	  call srange(kf,lb,ecnt(1),fp,dmin,dmax,dmean,
     &      adev, sdev,skew)
	  print '(i3,i3,2i5,4i3,i4,4i2,i4,i6,6g11.4)',
     &      n,(kpds(i),i=5,11),kpds(14),(kens(i),i=1,5),kf,dmin,
     &       dmax,dmean,adev,sdev,skew
	  print *
	  print *,'ensemble totals'
	  call srange(kf,lb,eatot(1),fp,dmin,dmax,dmean,
     &      adev, sdev,skew)
	  print '(i3,i3,2i5,4i3,i4,4i2,i4,i6,6g11.4)',
     &      n,(kpds(i),i=5,11),kpds(14),(kens(i),i=1,5),kf,dmin,
     &       dmax,dmean,adev,sdev,skew
	  print *
	  print *,'ensemble square totals'
	  call srange(kf,lb,evtot(1),fp,dmin,dmax,dmean,
     &      adev, sdev,skew)
	  print '(i3,i3,2i5,4i3,i4,4i2,i4,i6,6g11.4)',
     &      n,(kpds(i),i=5,11),kpds(14),(kens(i),i=1,5),kf,dmin,
     &       dmax,dmean,adev,sdev,skew
	  do ii=1,kf
	    if (ecnt(ii).ge.0.5) then
	      eavg(ii)=eatot(ii)/ecnt(ii)
	      if (ecnt(ii).ge.1.5) then
		eavt=eatot(ii)*eavg(ii)
c use of ecnt-1 means that these are estimates of
c the population variance and standard deviation
		if (evtot(ii).ge.eavt) then
		  evar(ii)=(evtot(ii)-eavt)/(ecnt(ii)-1.0)
		  estd(ii)=sqrt(evar(ii))
		else
c this test allows for small roundoff errors
c in the variance calculation
		  if (evtot(ii).lt.(eavt*0.99)) then
		    print *,'bad variance',
     &                  evtot(ii),eatot(ii),eavg(ii),eavt
     &                          ,ecnt(ii),ii,n
		  endif
		endif
	      endif
	    endif
	  enddo
          print *
          print *,'ensemble mean'
          call srange(kf,lb,eavg(1),fp,dmin,dmax,dmean,
     &      adev, sdev,skew)
          print '(i3,i3,2i5,4i3,i4,4i2,i4,i6,6g11.4)',
     &      n,(kpds(i),i=5,11),kpds(14),(kens(i),i=1,5),kf,dmin,
     &       dmax,dmean,adev,sdev,skew
          print *
          print *,'ensemble variance'
          call srange(kf,lb,evar(1),fp,dmin,dmax,dmean,
     &      adev, sdev,skew)
          print '(i3,i3,2i5,4i3,i4,4i2,i4,i6,6g11.4)',
     &      n,(kpds(i),i=5,11),kpds(14),(kens(i),i=1,5),kf,dmin,
     &       dmax,dmean,adev,sdev,skew
          print *
          print *,'ensemble standard deviation'
          call srange(kf,lb,estd(1),fp,dmin,dmax,dmean,
     &      adev, sdev,skew)
          print '(i3,i3,2i5,4i3,i4,4i2,i4,i6,6g11.4)',
     &      n,(kpds(i),i=5,11),kpds(14),(kens(i),i=1,5),kf,dmin,
     &       dmax,dmean,adev,sdev,skew
          print *
          print *,'end calculating mean and spread'

c write out ensemble mean and spread

          print *
          print *,'begin writing  mean and spread'

c extensions for ensemble mean
	  ipds=kpds
	  igds=kgds
	  iens=0

          ipds(23)=2
          iens(1)=1           !: OCT 41
          iens(2)=5           !: OCT 42
          iens(3)=0           !: OCT 43
          iens(4)=1           !: OCT 44
          iprob(1)=0          !: OCT 46
          iprob(2)=0          !: OCT 47
          xprob(1)=0.0        !: OCT 48-51
          xprob(2)=0.0        !: OCT 52-55
          iclust(1)=icnt      !: OCT 61
          print *
          print *,' before putgbex mean'
          print *,' ipds=',(ipds(l),l=1,25)
          print *,' iens=',(iens(l),l=1,5)
          print *,' iprob=',iprob
          print *,' xprob=',xprob
          print *,' iclust=',iclust
          print *,' imembr=',imembr

c write to mean member

	  if ( nfiles > (nskip+2) ) then
	    call putgbex(icfoag,
     &                     lf,ipds,igds,iens,iprob,xprob,
     &                     iclust,imembr,lb,eavg(1),iret)
	    if (iret.eq.0) then
	      print *,'putgbex mean succeeded'
	    else
	      print *,'putgbex mean failed, iret=',iret
	    endif
	  endif

c write mean to old style ensstat file if asked

	  do iep=1,nenspost
	    if (kpds(5).eq.ivar(iep)) then
	      if (kpds(6).eq.ilvt(iep)) then
		if (kpds(7).eq.ilev(iep)) then
		  call putgbex(icfotg(iep),
     &                   lf,ipds,igds,iens,iprob,xprob,
     &                   iclust,imembr,lb,eavg(1),iret)
		  if (iret.eq.0) then
		    print *,'putgbex es mean succeeded'
		  else
		    print *,'putgbex es mean failed, iret=',iret
		  endif
		endif
	      endif
	    endif
	  enddo

c extensions for ensemble standard deviation
	  ipds=kpds
	  igds=kgds
	  iens=0

          ipds(23)=2
          iens(1)=1           !: OCT 41
          iens(2)=5           !: OCT 42
          iens(3)=0           !: OCT 43
          iens(4)=11          !: OCT 44
          iprob(1)=0          !: OCT 46
          iprob(2)=0          !: OCT 47
          xprob(1)=0.0        !: OCT 48-51
          xprob(2)=0.0        !: OCT 52-55
          print *
          print *,' before putgbex std'
          print *,' ipds=',(ipds(l),l=1,25)
          print *,' iens=',(iens(l),l=1,5)
          print *,' iprob=',iprob
          print *,' xprob=',xprob
          print *,' iclust=',iclust
          print *,' imembr=',imembr

c write to spread member

	  if ( nfiles > (nskip+2) ) then
	    call putgbex(icfosg,lf,ipds,igds,iens,iprob,xprob,
     &                     iclust,imembr,lb,estd(1),iret)
	    if (iret.eq.0) then
	      print *,'putgbex std succeeded'
	    else
	      print *,'putgbex std failed, iret=',iret
	    endif
	  endif

c write spread to old style ensstat file if asked

	  do iep=1,nenspost
	    if (kpds(5).eq.ivar(iep)) then
	      if (kpds(6).eq.ilvt(iep)) then
		if (kpds(7).eq.ilev(iep)) then
		  call putgbex(icfotg(iep),
     &                   lf,ipds,igds,iens,iprob,xprob,
     &                   iclust,imembr,lb,estd(1),iret)
		  if (iret.eq.0) then
		    print *,'putgbex es std succeeded'
		  else
		    print *,'putgbex es std failed, iret=',iret
		  endif
		endif
	      endif
	    endif
	  enddo

	endif

      enddo lvar

c close files and deallocate arrays

      do ifile=1,nfiles
	call baclose(icfipg(ifile),iretipg)
	if ( iretipg .ne. 0 ) then
	  print *,'ifile,iretipg = ',ifile,iretipg
	endif
	call baclose(icfipi(ifile),iretipi)
	if ( iretipi .ne. 0 ) then
	  print *,'ifile,iretipi = ',ifile,iretipi
	endif
      enddo

      if ( nfiles > (nskip+2) ) then
	call baclose(icfoag,iretoag)
	if ( iretoag .ne. 0 ) then
	  print *,'iretoag = ',iretoag
	endif
	call baclose(icfosg,iretosg)
	if ( iretosg .ne. 0 ) then
	  print *,'iretosg = ',iretosg
	endif
      endif

      do iep=1,nenspost
	call baclose(icfopg(iep),iretopg)
	if ( iretopg .ne. 0 ) then
	  print *,'iep,iretopg = ',iep,iretopg
	endif
	call baclose(icfotg(iep),iretotg)
	if ( iretotg .ne. 0 ) then
	  print *,'iep,iretotg = ',iep,iretotg
	endif
      enddo

c     deallocate(ivar)
c     deallocate(ilvt)
c     deallocate(ilev)
c     deallocate(cfopg)
c     deallocate(cfotg)
c     deallocate(lfopg)
c     deallocate(lfotg)
c     deallocate(icfopg)
c     deallocate(icfotg)

c     deallocate(cfipg)
c     deallocate(cfipi)
c     deallocate(ifipg)
c     deallocate(ifipi)
c     deallocate(icfipg)
c     deallocate(icfipi)
c     deallocate(jpos)
c     deallocate(kpos)
c     deallocate(iskip)

      deallocate(f)
      deallocate(ecnt)
      deallocate(eatot)
      deallocate(eavg)
      deallocate(evtot)
      deallocate(evar)
      deallocate(estd)
      deallocate(lb)

      CALL W3TAGE('ENSSTAT')

      stop
      end

      subroutine srange(n,ld,d,fprint,dmin,dmax,dmean,adev,sdev,skew)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: SRANGE(N,LD,D,FPRINT,DMIN,DMAX,DMEAN,ADEV,SDEV,SKEW)
C   PRGMMR:Richard Wobus      ORG:NP20           DATE: 01-11-09
C
C ABSTRACT: THIS SUBROUTINE WILL CALCULATE THE MAXIMUM AND
C           MINIMUM AND OTHER STATISTICS OF AN ARRAY
C
C PROGRAM HISTORY LOG:
C   97-03-17   YUEJIAN ZHU (WD20YZ)
C   01-03-14   Richard Wobus (wx20rw) added deviations and skew
C   01-11-09   Richard Wobus (wx20rw) corrected names and indices
C
C USAGE:
C
C   INPUT ARGUMENTS:
C     N        -- INTEGER
C     LD(N)    -- LOGICAL OF DIMENSION N
C     D(N)     -- REAL ARRAY OF DIMENSION N
C     FPRINT   -- LOGICAL PRINT FLAG
C
C   OUTPUT ARGUMENTS:
C     DMIN     -- REAL NUMBER ( MINIMUM )
C     DMAX     -- REAL NUMBER ( MAXIMUM )
C     DMEAN    -- REAL NUMBER ( MEAN )
C     ADEV     -- REAL NUMBER ( AVERAGE DEVIATION )
C     SDEV     -- REAL NUMBER ( STANDARD DEVIATION )
C     SKEW     -- REAL NUMBER ( SKEWNESS )
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
      logical*1 ld
      logical fprint
      dimension ld(n),d(n)
      dmin=1.e30
      dmax=-1.e30
      sa=0.0
      ptsn=0.0
      do i=1,n
        if(ld(i)) then
          dmin=min(dmin,d(i))
          dmax=max(dmax,d(i))
          sa=sa+d(i)
	  ptsn=ptsn+1.0
        endif
      enddo
      if (ptsn.gt.0.0) then
        dmean=sa/ptsn
      else
	dmean=0.0
      endif 
      sl=0.0
      sv=0.0
      do i=1,n
	if (ld(i)) then
	  sl=sl+abs(d(i)-dmean)
	  sv=sv+(d(i)-dmean)**2
	endif
      enddo
      if (ptsn.gt.0.0) then
        adev=sl/ptsn
      else
	adev=0.0
      endif
      if (ptsn.gt.1.0) then
        sdev=sqrt(sv/(ptsn-1))
      else
	sdev=0.0
      endif
      if (sdev .gt. 0.0) then
	ss=0.0
	do i=1,n
	  if (ld(i)) then
	    devn=(d(i)-dmean)/sdev
	    ss=ss+devn**3
	  endif
	enddo
	skew=ss/ptsn
      else
	skew=0.0
      endif
      if (fprint) then
        print *,dmin,dmax,avg,adev,sdev,skew
      endif
      return
      end

