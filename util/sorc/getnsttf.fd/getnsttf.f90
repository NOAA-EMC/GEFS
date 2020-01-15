program getnsttf
!$$$  main program documentation block
!
! program:  rd sfc nemsio and repalce tsea with tref from nst anl
!
! prgmmr: Kate Zhou               date: 2017-07-31
!
!
! usage:
!   input files:
!
!   output files:
!
! attributes:
!   language: f95
!
!$$$

  use nemsio_module, only:  nemsio_init,nemsio_open,nemsio_close
  use nemsio_module, only:  nemsio_gfile,nemsio_getfilehead,nemsio_readrec,&
       nemsio_writerec,nemsio_readrecv,nemsio_writerecv

  implicit none

  real(4),parameter:: zero=0.0_4


  character*500 filenamein_nst,filenamein_sfc,filenameout
  character*3 charnanal
  integer lunin,lunout,iret,k,i
  integer nrec, lonb, latb, n, npts
  integer,dimension(7):: idate
  real,allocatable,dimension(:)   :: rwork1d,rwork1d0,landm


  type(nemsio_gfile) :: gfile, gfileo,nstfile


  call w3tagb('GETNSTTF',2011,0319,0055,'NP25')

! Get user input from command line
  call getarg(1,filenamein_sfc)
  call getarg(2,filenamein_nst)
  call getarg(3,filenameout)
     write(6,*)' '
     write(6,*)'Command line input'
     write(6,*)' filenamein_sfc      = ',trim(filenamein_sfc)
     write(6,*)' filenamein_nst      = ',trim(filenamein_nst)
     write(6,*)' filenameout   = ',trim(filenameout)

  lunin=21
  lunout=61


! Process input files (one file per task)

     call nemsio_init(iret)
! Read sfc anl file

     call nemsio_open(gfile,trim(filenamein_sfc),'READ',iret)

      if (iret == 0 ) then
         write(6,*)'open ', trim(filenamein_sfc)
     else
           write(6,*)'***ERROR*** ',trim(filenamein_sfc),' contains unrecognized format.  ABORT'
     endif

     call nemsio_getfilehead(gfile, nrec=nrec, idate=idate, dimx=lonb, dimy=latb, iret=iret)

     gfileo=gfile
! Read nst anl file
     call nemsio_open(nstfile,trim(filenamein_nst),'READ',iret)
      if (iret == 0 ) then
         write(6,*)'open ', trim(filenamein_nst)
     else
           write(6,*)'***ERROR*** ',trim(filenamein_nst),' contains unrecognized format.  ABORT'
     endif

           call nemsio_open(gfileo,trim(filenameout),'WRITE',iret )

        npts=lonb*latb
           write(6,*)'lonb latb ',lonb,latb,npts
        if (.not.allocated(rwork1d)) allocate(rwork1d(npts))
        if (.not.allocated(rwork1d0)) allocate(rwork1d0(npts))
        if (.not.allocated(landm)) allocate(landm(npts))

        
        do n=1,nrec
           rwork1d=zero
           call nemsio_readrec (gfile, n,rwork1d,iret)
           call nemsio_writerec(gfileo,n,rwork1d,iret)
           write(6,*)'n= ',n
        end do

!       Following fields are not averaged
!         slmsk = land sfc
!         vtype = vtype sfc
!         stype = sotyp sfc
!         slope = sltyp sfc
!         orog  = orog sfc

           rwork1d=zero
           rwork1d0=zero
           landm=zero
           call nemsio_readrecv(nstfile,'tref','sfc',1,rwork1d,iret)
           call nemsio_readrecv(nstfile,'tmp','sfc',1,rwork1d0,iret)
           call nemsio_readrecv(nstfile,'land','sfc',1,landm,iret)
          write(*,*)'land',landm(300:400)
        do i=1,npts
        if(landm(i).lt.1)then
          rwork1d0(i)=rwork1d(i)
        endif
        enddo
            
           call nemsio_writerecv(gfileo,'tmp','sfc',1,rwork1d0,iret)

        
        deallocate(rwork1d)
        
        call nemsio_close(gfile, iret)
        call nemsio_close(nstfile, iret)
        call nemsio_close(gfileo,iret)
           write(6,*)'Write new sfc nemsio file ',trim(filenameout),' iret=',iret


END program getnsttf
