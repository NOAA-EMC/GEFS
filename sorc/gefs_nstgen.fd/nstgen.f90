program nstgen
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
       nemsio_writerec,nemsio_readrecv,nemsio_writerecv,nemsio_writerecvw34

  implicit none

  real(4),parameter:: zero=0.0_4


  character*500 filenamein_nst,filenameout
  character*3 charnanal
  integer lunin,lunout,iret,k
  integer nrec, lonb, latb, n, npts, idrt
  real rlat_min,rlat_max 
  real rlon_min,rlon_max 
  integer,dimension(7):: idate
  real,allocatable,dimension(:)   :: rwork1d
  real,allocatable,dimension(:)   :: dx
  character,allocatable,dimension(:)   :: recname,reclevtyp
  integer,allocatable,dimension(:)   :: reclev


  type(nemsio_gfile) :: gfile, gfileo,nstfile,gfilew


  call w3tagb('GETNSTTF',2011,0319,0055,'NP25')

! Get user input from command line
  call getarg(1,filenamein_nst)
  call getarg(2,filenameout)
     write(6,*)' '
     write(6,*)'Command line input'
     write(6,*)' filenamein_nst      = ',trim(filenamein_nst)
     write(6,*)' filenameout   = ',trim(filenameout)

  lunin=21
  lunout=61

!get header from sfc file
! Process input files (one file per task)
     call nemsio_init(iret)
! Read nst anl file
     call nemsio_open(nstfile,trim(filenamein_nst),'READ',iret)
      if (iret == 0 ) then
         write(6,*)'open ', trim(filenamein_nst)
     else
           write(6,*)'***ERROR*** ',trim(filenamein_nst),' contains unrecognized format.  ABORT'
     endif

!WL
!           call nemsio_getfilehead(nstfile, nrec=nrec, idate=idate, dimx=lonb, dimy=latb,idrt=idrt,iret=iret)
           call nemsio_getfilehead(nstfile, nrec=nrec, idate=idate, dimx=lonb, dimy=latb,idrt=idrt,rlat_min=rlat_min,rlat_max=rlat_max,rlon_min=rlon_min,rlon_max=rlon_max,iret=iret)
!WL
        npts=lonb*latb
           write(6,*)'lonb latb ',lonb,latb,npts
        if (.not.allocated(rwork1d)) allocate(rwork1d(npts))
        if (.not.allocated(recname)) allocate(recname(1))
        if (.not.allocated(reclevtyp)) allocate(reclevtyp(1))
        if (.not.allocated(reclev)) allocate(reclev(1))
!WL
        
        recname(1)='tmp'
        reclevtyp(1)='sfc'
        reclev(1)=1
!WL
!        idrt=0
!WL
    
!       Following fields are not averaged
!         slmsk = land sfc
!         vtype = vtype sfc
!         stype = sotyp sfc
!         slope = sltyp sfc
!         orog  = orog sfc

        rwork1d=zero
        call nemsio_readrecv(nstfile,'tref','sfc',1,rwork1d,iret)
!        write(6,*) rwork1d(1:100)
        print *,idrt,rlat_min,rlat_max,rlon_min,rlon_max
        call nemsio_open(gfileo,trim(filenameout),'WRITE',iret,gdatatype='grib',modelname='gfs',nmeta=5,nrec=1, idrt=idrt,idate=idate, dimx=lonb,dimy=latb,dimz=1,recname=recname, reclevtyp=reclevtyp, reclev=reclev)
        call nemsio_writerecvw34(gfileo,'tmp','sfc',1,rwork1d,iret)
        
        deallocate(rwork1d)
        
        call nemsio_close(gfile, iret)
        call nemsio_close(nstfile, iret)
        call nemsio_close(gfileo,iret)
           write(6,*)'Write new sfc nemsio file ',trim(filenameout),' iret=',iret

END program nstgen
