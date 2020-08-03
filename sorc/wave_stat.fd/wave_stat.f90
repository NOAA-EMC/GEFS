!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
!     multiwave_gwes_stats
!
!     Origin: H.S. Chen, 2004
!     Upgrades:   D. Cao, 2008
!     NFCENS upgrade:  program nfcombwave_ensemble, Y.Y. Chao, Jun 2011
!     GWES upgrade: J-H. Alves, Jan 2014
!
!     Latest changes (GWES upgrade)
!     - Became multiwave_gwes_stats
!     - Resolution increased to 0.5 degrees
!     - Allow output grid to be different than input grid (use
!     allocatable arrays to get resolutions from input grib2 data)
!     - Buoy output using wgrib2 to extract directly from input data
!     - Parallelization of stats using mpiserial acroos parameters and
!     statistics
!     
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
      module ens_common
!
      implicit none
!
      integer,parameter :: iu11=11,iu12=12
      integer,parameter :: iu51=51,iu52=52,iu53=53,iu54=54,iu59=59
      real,parameter :: defv=1.0e10,tolr=1.0e-8,zero=0.0
!
      character :: parname*5, statstype*6
      integer :: kpds(25),kgds(22)
      integer :: ymdc,fhr,nnme,nnsc
      integer :: parcode(3)
      integer :: istop
      real,allocatable :: dumy1d(:)
! Grid
      real    :: rdlon(3),rdlat(3)
      integer :: nlola(2)
!
      character(len=13) :: fname
      logical*1,allocatable :: lbms2d(:,:),lbms1d(:)
      real,allocatable      :: fsum(:,:),fmean(:,:),fspre(:,:)
      character,allocatable        :: id_member(:)*2
      character(len=7),allocatable :: id_file(:)
      real,allocatable             :: f(:,:,:),fprob(:,:,:),scale(:)
!
      real :: gmean,gspre
      real,allocatable :: g(:),gprob(:)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
      endmodule ens_common
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
      PROGRAM gwes_stats
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! MAIN PROGRAM:  ensemble    conduct ensemble statistics.
!   PRGMMR: H.S. CHEN        ORG: W/MP21     DATE: 04-05-20
!
! ABSTRACT: conduct ensemble statistics.
!
! PROGRAM HISTORY LOG:
!   04-05-20  H.S. Chen      Origination and implement at NCEP.
!
! USAGE:
!**   Interface.
!     ----------
!       The program runs independently and must run after the
!       completion of all individual ensemble POST runs 
!
!     Input File: 
!     ------------
!     gwes.stats.inp: Provides date, parameter definitions, ensemble member
!                      names and numbers, probability level and input data
!                      file names (data_NN)
!     Output File: 
!     -------------
!      mean_out    - grib2 file containing combined ensemble mean
!      spread_out  - grib2 file containing combined ensemble spread
!      prob_out    - grib2 file containing combined ensemble probabilities of
!                    Hs exceeding a given value
!
!     Method.
!     -------
!       Interpolation using wgrib2
!
!     Externals.
!     ----------
!       wgrib2
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
! 
      use ens_common
!
      integer :: ierr,i,j,k,l,m,n,i1,i2,j1,j2,iret,leve
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
!     CALL W3TAGB('nww2ec  ',0097,0027,0075,'NP21   ')
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
!
      open (unit=iu59,file='test_out')
!
      read(*,*) ymdc,fhr,parname,parcode
      write(iu59,'(i10.10,2x,i3.3,2x,a5,i3,1x,i3,1x,i3)') ymdc,fhr,parname,parcode 
!      read(*,*) statstype
!      write(iu59,'(a6)') statstype
      read(*,*) nnme
      write(iu59,'(i4)') nnme
      allocate( id_member(nnme) )
      read(*,*) id_member(:)
      write(iu59,'(20(1x,a2))') id_member(:)
      read(*,*) nnsc
      write(iu59,'(i4)') nnsc
!    
      allocate( scale(nnsc) )
      scale(:) = 0.0
        read(*,*) scale(:)
        write(iu59,'(10f8.3)') scale(:)
!
      read(*,*) nlola 
      write(iu59,'(i6,1x,i6)') nlola
      nx=nlola(1)
      ny=nlola(2)
      nxy=nx*ny
      lcgrib=4*nxy
      allocate (dumy1d(nxy),lbms1d(nxy),lbms2d(nx,ny),fsum(nx,ny),         &
                fmean(nx,ny),fspre(nx,ny))
!
      read(*,*) rdlon 
      write(iu59,'(3f9.4)') rdlon
!
      read(*,*) rdlat 
      write(iu59,'(3f9.4)') rdlat
!
      allocate( f(nx,ny,nnme) )
      allocate( fprob(nx,ny,nnsc) )
!
      allocate( g(nnme) )
      allocate( gprob(nnsc) )
!
      allocate( id_file(nnme) )
      read(*,*) id_file(:)
      write(iu59,'(10(1x,a7))') id_file(:)
!
!  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
!
      f(:,:,:) = 0.0
      do m=1,nnme
!
        fname=id_file(m)
        write(iu59,*) fname
!
        open (iu11,file=fname,access='SEQUENTIAL',form='UNFORMATTED')
!
        write(iu59,'("*** read f(m), m =",i4)') m
        read (iu11,end=12,err=91,iostat=ierr)     &
             ((f(i,j,m),i=1,nx),j=ny,1,-1)
!
        where( f(:,:,m).ge.defv ) f(:,:,m) = zero
!
   12   continue
        close(iu11)
      enddo
!
! Calculate ensemble statistics.
!
      call ens_statistics
!
! Output.
!
! Set up bitmap, lbms2d(:,:) and lbms1d(:), for missing data indicator; 
! true for data, false for no data.
!
      lbms2d(:,:) = .true.
      where( fmean(:,:).le.zero ) lbms2d(:,:) = .false.
!
      ij=1
        do j=1,ny
        do i=1,nx
          lbms1d(ij)=lbms2d(i,j)
          ij=ij+1
        enddo
      enddo
!
! ensemble mean.
!
!      if (statstype .eq. 'avrage') then
        call baopen(iu52,'mean_out',iret)
        write(*,'("** After baopen for mean, iret = ",i5)') iret
        dumy1d(:)=0.0
        ij=1
        do j=1,ny
          do i=1,nx
            dumy1d(ij)=fmean(i,j)
            ij=ij+1
          enddo
        enddo
!
        call grbit2 (iu52,nx,ny,lcgrib,rdlon,rdlat,lbms1d,dumy1d,iret, &
                     ymdc,fhr,parcode,1,0,0,0,nnme)
        if( iret.ne.0 ) then
          write(*,'("**** ERR in grbit2, ymdc,fhr,parcode,parname: ")')
          write(*,'(1x,i10.10,1x,i3.3,i5,1x,a5)') ymdc,fhr,parcode,parname
        endif
!      endif
!
! ensemble spread.
!
!      if (statstype .eq. 'spread') then
        call baopen(iu53,'spread_out',iret)
        write(*,'("** After baopen for spread, iret = ",i5)') iret
        dumy1d(:)=0.0
        ij=1
        do j=1,ny
          do i=1,nx
          dumy1d(ij)=fspre(i,j)
            ij=ij+1
          enddo
        enddo
!
        call grbit2 (iu53,nx,ny,lcgrib,rdlon,rdlat,lbms1d,dumy1d,iret, &
                     ymdc,fhr,parcode,2,0,0,0,nnme)
        if( iret.ne.0 ) then
          write(*,'("**** ERR in grbit2, ymdc,fhr,parcode,parname: ")')
          write(*,'(1x,i10.10,1x,i3.3,i5,1x,a5)') ymdc,fhr,parcode,parname
        endif
!      endif
!
! ensemble probability.
!
!      if (statstype .eq. 'probab') then
        call baopen(iu54,'prob_out',iret)
        write(*,'("** After baopen for prob, iret = ",i5)') iret
        do l=1,nnsc
!
          dumy1d(:)=0.0
          ij=1
          do j=1,ny
            do i=1,nx
              dumy1d(ij)=fprob(i,j,l)
              ij=ij+1
            enddo
          enddo
!
          leve = nint(100.*scale(l))
          call grbit2 (iu54,nx,ny,lcgrib,rdlon,rdlat,lbms1d,dumy1d,    &
                       iret,ymdc,fhr,parcode,3,nnsc,l,leve,nnme)
          if( iret.ne.0 ) then
            write(*,'("** ERR in grbit2, ymdc,fhr,parcode,parname,leve:")')
            write(*,'(1x,i10.10,1x,i3.3,i5,1x,a5,i7)')   &
                    ymdc,fhr,parcode,parname,leve
          endif
        enddo
!      endif
!
!  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
!
   91 continue
!
!  deallocate.
!
      deallocate( id_member )
      deallocate( id_file,scale,fprob,f )
      deallocate( g,gprob )
      deallocate (dumy1d,lbms1d,lbms2d,fsum,fmean,fspre)
!
      close(iu51)
!
      call baclose(iu52,iret)
      write(*,'("** After baclose for mean, iret= ",i5)') iret
      call baclose(iu53,iret)
      write(*,'("** After baclose for spread, iret= ",i5)') iret
      call baclose(iu54,iret)
      write(*,'("** After baclose for prob, iret= ",i5)') iret
      close(iu59)
!
      STOP
      END
!
!
      subroutine ens_statistics
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
! Calculate ensemble statistics.
!   input : nx   = number of x points.
!           ny   = number of y points.
!           me   = number of ensemble members.
!           nnsc = number of scales for probability.
!           f(nx,ny,me)  = field array.
!           fsum(nx,ny)  = only used for calculating sum.
!           fscale(nnsc) = scale array.
!           statstype = type of statstics to compute (mean, spread,
!           prob) - feature added to increase paralellism
!   output: fmean(nx,ny)      = ensemble mean array.
!           fspre(nx,ny)      = ensemble spread array.
!           fprob(nx,ny,nnsc) = ensemble probability.
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
!
      use ens_common
!
!  local
!
      integer :: m,n,i,j,k
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
!
!  mean.
!
!     if(statstype .eq. 'avrage')then
!      write(*,*)statstype
      fsum(:,:) = 0.0
      do m=1,nnme
        fsum(:,:) = fsum(:,:) + f(:,:,m)
      enddo
      fmean(:,:) = fsum(:,:)/real(nnme)
!     endif
!
!  spread - deviation.
!
!     if(statstype .eq. 'spread')then
!      write(*,*)statstype
      fspre(:,:) = 0.0
      do m=1,nnme
        fspre(:,:) = fspre(:,:) + (f(:,:,m)-fmean(:,:))**2
      enddo
      fspre(:,:) = sqrt( fspre(:,:)/real(nnme) )
!     endif
!
!  probability.
!
!     if(statstype .eq. 'probab')then
!      write(*,*)statstype
      fprob(:,:,:) = 0.0
      do n=1,nnsc
        do m=1,nnme
          where( f(:,:,m).ge.scale(n) ) fprob(:,:,n)=fprob(:,:,n)+1.
        enddo     
        fprob(:,:,n) = fprob(:,:,n)/real(nnme)
      enddo     
!     endif
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
!
      return
      end
!
