!C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!C
!C MAIN PROGRAM: GFSENS_VORTEX_COMBINE
!C   PROGMMR: Richard Wobus    ORG: NP22       DATE: 2005-07-12
!C
!C COMBINES ENSEMBLE STORM FILES WITH RESCALED BACKGROUND FIELD
!C AS PART OF GLOBAL ENSEMBLE RELOCATION OF TROPICAL STORMS
!C
!C PROGRAM HISTORY LOG:
!C 2005-05-10  QINGFU LIU    -- NEW PROGRAM FOR ENSEMBLE
!C 2005-07-13  RICHARD WOBUS -- RENAME AND UPDATE DOCBLOCK
!C 2006-07-06  RICHARD WOBUS -- RENAME AND USE SIGIO
!C
!C
!C ATTRIBUTES:
!C   LANGUAGE: FORTRAN 90
!C   MACHINE:  IBM-SP
!C
!C$$$
!C
!C234567890123456789012345678901234567890123456789012345678901234567890
!C

      PROGRAM GEFS_VORTEX_COMBINE
      use sigio_module
      use sigio_r_module

      use nemsio_module
      use nemsio_gfs

      type(sigio_head):: heado
      type(sigio_data):: datao

      type(nemsio_gfile) :: gfile
      type(nemsio_head)  :: ghead
      type(nemsio_headv) :: gheadv
      type(nemsio_data)  :: gdata



      character ifile*7, kfile*7

      REAL(4),ALLOCATABLE :: WORK_3(:),WORK_4(:,:)
      REAL,   ALLOCATABLE :: WORK_8(:)
!      REAL,   ALLOCATABLE :: WK_S1(:,:),WK_S2(:,:),WK_G(:,:,:),
!     1                       WK_G2(:,:,:)
      REAL,   ALLOCATABLE :: WK_S1(:,:)

      real,   allocatable :: srlsphc(:)
      real,   allocatable :: srlsphcl(:,:)

      real,   allocatable :: srlpd(:,:)
      real,   allocatable :: srlpi(:,:)
      real,   allocatable :: srlpo(:,:)
      real,   allocatable :: srlhd(:,:)
      real,   allocatable :: srlhi(:,:)
      real,   allocatable :: srlho(:,:)
      real,   allocatable :: srltd(:,:,:)
      real,   allocatable :: srlti(:,:,:)
      real,   allocatable :: srlto(:,:,:)
      real,   allocatable :: srldd(:,:,:)
      real,   allocatable :: srldi(:,:,:)
      real,   allocatable :: srldo(:,:,:)
      real,   allocatable :: srlzd(:,:,:)
      real,   allocatable :: srlzi(:,:,:)
      real,   allocatable :: srlzo(:,:,:)
      real,   allocatable :: srlqd(:,:,:)
      real,   allocatable :: srlqi(:,:,:)
      real,   allocatable :: srlqo(:,:,:)

      REAL,   ALLOCATABLE :: PS1(:,:),T1(:,:,:),Q1(:,:,:)
      REAL,   ALLOCATABLE :: VOR1(:,:,:),DIV1(:,:,:)

      REAL(4) FHOUR,DUMMY(245)
     
      CHARACTER*2 MEMBER 
      CHARACTER*8 LAB(4)
      DIMENSION IDATE(4)

      integer * 4  idate7(7)
      integer inptyp
      logical  nopdpvv

!
      CHARACTER modelname*8
      real, allocatable :: DUMMY2(:)
      CHARACTER(LEN=20)       ::VLEVTYP,VNAME
      integer :: VLEV
      REAL(4),   ALLOCATABLE :: GLAT2D(:,:),DUMMY4(:)

      CALL W3TAGB('GEFS_VORTEX_COMBINE',2006,0187,0068,'NP20')
!Creal, allocatable :: DUMMY(:)
      READ(5,*)ITIM,IPAIR,FACT

      print*,'ITIM,IPAIR,FACT=',ITIM,IPAIR,FACT

      NSEM=1
!      IF(IPAIR.EQ.1)NSEM=3
!      IF(IPAIR.EQ.2)NSEM=5
!      IF(IPAIR.EQ.3)NSEM=7
!      IF(IPAIR.EQ.4)NSEM=9
!      IF(IPAIR.EQ.5)NSEM=11

      IF(IPAIR.GE.1)NSEM=9


      IUNIT = 54
      KUNIT = 58
      ifile='fort.54'
      kfile='fort.58'
!C
      PRINT*,'IUNIT,KUNIT,NSEM= ',IUNIT,KUNIT,NSEM 
      PRINT*,'iunit,kunit,nsem= ',iunit,kunit,nsem 
!c
      inptype=1
      call sigio_srohdc(iunit,ifile,heado,datao,iret)
      if (iret == 0) then
             inptyp=2    ! spectral GFS input




           print *,"   after read input"

           fhour=heado%fhour
           idate=heado%idate
           mwave=heado%jcap

           kmax=heado%levs
           imax=heado%lonb
           jmax=heado%latb
           itrac=heado%ntrac
           IKMAX=(ITRAC-3)*KMAX

      else
        print *,'sigio_srohdc failed,iret=',iret,ifile,iunit
        nopdpvv=.true.
	      call nemsio_init(ios)
        !            call nemsio_gfsgrd_open(gfile,ifile,'read',nopdpvv,
        !     &                           ghead,gheadv,iret=ios)
	      call nemsio_open(gfile,trim(ifile),'read',iret=ios)

        if (ios == 0) then
          inptyp = 1      
          call nemsio_getfilehead(gfile,version=ghead%version, &
            gdatatype=ghead%gdatatype, &
            dimz=ghead%dimz,dimx=ghead%dimx,dimy=ghead%dimy, &
            idvc=ghead%idvc,idsl=ghead%idsl,idvm=ghead%idvm,idrt=ghead%idrt, &
            ntrac=ghead%ntrac,nrec=ghead%nrec,recname=gheadv%recname, &
            jcap=ghead%jcap,idate=ghead%idate, &
            nfhour=ghead%nfhour,nfminute=ghead%nfminute, &
            nfsecondn=ghead%nfsecondn,nfsecondd=ghead%nfsecondd, &
            ncldt=ghead%ncldt,nsoil=ghead%nsoil, &
            modelname=ghead%modelname)
        else
           print *,'nemsio_open failed,ios=',ios
        endif
        idsl=ghead%idsl
        mwave=ghead%jcap
        idvm=ghead%idvm
        if (mod(idvm,10)==2)idvm=11
        itrac=ghead%ntrac
        kmax=ghead%dimz
        imax=ghead%dimx
        jmax=ghead%dimy
        idate7=ghead%IDATE

        NFHOUR=ghead%NFHOUR
        NFMINUTE=ghead%NFMINUTE
        NFSECONDN=ghead%NFSECONDN
        NFSECONDD=ghead%NFSECONDD
        FHOUR=real(NFHOUR,8)+real(NFMINUTE/60.,8)+  & 
        real(nfsecondn*1./(nfsecondd*360.),8)

        idate(1)=idate7(4)
        idate(2:3)=idate7(2:3)
        idate(4)=idate7(1)
        write (*,*) idate,imax,jmax,kmax
!        print*,' start reading nemsio data'
!
!        call nemsio_gfs_algrd(imax,jmax,kmax,itrac,gdata,nopdpvv)
!        call nemsio_gfs_rdgrd(gfile,gdata,iret=ios)
!
!        print*,' complete reading data, inptyp=', inptyp

!read data
        print*,' start reading data'
        modelname=ghead%modelname
	      IF ( TRIM(modelname) == 'GFS'  ) THEN
          call nemsio_gfs_algrd(imax,jmax,kmax,ntrac,gdata,nopdpvv)
          call nemsio_gfs_rdgrd(gfile,gdata,iret=ios)

        ELSEIF ( TRIM(modelname) == 'FV3GFS'  ) THEN
          print *, 'READING FV3GFS DATA'
          ALLOCATE(GDATA%ZS(IMAX,JMAX))
          ALLOCATE(GDATA%PS(IMAX,JMAX))
          ALLOCATE(GDATA%T(IMAX,JMAX,KMAX))
          ALLOCATE(GDATA%U(IMAX,JMAX,KMAX))
          ALLOCATE(GDATA%V(IMAX,JMAX,KMAX))
          ALLOCATE(GDATA%Q(IMAX,JMAX,KMAX,7))

          ALLOCATE(DUMMY2(imax*jmax))
          ! ugrd
          VNAME='ugrd' 
          VLEVTYP='mid layer' 
          DO VLEV=1, KMAX
          CALL NEMSIO_READRECV(GFILE,VNAME,VLEVTYP,VLEV, DUMMY2,0,IRET)
             IF ( IRET == 0 ) THEN
                GDATA%U(:,:,VLEV)=RESHAPE(DUMMY, (/IMAX,JMAX/) )
             ELSE
               PRINT *, 'ERROR in rdgrd (',TRIM(VNAME),') IRET=', IRET
               CALL ERREXIT (3)
             ENDIF
          ENDDO
          ! vgrd
          VNAME='vgrd' 
          VLEVTYP='mid layer' 
          DO VLEV=1, KMAX
          CALL NEMSIO_READRECV(GFILE,VNAME,VLEVTYP,VLEV, DUMMY2,0,IRET)
             IF ( IRET == 0 ) THEN
                GDATA%V(:,:,VLEV)=RESHAPE(DUMMY, (/IMAX,JMAX/) )
             ELSE
               PRINT *, 'ERROR in rdgrd (',TRIM(VNAME),') IRET=', IRET
               CALL ERREXIT (3)
             ENDIF
          ENDDO
          ! tmp
          VNAME='tmp' 
          VLEVTYP='mid layer' 
          DO VLEV=1, KMAX
          CALL NEMSIO_READRECV(GFILE,VNAME,VLEVTYP,VLEV, DUMMY2,0,IRET)
             IF ( IRET == 0 ) THEN
                GDATA%T(:,:,VLEV)=RESHAPE(DUMMY, (/IMAX,JMAX/) )
             ELSE
               PRINT *, 'ERROR in rdgrd (',TRIM(VNAME),') IRET=', IRET
               CALL ERREXIT (3)
             ENDIF
          ENDDO
          ! hgt
          VNAME='hgt' 
          VLEVTYP='sfc' 
           CALL NEMSIO_READRECV(GFILE,VNAME,VLEVTYP,1, DUMMY2,0,IRET)
             IF ( IRET == 0 ) THEN
                GDATA%ZS(:,:)=RESHAPE(DUMMY, (/IMAX,JMAX/) )
             ELSE
               PRINT *, 'ERROR in rdgrd (',TRIM(VNAME),') IRET=', IRET
               CALL ERREXIT (3)
             ENDIF
          ! pres
          VNAME='pres' 
          VLEVTYP='sfc' 
          CALL NEMSIO_READRECV(GFILE,VNAME,VLEVTYP,1,DUMMY2,0,IRET)
             IF ( IRET == 0 ) THEN
                GDATA%PS(:,:)=RESHAPE(DUMMY, (/IMAX,JMAX/) )
             ELSE
               PRINT *, 'ERROR in rdgrd (',TRIM(VNAME),') IRET=', IRET
               CALL ERREXIT (3)
             ENDIF

          ! spfh
          VNAME='spfh' 
          VLEVTYP='mid layer' 
          DO VLEV=1, KMAX
          CALL NEMSIO_READRECV(GFILE,VNAME,VLEVTYP,VLEV, DUMMY2,0,IRET)
             IF ( IRET == 0 ) THEN
                GDATA%Q(:,:,VLEV,1)=RESHAPE(DUMMY, (/IMAX,JMAX/) )
             ELSE
               PRINT *, 'ERROR in rdgrd (',TRIM(VNAME),') IRET=', IRET
               CALL ERREXIT (3)
             ENDIF
          ENDDO
          ! o3mr
          VNAME='o3mr' 
          VLEVTYP='mid layer' 
          DO VLEV=1, KMAX
          CALL NEMSIO_READRECV(GFILE,VNAME,VLEVTYP,VLEV, DUMMY2,0,IRET)
             IF ( IRET == 0 ) THEN
                GDATA%Q(:,:,VLEV,2)=RESHAPE(DUMMY, (/IMAX,JMAX/) )
             ELSE
               PRINT *, 'ERROR in rdgrd (',TRIM(VNAME),') IRET=', IRET
               CALL ERREXIT (3)
             ENDIF
          ENDDO
          ! clwmr
          VNAME='clwmr' 
          VLEVTYP='mid layer' 
          DO VLEV=1, KMAX
          CALL NEMSIO_READRECV(GFILE,VNAME,VLEVTYP,VLEV, DUMMY2,0,IRET)
             IF ( IRET == 0 ) THEN
                GDATA%Q(:,:,VLEV,3)=RESHAPE(DUMMY, (/IMAX,JMAX/) )
             ELSE
               PRINT *, 'ERROR in rdgrd (',TRIM(VNAME),') IRET=', IRET
               CALL ERREXIT (3)
             ENDIF
          ENDDO
          ! rwmr
          VNAME='rwmr' 
          VLEVTYP='mid layer' 
          DO VLEV=1, KMAX
          CALL NEMSIO_READRECV(GFILE,VNAME,VLEVTYP,VLEV, DUMMY2,0,IRET)
             IF ( IRET == 0 ) THEN
                GDATA%Q(:,:,VLEV,4)=RESHAPE(DUMMY, (/IMAX,JMAX/) )
             ELSE
               PRINT *, 'ERROR in rdgrd (',TRIM(VNAME),') IRET=', IRET
               CALL ERREXIT (3)
             ENDIF
          ENDDO
          ! icmr
          VNAME='icmr' 
          VLEVTYP='mid layer' 
          DO VLEV=1, KMAX
          CALL NEMSIO_READRECV(GFILE,VNAME,VLEVTYP,VLEV, DUMMY2,0,IRET)
             IF ( IRET == 0 ) THEN
                GDATA%Q(:,:,VLEV,5)=RESHAPE(DUMMY, (/IMAX,JMAX/) )
             ELSE
               PRINT *, 'ERROR in rdgrd (',TRIM(VNAME),') IRET=', IRET
               CALL ERREXIT (3)
             ENDIF
          ENDDO
          ! snmr
          VNAME='snmr' 
          VLEVTYP='mid layer' 
          DO VLEV=1, KMAX
          CALL NEMSIO_READRECV(GFILE,VNAME,VLEVTYP,VLEV, DUMMY2,0,IRET)
             IF ( IRET == 0 ) THEN
                GDATA%Q(:,:,VLEV,6)=RESHAPE(DUMMY, (/IMAX,JMAX/) )
             ELSE
               PRINT *, 'ERROR in rdgrd (',TRIM(VNAME),') IRET=', IRET
               CALL ERREXIT (3)
             ENDIF
          ENDDO
          ! grle
          VNAME='grle' 
          VLEVTYP='mid layer' 
          DO VLEV=1, KMAX
          CALL NEMSIO_READRECV(GFILE,VNAME,VLEVTYP,VLEV, DUMMY2,0,IRET)
             IF ( IRET == 0 ) THEN
                GDATA%Q(:,:,VLEV,7)=RESHAPE(DUMMY, (/IMAX,JMAX/) )
             ELSE
               PRINT *, 'ERROR in rdgrd (',TRIM(VNAME),') IRET=', IRET
               CALL ERREXIT (3)
             ENDIF
          ENDDO

        ENDIF ! TRIM(modelname)

!        call nemsio_close(gfile)
        print*,' complete reading data, inptyp=', inptyp
      endif !if (iret == 0)
!ccccccccccccccccccccccccccccccc
      PRINT*,'IKMAX=',IKMAX
      WRITE(6,210) (IDATE(I),I=1,4),FHOUR
210        FORMAT(5X,' INPUT DATE AND FCST HOUR ',4I5,F7.1/(2X,G13.6))
      print *,'idsl=',idsl,'nwave=',mwave,'idvm=',idvm, &
              'idvc=',idvc,'ntrac=',ntrac,'kmax=',kmax,'idate=',idate

      MAXWV=(MWAVE+1)*(MWAVE+2)/2
      MAXWV2=2*MAXWV
      MAXWV22=MAXWV2+1


      print *,"   after process input"

!      ALLOCATE ( WK_S1(MAXWV2,KMAX),WK_S2(MAXWV2,KMAX) )
!      ALLOCATE ( WK_G(IMAX,JMAX,KMAX),WK_G2(IMAX,JMAX,KMAX) )



      allocate (srlhi(imax,jmax))
      allocate (srlpi(imax,jmax))
      allocate (srlti(imax,jmax,kmax))
      allocate (srldi(imax,jmax,kmax))
      allocate (srlzi(imax,jmax,kmax))
      allocate (srlqi(imax,jmax,kmax))

      if (inptyp==2) then
        allocate (srlsphc(maxwv2))
        allocate (srlsphcl(maxwv2,kmax))
      endif

      print *,"   after allocate"

      ijmax=imax*jmax
      ijkmax=ijmax*kmax

      if(inptyp==2) then
	      srlsphc(:)=datao%hs(:)
        call sptez(0,mwave,4,imax,jmax,srlsphc,srlhi,1) 
        !c           call srangel (srlhi ,ijmax,' hs in  ',0)

        srlsphc(:)=datao%ps(:)
        call sptez(0,mwave,4,imax,jmax,srlsphc,srlpi,1) 
        !c           Call srangel (srlpi ,ijmax,' ps in  ',0)

        srlsphcl(:,:)=datao%t(:,:)
        call sptezm(0,mwave,4,imax,jmax,kmax,srlsphcl,srlti,1) 
        !c           Call srangel (srlti  ,ijkmax,' t in  ',0)

        srlsphcl(:,:)=datao%d(:,:)
        call sptezm(0,mwave,4,imax,jmax,kmax,srlsphcl,srldi,1) 
        !c           call srangel (srldi  ,ijkmax,' d in  ',0)

        srlsphcl(:,:)=datao%z(:,:)
        call sptezm(0,mwave,4,imax,jmax,kmax,srlsphcl,srlzi,1) 
        !c           call srangel (srlzi  ,ijkmax,' z in  ',0)
        srlsphcl(:,:)=datao%q(:,:,1)
        call sptezm(0,mwave,4,imax,jmax,kmax,srlsphcl,srlqi,1) 
        !c           call srangel (srlqi  ,ijkmax,' q in  ',0)

      elseif(inptyp==1) then

          srlhi(:,:)=gdata%zs(:,:)
          srlpi(:,:)=gdata%ps(:,:)
          srlti(:,:,:)=gdata%t(:,:,:)
          srldi(:,:,:)=gdata%u(:,:,:)
          srlzi(:,:,:)=gdata%v(:,:,:)
          srlqi(:,:,:)=gdata%q(:,:,:,1)

      endif

      ALLOCATE (PS1(IMAX,JMAX),T1(IMAX,JMAX,KMAX),Q1(IMAX,JMAX,KMAX))
      ALLOCATE (VOR1(IMAX,JMAX,KMAX),DIV1(IMAX,JMAX,KMAX))

      CALL GET_STM(PS1,T1,Q1,VOR1,DIV1,IMAX,JMAX,KMAX,NSEM,FACT,inptyp)

      call srangel (ps1,ijmax,' ps1 ',0)
      call srangel (t1,ijkmax,' t1 ',0)
      call srangel (q1,ijkmax,' q1 ',0)
      call srangel (vor1,ijkmax,' vor1 ',0)
      call srangel (div1,ijkmax,' div1 ',0)
      do k=1,kmax
        call srangel (t1(1,1,k),ijmax,' t1 ',k)
        call srangel (q1(1,1,k),ijmax,' q1 ',k)
        call srangel (vor1(1,1,k),ijmax,' vor1 ',k)
        call srangel (div1(1,1,k),ijmax,' div1 ',k)
      enddo

      print *,"   after get_stm"
      if (inptyp == 2) then
          ALLOCATE ( WK_S1(MAXWV2,KMAX) )
          ALLOCATE ( WORK_8(MAXWV22) )

          call SPTEZ(0,MWAVE,4,IMAX,JMAX,WORK_8,PS1,-1)
          datao%ps(:)=datao%ps(:)+WORK_8(:)


          CALL SPTEZM(0,MWAVE,4,IMAX,JMAX,KMAX,WK_S1,T1,-1)
          datao%t(:,:)=datao%t(:,:)+WK_S1(:,:)
          print *,"   after process t"


          CALL SPTEZM(0,MWAVE,4,IMAX,JMAX,KMAX,WK_S1,VOR1,-1)
	        datao%z(:,:)=datao%z(:,:)+WK_S1(:,:)
	        print *,"   after process vor"


          CALL SPTEZM(0,MWAVE,4,IMAX,JMAX,KMAX,WK_S1,DIV1,-1)
          datao%d(:,:)=datao%d(:,:)+WK_S1(:,:)
	        print *,"   after process div"


          CALL SPTEZM(0,MWAVE,4,IMAX,JMAX,KMAX,WK_S1,Q1,-1)
          datao%q(:,:,1)=datao%q(:,:,1)+WK_S1(:,:)
          print *,"   after process q"
!c      add these lines to zero out the vorticity as in the gfs code

          DO K=1,KMAX
	          datao%d(1,K)=0.
	          datao%z(1,K)=0.
          END DO
          DEALLOCATE ( WORK_8, WK_S1 )

          call sigio_swohdc(kunit, kfile, heado, datao, iret)
          print *, 'iret from sigio_swohdc =', iret,kunit

      elseif(inptyp == 1) then
        do i=1,imax
          do j=1,jmax
        !c           gdata%ps(i,j)=gdata%ps(i,j)+exp(PS1(i,j))*1000.
            gdata%ps(i,j)=exp(log(gdata%ps(i,j)/1000.)+PS1(i,j))*1000.
            do k=1,kmax
              gdata%t(i,j,k)=gdata%t(i,j,k)+T1(i,j,k)
              gdata%u(i,j,k)=gdata%u(i,j,k)+DIV1(i,j,k)
              gdata%v(i,j,k)=gdata%v(i,j,k)+VOR1(i,j,k)
              gdata%q(i,j,k,1)=gdata%q(i,j,k,1)+Q1(i,j,k)
           enddo
         enddo
        enddo
        print *,'datatype2=',ghead%gdatatype
        print *,'recname2=',gheadv%recname(1:3)

        IF ( TRIM(modelname) == 'GFS'  ) THEN
          call nemsio_gfsgrd_open(gfile,trim(kfile), &
           'write',nopdpvv,ghead,gheadv,iret=ios)
          if (ios /= 0) print *,'open nemsio write file,',trim(kfile), &
                                 'iret=',iret

          call nemsio_gfs_wrtgrd(gfile,gdata,iret=ios)
        ELSEIF ( TRIM(modelname) == 'FV3GFS'  ) THEN
          print *,'start writing FV3GFS nemsio'
             
          call nemsio_open(gfile,trim(kfile),'write',iret=ios)

          print *,'after  FV3GFS nemsio head writing'
          ! ugrd
          VNAME='ugrd' 
          VLEVTYP='mid layer' 
          DO VLEV=1, KMAX
            DUMMY2(:)=RESHAPE(GDATA%U(:,:,VLEV),(/IMAX*JMAX/) ) 
            CALL NEMSIO_WRITERECV(GFILE,VNAME,VLEVTYP,VLEV, DUMMY2,IRET=IRET)
          ENDDO
          ! vgrd
          VNAME='vgrd' 
          VLEVTYP='mid layer' 
          DO VLEV=1, KMAX
            DUMMY2(:)=RESHAPE(GDATA%V(:,:,VLEV),(/IMAX*JMAX/) ) 
            CALL NEMSIO_WRITERECV(GFILE,VNAME,VLEVTYP,VLEV, DUMMY2,IRET)
          ENDDO
          ! tmp
          VNAME='tmp' 
          VLEVTYP='mid layer' 
          DO VLEV=1, KMAX
            DUMMY2(:)=RESHAPE(GDATA%T(:,:,VLEV),(/IMAX*JMAX/) ) 
            CALL NEMSIO_WRITERECV(GFILE,VNAME,VLEVTYP,VLEV, DUMMY2,IRET)
          ENDDO
          ! hgt
          VNAME='hgt' 
          VLEVTYP='sfc' 
          DUMMY2(:)=RESHAPE(GDATA%ZS(:,:),(/IMAX*JMAX/) ) 
          CALL NEMSIO_WRITERECV(GFILE,VNAME,VLEVTYP,1, DUMMY2,IRET)
          ! pres
          VNAME='pres' 
          VLEVTYP='sfc' 
          DUMMY2(:)=RESHAPE(GDATA%PS(:,:),(/IMAX*JMAX/) ) 
          CALL NEMSIO_WRITERECV(GFILE,VNAME,VLEVTYP,1, DUMMY2,IRET)
          ! spfh
          VNAME='spfh' 
          VLEVTYP='mid layer' 
          DO VLEV=1, KMAX
            DUMMY2(:)=RESHAPE(GDATA%Q(:,:,VLEV,1),(/IMAX*JMAX/) ) 
            CALL NEMSIO_WRITERECV(GFILE,VNAME,VLEVTYP,VLEV, DUMMY2,IRET)
          ENDDO
          ! o3mr
          VNAME='o3mr' 
          VLEVTYP='mid layer' 
          DO VLEV=1, KMAX
            DUMMY(:)=RESHAPE(GDATA%Q(:,:,VLEV,2),(/IMAX*JMAX/) ) 
            CALL NEMSIO_WRITERECV(GFILE,VNAME,VLEVTYP,VLEV, DUMMY,IRET)
          ENDDO
          ! clwmr
          VNAME='clwmr' 
          VLEVTYP='mid layer' 
          DO VLEV=1, KMAX
            DUMMY2(:)=RESHAPE(GDATA%Q(:,:,VLEV,3),(/IMAX*JMAX/) ) 
            CALL NEMSIO_WRITERECV(GFILE,VNAME,VLEVTYP,VLEV, DUMMY2,IRET)
          ENDDO
          ! clwmr
          VNAME='clwmr' 
          VLEVTYP='mid layer' 
          DO VLEV=1, KMAX
            DUMMY2(:)=RESHAPE(GDATA%Q(:,:,VLEV,3),(/IMAX*JMAX/) ) 
            CALL NEMSIO_WRITERECV(GFILE,VNAME,VLEVTYP,VLEV, DUMMY2,IRET)
          ENDDO
          ! rwmr
          VNAME='rwmr' 
          VLEVTYP='mid layer' 
          DO VLEV=1, KMAX
            DUMMY2(:)=RESHAPE(GDATA%Q(:,:,VLEV,4),(/IMAX*JMAX/) ) 
            CALL NEMSIO_WRITERECV(GFILE,VNAME,VLEVTYP,VLEV, DUMMY2,IRET)
          ENDDO
          ! icmr
          VNAME='icmr' 
          VLEVTYP='mid layer' 
          DO VLEV=1, KMAX
            DUMMY2(:)=RESHAPE(GDATA%Q(:,:,VLEV,5),(/IMAX*JMAX/) ) 
            CALL NEMSIO_WRITERECV(GFILE,VNAME,VLEVTYP,VLEV, DUMMY2,IRET)
          ENDDO
          ! snmr
          VNAME='snmr' 
          VLEVTYP='mid layer' 
          DO VLEV=1, KMAX
            DUMMY2(:)=RESHAPE(GDATA%Q(:,:,VLEV,6),(/IMAX*JMAX/) ) 
            CALL NEMSIO_WRITERECV(GFILE,VNAME,VLEVTYP,VLEV, DUMMY2,IRET)
          ENDDO
          ! grle
          VNAME='grle' 
          VLEVTYP='mid layer' 
          DO VLEV=1, KMAX
            DUMMY2(:)=RESHAPE(GDATA%Q(:,:,VLEV,7),(/IMAX*JMAX/) ) 
            CALL NEMSIO_WRITERECV(GFILE,VNAME,VLEVTYP,VLEV, DUMMY2,IRET)
          ENDDO
        ENDIF
      

        if (ios /=0 ) then
          print *,'nemsio write grd,ret=',ios
        else
          print*,' complete writing data, inptyp=', inptyp
        endif
        call nemsio_close(gfile)

      endif
!c
	    call nemsio_finalize()
	    print *,"   after zero z and d"

      DEALLOCATE ( PS1,T1,Q1,VOR1,DIV1 )
      allocate (srlhd(imax,jmax))
      allocate (srlho(imax,jmax))
      allocate (srlpd(imax,jmax))
      allocate (srlpo(imax,jmax))
      allocate (srltd(imax,jmax,kmax))
      allocate (srlto(imax,jmax,kmax))
      allocate (srldd(imax,jmax,kmax))
      allocate (srldo(imax,jmax,kmax))
      allocate (srlzd(imax,jmax,kmax))
      allocate (srlzo(imax,jmax,kmax))
      allocate (srlqd(imax,jmax,kmax))
      allocate (srlqo(imax,jmax,kmax))
      if (inptyp ==2) then
        do nw=1,maxwv2
	        srlsphc(nw)=datao%hs(nw)
	      enddo
        call sptez(0,mwave,4,imax,jmax,srlsphc,srlho,1) 

        do nw=1,maxwv2
          srlsphc(nw)=datao%ps(nw)
        enddo
        call sptez(0,mwave,4,imax,jmax,srlsphc,srlpo,1) 

        do k=1,kmax
          do nw=1,maxwv2
            srlsphcl(nw,k)=datao%t(nw,k)
          enddo
        enddo
        call sptezm(0,mwave,4,imax,jmax,kmax,srlsphcl,srlto,1) 

        do k=1,kmax
          do nw=1,maxwv2
            srlsphcl(nw,k)=datao%d(nw,k)
          enddo
        enddo
        call sptezm(0,mwave,4,imax,jmax,kmax,srlsphcl,srldo,1) 

        do k=1,kmax
          do nw=1,maxwv2
            srlsphcl(nw,k)=datao%z(nw,k)
          enddo
        enddo
        call sptezm(0,mwave,4,imax,jmax,kmax,srlsphcl,srlzo,1) 

        do k=1,kmax
          do nw=1,maxwv2
            srlsphcl(nw,k)=datao%q(nw,k,1)
          enddo
        enddo
        call sptezm(0,mwave,4,imax,jmax,kmax,srlsphcl,srlqo,1) 

      elseif(inptyp==1) then

        srlho(:,:)=gdata%zs(:,:)
        srlpo(:,:)=gdata%ps(:,:)
        srlto(:,:,:)=gdata%t(:,:,:)
        srldo(:,:,:)=gdata%u(:,:,:)
        srlzo(:,:,:)=gdata%v(:,:,:)
        srlqo(:,:,:)=gdata%q(:,:,:,1)

      endif

      do j=1,jmax
        do i=1,imaxDUMMY_FV3GFS
          srlhd(i,j)=srlho(i,j)-srlhi(i,j)
          srlpd(i,j)=srlpo(i,j)-srlpi(i,j)
        enddo
      enddo

      do k=1,kmax
        do j=1,jmax
          do i=1,imax
            srltd(i,j,k)=srlto(i,j,k)-srlti(i,j,k)
            srldd(i,j,k)=srldo(i,j,k)-srldi(i,j,k)
            srlzd(i,j,k)=srlzo(i,j,k)-srlzi(i,j,k)
            srlqd(i,j,k)=srlqo(i,j,k)-srlqi(i,j,k)
          enddo
        enddo
      enddo

	    print *

      call srangel (srlhi,ijmax,'hs in   ',0)
      call srangel (srlhd,ijmax,'hs diff ',0)
      call srangel (srlho,ijmax,'hs out  ',0)

      print *

      call srangel (srlpi,ijmax,'ps in   ',0)
      call srangel (srlpd,ijmax,'ps diff ',0)
      call srangel (srlpo,ijmax,'ps out  ',0)

      print *

      call srangel (srlti,ijkmax,'t in   ',0)
      call srangel (srltd,ijkmax,'t diff ',0)
      call srangel (srlto,ijkmax,'t out  ',0)
      do k=1,kmax
        print *
        call srangel (srlti(1,1,k),ijmax,'t in   ',k)
        call srangel (srltd(1,1,k),ijmax,'t diff ',k)
        call srangel (srlto(1,1,k),ijmax,'t out  ',k)
      enddo
      print *

      call srangel (srldi,ijkmax,'d in   ',0)
      call srangel (srldd,ijkmax,'d diff ',0)
      call srangel (srldo,ijkmax,'d out  ',0)
      do k=1,kmax
        print *
        call srangel (srldi(1,1,k),ijmax,'d in   ',k)
        call srangel (srldd(1,1,k),ijmax,'d diff ',k)
        call srangel (srldo(1,1,k),ijmax,'d out  ',k)
      enddo
      print *

      call srangel (srlzi,ijkmax,'z in   ',0)
      call srangel (srlzd,ijkmax,'z diff ',0)
      call srangel (srlzo,ijkmax,'z out  ',0)
      do k=1,kmax
        print *
        call srangel (srlzi(1,1,k),ijmax,'z in   ',k)
        call srangel (srlzd(1,1,k),ijmax,'z diff ',k)
        call srangel (srlzo(1,1,k),ijmax,'z out  ',k)
      enddo
      print *

      call srangel (srlqi,ijkmax,'q in   ',0)
      call srangel (srlqd,ijkmax,'q diff ',0)
      call srangel (srlqo,ijkmax,'q out  ',0)
      do k=1,kmax
        print *
        call srangel (srlqi(1,1,k),ijmax,'q in   ',k)
        call srangel (srlqd(1,1,k),ijmax,'q diff ',k)
        call srangel (srlqo(1,1,k),ijmax,'q out  ',k)
      enddo
      print *


!c     call sigio_swdata(kunit, heado, datao, iret)
!c     print *, 'iret from sigio_swdata =', iret,kunit

!c
!c     all sigio_axdata(datao,iret)
!c     print *, 'iret for datao from sigio_axdata =', iret

!c     call sigio_sclose(kunit,iret)
!c     if (iret.ne.0) print *,'sigio_sclose failed,iret=',iret,kunit

	    print *,"   after write output"
!c      print*,'test3'

!c      DEALLOCATE ( WORK_8, WK_S1, WK_S2, WK_G )
!c      DEALLOCATE ( WK_G2, WORK_3,WORK_4 )
!c      DEALLOCATE ( PS1,T1,Q1,VOR1,DIV1 )

      CALL W3TAGE('GEFS_VORTEX_COMBINE')
!C
      STOP
      END
      
! This program only called by one pair member (n1,n2,n3,n4,n5) or (p1,p2,p3,p4,p5) 

       SUBROUTINE GET_STM(PS1,T1,Q1,VOR1,DIV1,IGU,JGU,KMAX,NSEM, &
     FACT,inptyp)

!        PARAMETER (IGU=384,JGU=190,KMAX=28)
        PARAMETER (MSTM=10)
!c       PARAMETER (FACT=0.05)        

        CHARACTER STNAME(MSTM,3)*3

        DIMENSION PS1(IGU,JGU),T1(IGU,JGU,KMAX),Q1(IGU,JGU,KMAX)
        DIMENSION VOR1(IGU,JGU,KMAX),DIV1(IGU,JGU,KMAX)

        REAL*4, ALLOCATABLE :: U1(:,:), V1(:,:), U2(:,:), V2(:,:)
        REAL*4, ALLOCATABLE :: ZS(:,:),PSL(:,:),TS(:,:)
        REAL*4, ALLOCATABLE :: T11(:,:,:,:), T12(:,:,:,:)

        DIMENSION TKE_C0(MSTM),TKE_PN(MSTM) 
        DIMENSION TT1_C0(MSTM),TT1_PN(MSTM),TP1_C0(MSTM),TP1_PN(MSTM) 
        REAL*4, ALLOCATABLE :: SAVE(:,:,:)
        integer inptyp

        MTV=4*KMAX+1
 
        ALLOCATE ( U1(IGU,JGU),V1(IGU,JGU),U2(IGU,JGU),V2(IGU,JGU) )
        ALLOCATE ( ZS(IGU,JGU),PSL(IGU,JGU),TS(IGU,JGU) )
        ALLOCATE ( T11(IGU,JGU,KMAX,MSTM),T12(IGU,JGU,KMAX,MSTM) )
        ALLOCATE ( SAVE(IGU,JGU,MTV) )

           print *,"     get_stm begin"
!        NSTM=5

!        READ(6,*)NSEM,NSTM

        NCT1=71
        NCT2=74
        NCT3=75

!        PRINT*,'NCT=',NCT1,NCT2,NCT3

        READ(NCT1)NSTM
        READ(NCT2)NSTM
        READ(NCT3)NSTM

        PRINT*,'NSEM=',NSEM,NSTM,IGU,JGU

        TP1_C0=0.
        TT1_C0=0.
        TKE_C0=0.

        DO KST=1,NSTM
          READ(NCT1)STNAME(KST,1)
          READ(NCT1)IWMIN1,IWMAX1,JWMIN1,JWMAX1
          READ(NCT1)               ! ZS
          READ(NCT1)               ! PSL

            U1=0.
          READ(NCT1)((U1(I,J),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)      ! P1
          
            DO J=JWMIN1,JWMAX1
            DO I=IWMIN1,IWMAX1
              TP1_C0(KST)=TP1_C0(KST)+U1(I,J)**2
            END DO
            END DO
          PRINT*,'KST,TP1_C0=',KST,TP1_C0(KST)

          READ(NCT1)               ! TS
          PRINT*,'STORM NAME=',STNAME(KST,1)
          PRINT*,'IX1,IX2,IY1,IY2=',IWMIN1,IWMAX1,JWMIN1,JWMAX1

            DO K=1,KMAX
              U1=0.
          READ(NCT1)((U1(I,J),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)    ! T'
              DO J=JWMIN1,JWMAX1
              DO I=IWMIN1,IWMAX1
                TT1_C0(KST)=TT1_C0(KST)+U1(I,J)**2
              END DO
              END DO
            END DO

            DO K=1,KMAX
              U1=0.
              V1=0.
          READ(NCT1)((U1(I,J),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)
          READ(NCT1)((V1(I,J),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)
              DO J=JWMIN1,JWMAX1
              DO I=IWMIN1,IWMAX1
                TKE_C0(KST)=TKE_C0(KST)+U1(I,J)**2+V1(I,J)**2
              END DO
              END DO
            END DO
          DO K=1,KMAX
            READ(NCT1)
          END DO
          PRINT*,'TKE_C0=',TKE_C0(KST)
        END DO

        TP1_PN=0.
        TT1_PN=0.
        TKE_PN=0.

        T11=0.
        T12=0.
    
        DO KST=1,NSTM
! read in member n 
          READ(NCT2)STNAME(KST,2)
          READ(NCT2)IWMIN1,IWMAX1,JWMIN1,JWMAX1
          READ(NCT2)    ! ZS
          READ(NCT2)    ! PSL
           U1=0.
          READ(NCT2)((U1(I,J),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)  ! P1
          READ(NCT2)    ! TS
          PRINT*,'STORM NAME=',STNAME(KST,2)
          PRINT*,'N IX1,IX2,IY1,IY2=',IWMIN1,IWMAX1,JWMIN1,JWMAX1
          READ(NCT2)((T11(I,J,1,KST),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)
!          write(99,*)IWMIN1,IWMAX1,JWMIN1,JWMAX1
!          WRITE(99,98)((T11(I,J,KST),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)
          DO K=2,KMAX
            READ(NCT2)((T11(I,J,K,KST),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)
          END DO
! read in member p
          READ(NCT3)STNAME(KST,3)
          READ(NCT3)IWMIN2,IWMAX2,JWMIN2,JWMAX2
          READ(NCT3)   ! ZS
          READ(NCT3)   ! PSL
           U2=0.
          READ(NCT3)((U2(I,J),I=IWMIN2,IWMAX2),J=JWMIN2,JWMAX2)
          READ(NCT3)   ! TS
          PRINT*,'STORM NAME=',STNAME(KST,3)
          PRINT*,'P IX1,IX2,IY1,IY2=',IWMIN2,IWMAX2,JWMIN2,JWMAX2
          READ(NCT3)((T12(I,J,1,KST),I=IWMIN2,IWMAX2),J=JWMIN2,JWMAX2)     !
          DO K=2,KMAX
            READ(NCT3)((T12(I,J,K,KST),I=IWMIN2,IWMAX2),J=JWMIN2,JWMAX2)
          END DO
!
          IF(STNAME(KST,2).NE.STNAME(KST,3))THEN
             PRINT*,'wrong pair'
             stop
          END IF

            DO J=1,JGU
            DO I=1,IGU
              TP1_PN(KST)=TP1_PN(KST)+ & 
                       (U2(I,J)-U1(I,J))**2
            END DO
            END DO
          PRINT*,'TP1_PN=',K,TP1_PN(KST)
 
          DO K=1,KMAX
            DO J=1,JGU
            DO I=1,IGU
              TT1_PN(KST)=TT1_PN(KST)+  &
                        (T12(I,J,K,KST)-T11(I,J,K,KST))**2
            END DO
            END DO
          END DO
          PRINT*,'TT1_PN=',K,TT1_PN(KST)

          DO K=1,KMAX
            U1=0.
            V1=0.
            U2=0.
            V2=0.
            READ(NCT2)((U1(I,J),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)
            READ(NCT2)((V1(I,J),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)
            READ(NCT3)((U2(I,J),I=IWMIN2,IWMAX2),J=JWMIN2,JWMAX2)
            READ(NCT3)((V2(I,J),I=IWMIN2,IWMAX2),J=JWMIN2,JWMAX2)
            DO J=1,JGU
            DO I=1,IGU
              TKE_PN(KST)=TKE_PN(KST)+   &
                       (U2(I,J)-U1(I,J))**2+(V2(I,J)-V1(I,J))**2
            END DO
            END DO
          END DO
          PRINT*,'TKE_PN=',K,TKE_PN(KST)
          DO K=1,KMAX
            READ(NCT2)
          END DO
          DO K=1,KMAX
            READ(NCT3)
          END DO
        END DO 

          PRINT*,'complete norm'

          rewind(NCT2)
          rewind(NCT3)

        READ(NCT2)NSTM
        READ(NCT3)NSTM
          
        SAVE=0.

        DO KST=1,NSTM

          READ(NCT2)STNAME(KST,2)
          READ(NCT2) IWMIN1,IWMAX1,JWMIN1,JWMAX1

          READ(NCT3)STNAME(KST,3)
          READ(NCT3)IWMIN2,IWMAX2,JWMIN2,JWMAX2
!
          IF(STNAME(KST,2).NE.STNAME(KST,3))THEN
             PRINT*,'wrong pair'
             stop
          END IF
           IF(STNAME(KST,1).NE.STNAME(KST,3))THEN
             PRINT*,'wrong control'
             stop
          END IF

          U1=0.
          U2=0.
          V1=0.
          V2=0.
         
!          write(96,*)IWMIN1,IWMAX1,JWMIN1,JWMAX1
!          WRITE(96,98)((T11(I,J,1,KST),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)
 
          READ(NCT2)((ZS(I,J),I=1,IGU),J=1,JGU)   ! ZS
          READ(NCT2)((PSL(I,J),I=1,IGU),J=1,JGU)     ! PSL
          READ(NCT2)((V1(I,J),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)
          READ(NCT2)((TS(I,J),I=1,IGU),J=1,JGU)      ! TS

!          CALL GET_LOGPS1(IGU,JGU,IWMIN1,IWMAX1,JWMIN1,JWMAX1,
!     &          ZS,PSL,V1,TS,T11(1,1,1,KST),U1)

!          READ(NCT3)((ZS(I,J),I=1,IGU),J=1,JGU)   ! ZS
!          READ(NCT3)((PSL(I,J),I=1,IGU),J=1,JGU)     ! PSL
          READ(NCT3)
          READ(NCT3)
          READ(NCT3)((V2(I,J),I=IWMIN2,IWMAX2),J=JWMIN2,JWMAX2)
!          READ(NCT3)((TS(I,J),I=1,IGU),J=1,JGU)      ! TS
          READ(NCT3)

!          CALL GET_LOGPS1(IGU,JGU,IWMIN2,IWMAX2,JWMIN2,JWMAX2,
!     &          ZS,PSL,V2,TS,T12(1,1,KST),U2)

            DO J=1,JGU
            DO I=1,IGU
              PS1(I,J)=FACT*(V2(I,J)-V1(I,J))* &
             SQRT(TP1_C0(KST)/(TP1_PN(KST)+1.E-10))
              T1(I,J,1)=FACT*(T12(I,J,1,KST)-T11(I,J,1,KST))* & 
            SQRT(TT1_C0(KST)/(TT1_PN(KST)+1.E-10))
            END DO
            END DO

!c uncomment next line temporarily
           write(97,*)IWMIN1,IWMAX1,JWMIN1,JWMAX1
!          WRITE(97,98)((PS1(I,J),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)
!          WRITE(99,98)((T1(I,J,1),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)
 98       FORMAT(10F15.3)

          CALL GET_LOGPS1(IGU,JGU,IWMIN1,IWMAX1,JWMIN1,JWMAX1, &  
            ZS,PSL,PS1,TS,T1(1,1,1),U1)

          DO J=1,JGU
          DO I=1,IGU
            SAVE(I,J,1)=SAVE(I,J,1)+U1(I,J)
!            SAVE(I,J,1)=U1(I,J)
          END DO
          END DO 
             
          DO K=2,MTV
            U1=0.
            U2=0.
            READ(NCT2)((U1(I,J),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)
            READ(NCT3)((U2(I,J),I=IWMIN2,IWMAX2),J=JWMIN2,JWMAX2)
            IF(K.LE.KMAX+1)THEN
              DO J=1,JGU
              DO I=1,IGU
                SAVE(I,J,K)=SAVE(I,J,K)+FACT* & 
            (U2(I,J)-U1(I,J))*SQRT(TT1_C0(KST)/(TT1_PN(KST)+1.E-10))
              END DO
              END DO
            ELSE IF(K.LE.(3*KMAX+1))THEN
              DO J=1,JGU
              DO I=1,IGU
                SAVE(I,J,K)=SAVE(I,J,K)+FACT*  &
            (U2(I,J)-U1(I,J))*SQRT(TKE_C0(KST)/(TKE_PN(KST)+1.E-20))
              END DO
              END DO
            ELSE
              DO J=1,JGU
              DO I=1,IGU
                SAVE(I,J,K)=0.
              END DO
              END DO
            END IF
          END DO
          
        END DO 

        DO J=1,JGU
        DO I=1,IGU
          PS1(I,J)=SAVE(I,J,1)            ! PS1 is the total log(Ps)
        END DO
        END DO
        DO K=1,KMAX
        DO J=1,JGU
        DO I=1,IGU
          T1(I,J,K)=SAVE(I,J,K+1)
          DIV1(I,J,K)=SAVE(I,J,2*K+KMAX)
          VOR1(I,J,K)=SAVE(I,J,2*K+KMAX+1)
          Q1(I,J,K)=SAVE(I,J,K+3*KMAX+1)
        END DO
        END DO
        END DO

        DEALLOCATE ( U1,V1,U2,V2 )
        DEALLOCATE ( ZS,PSL,TS )
        DEALLOCATE ( T11,T12 )
        DEALLOCATE ( SAVE )

           print *,"     get_stm end"
        END

         SUBROUTINE GET_LOGPS(IGU,JGU,IX1,IX2,JX1,JX2, &
              ZS,PSL,TS,U1)

      REAL*4 ZS(IGU,JGU),PSL(IGU,JGU)
      REAL*4 TS(IGU,JGU)
      REAL*4 U1(IGU,JGU)                ! output
!C
!C.. MAKE SFC PRESSURE FROM MSLP
!C
      G = 9.8
      R = 287.05
      GAMMA = 6.7*0.001
!C
! ENV data
      DO JH=1,JGU
      DO IH=1,IGU
      PMSL = ALOG(PSL(IH,JH)*1.0)
      A = (GAMMA * ZS(IH,JH)) / TS(IH,JH)
!c uncomment next line temporarily
       PRINT*,'IH,JH,APSL,TS=',IH,JH,A,PSL(IH,JH),TS(IH,JH)
      B = ALOG(1+A)
      C = (G*B)/(R*GAMMA)
      DD = PMSL - C
      D1 = EXP(DD)/1000.
      U1(IH,JH) = ALOG(D1)

      ENDDO
      ENDDO

      END

         SUBROUTINE GET_LOGPS1(IGU,JGU,IX1,IX2,JX1,JX2, &
              ZS,PSL,PS1,TS,T1,U1)
                           
      REAL*4  ZS(IGU,JGU),PSL(IGU,JGU)
      REAL*4  TS(IGU,JGU)
      REAL*4  U1(IGU,JGU)                ! output
      DIMENSION PS1(IGU,JGU),T1(IGU,JGU)
!C
!C.. MAKE SFC PRESSURE FROM MSLP
!C
      G = 9.8
      R = 287.05
      GAMMA = 6.7*0.001
!C
      DO JH=1,JGU
      DO IH=1,IGU
      PMSL = ALOG(PSL(IH,JH)+PS1(IH,JH))
      A = (GAMMA * ZS(IH,JH)) / (TS(IH,JH)+T1(IH,JH))
!      PRINT*,'IH,JH,A=',IH,JH,A
      B = ALOG(1+A)
      C = (G*B)/(R*GAMMA)
      DD = PMSL - C
      D1 = EXP(DD)/1000.
      U1(IH,JH) = ALOG(D1)
      ENDDO
      ENDDO
      PRINT*,'total ps',U1(21,21)
           
! ENV data
      DO JH=1,JGU
      DO IH=1,IGU
      PMSL = ALOG(PSL(IH,JH)*1.0)
      A = (GAMMA * ZS(IH,JH)) / TS(IH,JH)
      B = ALOG(1+A)
      C = (G*B)/(R*GAMMA)
      DD = PMSL - C
      D1 = EXP(DD)/1000.
      U1(IH,JH) = U1(IH,JH)-ALOG(D1)
!C      PRINT*,'IH,JH,APSL,TS=',IH,JH,A,PSL(IH,JH),TS(IH,JH),U1(IH,JH)
                           
      ENDDO
      ENDDO
      PRINT*,'ps pert',U1(21,21)
           
      END
          subroutine srangel(var,n,label,level)
!c print level, label, range, mean, avg dev, std dev, skew, n of zeroes
          dimension var(n)
          character*(*) label
          ptsn=n
          sa=0.0
          dmin=1.e40
          dmax=-1.e40
          nzero=0
          do j=1,n
            sa=sa+var(j)
            dmin=min(dmin,var(j))
            dmax=max(dmax,var(j))
	    if (var(j) .eq. 0.0) then
	      nzero=nzero+1
	    endif
          enddo
          avg=sa/ptsn
          sl=0.0
          sv=0.0
          do j=1,n
            sl=sl+abs(var(j)-avg)
            sv=sv+(var(j)-avg)**2
          enddo
          adev=sl/ptsn
          sdev=sqrt(sv/(ptsn-1))
          if (sdev.gt.0.0) then
            ss=0.0
            do j=1,n
              devn=(var(j)-avg)/sdev
              ss=ss+devn**3
            enddo
            skew=ss/ptsn
          else
            skew=0.0
          endif
	  if (level .eq. 0) then
	    print *,label,dmin,dmax,avg,adev,sdev,skew,nzero
	  else
	    print *,level,label,dmin,dmax,avg,adev,sdev,skew,nzero
	  endif
          return
          end


