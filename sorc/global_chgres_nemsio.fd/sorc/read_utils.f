 SUBROUTINE READ_FV3GFS_ATMS_DATA_NEMSIO(GFILEI, GFSDATAI, GFSHEADI)

 USE NEMSIO_MODULE
 USE NEMSIO_GFS

 IMPLICIT NONE

 TYPE(NEMSIO_GFILE) :: GFILEI
 TYPE(NEMSIO_DBTA)  :: GFSDATAI
 TYPE(NEMSIO_HEAD)  :: GFSHEADI

 INTEGER            :: L, IRET
 INTEGER            :: LONB, LATB, LEVSI

 REAL, ALLOCATABLE  :: TMP(:)

 PRINT*,"READ ATMOSPHERIC FIELDS FROM FV3GFS NEMSIO FILE."

 LONB = GFSHEADI%DIMX
 LATB = GFSHEADI%DIMY
 LEVSI = GFSHEADI%DIMZ

 ALLOCATE(TMP(LONB*LATB))

 PRINT*,'READ HGT'
 CALL NEMSIO_READRECV(GFILEI, 'hgt', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%ZS = RESHAPE(TMP,(/LONB,LATB/))

 PRINT*,'READ PRES'
 CALL NEMSIO_READRECV(GFILEI, 'pres', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%PS = RESHAPE(TMP,(/LONB,LATB/))

 PRINT*,'READ U WINDS'
 DO L = 1, LEVSI
   CALL NEMSIO_READRECV(GFILEI, 'ugrd', 'mid layer', L, TMP, IRET=IRET)
   IF (IRET /= 0) GOTO 99
   GFSDATAI%U(:,:,L) = RESHAPE(TMP,(/LONB,LATB/))
 ENDDO

 PRINT*,'READ V WINDS'
 DO L = 1, LEVSI
   CALL NEMSIO_READRECV(GFILEI, 'vgrd', 'mid layer', L, TMP, IRET=IRET)
   IF (IRET /= 0) GOTO 99
   GFSDATAI%V(:,:,L) = RESHAPE(TMP,(/LONB,LATB/))
 ENDDO

 PRINT*,'READ T'
 DO L = 1, LEVSI
   CALL NEMSIO_READRECV(GFILEI, 'tmp', 'mid layer', L, TMP, IRET=IRET)
   IF (IRET /= 0) GOTO 99
   GFSDATAI%T(:,:,L) = RESHAPE(TMP,(/LONB,LATB/))
 ENDDO

 PRINT*,'READ Q'
 DO L = 1, LEVSI
   CALL NEMSIO_READRECV(GFILEI, 'spfh', 'mid layer', L, TMP, IRET=IRET)
   IF (IRET /= 0) GOTO 99
   GFSDATAI%Q(:,:,L,1) = RESHAPE(TMP,(/LONB,LATB/))
 ENDDO

 PRINT*,'READ O3'
 DO L = 1, LEVSI
   CALL NEMSIO_READRECV(GFILEI, 'o3mr', 'mid layer', L, TMP, IRET=IRET)
   IF (IRET /= 0) GOTO 99
   GFSDATAI%Q(:,:,L,2) = RESHAPE(TMP,(/LONB,LATB/))
 ENDDO

 PRINT*,'READ CLWMR'
 DO L = 1, LEVSI
   CALL NEMSIO_READRECV(GFILEI, 'clwmr', 'mid layer', L, TMP, IRET=IRET)
   IF (IRET /= 0) GOTO 99
   GFSDATAI%Q(:,:,L,3) = RESHAPE(TMP,(/LONB,LATB/))
 ENDDO

 DEALLOCATE(TMP)

 RETURN

 99 CONTINUE
 PRINT*,'FATAL ERROR '
 STOP

 END SUBROUTINE READ_FV3GFS_ATMS_DATA_NEMSIO

 SUBROUTINE READ_FV3GFS_NSST_DATA_NEMSIO (MASK_INPUT,NSST_INPUT,IMI,JMI, &
                 NUM_NSST_FIELDS,NSST_YEAR,NSST_MON,NSST_DAY,    & 
                 NSST_HOUR,NSST_FHOUR)

!-----------------------------------------------------------------------
! Subroutine: read nsst data from a fv3gfs nemsio file
!
! Author: George Gayno/EMC
!
! Abstract: Reads an fv3gfs nsst file in nemsio format.  Places data
!           in the "nsst_input" array in the order expected by routine
!           nsst_chgres.
!
! Input files: 
!    "chgres.inp.sfc" - Input nsst nemsio file.  Note: fv3gfs
!                       outputs sfc and nsst data in a single file.
!
! Output files:  none
!
! History:
!   2018-05-31   Gayno - Initial version
!
! Condition codes:  all non-zero codes are fatal
!   109 - bad open of nst file "chgres.inp.sfc"
!   110 - bad read of "chgres.inp.sfc" header
!   112 - wrong number of nsst records
!   113 - bad read of landmask record.
!   114 - bad read of an nst file record.
!-----------------------------------------------------------------------

 use nemsio_module

 implicit none

 integer, parameter      :: nrec=18

 character(len=3)        :: levtyp
 character(len=8)        :: recname(nrec)

 integer, intent(in)     :: imi, jmi, num_nsst_fields
 integer, intent(out)    :: nsst_year, nsst_mon
 integer, intent(out)    :: nsst_day, nsst_hour

 real,    intent(out)    :: mask_input(imi,jmi)
 real,    intent(out)    :: nsst_input(imi,jmi,num_nsst_fields)
 real,    intent(out)    :: nsst_fhour

 integer(nemsio_intkind) :: iret, lev, nframe
 integer(nemsio_intkind) :: idate(7), nfhour

 integer                 :: j

 real(nemsio_realkind),allocatable :: dummy(:)

 type(nemsio_gfile)      :: gfile

 data recname   /"xt      ", "xs      ", "xu      ", &
                 "xv      ", "xz      ", "zm      ", &
                 "xtts    ", "xzts    ", "dtcool  ", &
                 "zc      ", "c0      ", "cd      ", &
                 "w0      ", "wd      ", "dconv   ", &
                 "ifd     ", "tref    ", "qrain   " /

 print*,"- READ INPUT FV3GFS NSST DATA IN NEMSIO FORMAT"

 if (nrec /= num_nsst_fields) then
   print*,"- FATAL ERROR: bad number of nsst records."
   call errexit(112)
 endif

! note: fv3gfs surface and nsst fields are in a single file.
  
 call nemsio_open(gfile, "chgres.inp.sfc", "read", iret=iret)
 if (iret /= 0) then
   print*,"- FATAL ERROR: bad open of chgres.inp.sfc."
   print*,"- IRET IS ", iret
   call errexit(109)
 endif

 print*,"- READ FILE HEADER"
 call nemsio_getfilehead(gfile,iret=iret, &
           idate=idate,nfhour=nfhour)
 if (iret /= 0) then
   print*,"- FATAL ERROR: bad read of chgres.inp.sfc header."
   print*,"- IRET IS ", iret
   call errexit(110)
 endif

 nsst_year=idate(1)
 nsst_mon=idate(2)
 nsst_day=idate(3)
 nsst_hour=idate(4)
 nsst_fhour=float(nfhour)

 levtyp='sfc'
 lev=1
 nframe=0

 allocate(dummy(imi*jmi))

!-----------------------------------------------------------------------
! Read land mask into its own variable
!-----------------------------------------------------------------------

 call nemsio_readrecv(gfile,"land",levtyp,lev, &
           dummy,nframe,iret)

 if (iret /= 0) then
   print*,"- FATAL ERROR: bad read of landmask record."
   print*,"- IRET IS ", iret
   call errexit(113)
 endif

 mask_input = reshape (dummy, (/imi,jmi/))

!-----------------------------------------------------------------------
! Read remaining records into nsst_input data structure.
! Note: fv3gfs files do not contain 'ifd' or 'zm' records.  Set
! to default values per recommendation of nsst developer.
!-----------------------------------------------------------------------

 print*,"- READ DATA RECORDS"

 do j = 1, nrec
   if (trim(recname(j)) == 'zm') then  
     nsst_input(:,:,j) = 0.0
     cycle
   endif
   if (trim(recname(j)) == 'ifd') then
     nsst_input(:,:,j) = 1.0
     cycle
   endif
   call nemsio_readrecv(gfile,recname(j),levtyp,lev, &
             dummy,nframe,iret)
   if (iret /= 0) then
     print*,"- FATAL ERROR: bad read of chgres.inp.sfc."
     print*,"- IRET IS ", iret
     call errexit(114)
   endif
   nsst_input(:,:,j) = reshape (dummy, (/imi,jmi/))
 enddo

 deallocate(dummy)

 call nemsio_close(gfile,iret=iret)

 END SUBROUTINE READ_FV3GFS_NSST_DATA_NEMSIO

 SUBROUTINE READ_FV3GFS_SFC_DATA_NEMSIO (IMI, JMI, LSOILI, GFSDATAI)

 USE NEMSIO_GFS
 USE NEMSIO_MODULE

 INTEGER, INTENT(IN)  :: IMI, JMI, LSOILI

 TYPE(NEMSIO_DBTA)        :: GFSDATAI
 TYPE(NEMSIO_GFILE)       :: GFILEISFC

 INTEGER(NEMSIO_INTKIND)  :: IRET

 REAL(NEMSIO_REALKIND)    :: TMP(IMI*JMI)

 CALL NEMSIO_OPEN(GFILEISFC,'chgres.inp.sfc','read',IRET=IRET)
 IF(IRET /= 0)THEN
   PRINT*,"FATAL ERROR OPENING chgres.inp.sfc"
   PRINT*,"IRET IS ", IRET
   CALL ERREXIT(244)
 ENDIF

 CALL NEMSIO_READRECV(GFILEISFC, 'crain', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) THEN  ! not in the analysis files.
   TMP = 0.0
 ENDIF
 GFSDATAI%SRFLAG = RESHAPE(TMP, (/IMI,JMI/) )

 CALL NEMSIO_READRECV(GFILEISFC, 'ffhh', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%FFHH = RESHAPE(TMP, (/IMI,JMI/) )
 
 CALL NEMSIO_READRECV(GFILEISFC, 'ffmm', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%FFMM = RESHAPE(TMP, (/IMI,JMI/) )
 
 CALL NEMSIO_READRECV(GFILEISFC, 'f10m', '10 m above gnd', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%F10M = RESHAPE(TMP, (/IMI,JMI/) )
 
 CALL NEMSIO_READRECV(GFILEISFC, 'tmp', '2 m above gnd', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%T2M = RESHAPE(TMP, (/IMI,JMI/) )
 
 CALL NEMSIO_READRECV(GFILEISFC, 'spfh', '2 m above gnd', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%Q2M = RESHAPE(TMP, (/IMI,JMI/) )
 
 CALL NEMSIO_READRECV(GFILEISFC, 'fricv', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%UUSTAR = RESHAPE(TMP, (/IMI,JMI/) )
 
 CALL NEMSIO_READRECV(GFILEISFC, 'tprcp', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%TPRCP = RESHAPE(TMP, (/IMI,JMI/) )
 
 CALL NEMSIO_READRECV(GFILEISFC, 'alnsf', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%ALNSF = RESHAPE(TMP, (/IMI,JMI/) )
 
 CALL NEMSIO_READRECV(GFILEISFC, 'alnwf', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%ALNWF = RESHAPE(TMP, (/IMI,JMI/) )
 
 CALL NEMSIO_READRECV(GFILEISFC, 'alvsf', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%ALVSF = RESHAPE(TMP, (/IMI,JMI/) )
 
 CALL NEMSIO_READRECV(GFILEISFC, 'cnwat', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%CANOPY = RESHAPE(TMP, (/IMI,JMI/) )
 
 CALL NEMSIO_READRECV(GFILEISFC, 'veg', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%VFRAC = RESHAPE(TMP, (/IMI,JMI/) )
 
 CALL NEMSIO_READRECV(GFILEISFC, 'facsf', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%FACSF = RESHAPE(TMP, (/IMI,JMI/) )
 
 CALL NEMSIO_READRECV(GFILEISFC, 'facwf', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%FACWF = RESHAPE(TMP, (/IMI,JMI/) )
 
 CALL NEMSIO_READRECV(GFILEISFC, 'tmp', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%TSEA = RESHAPE(TMP, (/IMI,JMI/) )
 
 CALL NEMSIO_READRECV(GFILEISFC, 'land', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%SLMSK = RESHAPE(TMP, (/IMI,JMI/) )

 CALL NEMSIO_READRECV(GFILEISFC, 'sfcr', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%ZORL = RESHAPE(TMP, (/IMI,JMI/) )
 GFSDATAI%ZORL = GFSDATAI%ZORL * 100.0  ! convert to cm
 
 CALL NEMSIO_READRECV(GFILEISFC, 'orog', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%OROG = RESHAPE(TMP, (/IMI,JMI/) )
 
 CALL NEMSIO_READRECV(GFILEISFC, 'vtype', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%VTYPE = NINT(RESHAPE(TMP, (/IMI,JMI/) ))
 
 CALL NEMSIO_READRECV(GFILEISFC, 'sotyp', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%STYPE = NINT(RESHAPE(TMP, (/IMI,JMI/) ))
 
 CALL NEMSIO_READRECV(GFILEISFC, 'weasd', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%SHELEG = RESHAPE(TMP, (/IMI,JMI/) )

 CALL NEMSIO_READRECV(GFILEISFC, 'icec', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%FICE = RESHAPE(TMP, (/IMI,JMI/) )

 CALL NEMSIO_READRECV(GFILEISFC, 'icetk', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%HICE = RESHAPE(TMP, (/IMI,JMI/) )

 CALL NEMSIO_READRECV(GFILEISFC, 'snoalb', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%SNOALB = RESHAPE(TMP, (/IMI,JMI/) )

 CALL NEMSIO_READRECV(GFILEISFC, 'snod', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%SNWDPH = RESHAPE(TMP, (/IMI,JMI/) )
 GFSDATAI%SNWDPH = GFSDATAI%SNWDPH * 1000.0 ! convert to mm

 CALL NEMSIO_READRECV(GFILEISFC, 'sltyp', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%SLOPE = RESHAPE(TMP, (/IMI,JMI/) )

 CALL NEMSIO_READRECV(GFILEISFC, 'shdmin', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%SHDMIN = RESHAPE(TMP, (/IMI,JMI/) )

 CALL NEMSIO_READRECV(GFILEISFC, 'shdmax', 'sfc', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%SHDMAX = RESHAPE(TMP, (/IMI,JMI/) )

 CALL NEMSIO_READRECV(GFILEISFC, 'soilw', '0-10 cm down', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%SMC(:,:,1) = RESHAPE(TMP, (/IMI,JMI/) )

 CALL NEMSIO_READRECV(GFILEISFC, 'soilw', '10-40 cm down', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%SMC(:,:,2) = RESHAPE(TMP, (/IMI,JMI/) )

 CALL NEMSIO_READRECV(GFILEISFC, 'soilw', '40-100 cm down', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%SMC(:,:,3) = RESHAPE(TMP, (/IMI,JMI/) )

 CALL NEMSIO_READRECV(GFILEISFC, 'soilw', '100-200 cm down', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%SMC(:,:,4) = RESHAPE(TMP, (/IMI,JMI/) )

 CALL NEMSIO_READRECV(GFILEISFC, 'soill', '0-10 cm down', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%SLC(:,:,1) = RESHAPE(TMP, (/IMI,JMI/) )

 CALL NEMSIO_READRECV(GFILEISFC, 'soill', '10-40 cm down', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%SLC(:,:,2) = RESHAPE(TMP, (/IMI,JMI/) )

 CALL NEMSIO_READRECV(GFILEISFC, 'soill', '40-100 cm down', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%SLC(:,:,3) = RESHAPE(TMP, (/IMI,JMI/) )

 CALL NEMSIO_READRECV(GFILEISFC, 'soill', '100-200 cm down', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%SLC(:,:,4) = RESHAPE(TMP, (/IMI,JMI/) )

 CALL NEMSIO_READRECV(GFILEISFC, 'tmp', '0-10 cm down', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%STC(:,:,1) = RESHAPE(TMP, (/IMI,JMI/) )

 CALL NEMSIO_READRECV(GFILEISFC, 'tmp', '10-40 cm down', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%STC(:,:,2) = RESHAPE(TMP, (/IMI,JMI/) )

 CALL NEMSIO_READRECV(GFILEISFC, 'tmp', '40-100 cm down', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%STC(:,:,3) = RESHAPE(TMP, (/IMI,JMI/) )

 CALL NEMSIO_READRECV(GFILEISFC, 'tmp', '100-200 cm down', 1, TMP, IRET=IRET)
 IF (IRET /= 0) GOTO 99
 GFSDATAI%STC(:,:,4) = RESHAPE(TMP, (/IMI,JMI/) )

 CALL NEMSIO_CLOSE(GFILEISFC, IRET=IRET)

 RETURN

 99 CONTINUE
 PRINT*,"FATAL ERROR READING DATA FROM chgres.inp.sfc"
 PRINT*,"IRET IS ", IRET
 CALL ERREXIT(245)

 END SUBROUTINE READ_FV3GFS_SFC_DATA_NEMSIO
