      SUBROUTINE FOUR2GRID_thread(SYN_GR_A_1,SYN_GR_A_2,
     &  LON_DIM,LONS_LAT,LONL,LOT,lan,me)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      use machine
      IMPLICIT NONE
!!
      INTEGER              INIT,LOT,LONL
      INTEGER              lan,me
      INTEGER              IBMSIGN
      REAL(KIND=kind_dbl_prec) AUX1CRS(42002)
      REAL(KIND=KIND_EVOD) SCALE_IBM
      INTEGER              LON_DIM,LONS_LAT
      REAL(KIND=KIND_EVOD) SYN_GR_A_1(LONL*LOT)
      REAL(KIND=KIND_EVOD) SYN_GR_A_2(LONL*LOT)
!________________________________________________________
      INTEGER              NUM_THREADS
      INTEGER              NVAR_THREAD_MAX
      INTEGER              NVAR_1,NVAR_2
      INTEGER              THREAD
      INTEGER              lot_thread
      INTEGER              indbeg
!________________________________________________________
      NUM_THREADS=min(NUM_PARTHDS(),lot)
 
      NVAR_THREAD_MAX=(lot+NUM_THREADS-1)/NUM_THREADS
 
!100   format('thread=',i2,2x,
!    & ' lot=',i3,2x,' indbeg=',i6,2x,' lan=',i2,2x,' me_fou=',i3,
!    & ' lon_dim=',i3,2x,' lons_lat=',i3)
!200   format('thread=',i2,2x,' nvar_1=',i6,2x,' nvar_2=',i6,2x
!    &,' lot=',i3,2x,' indbeg=',i6,2x,' lan=',i2,2x,' me_fou=',i3,
!    & ' lon_dim=',i3,2x,' lons_lat=',i3)
      if ( kind_evod .eq. 8 ) then !------------------------------------
!$OMP PARALLEL DO SHARED(syn_gr_a_1,syn_gr_a_2,lon_dim,lons_lat)
!$OMP+SHARED(lot,NUM_THREADS,NVAR_THREAD_MAX)
!$OMP+PRIVATE(THREAD,NVAR_1,NVAR_2,lot_thread,indbeg,AUX1CRS)
!$OMP+PRIVATE(ibmsign,scale_ibm,INIT)
 
         DO THREAD=1,NUM_THREADS   ! START OF THREAD LOOP ..............
            NVAR_1=(THREAD-1)*NVAR_THREAD_MAX+1
            NVAR_2=MIN(NVAR_1+NVAR_THREAD_MAX-1,lot)
            lot_thread=NVAR_2 - NVAR_1 +1
         indbeg=1+LON_DIM*(NVAR_1-1)
!sela    print 200,thread,nvar_1,nvar_2,lot_thread,indbeg,lan,me,lon_dim
!sela& ,lons_lat
!sela    print 100,thread,lot_thread,indbeg,lan,me,lon_dim
!sela& ,lons_lat
 
         INIT=1
         IBMSIGN=-1
         SCALE_IBM=1.0D0
         CALL DCRFT(INIT,
     X              SYN_GR_A_1(indbeg)   ,LON_DIM/2,
     X              SYN_GR_A_2(indbeg)   ,LON_DIM,
     X              LONS_LAT,LOT_thread,IBMSIGN,SCALE_IBM,
     X              AUX1CRS,22000,
     X              AUX1CRS(22001),20000)
         INIT=0
         CALL DCRFT(INIT,
     X              SYN_GR_A_1(indbeg)   ,LON_DIM/2,
     X              SYN_GR_A_2(indbeg)   ,LON_DIM,
     X              LONS_LAT,LOT_thread,IBMSIGN,SCALE_IBM,
     X              AUX1CRS,22000,
     X              AUX1CRS(22001),20000)
 
      enddo  ! fin thread loop .........................................
      else !------------------------------------------------------------
!$OMP PARALLEL DO SHARED(syn_gr_a_1,syn_gr_a_2,lon_dim,lons_lat)
!$OMP+SHARED(lot,NUM_THREADS,NVAR_THREAD_MAX)
!$OMP+PRIVATE(THREAD,NVAR_1,NVAR_2,lot_thread,indbeg,AUX1CRS)
!$OMP+PRIVATE(ibmsign,scale_ibm,INIT)
 
         DO THREAD=1,NUM_THREADS   ! START OF THREAD LOOP ..............
            NVAR_1=(THREAD-1)*NVAR_THREAD_MAX+1
            NVAR_2=MIN(NVAR_1+NVAR_THREAD_MAX-1,lot)
            lot_thread=NVAR_2 - NVAR_1 +1
         indbeg=1+LON_DIM*(NVAR_1-1)
!sela    print 200,thread,nvar_1,nvar_2,lot_thread,indbeg,lan,me,lon_dim
!sela& ,lons_lat
!sela    print 100,thread,lot_thread,indbeg,lan,me,lon_dim
!sela& ,lons_lat
 
         INIT=1
         IBMSIGN=-1
         SCALE_IBM=1.0D0
         CALL SCRFT(INIT,
     X              SYN_GR_A_1(indbeg)   ,LON_DIM/2,
     X              SYN_GR_A_2(indbeg)   ,LON_DIM,
     X              LONS_LAT,LOT_thread,IBMSIGN,SCALE_IBM,
     X              AUX1CRS,22000,
     X              AUX1CRS(22001),20000,
     X              AUX1CRS(22001),0)
         INIT=0
         CALL SCRFT(INIT,
     X              SYN_GR_A_1(indbeg)   ,LON_DIM/2,
     X              SYN_GR_A_2(indbeg)   ,LON_DIM,
     X              LONS_LAT,LOT_thread,IBMSIGN,SCALE_IBM,
     X              AUX1CRS,22000,
     X              AUX1CRS(22001),20000,
     X              AUX1CRS(22001),0)
 
      enddo  ! fin thread loop .........................................
      endif !-----------------------------------------------------------
!!
      RETURN
      END
      SUBROUTINE GRID2FOUR_thread(ANL_GR_A_2,ANL_GR_A_1,
     &                     LON_DIM,LONS_LAT,LONL,LOT)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      use machine
      IMPLICIT NONE
!!
      INTEGER              INIT,LOT,LONL
      INTEGER              IBMSIGN
      REAL(KIND=kind_dbl_prec) AUX1CRS(42002)
      REAL(KIND=KIND_EVOD) SCALE_IBM,RONE
      INTEGER              LON_DIM,LONS_LAT
      REAL(KIND=KIND_EVOD) ANL_GR_A_1(LONL*LOT)
      REAL(KIND=KIND_EVOD) ANL_GR_A_2(LONL*LOT)
!________________________________________________________
      INTEGER              NUM_THREADS
      INTEGER              NVAR_THREAD_MAX
      INTEGER              NVAR_1,NVAR_2
      INTEGER              THREAD
      INTEGER              lot_thread
      INTEGER              indbeg
!________________________________________________________
      NUM_THREADS=min(NUM_PARTHDS(),lot)
 
      NVAR_THREAD_MAX=(lot+NUM_THREADS-1)/NUM_THREADS
      if ( kind_evod .eq. 8 ) then !------------------------------------
!$OMP PARALLEL DO SHARED(anl_gr_a_1,anl_gr_a_2,lon_dim,lons_lat)
!$OMP+SHARED(lot,NUM_THREADS,NVAR_THREAD_MAX)
!$OMP+SHARED(ibmsign,scale_ibm,rone)
!$OMP+PRIVATE(THREAD,NVAR_1,NVAR_2,lot_thread,indbeg,init,AUX1CRS)
 
         DO THREAD=1,NUM_THREADS   ! START OF THREAD LOOP ..............
            NVAR_1=(THREAD-1)*NVAR_THREAD_MAX+1
            NVAR_2=MIN(NVAR_1+NVAR_THREAD_MAX-1,lot)
            lot_thread=NVAR_2 - NVAR_1 +1
 
         indbeg=1+LON_DIM*(NVAR_1-1)
 
         INIT=1
         IBMSIGN=1
         RONE=1.0D0
         SCALE_IBM=RONE/LONS_LAT
         CALL DRCFT(INIT,
     X              ANL_GR_A_2(indbeg),   LON_DIM,
     X              ANL_GR_A_1(indbeg),   LON_DIM/2,
     X              LONS_LAT,LOT_thread,IBMSIGN,SCALE_IBM,
     X              AUX1CRS,22000,
     X              AUX1CRS(22001),20000)
         INIT=0
         CALL DRCFT(INIT,
     X              ANL_GR_A_2(indbeg),   LON_DIM,
     X              ANL_GR_A_1(indbeg),   LON_DIM/2,
     X              LONS_LAT,LOT_thread,IBMSIGN,SCALE_IBM,
     X              AUX1CRS,22000,
     X              AUX1CRS(22001),20000)
 
      enddo  ! fin thread loop .........................................
      else !------------------------------------------------------------
!$OMP PARALLEL DO SHARED(anl_gr_a_1,anl_gr_a_2,lon_dim,lons_lat)
!$OMP+SHARED(lot,NUM_THREADS,NVAR_THREAD_MAX)
!$OMP+SHARED(ibmsign,scale_ibm,rone)
!$OMP+PRIVATE(THREAD,NVAR_1,NVAR_2,lot_thread,indbeg,init,AUX1CRS)
 
         DO THREAD=1,NUM_THREADS   ! START OF THREAD LOOP ..............
            NVAR_1=(THREAD-1)*NVAR_THREAD_MAX+1
            NVAR_2=MIN(NVAR_1+NVAR_THREAD_MAX-1,lot)
            lot_thread=NVAR_2 - NVAR_1 +1
 
         indbeg=1+LON_DIM*(NVAR_1-1)
 
         INIT=1
         IBMSIGN=1
         RONE=1.0D0
         SCALE_IBM=RONE/LONS_LAT
         CALL SRCFT(INIT,
     X              ANL_GR_A_2(indbeg),   LON_DIM,
     X              ANL_GR_A_1(indbeg),   LON_DIM/2,
     X              LONS_LAT,LOT_thread,IBMSIGN,SCALE_IBM,
     X              AUX1CRS,22000,
     X              AUX1CRS(22001),20000,
     X              AUX1CRS(22001),0)
         INIT=0
         CALL SRCFT(INIT,
     X              ANL_GR_A_2(indbeg),   LON_DIM,
     X              ANL_GR_A_1(indbeg),   LON_DIM/2,
     X              LONS_LAT,LOT_thread,IBMSIGN,SCALE_IBM,
     X              AUX1CRS,22000,
     X              AUX1CRS(22001),20000,
     X              AUX1CRS(22001),0)
 
      enddo  ! fin thread loop .........................................
      endif !-----------------------------------------------------------
!!
      RETURN
      END
