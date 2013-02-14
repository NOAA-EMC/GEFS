c
c***********************************************************************
c
      SUBROUTINE OUT_PARA(dt)
c
c***********************************************************************
c
      use resol_def
      use layout1
      implicit none
c
      real dt
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
      IF (me.eq.0) THEN
        write(6,*)
        write(6,*)'This is the reduced-grid code with new phys'
        write(6,9100) JCAP,LEVS
        write(6,9110) NODES
cjfe    write(6,9115) NUM_PARTHDS()
        write(6,9120) LEVS,LATG,LONF
        write(6,9140) dt
c        write(6,9130)
      ENDIF
 
 9100 format( /5x,'   T',i3,'-',i2,' FORECAST MODEL',
     .        /5x,' ===========================')
 9110 format(  5x,' Number of MPI Tasks:   ',i4)
 9115 format(  5x,' Number of Threads:     ',i4)
 9120 format( /5x,' Number of levels:      ',i4,
     .        /5x,' Number of latitudes:   ',i4,
     .        /5x,' Number of longitudes:  ',i4)
 9130 format(//'                               PERFORMANCE'
     .        /'                             Times in seconds'
     .        /'                        Rates in Mbyte/s or Mflop/s'
     .       //'         TRANSPOSE    GDSUM',
     .         '         FFT       FLNS2FG      FL2FLN    FIDI/PHYS'
     .        /' Step   Time  Rate  Time  Rate ',
     .         ' Time  Rate  Time  Rate  Time  Rate  Time  Rate '
     .        /' --------------------------------------',
     .         '---------------------------------------')
 9140 format( /5x,' Timestep:   ',F6.1,' seconds')
 
      RETURN
      END
c
c***********************************************************************
c
      SUBROUTINE BAR3(FE,FO,name,levl)
c
c***********************************************************************
c
      use resol_def
      use layout1
      implicit none
      real FE(len_trie_ls,2,levs),ffbar
      real FO(len_trio_ls,2,levs)
      integer n,l,joff,i,jlev,levl
      character*3 name
c
      do jlev=1,levl
        ffbar=0.
        DO i=1,len_trie_ls
          ffbar=ffbar+FE(i,1,jlev)*FE(i,1,jlev)
        ENDDO
        DO i=1,len_trie_ls
          ffbar=ffbar+FE(i,2,jlev)*FE(i,2,jlev)
        ENDDO
        ffbar=ffbar/2.
        DO i=1,len_trio_ls
          ffbar=ffbar+FO(i,1,jlev)*FO(i,1,jlev)
        ENDDO
        DO i=1,len_trio_ls
          ffbar=ffbar+FO(i,2,jlev)*FO(i,2,jlev)
        ENDDO
        ffbar=sqrt(ffbar)
        write(*,101)'rms ',name,' lev ',jlev,' = ',ffbar
      enddo
c
 101  format(3A,i3,A,ES24.17)
c
      RETURN
      END
