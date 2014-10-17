      module date_def
!     use resol_def
      use machine
      implicit none
      save
      integer idate(4)
      real(kind=kind_evod) fhour,shour,thour,z00
      REAL(KIND=KIND_EVOD) ,ALLOCATABLE :: spdmax(:)

      end module date_def
