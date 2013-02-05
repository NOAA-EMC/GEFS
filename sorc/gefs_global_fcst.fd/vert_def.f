      module vert_def
!     use resol_def
      use machine
      implicit none

      save
      REAL(KIND=KIND_EVOD) ,ALLOCATABLE :: am(:,:),bm(:,:),cm(:,:),
     . dm(:,:,:),tor(:), si(:),sl(:),del(:),rdel2(:),ci(:),
     . cl(:),tov(:),sv(:)
      real(kind=kind_evod), allocatable :: slk(:), sik(:)
      end module vert_def
