       module matrix_sig_def
!     use resol_def
      use machine
      implicit none
      save
       real(kind=kind_evod) , allocatable ::
     .  tor_sig(:), d_m(:,:,:),dm205(:,:,:)
       end module matrix_sig_def
