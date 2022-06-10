Module Settings

IMPLICIT NONE

PRIVATE

PUBLIC run_settings


type run_settings
    !npx y number processors
    ! ndx y size local dimension
    integer :: nproc, npx, npy, ndx, ndy, steps, iterations
    ! file names
    integer :: ngdx, ngdy, ngpx, ngpy !global x and y dimensions and processor numbers



end type run_settings


End Module Settings
