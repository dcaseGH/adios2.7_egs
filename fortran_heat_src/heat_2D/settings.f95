Module Settings

IMPLICIT NONE

PRIVATE

PUBLIC run_settings!, define_local_settings


type run_settings
    !npx y number processors
    ! ndx y size local dimension
    integer :: nproc, npx, npy, ndx, ndy, steps, iterations
    ! file names
    integer :: ngdx, ngdy, ngpx, ngpy !global x and y dimensions and processor numbers

    contains
    procedure :: define_local_settings


end type run_settings

contains

Subroutine define_local_settings(self, my_rank)

  ! define local and global dimensions, steps, mpi stuff etc
  ! pass in command line args and parse them

  class(run_settings), intent (INOUT) :: self
  character(len=12), dimension(:), allocatable :: args
  integer, intent(IN) :: my_rank
  integer :: itemp, num_args, i

  write(0,*) "Defining local settings"
  self%npx                  = 10
  self%npy                  = 5
  self%ndx                  = 10
  self%ndy                  = 5
  self%steps                = 10
  self%iterations           = 10
  self%nproc      = my_rank

  num_args = command_argument_count()
  allocate(args(num_args))
  do i = 1, num_args
     call get_command_argument(i, args(i))
  end do

  !Use ridiculous fortran 'internal IO'???
  read( args(1), '(i6)' ) itemp
  self%ngpx       = itemp
  read( args(2), '(i6)' ) itemp
  self%ngpy       = itemp

End Subroutine define_local_settings


End Module Settings
