program main

  ! test miniapp for adios2
  ! hard set local dimensions ndx, ndy - global calculated from these (easier than odd numbers per local domain)
  !

  use mpi
  use settings, only : run_settings!, initialise_settings
  use heat_transfer, only : data_object, apply_diffusion, exchange, initialise

  implicit none

  integer :: ierr, num_ranks, my_rank, &
             k_block_start, k_block_end, k_block_size, &
             t, iter, &
             num_args, i, itemp
  character(len=12), dimension(:), allocatable :: args

  TYPE(run_settings) :: local_settings
  TYPE(data_object)  :: local_data

  call MPI_INIT(ierr)
  call MPI_COMM_RANK (MPI_COMM_WORLD, my_rank, ierr)
  call MPI_COMM_SIZE (MPI_COMM_WORLD, num_ranks, ierr)

  num_args = command_argument_count()
  allocate(args(num_args))
  do i = 1, num_args
     call get_command_argument(i, args(i))
  end do

  write(0,*) "Running with rank ", my_rank, " of ", num_ranks
  !call initialise_settings(my_rank)
!  local_settings%nproc      = my_rank
  local_settings%npx        = 10
  local_settings%npy        = 5
  local_settings%ndx        = 10
  local_settings%ndy        = 5
  local_settings%steps      = 10
  local_settings%iterations = 10

  !Use ridiculous fortran 'internal IO'???
  read( args(1), '(i6)' ) itemp
  local_settings%ngpx       = itemp
  read( args(2), '(i6)' ) itemp
  local_settings%ngpy       = itemp

  write(0,*) "Global x and y distribution = ", local_settings%ngpx, " ", local_settings%ngpy
  if (local_settings%ngpx * local_settings%ngpy .ne. num_ranks) then
     stop 'pass arguments so that decomposition in x * y = number of ranks'
  end if

  call initialise(local_settings, local_data)

  do t = 1, local_settings%steps

      if (my_rank .eq. 0) write(0,*) "Step ", t
      do iter = 1, local_settings%iterations

          call apply_diffusion(local_settings, local_data)

         ! mpi
         ! BC

      end do !iter

      ! Write something
  end do !t

  call MPI_FINALIZE ( ierr )

end program
