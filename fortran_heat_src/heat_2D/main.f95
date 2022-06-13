program main

  ! test miniapp for adios2
  ! hard set local dimensions ndx, ndy - global calculated from these (easier than odd numbers per local domain)
  !

  use mpi
  use settings, only : run_settings!, define_local_settings
  use heat_transfer, only : data_object, apply_diffusion, exchange, initialise

  implicit none

  integer :: ierr, num_ranks, my_rank, &
             k_block_start, k_block_end, k_block_size, &
             t, iter, &
             num_args, i, itemp

  TYPE(run_settings) :: local_settings
  TYPE(data_object)  :: local_data

  call MPI_INIT(ierr)
  call MPI_COMM_RANK (MPI_COMM_WORLD, my_rank, ierr)
  call MPI_COMM_SIZE (MPI_COMM_WORLD, num_ranks, ierr)

  write(0,*) "Running with rank ", my_rank, " of ", num_ranks
  call local_settings%define_local_settings(my_rank)

  write(0,*) "Global x and y distribution = ", local_settings%ngpx, " ", local_settings%ngpy
  if (local_settings%ngpx * local_settings%ngpy .ne. num_ranks) then
     stop 'pass arguments so that decomposition in x * y = number of ranks'
  end if

  call initialise(local_settings, local_data)

  ! do more work and write more data
  do t = 1, local_settings%steps

      if (my_rank .eq. 0) write(0,*) "Step ", t
      ! increase this to do more work/comms per write
      do iter = 1, local_settings%iterations

          call apply_diffusion(local_settings, local_data)

         ! mpi
         call exchange(local_settings, local_data)
         ! BC

      end do !iter

      ! Write something
  end do !t

  call MPI_FINALIZE ( ierr )

end program
