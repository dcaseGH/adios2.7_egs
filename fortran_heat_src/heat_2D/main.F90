program main

  ! test miniapp for adios2
  ! hard set local dimensions ndx, ndy - global calculated from these (easier than odd numbers per local domain)
  !

  use mpi
  use adios2
  use settings, only : run_settings!, define_local_settings
  use heat_transfer, only : data_object, apply_diffusion, exchange, initialise, apply_heat

  implicit none

  integer :: ierr, num_ranks, my_rank, &
             k_block_start, k_block_end, k_block_size, &
             t, iter, &
             num_args, i, itemp

  ! stuff to run calc
  TYPE(run_settings) :: local_settings
  TYPE(data_object)  :: local_data
  double precision   :: edge_temp

  !declare adios variables
  type(adios2_adios) :: adios
  type(adios2_io) :: ioPut, ioGet
  type(adios2_variable) :: temperature, var_temperatures
  type(adios2_engine) :: bpWriter, hdf5Writer!, hdf5Reader !Do I want a reader?
  integer(kind=8), dimension(2) :: ishape, istart, icount

  call MPI_INIT(ierr)
  call MPI_COMM_RANK (MPI_COMM_WORLD, my_rank, ierr)
  call MPI_COMM_SIZE (MPI_COMM_WORLD, num_ranks, ierr)

  write(0,*) "Running with rank ", my_rank, " of ", num_ranks
  call local_settings%define_local_settings(my_rank)

  call initialise(local_settings, local_data)
  write(0,*) "x and y distribution = ", local_settings%npx, " ", local_settings%npy
  if (local_settings%npx * local_settings%npy .ne. num_ranks) then
     stop 'pass arguments so that decomposition in x * y = number of ranks'
  end if

  call adios2_init( adios, MPI_COMM_WORLD, ierr )
  call adios2_declare_io( ioPut, adios, 'TempWrite', ierr )
!     !declare hdf5
  call adios2_set_engine(ioPut, 'HDF5', ierr)

  icount = (/         local_settings%ndx, local_settings%ndy     /)
  istart = (/ my_rank * local_settings%ndx ,    0  /)
  ishape = (/ num_ranks* local_settings%ndx,   local_settings%ndy  /)

  !var_temperatures has global shape and local start and count
  call adios2_define_variable( var_temperatures, ioPut, 'temperatures', &
                               adios2_type_dp, 2, &
                               ishape, istart, icount, adios2_constant_dims, &
                               ierr )

  call adios2_open( bpWriter, ioPut, 'initial_dat.hdf5', adios2_mode_write, &
                    ierr )
  call adios2_put( bpWriter, var_temperatures, local_data%Temp, ierr )

  call adios2_close( bpWriter, ierr )

  edge_temp = 293.0 !set edges
  ! do more work and write more data
  do t = 1, local_settings%steps

      if (my_rank .eq. 0) write(0,*) "Step ", t
      ! increase this to do more work/comms per write
      do iter = 1, local_settings%iterations

         ! operator
         call apply_diffusion(local_settings, local_data)
         ! mpi
         call exchange(local_settings, local_data)
         ! BC
         call apply_heat(edge_temp, local_settings, local_data)

      end do !iter

      ! Write something
  end do !t

  call MPI_FINALIZE ( ierr )

end program
