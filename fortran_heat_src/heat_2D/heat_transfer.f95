Module heat_transfer

  use settings, only : run_settings
  use mpi

IMPLICIT NONE

PRIVATE !data_object

PUBLIC data_object, apply_diffusion, initialise, exchange

TYPE data_object
  double precision, Allocatable, Dimension(:,:) :: Temp, temp_Temp
  double precision :: omega

end type data_object

CONTAINS

SUBROUTINE apply_diffusion(local_settings, local_data)

   ! Numerical gradient

   TYPE(run_settings), intent(IN)    :: local_settings
   TYPE(data_object), intent(INOUT)  :: local_data

   integer :: i, j

   do j = 1, local_settings%ndy
      do i = 1, local_settings%ndx

         local_data%temp_Temp(i,j) = (local_data%omega / 4) * ( local_data%temp_Temp(i+1,j) + &
                                                                local_data%temp_Temp(i-1,j) + &
                                                                local_data%temp_Temp(i,j+1) + &
                                                                local_data%temp_Temp(i,j-1) ) &
                                   + (1. - local_data%omega) *  local_data%temp_Temp(i,j)
      end do !i
   end do !j

   local_data%temp = local_data%temp_Temp

END SUBROUTINE apply_diffusion

SUBROUTINE initialise(local_settings, local_data)

   ! just invent some data

   TYPE(run_settings), intent(IN)    :: local_settings
   TYPE(data_object), intent(INOUT)  :: local_data

   integer :: i, j
   double precision :: hx, hy, x, y

   local_data%omega = 0.8 !because it needs to be something...

   ALLOCATE(local_data%Temp(0:local_settings%ndx+1,0:local_settings%ndy+1))
   ALLOCATE(local_data%temp_Temp(0:local_settings%ndx+1,0:local_settings%ndy+1))

   local_data%temp      = 0.
   local_data%temp_Temp = 0.

   hx = 2.0 * 4.0 * atan(1.0) / local_settings%ngdx
   hy = 2.0 * 4.0 * atan(1.0) / local_settings%ngdy

   do j=local_settings%ndy * local_settings%npy, local_settings%ndy * (local_settings%npy + 1) + 1
      y = 0.0 + hy * (j - 1);
      do i=local_settings%ndx * local_settings%npx, local_settings%ndx * (local_settings%npx + 1) + 1
!      do i=0, local_settings%ndx + 1
         x = 0. + hx * (i-1)
         local_data%Temp(i,j) = cos(8 * x) + cos(6 * x) - cos(4 * x) + cos(2 * x) &
                              - cos(x) + sin(8 * y) - sin(6 * y) + sin(4 * y) &
                              - sin(2 * y) + sin(y)
      end do
   end do

END SUBROUTINE initialise

SUBROUTINE exchange(local_settings, local_data)

   TYPE(run_settings), intent(IN)    :: local_settings
   TYPE(data_object), intent(INOUT)  :: local_data

   !mpi datatypes are integers in fortran?
   integer :: ierr, mpi_vector, tag
   !TYPE(MPI_Status) :: mystatus


   ! do I want x lots of y or y lots of x??
   call MPI_Type_vector(local_settings%ndx+2, 1, local_settings%ndy + 2, MPI_REAL8, mpi_vector, ierr)
   call MPI_Type_commit(mpi_vector, ierr)

    !Exchange ghost cells, in the order left-right-up-down

    ! send to left + receive from right
    tag = 1
!    if (local_settings% rank_left .ge. 0) THEN
!    {
!        write(0,*) "Rank " , local_settings%rank << " send left to rank "
!        //          << m_s.rank_left << std::endl;
!        MPI_Send(m_TCurrent[0] + 1, 1, tColumnVector, m_s.rank_left, tag, comm);
!    }
!    if (m_s.rank_right >= 0)
!    {
!        // std::cout << "Rank " << m_s.rank << " receive from right from rank "
!        //          << m_s.rank_right << std::endl;
!        MPI_Recv(m_TCurrent[0] + (m_s.ndy + 1), 1, tColumnVector,
!                 m_s.rank_right, tag, comm, &status);
!    }
!
!    // send to right + receive from left
!    tag = 2;
!    if (m_s.rank_right >= 0)
!    {
!        // std::cout << "Rank " << m_s.rank << " send right to rank "
!        //          << m_s.rank_right << std::endl;
!        MPI_Send(m_TCurrent[0] + m_s.ndy, 1, tColumnVector, m_s.rank_right, tag,
!                 comm);
!    }


   ! Cleanup the custom column vector type
    call MPI_Type_free(mpi_vector, ierr)


END SUBROUTINE exchange


End Module heat_transfer

