# Compiler
CC   = mpif90
OPTS = -g
#OPTS = -std=c++11

# Project name
PROJECT = heat2D

# Libraries
#LIBS = `adios2-config --fortran-libs` -L/opt/spack/opt/spack/linux-ubuntu18.04-haswell/gcc-7.5.0/hdf5-1.10.7-ntpmkdghhettk5ms7cfgmh4tjc2rvgcn/lib
LIBS = -Wl,-rpath,/opt/spack/opt/spack/linux-ubuntu18.04-haswell/gcc-7.5.0/adios2-2.7.1-mjyq2mikhh7bee65gsos67q6o4m5ixvk/lib /opt/spack/opt/spack/linux-ubuntu18.04-haswell/gcc-7.5.0/adios2-2.7.1-mjyq2mikhh7bee65gsos67q6o4m5ixvk/lib/libadios2_fortran_mpi.so.2.7.1 /opt/spack/opt/spack/linux-ubuntu18.04-haswell/gcc-7.5.0/adios2-2.7.1-mjyq2mikhh7bee65gsos67q6o4m5ixvk/lib/libadios2_fortran.so.2.7.1 -Wl,-rpath-link,/opt/spack/opt/spack/linux-ubuntu18.04-haswell/gcc-7.5.0/adios2-2.7.1-mjyq2mikhh7bee65gsos67q6o4m5ixvk/lib -L/opt/spack/opt/spack/linux-ubuntu18.04-haswell/gcc-7.5.0/hdf5-1.10.7-ntpmkdghhettk5ms7cfgmh4tjc2rvgcn/lib
#INCS = `adios2-config --fortran-flags` -I/opt/spack/opt/spack/linux-ubuntu18.04-haswell/gcc-7.5.0/hdf5-1.10.7-ntpmkdghhettk5ms7cfgmh4tjc2rvgcn/include -I/.
INCS = -DADIOS2_USE_MPI -I/opt/spack/opt/spack/linux-ubuntu18.04-haswell/gcc-7.5.0/adios2-2.7.1-mjyq2mikhh7bee65gsos67q6o4m5ixvk/include/adios2/fortran -I/opt/spack/opt/spack/linux-ubuntu18.04-haswell/gcc-7.5.0/hdf5-1.10.7-ntpmkdghhettk5ms7cfgmh4tjc2rvgcn/include -I/.


#SRCS = #$ # ( # sh #ell find . -name '*.f95')
OBJS = settings.o heat_transfer.o main.o

$(PROJECT): $(OBJS)
	$(CC) $(OPTS) $^ $(INCS) -o $(PROJECT) $(LIBS) -lhdf5

%.o: %.F90
	$(CC) $(OPTS) $(INCS) -c $< $(LIBS) -o $@

clean:
	rm -f $(PROJECT) *.o *.mod
