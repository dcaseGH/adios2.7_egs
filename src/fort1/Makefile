CXXFLAGS=`adios2-config --fortran-flags`
LDFLAGS=`adios2-config --fortran-libs`

all: egRead.exe

egRead.exe: helloBPReaderHeatMap3D.F90
	mpif90 -o $@  ${CXXFLAGS} $^  ${LDFLAGS}

clean:
	rm *.exe
