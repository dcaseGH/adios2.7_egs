CXXFLAGS=`adios2-config --cxx-flags`
LDFLAGS=`adios2-config --cxx-libs`

all: egRead.exe egWrite.exe

egRead.exe: helloBPReader.cpp
	mpic++ -o $@  ${CXXFLAGS} $^  ${LDFLAGS}

egWrite.exe: helloBPWriter.cpp
	mpic++ -o $@  ${CXXFLAGS} $^  ${LDFLAGS}

clean:
	rm *.exe
