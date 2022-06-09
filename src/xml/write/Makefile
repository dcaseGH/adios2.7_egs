# Compiler
CC   = mpic++
#OPTS = -std=c++11

# Project name
PROJECT = egXml

# Libraries
LIBS = `adios2-config --cxx-libs` -L/opt/spack/opt/spack/linux-ubuntu18.04-haswell/gcc-7.5.0/hdf5-1.10.7-ntpmkdghhettk5ms7cfgmh4tjc2rvgcn/lib
INCS = `adios2-config --cxx-flags` -I/opt/spack/opt/spack/linux-ubuntu18.04-haswell/gcc-7.5.0/hdf5-1.10.7-ntpmkdghhettk5ms7cfgmh4tjc2rvgcn/include

SRCS = $(shell find . -name '*.cpp')
OBJS = $(patsubst %.cpp,%.o,$(SRCS))

# Targets
$(PROJECT): $(OBJS)
	$(CC) $(OPTS) $(OBJS) $(INCS) $(LIBS) -o $@ -lhdf5

%.o: %.cpp
	$(CC) $(OPTS) -c $< $(INCS) -o $@

clean:
	rm $(PROJECT) out -Rf

