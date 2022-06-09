#!/bin/bash --login

echo 'Running xml cases'
cd /here/src/xml/write
mpirun -np 2 egXml ../heat_file.xml heat.h5 2 1 5 10 10 10
mpirun -np 2 egXml ../heat_hdf5.xml heat.h5 2 1 5 10 10 10
h5dump -d T /here/src/xml/write/heat.0.7.h5

