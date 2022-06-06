#!/bin/bash --login

echo 'Running basic examples'
cd /here/src
mpirun -np 2 egWrite.exe
mpirun -np 2 egRead.exe

