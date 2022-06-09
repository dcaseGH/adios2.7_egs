#!/bin/bash --login

echo 'Running (note - a login shell to get correct path etc)'
cd /here/src/fort1
mpirun -np 2 egRead.exe
