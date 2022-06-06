#!/bin/bash --login

echo 'Running (note - a login shell to get correct path etc)'
cd /here/src
make clean
make
