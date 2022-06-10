# Notes

Examples stolen and mangled from main source. Mess around with these

Make a container (e.g... rename file and...)
docker build -t adios2-int .

run the scripts (e.g...)
docker run -it -v `pwd`:/here adios2-int /here/build.sh

## Miniapp
this is the fortran heat 2d thing
roughly copied from the xml case, converted to fortran
currently run build.sh from within container . mpirun -np 2 1 2 ./temp.exe