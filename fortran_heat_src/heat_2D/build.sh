mpif90 -g -c settings.f95
mpif90 -g -c heat_transfer.f95
mpif90 -g -c main.f95
mpif90 -g -o temp.exe main.o settings.o heat_transfer.o -I/.
