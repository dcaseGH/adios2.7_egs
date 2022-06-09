# Notes

Examples stolen and mangled from main source. Mess around with these

Make a container (e.g... rename file and...)
docker build -t adios2-int .

run the scripts (e.g...)
docker run -it -v `pwd`:/here adios2-int /here/build.sh
