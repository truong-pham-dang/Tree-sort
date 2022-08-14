# How to build code ?
# 1. Using gfortran
Simply execute 
```
make
``` 
to build code.

# 2. Using intel fortran compiler from OneApi (MacOS)
Initialize OneApi environment by
```
source /opt/intel/oneapi/setvars.sh
```
To build code, execute:
```
make -f TARGET=intel OS=macos
```