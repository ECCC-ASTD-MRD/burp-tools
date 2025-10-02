
Collection of tools to manipulate RPN standard files

### Components
  * [libburp](src/libburp/README.md)
  * [libburpc](src/libburpc/README.md)
  * [libburpidl](src/libburpidl/README.md)
  * [brpvoir](src/brpvoir/README.md)
  * [editbrp](src/editbrp/README.md)

# Compilation

## At CMC

### Build dependencies

- CMake 3.20+
- librmn
- IDL (optional, needed for libburpidl)

Note: **cmake_rpn** is included as a submodule.  Please clone with the
**--recursive** flag or run **git submodule update --init --recursive** in the
git repo after having cloned.

### Environment

Load the right environment, depending on the architecture you need.  This
will load the specified compiler and its parameters, and set the
`EC_CMAKE_MODULE_PATH` variable for the `cmake_rpn` modules.

- Example for ppp6/sc6 and icelake specific architecture:

```
. r.load.dot mrd/rpn/code-tools/latest/env/rhel-8-icelake-64@inteloneapi-2025.1.0
```

- Example for generic architecture on ppp6/sc6:

```
. r.load.dot mrd/rpn/code-tools/latest/env/rhel-8-amd64-64@inteloneapi-2025.1.0
```

- Example for GNU on any architecture:

```
. r.load.dot mrd/rpn/code-tools/latest/env/gnu
```

Since the default version of CMake available on ECCC systems is probably too
old, you need to load a version newer than 3.20.  For example: `. ssmuse-sh
-d main/opt/cmake/cmake-3.21.1`.

Load the latest stable version of librmn.

### Build and install

```
mkdir build
cd build
cmake .. -DCMAKE_INSTALL_PREFIX=${your_choice} [-DCMAKE_PREFIX_PATH=PATH_TO_RMN_ROOT_DIRECTORY]
make check
make -j 4
make install
```

## Outside CMC (external users)

### Build dependencies

- CMake 3.20+
- librmn with shared libraries (https://github.com/ECCC-ASTD-MRD/librmn/)

`cmake_rpn` is included as a git submodule.  Please clone with the
`--recurse --remote-submodules` options, or run `git submodule update --init
--remote` in the git repo after having cloned.

### Build and install

```
mkdir build
cd build
cmake .. -DCMAKE_INSTALL_PREFIX=${your_choice} -DCMAKE_PREFIX_PATH=PATH_TO_RMN_ROOT_DIRECTORY
make -j 4
make check
make install
```
