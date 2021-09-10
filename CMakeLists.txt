# CMake-based build system.

# I don't want to have to edit the master list of source files in more
# than one place, so let's get CMake to use its built-in file and
# string handling abilities to read the list out of Makefile.am
# alongside it.

cmake_minimum_required(VERSION 3.0)
project(libcharset LANGUAGES C)

file(READ ${CMAKE_CURRENT_SOURCE_DIR}/Makefile.am MAKEFILE_AM)
string(REPLACE "\\\n" " " MAKEFILE_AM ${MAKEFILE_AM})
string(REGEX MATCHALL "[^ \n]* = [^\n]*" MAKEFILE_AM_DEFS ${MAKEFILE_AM})

foreach(MAKEFILE_AM_DEF ${MAKEFILE_AM_DEFS})
  if(${MAKEFILE_AM_DEF} MATCHES "^([a-z][^ ]*)_SOURCES = (.*)")
    set(TARGET ${CMAKE_MATCH_1})
    string(REGEX MATCHALL "[^ ]*\\.c" SOURCES ${CMAKE_MATCH_2})
    if ("libcharset_a" STREQUAL ${TARGET})
      add_library(charset STATIC ${SOURCES}
        ${CMAKE_CURRENT_BINARY_DIR}/sbcsdat.h)
      target_include_directories(charset PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR})
    elseif(NOT LIBCHARSET_LIBRARY_ONLY)
      add_executable(${TARGET} ${SOURCES}
        ${CMAKE_CURRENT_BINARY_DIR}/sbcsdat.h)
      target_include_directories(${TARGET} PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR})
      target_link_libraries(${TARGET} charset)
    endif()
  endif()
endforeach()

add_custom_command(OUTPUT sbcsdat.c
  COMMAND perl ${CMAKE_CURRENT_SOURCE_DIR}/sbcsgen.pl
  ${CMAKE_CURRENT_SOURCE_DIR}/sbcs.dat
  --source=${CMAKE_CURRENT_BINARY_DIR}/sbcsdat.c
  DEPENDS sbcsgen.pl sbcs.dat)
add_custom_command(OUTPUT sbcsdat.h
  COMMAND perl ${CMAKE_CURRENT_SOURCE_DIR}/sbcsgen.pl
  ${CMAKE_CURRENT_SOURCE_DIR}/sbcs.dat
  --header=${CMAKE_CURRENT_BINARY_DIR}/sbcsdat.h
  DEPENDS sbcsgen.pl sbcs.dat)
