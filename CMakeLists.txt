cmake_minimum_required(VERSION 3.5)
project(halibut LANGUAGES C)

set(LIBCHARSET_LIBRARY_ONLY ON)
include_directories(charset ${CMAKE_CURRENT_BINARY_DIR}/charset)
add_subdirectory(charset)

if(CMAKE_SYSTEM_NAME MATCHES "Windows")
  add_compile_definitions(_CRT_SECURE_NO_WARNINGS)
endif()

add_executable(halibut
  biblio.c
  bk_html.c
  bk_info.c
  bk_man.c
  bk_paper.c
  bk_pdf.c
  bk_ps.c
  bk_text.c
  bk_whlp.c
  contents.c
  deflate.c
  error.c
  help.c
  huffman.c
  in_afm.c
  in_pf.c
  in_sfnt.c
  index.c
  input.c
  keywords.c
  licence.c
  lz77.c
  lzx.c
  main.c
  malloc.c
  misc.c
  psdata.c
  tree234.c
  ustring.c
  version.c
  wcwidth.c
  winchm.c
  winhelp.c)
target_link_libraries(halibut charset)

if(CMAKE_VERSION VERSION_LESS 3.14)
  # CMake 3.13 and earlier required an explicit install destination.
  install(TARGETS halibut RUNTIME DESTINATION bin)
else()
  # 3.14 and above selects a sensible default, which we should avoid
  # overriding here so that end users can override it using
  # CMAKE_INSTALL_BINDIR.
  install(TARGETS halibut)
endif()

add_subdirectory(doc)
