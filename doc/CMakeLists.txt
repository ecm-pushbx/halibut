# Halibut is used to build its own documentation. So we can only build
# the documentation if we're not cross-compiling, or if the caller of
# cmake has defined NATIVE_HALIBUT to point at a Halibut binary that
# can run on the build host.

if(CMAKE_CROSSCOMPILING AND NOT NATIVE_HALIBUT)
  message(WARNING "Cannot build the Halibut documentation in a cross-compile; "
    "define NATIVE_HALIBUT if you need to do it anyway")
else()

  include(GNUInstallDirs)

  if(CMAKE_CROSSCOMPILING)
    set(HALIBUT ${NATIVE_HALIBUT})
  else()
    set(HALIBUT $<TARGET_FILE:halibut>)
  endif()

  set(manual_sources
    ${CMAKE_CURRENT_SOURCE_DIR}/blurb.but
    ${CMAKE_CURRENT_SOURCE_DIR}/intro.but
    ${CMAKE_CURRENT_SOURCE_DIR}/running.but
    ${CMAKE_CURRENT_SOURCE_DIR}/input.but
    ${CMAKE_CURRENT_SOURCE_DIR}/output.but
    ${CMAKE_CURRENT_SOURCE_DIR}/licence.but
    ${CMAKE_CURRENT_SOURCE_DIR}/manpage.but
    ${CMAKE_CURRENT_SOURCE_DIR}/index.but
    )

  # Do the manual build in a subdirectory, to avoid the install
  # command making a CMakeFiles directory in the output location.
  set(manual_dir ${CMAKE_CURRENT_BINARY_DIR}/manual)
  file(MAKE_DIRECTORY ${manual_dir})
  add_custom_target(manual ALL
    BYPRODUCTS
      ${manual_dir}/index.html
      ${manual_dir}/halibut.txt
      ${manual_dir}/halibut.info
      ${manual_dir}/halibut.ps
      ${manual_dir}/halibut.pdf
      ${manual_dir}/halibut.chm
    COMMAND
      ${HALIBUT}
      --html
      --text=halibut.txt
      --info=halibut.info
      --ps=halibut.ps
      --pdf=halibut.pdf
      --chm=halibut.chm
      ${manual_sources}
    WORKING_DIRECTORY ${manual_dir}
    DEPENDS halibut ${manual_sources})

  add_custom_target(manpage ALL
    BYPRODUCTS
      halibut.1
    COMMAND
      ${HALIBUT}
      --man=halibut.1
      ${CMAKE_CURRENT_SOURCE_DIR}/manpage.but
    DEPENDS halibut ${CMAKE_CURRENT_SOURCE_DIR}/manpage.but)

  install(DIRECTORY ${manual_dir}/
    DESTINATION ${CMAKE_INSTALL_DOCDIR}
    FILES_MATCHING PATTERN "*.html")

  install(DIRECTORY ${manual_dir}/
    DESTINATION ${CMAKE_INSTALL_INFODIR}
    FILES_MATCHING PATTERN "*.info*")

  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/halibut.1
    DESTINATION ${CMAKE_INSTALL_MANDIR}/man1)

endif()
