SET(CPACK_GENERATOR "DEB")

SET(CPACK_DEBIAN_PACKAGE_NAME         "${PACKAGE_BASE_NAME}")
SET(CPACK_DEBIAN_PACKAGE_VERSION      "${CPACK_PACKAGE_VERSION}${CPACK_PACKAGE_REVISION}")
SET(CPACK_DEBIAN_PACKAGE_MAINTAINER   "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>")
SET(CPACK_DEBIAN_PACKAGE_DESCRIPTION  "Event recording and replay framework (Common Lisp implementation)
 The rsbag system provides infrastructure for recording, transforming
 and replaying RSB events.")
SET(CPACK_DEBIAN_PACKAGE_PRIORITY     "optional")
SET(CPACK_DEBIAN_PACKAGE_SECTION      "lisp")
SET(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "all")
SET(CPACK_DEBIAN_PACKAGE_DEPENDS      "common-lisp-controller (>= 5.11)")

# Generate postinst and prerm hooks
SET(POSTINST_SCRIPT "${CMAKE_CURRENT_BINARY_DIR}/postinst")
SET(PRERM_SCRIPT    "${CMAKE_CURRENT_BINARY_DIR}/prerm")
FILE(WRITE "${POSTINST_SCRIPT}"
           "#!/bin/sh\n\n                                              \\
            set -e\n                                                   \\
            if [ \"$1\" = \"configure\" ] &&                           \\
                 which register-common-lisp-source > /dev/null; then\n \\
              register-common-lisp-source \"${SYSTEM_NAME}\"\n         \\
            fi\n\n")
FILE(WRITE "${PRERM_SCRIPT}"
           "#!/bin/sh\n\n                                                \\
            set -e\n                                                     \\
            if [ \"$1\" = \"remove\" ]                                   \\
                 || [ \"$1\" = \"upgrade\" ]                             \\
                 || [ \"$1\" = \"deconfigure\" ]; then\n                 \\
              if which unregister-common-lisp-source > /dev/null; then\n \\
                unregister-common-lisp-source \"${SYSTEM_NAME}\"\n       \\
              fi\n                                                       \\
            fi\n\n")
EXECUTE_PROCESS(COMMAND chmod 755 "${POSTINST_SCRIPT}" "${PRERM_SCRIPT}")
SET(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA "${POSTINST_SCRIPT};${PRERM_SCRIPT}")

SET(CPACK_PACKAGE_FILE_NAME "${CPACK_DEBIAN_PACKAGE_NAME}-${CPACK_DEBIAN_PACKAGE_VERSION}_${CPACK_DEBIAN_PACKAGE_ARCHITECTURE}")

MESSAGE(STATUS "Debian Package: ${CPACK_DEBIAN_PACKAGE_NAME} (${CPACK_DEBIAN_PACKAGE_VERSION}) [${CPACK_PACKAGE_FILE_NAME}.deb]")
