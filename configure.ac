# Process this file with autoconf to produce a configure script.
#
# $Id$
#

# Prelude
AC_INIT(xpgp,5.3.1,[cardiel@fis.ucm.es])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([config.h.in])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

# Checks for programs
AC_PROG_F77

# Safety check
AC_CONFIG_SRCDIR([src/xpgp.f])

# Define directories to search for libraries
AC_MSG_CHECKING([uname -s for detecting host operating system])
OSNAME=`uname -s`
case "$OSNAME" in
  Darwin*)
    AC_MSG_RESULT(Darwin)
    LDFLAGS="$LDFLAGS -L/opt/local/lib"
  ;;
  Linux*)
    AC_MSG_RESULT(Linux)
    LDFLAGS="$LDFLAGS -L/usr/local/src/cfitsio/lib -L/usr/local/pgplot"
  ;;
esac

# Checks for libraries
AC_MSG_RESULT(LDFLAGS set to... $LDFLAGS)
AC_CHECK_LIB([X11], main,,AC_MSG_ERROR(X11 library not found))
AC_CHECK_LIB([pgplot], main,,AC_MSG_ERROR(pgplot library not found))
AC_CHECK_LIB([cfitsio], main,,AC_MSG_ERROR(cfitsio library not found))
# Define in LIBS all the previous libraries
LIBS="$LIBS"

# Definitions
DEFAULT_AUXDIR="/usr/local/share/xpgp"
AUXDIR=${AUXDIR="$DEFAULT_AUXDIR"}
AC_ARG_VAR(AUXDIR,[Directory to install auxiliary xpgp files])

DEFAULT_NDATAMAX="100000"
NDATAMAX=${NDATAMAX="$DEFAULT_NDATAMAX"}
AC_ARG_VAR(NDATAMAX,[Maximum number of data points per buffer])

DEFAULT_LENLINEA="1000"
LENLINEA=${LENLINEA="$DEFAULT_LENLINEA"}
AC_ARG_VAR(LENLINEA,[Maximum number of characters/line in input file])

DEFAULT_NSIMULMAX="1000"
NSIMULMAX=${NSIMULMAX="$DEFAULT_NSIMULMAX"}
AC_ARG_VAR(NSIMULMAX,[Maximum number of numerical simulations])

# Output files
AC_CONFIG_FILES([Makefile
                 src/button.inc
                 src/fcompil.inc
                 src/lenlinea.inc
                 src/nbuffmax.inc
                 src/ndatamax.inc
                 src/ndegmax.inc
                 src/nfixedmax.inc
                 src/nknotsmax.inc
                 src/nsimulmax.inc
                 src/version.inc
                 src/xpgpdir.inc
                 src/Makefile
                 auxdir/Makefile])
AC_OUTPUT
