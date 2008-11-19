# Process this file with autoconf to produce a configure script.
#
# $Id$
#

# Prelude
AC_INIT(xpgp,05.2,[ncl@astrax.fis.ucm.es])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([config.h.in])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

# Checks for programs
AC_PROG_F77

# Safety check
AC_CONFIG_SRCDIR([src/xpgp.f])

# Checks for libraries
AC_CHECK_LIB([cfitsio], main,,AC_MSG_ERROR(cfitsio library not found))
STTL_LIB_PGPLOT
LIBS="$LIBS $PGPLOT_LIBS"

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
                 m4/Makefile
                 auxdir/Makefile])
AC_OUTPUT
