# Process this file with autoconf to produce a configure script.
#
# $Id$
#

# Prelude
AC_INIT(xpgp,5.0,[ncl@astrax.fis.ucm.es])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([config.h.in])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

# Checks for programs
AC_PROG_F77

# Safety check
AC_CONFIG_SRCDIR([src/xpgp.f])

# Checks for libraries
STTL_LIB_PGPLOT
LIBS="$LIBS $PGPLOT_LIBS"

# Definitions
DEFAULT_AUXDIR="/usr/local/share/xpgp/auxdir"
AUXDIR=${AUXDIR="$DEFAULT_AUXDIR"}
AC_ARG_VAR(AUXDIR,[Directory to install auxiliary files])

DEFAULT_NDATAMAX="100000"
NDATAMAX=${NDATAMAX="$DEFAULT_NDATAMAX"}
AC_ARG_VAR(NDATAMAX,[Maximum number of data points per buffer])

DEFAULT_NSIMULMAX="1000"
NSIMULMAX=${NSIMULMAX="$DEFAULT_NSIMULMAX"}
AC_ARG_VAR(NSIMULMAX,[Maximum number of numerical simulations])

# Output files
AC_CONFIG_FILES([Makefile
                 auxdir/Makefile
                 m4/Makefile
                 src/fcompil.inc
                 src/ndatamax.inc
                 src/nsimulmax.inc
                 src/version.inc
                 src/xpgpdir.inc
                 src/Makefile])
AC_OUTPUT
