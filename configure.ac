# Process this file with autoconf to produce a configure script.
AC_INIT(xpgp,2.1.0,[ncl@astrax.fis.ucm.es])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([config.h.in])
AM_INIT_AUTOMAKE([foreign])

AC_REVISION($Revision$)
# Checks for programs.
AC_PROG_CC
AC_PROG_FC

# Checks for libraries.
STTL_LIB_PGPLOT
LIBS="$LIBS $PGPLOT_LIBS"
#AC_LANG([Fortran 77])
#AC_CHECK_LIB(button,button, ,AC_MSG_ERROR(could not find button))

DEFAULT_XPGP_NSIMUL=101
DEFAULT_XPGP_NDATAMAX=100001
DEFAULT_XPGP_DIR="/usr/local/share/xpgp"

XPGP_NSIMUL=${XPGP_NSIMUL="$DEFAULT_XPGP_NSIMUL"}
XPGP_NDATAMAX=${XPGP_NDATAMAX="$DEFAULT_XPGP_NDATAMAX"}
XPGP_DIR=${XPGP_DIR="$DEFAULT_XPGP_DIR"}

AC_ARG_VAR(XPGP_NSIMUL,[Description])
AC_ARG_VAR(XPGP_NDATAMAX,[Description])
AC_ARG_VAR(XPGP_DIR,[Description])

AC_CONFIG_FILES([Makefile 
		m4/Makefile
		src/nsimul.inc
		src/xpgpdir.inc
		src/ndatamax.inc
		src/exec_function1.f
		src/exec_function2.f
		src/Makefile])
AC_OUTPUT
