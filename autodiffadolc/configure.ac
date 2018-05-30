# Process this file with autoconf to produce a configure script.
AC_INIT(Autodiffadolc, 0.1, sriharikrishna@gmail.com)
AC_CONFIG_SRCDIR([src/adolc_r_wrap.cpp])
AC_CONFIG_AUX_DIR([autoconf])
AC_CONFIG_MACRO_DIR([autoconf])
#AC_CONFIG_HEADER([src/cconfig.h])

# find R home and set CC/CFLAGS
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
RBIN="${R_HOME}/bin/R"
CC=`"${RBIN}" CMD config CC`;
CFLAGS=`"${RBIN}" CMD config CFLAGS`
CPP=`"${RBIN}" CMD config CPP`;
AC_SUBST(R_HOME)

# platform check
#AC_CANONICAL_HOST

# Checks for programs.
AC_PROG_CC

# Checks for libraries.

# Checks for header files.
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([string.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
#AC_HEADER_TIME

# Checks for library functions.
#AC_FUNC_MALLOC
#AC_TYPE_SIGNAL

AC_PATH_PROG([PKGCONF],[pkg-config],[],[$PATH:/usr/local/bin:ext/bin:ext:/sw/bin:/opt/bin])

AX_BOOST_BASE([1.54],[boost_new=yes],[boost_new=no])
if test x$boost_new = xyes ; then
CPPFLAGS="$CPPFLAGS $BOOST_CPPFLAGS"
AC_LANG_PUSH([C++])
#AC_CHECK_HEADERS([boost/pool/pool_alloc.hpp])
AC_CHECK_HEADER(boost/regex.h,,AC_MSG_ERROR([Cannot find boost/regex.h! Please install boost (http://www.boost.org/)]))
AC_LANG_POP([C++])
AX_BOOST_SYSTEM
LDFLAGS="$LDFLAGS $BOOST_LDFLAGS $BOOST_SYSTEM_LIB"
fi

COLPACK_CONF

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
