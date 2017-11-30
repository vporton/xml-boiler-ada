AC_INIT([XML Boiler],[0.0.1])
AC_PREREQ([2.62])
AC_REVISION($Revision$)dnl
AM_INIT_AUTOMAKE([1.11 check-news std-options -Wobsolete -Wportability -Wsyntax -Wunsupported])
AM_MAINTAINER_MODE

AC_CANONICAL_HOST

dnl Checks for programs.
AM_SANITY_CHECK
dnl AC_PROG_CC
dnl AM_PROG_CC_C_O
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

AC_ARG_ENABLE([debug],
[  --enable-debug    Turn on debugging],
[case "${enableval}" in
  yes) debug=true ;;
  no)  debug=false ;;
  *) AC_MSG_ERROR([bad value ${enableval} for --enable-debug]) ;;
esac],[debug=false])
AM_CONDITIONAL([DEBUG], [test x$debug = xtrue])

dnl LT_INIT

# Find a tar command for 'make dist'
AC_CHECK_PROGS(TAR, gnutar gtar tar)

AC_ARG_WITH(gprbuild,   [  --with-gprbuild(=gprbuild)   Ada builder], GPRBUILD="$withval")
AC_ARG_WITH(gprclean,   [  --with-gprclean(=gprclean)   Ada cleaner], GPRCLEAN="$withval")
AC_ARG_WITH(gprinstall, [  --with-gprbuild(=gprinstall) Ada installer], GPRINSTALL="$withval")
AC_ARG_WITH(gnatprep, [  --with-gnatprep(=gnatprep) Ada preprocessor], GNATPREP="$withval")

AC_CHECK_PROGS(GPRBUILD, gprbuild)
AC_CHECK_PROGS(GPRCLEAN, gprclean)
AC_CHECK_PROGS(GPRINSTALL, gprinstall)
AC_CHECK_PROGS(GNATPREP, gnatprep)

AC_SUBST(GPRBUILD)
AC_SUBST(GPRCLEAN)
AC_SUBST(GPRINSTALL)
AC_SUBST(GNATPREP)

AC_SUBST(GPRBUILDFLAGS)
AC_ARG_VAR(GPRBUILDFLAGS, [Flags for gprbuild (such as [-cargs opts]])

AC_ARG_VAR(GPR_PROJECT_PATH, [Path for GNAT projects])


AM_MISSING_PROG(ACLOCAL, aclocal, $missing_dir)
AM_MISSING_PROG(AUTOCONF, autoconf, $missing_dir)
AM_MISSING_PROG(AUTOMAKE, automake, $missing_dir)
AM_MISSING_PROG(AUTOHEADER, autoheader, $missing_dir)


AC_CONFIG_FILES([Makefile])

AC_OUTPUT
