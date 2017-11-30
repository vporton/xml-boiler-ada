# -*- Mode: Makefile -*-
#
# Makefile.am - automake file for XML Boiler
#
# Copyright (C) 2014 Victor Porton - http://freesoft.portonvictor.org/
#
# This package is Free Software
#
# See LICENSE.html or LICENSE.txt at the top of this package for the
# full license terms.

soversion = 0.0.1
soname = libxmlboiler.so.$(soversion)

# ADAINCLUDE = $(includedir)/librdf
# ADALIB = $(libdir)/librdf

LIB_DIR = $(builddir)/lib

EXTRA_DIST = libxmlboiler.gpr src/*.ad[bs]

# $(LIB_DIR)/librdf-ada.so: my-dynamic-lib

if DEBUG
DEBUG_MODE=debug
else
DEBUG_MODE=release
endif

GPRBUILD = @GPRBUILD@ --relocate-build-tree -p -s -we
GPRCLEAN = @GPRCLEAN@
GPRINSTALL = @GPRINSTALL@ --prefix=$(prefix) --relocate-build-tree
GNATPREP = @GNATPREP@

GPR_PROJECT_PATH = "@GPR_PROJECT_PATH@"

my-dynamic-lib: src/boiler-config.ads
	GPR_PROJECT_PATH=$(GPR_PROJECT_PATH) $(GPRBUILD) libxmlboiler.gpr \
                 -XLIBRARY_KIND=dynamic -XOBJ_DIR=$(builddir)/obj-dynamic -Xsoversion=$(soname) -XMODE=Install -XDEBUG_MODE=$(DEBUG_MODE) @GPRBUILDFLAGS@

my-static-lib: src/boiler-config.ads
	GPR_PROJECT_PATH=$(GPR_PROJECT_PATH) $(GPRBUILD) libxmlboiler.gpr \
                 -XLIBRARY_KIND=static -XOBJ_DIR=$(builddir)/obj-static -Xsoversion=$(soname) -XMODE=Install -XDEBUG_MODE=$(DEBUG_MODE) @GPRBUILDFLAGS@

src/boiler-config.ads: src/boiler-config.ads.in
	$(GNATPREP) -T -Dsrcdir=\"$(srcdir)\" -Ddatadir=\"$(datadir)\" $< $@

check-syntax:
	export GPR_PROJECT_PATH=$(GPR_PROJECT_PATH) &&  $(GPRBUILD) libxmlboiler.gpr -c \
                 -XLIBRARY_KIND=static -XOBJ_DIR=$(builddir)/obj-static -Xsoversion=$(soname) -XMODE=Install -XDEBUG_MODE=check @GPRBUILDFLAGS@

all-local: my-dynamic-lib

clean-local:
	GPR_PROJECT_PATH=$(GPR_PROJECT_PATH) $(GPRCLEAN) -r libxmlboiler.gpr -XLIBRARY_KIND=dynamic -XOBJ_DIR=$(builddir)/obj-dynamic -Xsoversion=$(soname) -XMODE=Install
	GPR_PROJECT_PATH=$(GPR_PROJECT_PATH) $(GPRCLEAN) -r libxmlboiler.gpr -XLIBRARY_KIND=static -XOBJ_DIR=$(builddir)/obj-static -Xsoversion=$(soname) -XMODE=Install
	rm $(srcdir)/src/boiler-config.ads

# install-data-local:
# 	$(GPRINSTALL) -f -p librdf.gpr -XLIBRARY_KIND=dynamic -XOBJ_DIR=$(builddir)/obj-dynamic -Xsoversion=$(soname) -XMODE=Install --build-name=dynamic
# 	$(GPRINSTALL) -f -p librdf.gpr -XLIBRARY_KIND=static -XOBJ_DIR=$(builddir)/obj-static -Xsoversion=$(soname) -XMODE=Install --build-name=static

# uninstall-local:
# 	$(GPRINSTALL) --uninstall -f -p librdf.gpr -XLIBRARY_KIND=dynamic -XOBJ_DIR=$(builddir)/obj-dynamic -Xsoversion=$(soname) -XMODE=Install
# 	$(GPRINSTALL) --uninstall -f -p librdf.gpr -XLIBRARY_KIND=static -XOBJ_DIR=$(builddir)/obj-static -Xsoversion=$(soname) -XMODE=Install

.PHONY: my-dynamic-lib my-static-lib check-syntax
