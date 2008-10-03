#------------------------------------------------------------------------------
# Version 3-August-2000
#------------------------------------------------------------------------------
# To install xpgp properly, you must follow these three steps:
# make include
# make xpgp
# make clean
#------------------------------------------------------------------------------
# libraries:
PGPLOTDIR = /usr/local/pgplot
BUTLIB  = /usr/local/reduceme/button/libbutton.a
X11DIR  = /usr/X11R6/lib
NSIMUL  = 100
# macro definitions
FSOURCE = cextrae.f \
          chupper.f \
          copydata.f \
          downhill.f \
          downhilld.f \
          editor.f \
          exec_function1.f \
          exec_function2.f \
          fextrae.f \
          find_nearest.f \
          fitpol.f \
          fitpoley.f \
          fitpoleyb.f \
          fmean0.f \
          give_errors.f \
          give_statistics.f \
          histogram.f \
          iofunctions.f \
          ks_func.f \
          ksone.f \
          labels.f \
          leenewfile.f \
          linreg.f \
          linregexy.f \
          linregey.f \
          ludcmp.f \
          lusolv.f \
          onlyone.f \
          ordena1f.f \
          ordena1f1i.f \
          plot_settings.f \
          probks.f \
          sfitpol.f \
          show_buffers.f \
          show_fitpol.f \
          slinreg.f \
          spearman.f \
          statistic.f \
          truebeg.f \
          truelen.f \
          updatekey.f \
	  updatelimits.f \
          updateplot.f \
          xpgp.f \
          yfunkd_linregexy.f
FOBJECT = cextrae.o \
          chupper.o \
          copydata.o \
          downhill.o \
          downhilld.o \
          editor.o \
          exec_function1.o \
          exec_function2.o \
          fextrae.o \
          find_nearest.o \
          fitpol.o \
          fitpoley.o \
          fitpoleyb.o \
          fmean0.o \
          give_errors.o \
          give_statistics.o \
          histogram.o \
          iofunctions.o \
          ks_func.o \
          ksone.o \
          labels.o \
          leenewfile.o \
          linreg.o \
          linregexy.o \
          linregey.o \
          ludcmp.o \
          lusolv.o \
          onlyone.o \
          ordena1f.o \
          ordena1f1i.o \
          plot_settings.o \
          probks.o \
          sfitpol.o \
          show_buffers.o \
          show_fitpol.o \
          slinreg.o \
          spearman.o \
          statistic.o \
          truebeg.o \
          truelen.o \
          updatekey.o \
	  updatelimits.o \
          updateplot.o \
          xpgp.o \
          yfunkd_linregexy.o
CSOURCE = ranred_.c \
          redsystem_.c
COBJECT = ranred_.o \
          redsystem_.o
# Default rule to create program
xpgp:  $(FOBJECT) $(COBJECT)
	g77 -o $@ $(FOBJECT) $(COBJECT) $(BUTLIB) -L$(PGPLOTDIR) -L$(X11DIR) -lpgplot -lX11
# Target to clean object modules
clean:    $(FOBJECT) $(COBJECT)
	rm -f $(FOBJECT) $(COBJECT)
	rm -f xpgpdir.inc nsimul.inc
# Target to touch source modules
touch:
	touch $(FSOURCE) $(CSOURCE)
# Target to create the file xpgpdir.inc
include:
	rm -f xpgpdir.inc
	echo "        CHARACTER*255 XPGPDIR" > xpgpdir.inc
	echo "        PARAMETER(XPGPDIR='`pwd`')" >> xpgpdir.inc
	rm -f nsimul.inc
	echo "        INTEGER NSIMULMAX" > nsimul.inc
	echo "        PARAMETER(NSIMULMAX=$(NSIMUL))" >> nsimul.inc
	touch $(FSOURCE) $(CSOURCE)
# second level dependencies
.f.o: $(FSOURCE)
	g77 -c $?
.c.o: $(CSOURCE)
	gcc -c $?
# definitions
.PRECIOUS: xpgp
