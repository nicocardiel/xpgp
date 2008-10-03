#------------------------------------------------------------------------------
# Version 03-October-2007
#------------------------------------------------------------------------------
# To install xpgp properly, you must follow these three steps:
# make include
# make xpgp
# make clean
#------------------------------------------------------------------------------
# libraries:
PGPLOTDIR = /usr/local/pgplot
X11DIR  = /usr/lib64
NSIMUL  = 100
NDATAMAX= 100000
# macro definitions
FSOURCE = button.f buttqbr.f buttqcf.f buttqch.f buttqex.f \
          buttqit.f buttqpr.f buttqxb.f buttqyb.f buttqytext.f buttsbr.f \
          buttscf.f buttsch.f buttsex.f buttsit.f buttspr.f buttsxb.f \
          buttsyb.f buttsytext.f ifbutton.f rpgband.f rpgbegin.f rpgbegok.f \
          rpgenv.f rpgeras.f rpgerasb.f rpgerasw.f \
          cextrae.f \
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
          ranred.f \
          sfitpol.f \
          show_buffers.f \
          show_fitpol.f \
          slinreg.f \
          spearman.f \
          statistic.f \
          systemfunction.f \
          truebeg.f \
          truelen.f \
          updatekey.f \
	  updatelimits.f \
          updateplot.f \
          xpgp.f \
          yfunkd_linregexy.f
FOBJECT = button.o buttqbr.o buttqcf.o buttqch.o buttqex.o \
          buttqit.o buttqpr.o buttqxb.o buttqyb.o buttqytext.o buttsbr.o \
          buttscf.o buttsch.o buttsex.o buttsit.o buttspr.o buttsxb.o \
          buttsyb.o buttsytext.o ifbutton.o rpgband.o rpgbegin.o rpgbegok.o \
          rpgenv.o rpgeras.o rpgerasb.o rpgerasw.o \
          cextrae.o \
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
          ranred.o \
          sfitpol.o \
          show_buffers.o \
          show_fitpol.o \
          slinreg.o \
          spearman.o \
          statistic.o \
          systemfunction.o \
          truebeg.o \
          truelen.o \
          updatekey.o \
	  updatelimits.o \
          updateplot.o \
          xpgp.o \
          yfunkd_linregexy.o
# Default rule to create program
xpgp:  $(FOBJECT)
	g77 -o $@ $(FOBJECT) -L$(PGPLOTDIR) -L$(X11DIR) -lpgplot -lX11
# Target to clean object modules
clean:    $(FOBJECT)
	rm -f $(FOBJECT)
	rm -f xpgpdir.inc nsimul.inc ndatamax.inc
# Target to touch source modules
touch:
	touch $(FSOURCE)
# Target to create the file xpgpdir.inc
include:
	rm -f xpgpdir.inc
	echo "        CHARACTER*255 XPGPDIR" > xpgpdir.inc
	echo "        PARAMETER(XPGPDIR='`pwd`')" >> xpgpdir.inc
	rm -f nsimul.inc
	echo "        INTEGER NSIMULMAX" > nsimul.inc
	echo "        PARAMETER(NSIMULMAX=$(NSIMUL))" >> nsimul.inc
	rm -f ndatamax.inc
	echo "        INTEGER NDATAMAX" > ndatamax.inc
	echo "        PARAMETER(NDATAMAX=$(NDATAMAX))" >> ndatamax.inc
	touch $(FSOURCE)
# second level dependencies
.f.o: $(FSOURCE)
	g77 -c $?
# definitions
.PRECIOUS: xpgp
