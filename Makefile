#------------------------------------------------------------------------------
# Version 23-September-2008
#------------------------------------------------------------------------------
# To install xpgp properly, you must follow these three steps:
# make include
# make xpgp
# make clean
#------------------------------------------------------------------------------
# libraries:
VERSION = 04.6
PGPLOTDIR = /usr/local/pgplot
X11DIR  = /usr/lib64
FIODIR  = /usr/local/cfitsio
NSIMUL  = 100
NDATAMAX= 100000
FCOMPIL = gfortran -g -O3 -Wall
# macro definitions
FSOURCE = button.f buttqbr.f buttqcf.f buttqch.f buttqex.f \
          buttqit.f buttqpr.f buttqxb.f buttqyb.f buttqytext.f buttsbr.f \
          buttscf.f buttsch.f buttsex.f buttsit.f buttspr.f buttsxb.f \
          buttsyb.f buttsytext.f ifbutton.f rpgband.f rpgbegin.f rpgbegok.f \
          rpgenv.f rpgeras.f rpgerasb.f rpgerasw.f \
          binsearch.f \
          cextrae.f \
          chupper.f \
          cleantab.f \
          copydata.f \
          combpf.f \
          cubspl.f \
          cubsplx.f \
          downhill.f \
          downhilld.f \
          editor.f \
          exec_function1.f \
          exec_function2.f \
          factorialpf.f \
          fextrae.f \
          find_nearest.f \
          findmml.f \
          fitpol.f \
          fitpoley.f \
          fitpoleyb.f \
          fits_printerror.f \
          fmean0.f \
          fpoly.f \
          give_errors.f \
          give_statistics.f \
          histogram.f \
          indexr.f \
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
          merge_knots.f \
          onlyone.f \
          ordena1f.f \
          ordena1f1i.f \
          otherfit.f \
          plot_settings.f \
          polfit.f \
          probks.f \
          pseudofit.f \
          randomizedata.f \
          ranred.f \
          ranspl.f \
          read_nb.f \
          selbuffer.f \
          sfitpol.f \
          show_buffers.f \
          show_fitpol.f \
          slinreg.f \
          spearman.f \
          splfit.f \
          statistic.f \
          systemfunction.f \
          truebeg.f \
          truelen.f \
          tolog77.f \
          updatekey.f \
	  updatelimits.f \
          updateplot.f \
          xpgp.f \
          yfunk_pseudo.f \
          yfunk_splfit.f \
          yfunk_splfit1.f \
          yfunk_splfit2.f \
          yfunk_splfit3.f \
          yfunkd_linregexy.f
FOBJECT = $(FSOURCE:.f=.o)
# Default rule to create program
xpgp:  $(FOBJECT)
	$(FCOMPIL) -o $@ $(FOBJECT) -L$(PGPLOTDIR) -L$(FIODIR) -L$(X11DIR) -lpgplot -lcfitsio -lX11
# Target to clean object modules
clean:    $(FOBJECT)
	rm -f $(FOBJECT)
	rm -f fcompil.f xpgpdir.inc nsimul.inc ndatamax.inc
# Target to touch source modules
touch:
	touch $(FSOURCE)
# Target to create included files
include:
	rm -f fcompil.inc
	echo "        CHARACTER*255 FCOMPIL" > fcompil.inc
	echo "        PARAMETER(FCOMPIL='$(FCOMPIL)')" >> fcompil.inc
	rm -f xpgpdir.inc
	echo "        CHARACTER*255 XPGPDIR" > xpgpdir.inc
	echo "        PARAMETER(XPGPDIR='`pwd`')" >> xpgpdir.inc
	rm -f nsimul.inc
	echo "        INTEGER NSIMULMAX" > nsimul.inc
	echo "        PARAMETER(NSIMULMAX=$(NSIMUL))" >> nsimul.inc
	rm -f ndatamax.inc
	echo "        INTEGER NDATAMAX" > ndatamax.inc
	echo "        PARAMETER(NDATAMAX=$(NDATAMAX))" >> ndatamax.inc
	rm -f version.inc
	echo "        CHARACTER*4 VERSION" > version.inc
	echo "        PARAMETER(VERSION='$(VERSION)')" >> version.inc
	touch $(FSOURCE)
# second level dependencies
.f.o: $(FSOURCE)
	$(FCOMPIL) -c $?
# definitions
.PRECIOUS: xpgp
