SHELL=  /bin/sh
COMP=   ifort
FFLAGS= -O3 -g -traceback -convert big_endian

ens_trak_ave:      ens_trak_ave.f
	@echo " "
	@echo "  Compiling the ensemble track averaging program....."
	$(COMP) $(FFLAGS) ens_trak_ave.f -o ens_trak_ave
	@echo " "

CMD = ens_trak_ave

clean:
	-rm -f  *.mod

install:
	mv $(CMD) ../../exec/$(CMD)
