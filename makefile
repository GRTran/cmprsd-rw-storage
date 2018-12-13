#Set the compilers and linker
FC=gfortran
CC=mpicc
LD=gfortran

#Set the objects
OBJS=			    Compressed-Storage/IntNLengthVector.o						\
							Compressed-Storage/DblNLengthVector.o						\
							Compressed-Storage/SparseVectorForm.o						\
							Compressed-Storage/SparseMatrixForm.o						\
							vectest.o

#Set up the MODS so it contains the same as OBJS but with the .o replaced by .mod
MODS= $(OBJS:.o=.mod)

#Set the executable name
EXEC=vectest

#Set up a variable to represent the makefile
DEFAULT=makefile

# Compiler options
MY_OPTIONS    =           -fcheck=bounds -ffree-line-length-800 -g

#Set directories to look for source files, etc
VPATH = $(SCRIPTS_PATH)

#Default make command requires the executable to be up to date
all : $(EXEC)

#Objects required to be updated if the makefile has changed
%.o : $(DEFAULT)

#Object files required to be updated if corresponding .F90 files have changed
%.o : %.f90
	$(FC) $(MY_OPTIONS) $(FFLAGS) -c -o $@ $<

#Object files required to be updated if corresponding .f files have changed
%.o : %.f
	$(FC) $(FFLAGS) -c -o $@ $<

#Object files required to be updated if corresponding .c files have changed
%.o : %.c
	$(CC) $(CFLAGS) -c -o $@ $<

#For the executable to be up to date the object files must be up to date. Then link the objects
$(EXEC): $(OBJS)
	$(LD) $^ $(LFLAGS) -g -o $@

#Clean the directory and any directories searched
.PHONY : clean
clean :
	rm -f $(OBJS) $(EXEC) $(MODS) *.o *.mod
	rm -f $(VPATH)/$(OBJS) $(VPATH)/$(MODS)
