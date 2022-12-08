FC=mpiifort
FLAGS= -c -O3
SUFFIX= 
LF = -warn all 
#LF =  -q64  
HOME = /home/lzx/zz/module_of_matrix
LIBS= $(HOME)/Modules/modules_90.a \
      $(HOME)/MyEis/libeis.a \
      $(HOME)/MyNag/libnag.a \
      $(HOME)/MyLin/liblin.a \
      $(HOME)/Ran/libran.a \
#      $(HOME)/LaPack/lapack.a \
$      $(HOME)/Blas/libblas.a
LIBS+= -mkl
all:
	cp $(HOME)/Modules/*.mod . ;\
	(make -f Compile  FC="$(FC)" `mpiifort -showme:compile`  LF="$(LF)" FLAGS="$(FLAGS)"  LIBS="$(LIBS)" SUFFIX="$(SUFFIX)") 
clean: 	
	(make -f Compile  clean );\
	rm *.mod
