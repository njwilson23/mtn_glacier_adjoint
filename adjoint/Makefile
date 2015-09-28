ifndef F90C
	F90C=gfortran
endif

RTSUPP=w2f__types OAD_active OAD_cp OAD_tape OAD_rev

adjoint: $(addsuffix .o, $(RTSUPP) iaddr) kees_model.pre.xb.x2w.w2f.post.o ad_rev_model.f90
	${F90C} -o $@ $^

kees_model.pre.xb.x2w.w2f.post.f90 $(addsuffix .f90, $(RTSUPP)) iaddr.c : toolChain 

toolChain : kees_model.f90
	openad -c -m rj $<

%.o : %.f90
	${F90C} -o $@ -c $< 

%.o : %.c
	${CC} -o $@ -c $< 

clean: 
	rm -f ad_template* ad_inline.f OAD_* w2f__*  iaddr* 
	rm -f kees_model.pre.* *.B *.xaif *.o *.mod run_ad *~ 

.PHONY: clean toolChain
