fort=gfortran
flags=-ffree-form -pedantic -Wall
reverse_obj = OAD_tape.o OAD_rev.o OAD_active.o OAD_cp.o

all: fwd_model finite_diff

fwd_model: kees_model.o fwd_model.f90
	$(fort) $(flags) $^ -o fwd_model

finite_diff: kees_model.o finite_diff.f90
	$(fort) $(flags) $^ -o finite_diff

## Compile the tangent linear model using forward mode
#ad_forward: w2f__types.o OAD_active.o kees_model.pre.xb.x2w.w2f.post.o ad_fwd_model.o
#	$(fort) $^ -o ad_fwd_model
#
## Compile the tangent linear model using reverse mode
#ad_reverse: w2f__types.o $(reverse_obj) kees_model.pre.xb.x2w.w2f.post.o ad_rev_model.o
#	$(fort) $^ -o ad_rev_model
#
## Run the automatic openad script (forward mode)
#kees_model.pre.xb.x2w.w2f.post.f90: kees_model.f90
#	openad -c -m f $<
#
## Run the automatic openad script (forward mode)
#ad_f: kees_model.f90
#	openad -c -m f $<
#
## Run the automatic openad script (reverse mode)
#ad_rj: kees_model.f90
#	openad -c -m rj $<

%.o: %.f90
	$(fort) -c $< -o $@

clean:
	rm *.o *.log~ *mod-whirl kees_model.pre* w2f__types* OAD_* oad_* iaddr.c
	rm ad_inline.f ad_f.f90 ad_template.f
	rm fwd_model
	rm ad_fwd_model

.PHONY: clean all

