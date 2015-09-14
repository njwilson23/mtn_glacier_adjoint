fort=gfortran
flags=-ffree-form -pedantic -Wall
reverse_obj = OAD_tape.o OAD_rev.o OAD_cp.o

# Compile the forward model
fwd_model: kees_model.f90 fwd_model.f90
	$(fort) $(flags) $^ -o fwd_model

# Compile the brute force TLM
fd_model: kees_model.f90 fd_model.f90
	$(fort) $(flags) $^ -o fd_model 

# Compile the tangent linear model using forward mode
ad_forward: w2f__types.o OAD_active.o kees_model.pre.xb.x2w.w2f.post.o ad_fwd_model.o
	$(fort) $^ -o ad_fwd_model

# Compile the tangent linear model using reverse mode
ad_reverse: w2f__types.o $(reverse_obj) OAD_active.o kees_model.pre.xb.x2w.w2f.post.o ad_rev_model.o
	$(fort) $^ -o ad_rev_model

# Run the automatic openad script (forward mode)
kees_model.pre.xb.x2w.w2f.post.f90: kees_model.f90
	openad -c -m f $<

# Run the automatic openad script (forward mode)
ad_f: kees_model.f90
	openad -c -m f $<

# Run the automatic openad script (reverse mode)
ad_rj: kees_model.f90
	openad -c -m rj $<

%.o: %.f90
	$(fort) -c $< -o $@

clean:
	rm fwd_model *.o *.log~ *.B *.xaif *mod-whirl

.PHONY: clean toolchain ad_f ad_rj 

