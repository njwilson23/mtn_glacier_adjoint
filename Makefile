fort=gfortran
flags=-ffree-form -pedantic -Wall

# Compile the forward model
fwd_model: kees_model.f90 fwd_model.f90
	$(fort) $(flags) $^ -o fwd_model

# Compile the brute force TLM
fd_model: kees_model.f90 fd_model.f90
	$(fort) $(flags) $^ -o fd_model 

# Compile the tangent linear model
tlm_model: w2f__types.o OAD_active.o kees_model.pre.xb.x2w.w2f.post.o tlm_model.o
	$(fort) $^ -o tlm_model

# Compile the adjoint model
adj_model: w2f__types.o OAD_active.o kees_model.pre.xb.x2w.w2f.post.o tlm_model.o
	$(fort) $^ -o adj_model

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

.PHONY: clean toolchain
