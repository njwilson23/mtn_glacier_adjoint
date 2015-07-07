fort=gfortran
flags=-ffree-form -pedantic -Wall

# Compile the forward model
fwd_model: kees_model.f90 fwd_model.f90
	$(fort) $(flags) $^ -o fwd_model

# Compile the brute force TLM
brute_force: kees_model.f90 brute_force.f90
	$(fort) $(flags) $^ -o brute_force

# Compile the tangent linear model
tlm_model: w2f__types.o OAD_active.o kees_model.pre.xb.x2w.w2f.post.o tlm_model.o
	$(fort) $^ -o tlm_model

# Run the automatic openad script
toolchain: kees_model.f90
	openad -c -m f $<

%.o: %.f90
	$(fort) -c $< -o $@

clean:
	rm fwd_model *.o *.log~ *.B *.xaif *mod-whirl

.PHONY: clean toolchain
