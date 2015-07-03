fort=gfortran
flags=-ffree-form -pedantic -Wall

kees_model: kees_model.f90
	$(fort) $(flags) kees_model.f90 -o kees_model

tlm_model: w2f__types.o OAD_active.o kees_model.pre.xb.x2w.w2f.post.o tlm_driver.o
	$(fort) $^ -o $@

%.o: %.f90
	$(fort) -c $< -o $@

clean:
	rm kees_model *.o *.log~
