fort=gfortran

kees_model:
	$(fort) -ffree-form -pedantic -Wall kees_model.f90 -o kees_model

clean:
	rm kees_model
