fort=gfortran
flags=-ffree-form -pedantic -Wall

kees_model: kees_model.f90
	$(fort) $(flags) kees_model.f90 -o kees_model

clean:
	rm kees_model
