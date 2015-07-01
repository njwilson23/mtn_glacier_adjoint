fort=gfortran

all: kees_model kees_model.f90

kees_model:
	$(fort) kees_model.f90 -o kees_model

clean:
	rm kees_model
