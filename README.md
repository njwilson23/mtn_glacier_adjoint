# Mountain glacier automatic differentiation

This implements a [numerical mountain glacier model](http://websrv.cs.umt.edu/isis/index.php/Kees%27_assignment).

## Build

`make fwd_model` builds the forward model that calculates glacier volume. The accessory program `keesplot.py` can be used to draw the glacier profiles computed by the forward model.

`make toolchain` performs the AD

`make tlm_model` builds the tangent linear model to calculate volume sensitivity to mass balance perturbations

`make brute_force` builds a program that runs the forward model and used finite differences to estimate sensitivities
