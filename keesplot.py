#! /usr/bin/env python
import os
import sys
import matplotlib.pyplot as plt

# os.system("gfortran kees_model.f90 -o kees_model")
# os.system("./kees_model")

def plot_data(d):
    x = d[:50]
    b = d[50:100]
    s = d[100:]
    plt.plot(x, b, "-k")
    plt.plot(x, s, "-c")
    return

for fnm in sys.argv[1:]:
    with open(fnm) as f:
        d = [float(a) for a in f.read().split()]
        plot_data(d)

plt.tight_layout()
plt.show()
