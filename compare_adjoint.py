import matplotlib.pyplot as plt
import numpy as np

fd = np.fromfile('dvol_dsmr.dat', dtype=np.float64, sep=" ")
adj = np.fromfile('adjoint/dvol_dsmr.dat', dtype=np.float64, sep=" ")

plt.subplot(2, 1, 1)
plt.plot(fd, label='finite difference sensitivities')
plt.plot(adj, label='adjoint sensitivities')
plt.legend(loc='best')

plt.subplot(2, 1, 2)
plt.plot(adj-fd)
plt.show()
