import matplotlib.pyplot as plt
from scipy import signal
import numpy as np
import random
def work():
    I = 5e-9 # nA Constant input current
    C = 1e-9 # nF farads Capacitance
    Rm = 10e6 # MOhms Leak/Membrane Resistance
    tau_m = Rm * C # Membrane time constant
    # u(t) membrane potential at time t
    dt = 0.002 #2 ms time step

    T_stop = 0.1 # 1 s simulation
    T=np.arange (0, T_stop, 0.002)
    T=np.ndarray.tolist(T)
    U_th = -0.040 # -40 mV potential threshold
    U_reset = -0.080 # -80 mV reset potential
    U_rest = -0.075# -75 mV Rest potential

    U_m = [U_reset] # membrane potential


    tau_rest = 0.004 # (ms) duration resting state period
    ref = 0.0 #resting state counter

    for t in range(len(T)):
        if ref >= tau_rest:
            if U_m[t] > U_th:
                ref = 0.0
                list.append(U_m,U_reset)
            else: 
                list.append(U_m,U_m[t] + dt*(-(U_m[t]) + Rm*I) / tau_m)
        else :
            list.append(U_m,U_rest)
            ref = ref + dt


    list.remove(U_m,U_m[len(U_m)-1])

    plt.plot(T,U_m)

    plt.show()

work()