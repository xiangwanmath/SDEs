import time as time
import numpy as np
import pandas as pd
# import matplotlib.pyplot as plt


# Main Calculation
# 

def linear_SDE_MET(mu, sigma, a, b, N_BM, N_time_step):

# Calculation of Mean Exit Time for Linear SDE:
# dX = mu X dt + sigma X dW
#
# [a,b]: the domain of interest
# N_BM: number of Brownion Motion
# SDEsolutions_T = 100 is set to make sure the exit happens
# N_time_step: number of time step
#
    start = time.time()

    # Parameters
    SDEsolutions_mu = mu
    SDEsolutions_sigma = sigma
    lowerbound = a
    upperbound = b

    SDEsolutions_T = 100
    dt = SDEsolutions_T / N_time_step
    sqrtdt = np.sqrt(dt)

    ic_N_BM = 50
    IC_array = np.linspace(lowerbound, upperbound, ic_N_BM * 3)
    max_steps = N_time_step * 10   # to make sure no infinite iteration

    # the first 20% and last 20% has `ic_N_BM` many nodes
    # the middle 60% has `ic_N_BM` many nodes

    # length = upperbound-lowerbound
    # ic1 = np.linspace(lowerbound,lowerbound+(length*0.2),int(ic_N_BM))
    # ic2 = np.linspace(lowerbound+(length*0.2),upperbound-(length*0.2),int(ic_N_BM))
    # ic3 = np.linspace(upperbound-(length*0.2),upperbound,int(ic_N_BM))

    # # append all nodes together; avoid double couting at the matching nodes
    # # by ignoring the last element in ic1 and first element in ic3
    # IC_array = np.append( np.append(ic1[0:-1] , ic2) , ic3[1:])

    ############# Main Calculation #############
    meanexittime = np.zeros(len(IC_array))
    for i in range(len(IC_array)):
        # print("Hello", i)
        IC = IC_array[i]
        # SDEsolution = IC

        exittimes_array = np.zeros(N_BM)
        # t_values = np.zeros(N_BM)
        for k in range(N_BM):
            n_steps = 0
            WP = 0
            SDEsolution = IC

            while SDEsolution < upperbound and SDEsolution > lowerbound:
                if n_steps == max_steps:
                    print("Too many steps")
                    break
                WP = WP + np.random.normal(loc=0,scale=sqrtdt)
                t_value = dt*n_steps
                SDEsolution = IC_array[i] * np.exp((SDEsolutions_mu-0.5*SDEsolutions_sigma**2)* t_value + SDEsolutions_sigma*WP)
                # print(WP,t_value,SDEsolution)

                n_steps += 1

    #         t_values[k] = dt*n_steps
            exittimes_array[k] = dt*n_steps
        # print(exittimes_array)

        df = pd.DataFrame({'exittimes': exittimes_array})
        # meanexittime[i] = df.loc[df['exittimes'] < max_steps, 'exittimes'].mean() # this never happens
        meanexittime[i] = df.mean()

    end = time.time()
    return IC_array, meanexittime, end - start, dt

# testing
# a=2
# b=8
# mu=1
# sigma=1
# ic, met, running_time, delta_t = linear_SDE_MET(mu, sigma, a, b, 1000, 1000)
# print(met)



# Saving Result data into CSV files
#

def MXT_data_to_CSV(IC_array, meanexittime, mu, sigma, a, b, dt, running_time):
    file_name = f"{mu}-{sigma}-{a}-{b}-{dt}-new.csv"

    data = np.array([IC_array, meanexittime])
    data = data.T  # transpose to column vectors
    np.savetxt(file_name, data, delimiter=',', fmt='%.10f', header='IC,MeanExitTime',
               comments = f"running_time,{running_time}\ndt,{dt}\n")

# MXT_data_to_CSV(ic, met, mu, sigma, a, b, delta_t, running_time)



# Plotting and Graph Saving
#

def MXT_plot(IC_array, meanexittime, mu, sigma, lowerbound, upperbound, dt, running_time):
# This plot the MXT together with the theoretic curve
    plt.plot(IC_array, meanexittime, label = "simulation")


    ## theoretic result
    ic = IC_array
    sde_sigma = sigma
    sde_mu = mu
    u = (1/(0.5*sde_sigma**2 - sde_mu)) * (np.log(ic/lowerbound) - ((1 - (ic/lowerbound)**(1 - (2*sde_mu)/sde_sigma**2))/(1 - (upperbound/lowerbound)**(1 - (2*sde_mu)/sde_sigma**2)))*np.log(upperbound/lowerbound))
    plt.plot(ic,u, label = "theoretic")

    ## cosmetic
    plt.xlabel("Initial Value")
    plt.ylabel("MeanExitTime")
    title_line1 = f"Linear SDE $dX = \mu X(t)dt + \sigma X(t)dW$ with $\mu$ = {mu}, $\sigma$ = {sigma}\n"
    title_line2 = f"Simulation parameters: dt = {dt}; grid number of IC = {len(IC_array)}\n"
    title_line3 = f"Time cost = {running_time} seconds"
    plt.title( title_line1 + title_line2 + title_line3)

    plt.legend()
    plt.grid()
    # plt.show()

    plt.savefig(f"Linear_mu={mu}_sigma={sigma}_dt={dt}.png")




def main():
    # SDEsolutions_T = 100 in the first function by default

    mu = 1.0
    sigma = 1.0
    a = 2.0
    b = 8.0
    N_BM = int(1e3)

    for i in range(3,5):
        N_time_step = 10**i

        (IC, MET, running_time, delta_t) = linear_SDE_MET(mu, sigma, a, b, N_BM, N_time_step)
#        MXT_plot(IC, MET, mu, sigma, a, b, delta_t, running_time)
        MXT_data_to_CSV(IC, MET, mu, sigma, a, b, delta_t, running_time)
        print(f"{running_time}s\n")



main()
