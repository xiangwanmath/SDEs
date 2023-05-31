#!/usr/bin/env python
# coding: utf-8

# In[34]:


import matplotlib.pyplot as plt
import numpy as np
import math

def factorial(n):
    if (n == 1) or (n==0):
        return 1
    else:
        return int(n*factorial(n-1))
#print(factorial(0))


def binomial(n,p):
    pX = np.array([])
    for i in range(0,n+1):
        pX = np.append(pX,((1-p)**(n-i))*(p**i)*factorial(n)/(factorial(i)*factorial(n-i)))
        #print(pX)
    plt.step([i for i in range(0, n+1)], pX)
    #plt.bar(pX, density=True, bins=n)
    plt.show
    pass

#binomial(10,0.5)


def geometric(p):
    pXN = np.array([])
    #print(pXN)
    for n in range(1,1000):
        pXN = np.append(pXN,(1-p)**(n-1)*p)
        #print(pXN)
    plt.step([i for i in range(1,1000)], pXN)
    plt.show
    plt.xlim([0,100])
    pass
#geometric(0.5)


def poisson(lam):
    pXN = np.array([])
    #print(pXN)
    for i in range(0,101):
        pXN = np.append(pXN,math.exp(-lam)*(lam**i)/factorial(i))
        #print(pXN)
    plt.step([i for i in range(0,101)], pXN)
    plt.show
    plt.xlim([0,15])
    pass

#poisson(5)

def uniform(a,b):
    plt.axhline(y= 1/(b-a))
    plt.xlim([a,b])
    plt.show
    pass

uniform(2,6)

def expoDist(lam):
    x = np.linspace(start = 0,stop=1,num=10)
    print(x)
    fx= np.array([])
    for i in x: 
        fx = np.append(fx,math.exp((-lam*i))*lam)
        #print(fx)
    plt.plot(x,fx)
    plt.xlim([0,1])
    plt.show
    pass

expoDist(6)

def gamma(lam,alpha):
    x = np.linspace(start = 0,stop=50,num=100)
    #print(x)
    fx= np.array([])
    for i in x: 
        fx = np.append(fx,math.exp((-lam*i))*lam*((lam*i)**(alpha-1))/factorial(alpha-1))
        #print(fx)
    plt.plot(x,fx)
    plt.xlim([0,10])
    plt.show
    pass

gamma(2,4)

def normaldist(mu,sig):
    x = np.linspace(start = mu-(6*sig),stop=mu+(6*sig),num=2000)
    #print(x)
    fx= np.array([])
    for i in x: 
        fx = np.append(fx, math.exp(-((i-mu)**2)/2*(sig**2))*(1/(sig*math.sqrt(2*math.pi))))
        #print(fx)
    plt.plot(x,fx)
    pass

#normaldist(6,1)


# In[ ]:





# In[ ]:




