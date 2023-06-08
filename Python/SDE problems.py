#!/usr/bin/env python
# coding: utf-8

# In[19]:


#PC-Exercise 1.3.1

import numpy as np
import numpy.random as rd
from scipy import stats
import matplotlib.pyplot as plt

def f(U1, rSqrd):
    return U1*np.sqrt(-2*np.log(rSqrd)/rSqrd)

def simul(N):
    out = np.zeros(N)
    for i in range(N):
        rSqrd = 2
        while rSqrd > 1:
            U1 = rd.uniform(1,-1)
            U2 = rd.uniform(1,-1)
            rSqrd = U1**2 + U2**2
        X = f(U1,rSqrd)
        out[i] = X
    return out
            
#print(U1,U2)
print(simul(10))


X = simul(10**4)
plt.figure
plt.hist(X, bins=100,density=True)
xx = np.linspace(-4,4,100)
plt.plot(xx,stats.norm.pdf(xx))
plt.xlim(-4,4)


# In[69]:


#PC-Exercise 1.4.4

randomList = []
for i in range(0,10**5):
    n = rd.rand()
    randomList.append(n)
#print(sorted(randomlist))
plt.hist(sorted(randomList), bins=[round((-0.05+ n*0.05), 2) for n in range(0,23)])
plt.show

mean = sum(randomList)/len(randomList)


stdev= sum([(x - mean)**2 for x in randomList])/(len(randomList)-1)
print(mean,stdev)


# In[59]:


#PC-Exercise 1.4.5

randomList = []
for i in range(0,10**5):
    n = rd.exponential(scale=1/2)
    randomList.append(n)
#print(sorted(randomlist))

plt.hist(sorted(randomList), bins=[round((n*0.05), 2) for n in range(0,41)])
plt.show

mean = sum(randomList)/len(randomList)


stdev= sum([(x - mean)**2 for x in randomList])/(len(randomList)-1)
print(mean,stdev)


# In[67]:


#PC-Exercise 1.4.6

randomList = []
for i in range(0,10**5):
    n = rd.normal()
    #print(n)
    randomList.append(n)
#print(sorted(randomlist))

plt.hist(sorted(randomList), bins=[round(-2.5+(n*0.05), 2) for n in range(0,101)])
plt.show

mean = sum(randomList)/len(randomList)


stdev= sum([(x - mean)**2 for x in randomList])/(len(randomList)-1)
print(mean,stdev)


# In[87]:


#PC-Exercise 1.4.10

randomList = []
for i in range(0,10**6):
    n = rd.exponential(scale=2)
    randomList.append(n)
#print(sorted(randomlist))

mean = sum(randomList)/len(randomList)

for i in range(5):
    print(np.mean([x for x in randomList if x>=i]))
    
    
# biggerThanOne = np.mean([x for x in randomList if x>=1])
# biggerThanTwo = np.mean([x for x in randomList if x>=2])
# biggerThanThree = np.mean([x for x in randomList if x>=3])
# biggerThanFour = np.mean([x for x in randomList if x>=4])

stdev= sum([(x - mean)**2 for x in randomList])/(len(randomList)-1)
#print(mean,stdev)


# In[ ]:


#PC-Exercise 1.4.12

from numpy import random, sqrt, log, sin, cos, pi






def box_muller_sample(n=1):
    
    u1 = rd.rand(n)
    u2 = rd.rand(n)
    
    r = sqrt(-2*log(u1))
    x = cos(2*pi*u2)
    y = sin(2*pi*u2)
    z1 = r*x
    z2 = r*y
    
    return z1, z2


# h>0
def gausRandPair(mu,h):
    


# In[ ]:


#PC-Exercise 1.4.13


# In[121]:


#PC-Exercise 1.5.3
# Yn = X + Zn


def Y(n):
    dataSet = np.zeros(n)
    #print(dataSet)
    for x in range(1,n+1):
        dataSet[x-1] = rd.rand() + rd.normal(0,scale = 1/x)
    return dataSet
z = Y(10**6)


plt.hist(z,bins=1000,density=True)
plt.xlim([-.2,1.2])
plt.show()




# In[192]:


#PC-Exercise 1.8.2


#print(randVector)

def wienerProcess(T,N):
    rd.seed(24096)
    randVector = rd.normal(scale=T/N, size=N)
    x = np.linspace(start = 0,stop=T,num=N)
    for i in range(1,len(randVector)):
        randVector[i] = randVector[i] + randVector[i-1]
    return (x,randVector)

#plt.plot(wienerProcess(100,10000)[0],wienerProcess(100,10000)[1])


# In[ ]:


#1.8.6

def compDerivative(T,N,L):
    h = [np.round(0.1*x,decimals =1) for x in range(1,11)]
    proc = wienerProcess(T,N)[1]
    xvals = wienerProcess(T,N)[0]
    index = np.where(xvals==L)[0][0]
    diff = 
    for x in range(0,len(h)):
        diff[x] = (proc[index+h]-proc[index])/h
    print(h,diff)
    
    plt.plot(h,diff)
compDerivative(100,1001,60)

